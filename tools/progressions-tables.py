#!/usr/bin/env python3
"""Turn a capture into transition tables.

    ./progressions-tables.py run.jsonl

Reads the JSONL written by progressions-capture.py and reports, for each chord
type observed as a PREDECESSOR, which chords followed it and how often — the
tables described in docs/kb/reference/progressions-generator.md.

Recognition is exact rather than scored, and can afford to be: Progressions
pins the root as the bottom note of every chord ("the root note stays fixed as
the bottom note of any chord"), so the ROOT is given by the lowest note and
only the interval set has to be matched. That is why the capture keeps MIDI
numbers and not just pitch classes.

The ping-pong is the one wrinkle, and not a small one. With Auto Reset the app
plays a palette forward, turns at the last chord, walks back to the root, and
only then regenerates — so HALF the observed pairs are reversed and are not
successor relations at all. Measured on a two-minute run: 91% of distinct edges
also occurred backwards.

`backward_mask` undoes it. A turn is a chord flanked by the same chord on both
sides, and the mirror it opens is followed outward for as long as it holds, so
a backward run is found by its own symmetry rather than by assuming a palette
size. On that same run it takes the reversed edges from 91% to ZERO and keeps
exactly half the transitions, which is what a there-and-back should give.

The turn at the ROOT end is invisible — the app regenerates there, so the
chords either side differ — which is why only the far turn is detected, and why
that is enough.
"""
import argparse
import json
import sys
from collections import Counter, defaultdict

# --- the palette, mirroring Harmonia.Palette -------------------------------

QUALITY = {
    "Maj": [0, 4, 7], "Min": [0, 3, 7], "Dim": [0, 3, 6], "Aug": [0, 4, 8],
    "Maj7": [0, 4, 7, 11], "Min7": [0, 3, 7, 10], "Dom7": [0, 4, 7, 10],
    "HalfDim": [0, 3, 6, 10], "FullyDim": [0, 3, 6, 9], "MinMaj7": [0, 3, 7, 11],
    "AugMaj7": [0, 4, 8, 11], "MajFlat5": [0, 4, 6], "Min7Sharp5": [0, 3, 8, 10],
    "Quartal3": [0, 5, 10], "Quartal4": [0, 5, 10, 15],
    "Mystic": [0, 2, 4, 6, 9, 10],
}
DEGREE = {1: 0, 2: 2, 3: 4, 4: 5, 5: 7, 6: 9, 7: 11, 9: 14, 11: 17, 13: 21}

def apply(acc, t):
    kind, n = t
    if kind == "NoFifth":
        return [x for x in acc if x not in (6, 7, 8)]
    if kind == "NoThird":
        return [x for x in acc if x not in (3, 4)]
    iv = DEGREE[n]
    if kind == "Add":   return [iv % 12] + acc
    if kind == "Sharp": return [(iv + 1) % 12] + acc
    if kind == "Flat":  return [(iv + 11) % 12] + acc
    if kind == "Sus":   return [iv % 12] + [x for x in acc if x not in (3, 4)]
    raise ValueError(kind)

def pcs_of(quality, tensions):
    acc = list(QUALITY[quality])
    for t in tensions:
        acc = apply(acc, t)
    return tuple(sorted({x % 12 for x in acc}))

A = lambda n: ("Add", n)
S = lambda n: ("Sharp", n)
F = lambda n: ("Flat", n)
U = lambda n: ("Sus", n)
N5, N3 = ("NoFifth", 0), ("NoThird", 0)

PALETTE = [
    ("", "Maj", []), ("m", "Min", []),
    ("sus2", "Maj", [U(2)]), ("sus4", "Maj", [U(4)]), ("aug", "Aug", []),
    ("dim", "Dim", []), ("7(no5)", "Dom7", [N5]), ("m7(no5)", "Min7", [N5]),
    ("M6", "Maj", [A(6)]), ("m6", "Min", [A(6)]), ("7", "Dom7", []),
    ("m7", "Min7", []), ("°7", "FullyDim", []), ("ø7", "HalfDim", []),
    ("7sus4", "Dom7", [U(4)]), ("M7", "Maj7", []), ("7#5", "Aug", [F(7)]),
    ("M7b5", "MajFlat5", [A(7)]), ("mM7", "MinMaj7", []),
    ("9", "Dom7", [A(9)]), ("9b5", "MajFlat5", [F(7), A(9)]), ("m9", "Min7", [A(9)]),
    ("m9b5", "HalfDim", [A(9)]), ("M7+9", "Maj7", [A(9)]), ("M6+9", "Maj", [A(6), A(9)]),
    ("m6+9", "Min", [A(6), A(9)]), ("mM9", "MinMaj7", [A(9)]),
    ("9sus4", "Dom7", [U(4), A(9)]), ("9sus4+13", "Dom7", [U(4), A(9), A(13)]),
    ("M9b5", "MajFlat5", [A(7), A(9)]),
    ("9#11", "Dom7", [A(9), S(11)]), ("m11", "Min7", [A(9), A(11)]),
    ("M11", "Maj7", [A(9), A(11)]), ("M9#11", "Maj7", [A(9), S(11)]),
    ("°11", "FullyDim", [A(9), A(11)]), ("13", "Dom7", [A(9), A(13)]),
    ("M9+13", "Maj7", [A(9), A(13)]), ("q3", "Quartal3", []), ("q4", "Quartal4", []),
    ("Mystic", "Mystic", []),
]

BY_PCS = defaultdict(list)
for sfx, q, ts in PALETTE:
    BY_PCS[pcs_of(q, ts)].append(sfx)

NAMES = ["C", "Db", "D", "Eb", "E", "F", "Gb", "G", "Ab", "A", "Bb", "B"]


MAX_MISSING = 2


def recognise(notes):
    """(root pc, suffix), or (bass, None) when nothing in the palette fits.

    The bass is the root MOST of the time — the source pins it there for open
    voicings — but not always, and two documented behaviours break a naive
    bass-is-root lookup:

      * INVERSIONS. `Inv` is a control on the pad, so a captured [0,5,9] with C
        in the bass is an F major triad in second inversion and not a chord
        rooted on C at all.
      * OMITTED NOTES. An open voicing may drop a tone — "if you see an 'x'
        within the variation, this represents a note that is not played" — so
        an observed set is often a SUBSET of its chord. A five-note Mystic
        missing its ♯11 is still a Mystic.

    So: try all twelve roots, require every observed note to belong to the
    candidate, and rank by how much was left unexplained, preferring the bass
    as root where it fits. Capped at two missing tones, because a triad is a
    subset of almost everything and would otherwise match the whole palette.
    """
    pcs = {n % 12 for n in notes}
    bass = min(notes) % 12
    best = None
    for root in range(12):
        ivs = frozenset((p - root) % 12 for p in pcs)
        for key, names in BY_PCS.items():
            full = frozenset(key)
            if not ivs <= full:
                continue
            missing = len(full - ivs)
            if missing > MAX_MISSING or missing >= len(ivs):
                continue
            score = (missing, 0 if root == bass else 1, len(full))
            if best is None or score < best[0]:
                best = (score, root, names[0])
    return (best[1], best[2]) if best else (bass, None)


def backward_mask(seq):
    """Which chords sit in the backward half of a ping-pong.

    A turn is `seq[t-1] == seq[t+1]`; from there the mirror is followed outward
    while it holds. Self-verifying: if the mask is right, no surviving edge
    should occur in both directions.
    """
    back = [False] * len(seq)
    for t in range(1, len(seq) - 1):
        if seq[t - 1] != seq[t + 1]:
            continue
        k = 1
        while t - k >= 0 and t + k < len(seq) and seq[t - k] == seq[t + k]:
            back[t + k] = True
            k += 1
    return back


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("path", nargs="+")
    ap.add_argument("--min-notes", type=int, default=3,
                    help="ignore events thinner than this (stray notes)")
    args = ap.parse_args()

    # Each file is its own stream; concatenating them would invent a
    # transition across every join and confuse the mirror detection.
    runs = [[json.loads(l) for l in open(path) if l.strip()] for path in args.path]
    runs = [[r for r in run if len(r["notes"]) >= args.min_notes] for run in runs]
    print(f"{sum(map(len, runs))} chords in {len(runs)} run(s)")

    reads = [[recognise(r["notes"]) for r in run] for run in runs]
    read = [c for run in reads for c in run]
    chords = [c for run in runs for c in run]
    unknown = [r for r in read if r[1] is None]
    print(f"recognised {len(read) - len(unknown)}/{len(read)} "
          f"({100 * (len(read) - len(unknown)) // max(1, len(read))}%)")
    if unknown:
        c = Counter(tuple(sorted({(n - min(r['notes'])) % 12 for n in r['notes']}))
                    for r, k in zip(chords, read) if k[1] is None)
        print("  most common unrecognised interval sets:")
        for ivs, n in c.most_common(6):
            print(f"    {ivs}  x{n}")

    # Successor counts, keyed on the PREVIOUS chord's type. An edge counts
    # only when NEITHER end sits inside a mirrored run.
    back = [b for run in reads for b in backward_mask(run)]
    print(f"{sum(back)} chords inside a backward run, excluded")
    tables = defaultdict(Counter)
    pairs = 0
    for i in range(len(read) - 1):
        if back[i] or back[i + 1]:
            continue
        (r0, t0), (r1, t1) = read[i], read[i + 1]
        if t0 is None or t1 is None or (r0, t0) == (r1, t1):
            continue
        tables[t0][((r1 - r0) % 12, t1)] += 1
        pairs += 1

    print(f"\n{pairs} transitions over {len(tables)} predecessor types\n")
    for t0, succ in sorted(tables.items(), key=lambda kv: -sum(kv[1].values())):
        total = sum(succ.values())
        print(f"  {t0 or 'M':<10} {total:>4} transitions, "
              f"{len(succ):>3} distinct successors")
        for (off, t1), n in succ.most_common(8):
            print(f"        +{off:<3} {t1 or 'M':<10} x{n}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
