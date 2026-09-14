# Captures

Raw MIDI from Progressions, one chord per line, written by
`tools/progressions-capture.py` and read by `tools/progressions-tables.py`.

Committed rather than gitignored: these are **measurements**, and the claims in
`docs/kb/reference/progressions-generator.md` and the rules in
`Harmonia.Walk` rest on them. A finding whose evidence has been deleted is an
assertion.

| run | settings | length | chords |
|---|---|---|---|
| `run-2026-09-14-1beat.jsonl` | C major, Freedom 5, Complexity Extreme, 1 beat @ 200bpm, block chords, no bass note, no humanize, duplicates on | 44 min (killed by memory pressure at 2651 s of an intended 3600) | 8,831 |
| `run-2026-09-14-1beat-b.jsonl` | same | 15 min continuation | 3,000 |

Each file is a separate stream and must stay that way: concatenating them would
invent a transition across the join. `progressions-tables.py` takes several
paths and keeps them apart.

## What the session bought

**11,831 chords → 5,522 forward transitions, 37 of 40 types, 100% recognised.**
Half of every capture is the Auto Reset ping-pong walking backwards and is
discarded.

Coverage, by observations per predecessor:

| band | types | |
|---|---|---|
| solid, ≥200 obs | 11 | `M m m7 m6+9 7#5 7sus4 M7+9 M9♭5 q4 °7 ø7` |
| usable, 80–199 | 14 | |
| thin, <80 | 12 | `sus2 sus4 aug mM7 mM9 m9♭5 9♭5 9♯11 M9♯11 °11 7(no5) m7(no5)` |
| never seen | 3 | `13 9sus4+13 M11` |

Trees run **80–120 successors wide**, rather larger than the paper's *"average
of 65"*. Saturation varies: `°7` has 101 distinct successors over 380
observations with only 12% seen once, so it is well mapped; `7♯5` has 119 over
270 with 36% seen once, and is not. At Freedom 5 and
Extreme the distribution is flat — 33 distinct types, nothing above 10% — which
buys breadth at the cost of depth.

**A run at lower complexity would be complementary**, not more of the same:
complexity bands the successors as well as the chords, so the same data rate
lands in a smaller tree and the tables get deeper.
