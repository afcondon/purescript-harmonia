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
| `run-2026-09-14-1beat-b.jsonl` | same | continuation | — |

Each file is a separate stream and must stay that way: concatenating them would
invent a transition across the join. `progressions-tables.py` takes several
paths and keeps them apart.

## What a run is worth

Half of every capture is the Auto Reset ping-pong walking backwards and is
discarded, so 8,831 chords yielded 4,116 usable transitions. At Freedom 5 and
Extreme the distribution is flat — 33 distinct types, nothing above 10% — which
buys breadth at the cost of depth.

**A run at lower complexity would be complementary**, not more of the same:
complexity bands the successors as well as the chords, so the same data rate
lands in a smaller tree and the tables get deeper.
