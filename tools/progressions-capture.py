#!/usr/bin/env python3
"""Capture chords from Progressions (or any MIDI source) as JSONL.

Measures the transition tables described in
docs/kb/reference/progressions-generator.md. The app's Replace menu shows which
chords MAY follow a given chord; a long recording shows which ones it actually
CHOOSES, and how often. Both are wanted, and only this one scales.

    ./progressions-capture.py --seconds 1200 --out run.jsonl

Needs `python-rtmidi`, which is not in the system Python. There is a venv with
it beside midi-oracle, the ecosystem's other MIDI tool:

    ../../live-coding/midi-oracle/.venv/bin/python tools/progressions-capture.py ...

SETTINGS AT THE SOURCE, all of which change what is captured:

  Block Chords mode   - an arpeggiator or strummer spreads one chord over
                        hundreds of milliseconds, and there is then no gap
                        between chords for the grouping to find.
  Bass Note OFF       - it adds a note two octaves down that is not part of
                        the chord, and it would land in every pitch-class set.
  Humanize OFF        - its timing variation is exactly what the grouping
                        window has to beat.
  Freedom 5,          - nothing filtered, every band reachable, so one run
  Complexity Extreme    covers as much of the vocabulary as possible.
  Duplicates ON       - each draw independent, which is what makes the counts
                        a distribution rather than a sampling-without-
                        replacement artefact.

Each line is one chord:

    {"t": 12.437, "notes": [43, 50, 57, 62, 65], "pcs": [2, 5, 7, 10]}

`t` is seconds from the start, `notes` the MIDI numbers as played (so the open
voicing is preserved — useful on its own for the voicing work), `pcs` the
distinct pitch classes.

Notes are grouped into a chord by arrival time. Progressions plays block chords
simultaneously, so the window only has to beat the gap between chords; 80 ms is
far tighter than any musical spacing and far looser than the few hundred
microseconds a simultaneity actually spans. Turn Humanize OFF at the source and
this is never close.
"""
import argparse
import json
import sys
import time

import rtmidi

GAP = 0.080  # seconds; a new chord starts when nothing has arrived for this long


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", default="AUDIO4c USB2",
                    help="substring of the MIDI input port name")
    ap.add_argument("--seconds", type=float, default=1200.0)
    ap.add_argument("--out", default="capture.jsonl")
    ap.add_argument("--channel", type=int, default=None,
                    help="only this channel (1-16); default all")
    args = ap.parse_args()

    midiin = rtmidi.MidiIn()
    ports = midiin.get_ports()
    match = [i for i, p in enumerate(ports) if args.port in p]
    if not match:
        print(f"no MIDI input matching {args.port!r}", file=sys.stderr)
        for i, p in enumerate(ports):
            print(f"  {i} {p!r}", file=sys.stderr)
        return 1
    midiin.open_port(match[0])
    print(f"listening on {ports[match[0]]!r} for {args.seconds:.0f}s "
          f"-> {args.out}", file=sys.stderr)

    t0 = time.perf_counter()
    pending = []        # notes of the chord being assembled
    pending_at = None   # when its first note arrived
    chords = 0

    with open(args.out, "w") as fh:
        while True:
            now = time.perf_counter() - t0
            if now >= args.seconds:
                break

            msg = midiin.get_message()
            if msg is None:
                # Nothing arriving: close the chord if it has gone quiet.
                if pending and (time.perf_counter() - t0) - pending_last > GAP:
                    chords += flush(fh, pending_at, pending)
                    pending, pending_at = [], None
                time.sleep(0.001)
                continue

            data, _delta = msg
            if len(data) < 3:
                continue
            status, note, vel = data[0], data[1], data[2]
            kind, ch = status & 0xF0, (status & 0x0F) + 1
            # Note-ON only. A note-off says when a chord ENDED, which is not
            # what a transition table is about, and mixing them in would double
            # every event and break the grouping.
            if kind != 0x90 or vel == 0:
                continue
            if args.channel is not None and ch != args.channel:
                continue

            if not pending:
                pending_at = now
            pending.append(note)
            pending_last = now

    if pending:
        chords += flush(None, pending_at, pending, args.out)
    print(f"{chords} chords captured", file=sys.stderr)
    return 0


def flush(fh, at, notes, path=None):
    notes = sorted(set(notes))
    row = {"t": round(at, 4), "notes": notes,
           "pcs": sorted({n % 12 for n in notes})}
    line = json.dumps(row) + "\n"
    if fh is not None:
        fh.write(line)
        fh.flush()
    else:
        with open(path, "a") as f:
            f.write(line)
    return 1


if __name__ == "__main__":
    sys.exit(main())
