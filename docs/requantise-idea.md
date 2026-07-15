# The re-quantise morph: Odonus × Vetula × the Harmonia deduction engine

*Captured 2026-07-15 (Andrew, during the kibitzer JTMS/SMT thinking
session). An application idea that would make Harmonia the second
client of the JTMS-style rules kernel prototyped in kibitzer
(see kibitzer/docs/smt-jtms-brainstorm.md).*

## The pipeline

1. **Generate**: Odonus (the Cartesian multi-playhead sequencer in
   Triggerfish) plays material drawn from a harmonic exploration in
   Vetula — complex, interesting chords driving interlocking
   patterns. Multi-playhead phase machinery is mechanically rich but
   harmonically indifferent: the output is "interesting but not quite
   flowing" — Reich / Riley / Glass / Yucatán-marimba / mbira
   territory, slightly wrong.
2. **Record** the session (Amphora is the natural store).
3. **Morph**: a Harmonia deduction-engine pass re-quantises the
   recording into something more or less musical, *using rules*.

## Why this fits the JTMS shape exactly

- **Analysis is monotone deduction**: from the recording + Vetula's
  chord timeline, derive facts — active harmonic region per span,
  out-of-set tones, voice collisions, implied scale per passage.
  Saturation, not search; fast.
- **Repair with provenance**: each edit (pitch nudge to nearest
  chord/scale tone, octave fold, timing snap, suppression, tie) is
  licensed by a named rule with premises. The morphed session carries
  a derivation feed — "F3 → E3 at bar 12: active Vetula chord lacks
  F; E is the nearest chord tone (NearestChordTone, premises E-4,
  V-7)" — the kibitzer "How the watcher knows" pane, for edits.
- **The morph is a dial**: Harmonia already has grades/anchors —
  apply rules of grade ≤ k, or interpolate rule strength 0..1. The
  same recording at morph 0 (raw Odonus) → 1 (fully lawful). Because
  every edit is justified, the dial is *auditable*: scrub it like a
  kibitzer timeline. Semantic zoom for musicality.
- **Style packs as rule sets**: rules-as-data means "Species",
  "In C", "Shona mbira", "Yucatán marimba" are just different rule
  values — user-extensible at the eDSL level, property-testable
  (soundness against a pitch-set oracle; view-monotonicity has an
  analogue: adding context never invalidates a licensed edit).
- **Non-destructive and content-addressed**: source session + rule
  set + morph strength → derived artifact; hash all three and Amphora
  reproduces the morph forever.

## Sequencing

Offline pass over a recorded session first (Node or BEAM); the
incremental nature of saturation leaves the door open to a live
variant later. This is the "Harmonia scale-inference spike" from the
kibitzer brainstorm doc, grown a product: the spike prices the JTMS
kernel API, this gives it a reason to exist.
