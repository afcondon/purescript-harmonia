/* harmonia — the engraved sheet.
 *
 * Every staff on this page is genuine engraving: the ABC below is handed to
 * Verovio (WASM, self-hosted) and rendered to SVG. Each example has a
 * "show the code" popup revealing the PureScript that produces it.
 *
 *   hero  — one progression, reflavoured through three modes
 *   A     — voice leading (common tones tied)
 *   B     — a line becoming tintinnabuli (Pärt's T-voice rule)
 *   C     — progressive reharmonization (richer recipes, same skeleton)
 */

// ── hero: same voicings, three modes; only the flats differ ─────────────────
const HERO_PRE = 'X:1\nL:1/2\nM:4/4\nK:C\n';
const MODES = [
  { id: 'modeIonian',     abc: '[CEG]2 [CFA]2 | [DGB]2 [EGc]2 |]' },
  { id: 'modeMixolydian', abc: '[CEG]2 [CFA]2 | [DG_B]2 [EGc]2 |]' },
  { id: 'modeAeolian',    abc: '[C_EG]2 [CF_A]2 | [DG_B]2 [_EGc]2 |]' },
];

// ── movements: each carries its own complete ABC ────────────────────────────

// A — the same ii–V–I two ways: block-voiced (roots leap) vs voice-led (least
// motion, common tones held on pitch). Shown side by side; Verovio's ABC path
// doesn't emit ties, so the point is made by the near-stationary noteheads.
const FIG_VL_BLOCK = `
X:1
L:1/2
M:4/4
K:C
[DFAc]2 [GBdf]2 | [CEGB]4 |]
`;
const FIG_VL_LED = `
X:1
L:1/2
M:4/4
K:C
[CDFA]2 [B,DFG]2 | [B,CEG]4 |]
`;

// B — tintinnabuli: M-voice (stepwise A-minor), and its T-voice shadow (each
// note snapped to the nearest A-minor triad tone above).
const FIG_TINT_M = `
X:1
L:1/4
M:4/4
K:Am
A B c d | e d c B |]
`;
const FIG_TINT_MT = `
X:1
L:1/4
M:4/4
K:Am
V:1
c c e e | a e e c |]
V:2
A B c d | e d c B |]
`;

// C — one turnaround (I vi ii V), enriched degree by degree.
const REHARM = [
  { id: 'figReharmTriads', abc: '[CEG]2 [A,CE]2 | [DFA]2 [G,B,D]2 |]' },
  { id: 'figReharm7',      abc: '[CEGB]2 [A,CEG]2 | [DFAc]2 [G,B,DF]2 |]' },
  { id: 'figReharm9',      abc: '[CEGBd]2 [A,CEGB]2 | [DFAce]2 [G,B,DFA]2 |]' },
];
const REHARM_PRE = 'X:1\nL:1/2\nM:4/4\nK:C\n';

const baseOpts = {
  from: 'abc',
  header: 'none',
  footer: 'none',
  pageMarginTop: 4,
  pageMarginBottom: 4,
  pageMarginLeft: 4,
  pageMarginRight: 4,
  adjustPageHeight: true,
  breaks: 'none',
  svgViewBox: true,
  svgHtml5: true,
  font: 'Leipzig',
  scale: 40,
};

function paint(tk, id, abc, opts) {
  const el = document.getElementById(id);
  if (!el) return;
  tk.setOptions(Object.assign({}, baseOpts, opts || {}));
  tk.loadData(abc.trim());
  el.innerHTML = tk.renderToSVG(1);
}

// ── the reusable code modal — populated from a <template> per example ────────
function wireModal() {
  const dlg = document.getElementById('codeModal');
  if (!dlg) return;
  const title = dlg.querySelector('.code-title');
  const body = dlg.querySelector('.code-body');

  function open(key) {
    const tpl = document.getElementById('code-' + key);
    if (!tpl) return;
    title.innerHTML = tpl.dataset.title || '';
    body.replaceChildren(tpl.content.cloneNode(true));
    dlg.showModal();
  }

  document.querySelectorAll('.code-toggle').forEach((btn) =>
    btn.addEventListener('click', () => open(btn.dataset.code)));
  dlg.querySelector('.code-close').addEventListener('click', () => dlg.close());
  dlg.addEventListener('click', (e) => { if (e.target === dlg) dlg.close(); });
}

function boot() {
  const tk = new verovio.toolkit();
  MODES.forEach((m) => paint(tk, m.id, HERO_PRE + m.abc));
  paint(tk, 'figVLBlock', FIG_VL_BLOCK, { scale: 42 });
  paint(tk, 'figVLLed', FIG_VL_LED, { scale: 42 });
  paint(tk, 'figTintM', FIG_TINT_M, { scale: 40 });
  paint(tk, 'figTintMT', FIG_TINT_MT, { scale: 40 });
  REHARM.forEach((r) => paint(tk, r.id, REHARM_PRE + r.abc, { scale: 40 }));
  wireModal();
}

// The light UMD build exposes a global `verovio` whose WASM module fires
// onRuntimeInitialized once ready; guard for the already-initialised case.
if (window.verovio && verovio.module) {
  if (verovio.module.calledRun) boot();
  else verovio.module.onRuntimeInitialized = boot;
} else {
  window.addEventListener('load', () => {
    verovio.module.onRuntimeInitialized = boot;
  });
}
