// Sankey data, encoded for Plotly's parallel-arrays format.
// Plotly sankey wants:
//   node: { label: string[], color: string[], x: number[], y: number[], customdata: any[] }
//   link: { source: number[], target: number[], value: number[], color: string[], customdata: any[] }
// where source/target are indices into the node arrays.

export const palette = {
  blue: "#1f77b4",
  orange: "#ff7f0e",
  // semi-transparent ribbon fills
  blueLink: "rgba(31,119,180,0.4)",
  orangeLink: "rgba(255,127,14,0.4)",
};

export const formatValue = (n: number): string => {
  if (n >= 1_000_000) return (n / 1_000_000).toFixed(1).replace(/\.0$/, "") + "M";
  if (n >= 1_000) return Math.round(n / 1_000) + "K";
  return String(n);
};

type NodeDef = {
  label: string;
  color: string;
  x: number;   // 0..1
  y: number;   // 0..1
  navigateTo?: string;
};

// Order matches the screenshot, columns left → right.
// x/y are normalized; Plotly clamps near-edges, so avoid exact 0 / 1.
const nodes: NodeDef[] = [
  // column 0
  { label: "Newly Summoned",        color: palette.blue,   x: 0.01, y: 0.10 },
  { label: "Postponed In",          color: palette.blue,   x: 0.01, y: 0.92 },
  // column 1
  { label: "Summoned",              color: palette.blue,   x: 0.18, y: 0.40, navigateTo: "Summoning Jurors" },
  // column 2
  { label: "Qualified & Available", color: palette.blue,   x: 0.38, y: 0.18 },
  { label: "Unavailable",           color: palette.orange, x: 0.38, y: 0.85 },
  // column 3
  { label: "In Person",             color: palette.blue,   x: 0.58, y: 0.10, navigateTo: "Jurors In Court" },
  { label: "On Call",               color: palette.orange, x: 0.58, y: 0.55 },
  // column 4
  { label: "Sent For Selection",    color: palette.orange, x: 0.78, y: 0.07 },
  { label: "Not Selected",          color: palette.orange, x: 0.78, y: 0.32 },
  // column 5
  { label: "Sworn on Jury",         color: palette.blue,   x: 0.99, y: 0.05 },
  { label: "Released",              color: palette.orange, x: 0.99, y: 0.18 },
];

const idx = (label: string) => nodes.findIndex((n) => n.label === label);

type LinkDef = { source: string; target: string; value: number };

const linkDefs: LinkDef[] = [
  { source: "Newly Summoned",        target: "Summoned",              value: 10_800_000 },
  { source: "Postponed In",          target: "Summoned",              value: 911_000 },
  { source: "Summoned",              target: "Qualified & Available", value: 5_200_000 },
  { source: "Summoned",              target: "Unavailable",           value: 6_500_000 },
  { source: "Qualified & Available", target: "In Person",             value: 1_600_000 },
  { source: "Qualified & Available", target: "On Call",               value: 3_500_000 },
  { source: "In Person",             target: "Sent For Selection",    value: 653_000 },
  { source: "In Person",             target: "Not Selected",          value: 1_000_000 },
  { source: "Sent For Selection",    target: "Sworn on Jury",         value: 102_000 },
  { source: "Sent For Selection",    target: "Released",              value: 551_000 },
];

// Ribbon color follows the *target* node's color (matches the screenshot:
// flows turn orange when they hit an orange terminal).
const linkColor = (targetLabel: string): string =>
  nodes[idx(targetLabel)].color === palette.orange ? palette.orangeLink : palette.blueLink;

export const sankey = {
  node: {
    label: nodes.map((n) => n.label),
    color: nodes.map((n) => n.color),
    x: nodes.map((n) => n.x),
    y: nodes.map((n) => n.y),
    navigateTo: nodes.map((n) => n.navigateTo ?? null),
  },
  link: {
    source: linkDefs.map((l) => idx(l.source)),
    target: linkDefs.map((l) => idx(l.target)),
    value: linkDefs.map((l) => l.value),
    color: linkDefs.map((l) => linkColor(l.target)),
    label: linkDefs.map((l) => `${l.source} → ${l.target}`),
    formatted: linkDefs.map((l) => formatValue(l.value)),
  },
};
