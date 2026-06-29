// Data shape required by @nivo/sankey:
//   nodes: [{ id: string, ...anyExtras }]
//   links: [{ source: string, target: string, value: number, ...anyExtras }]
//
// Node ids match the labels used in the original Plotly sankey.

export type NivoNode = {
  id: string;            // unique id (also used as default label)
  nodeColor?: string;    // optional override (consumed via `colors` callback)
  navigateTo?: string;   // if set, node is clickable for routing
};

export type NivoLink = {
  source: string;
  target: string;
  value: number;
};

export type SankeyData = {
  nodes: NivoNode[];
  links: NivoLink[];
};

// Palette — keep in sync with the original chart.
export const palette = {
  blue: "#1f77b4",
  orange: "#ff7f0e",
  lightOrange: "#ffbb78",
};

export const juryData: SankeyData = {
  nodes: [
    { id: "Postponed In",       nodeColor: palette.blue },
    { id: "Newly Summoned",     nodeColor: palette.blue },
    { id: "Summoned",           nodeColor: palette.blue,   navigateTo: "Summoning Jurors" },
    { id: "Qualified & Available", nodeColor: palette.blue },
    { id: "Unavailable",        nodeColor: palette.orange },
    { id: "In Person",          nodeColor: palette.blue,   navigateTo: "Jurors In Court" },
    { id: "On Call",            nodeColor: palette.orange },
    { id: "Sent For Selection", nodeColor: palette.orange },
    { id: "Not Selected",       nodeColor: palette.orange },
    { id: "Sworn on Jury",      nodeColor: palette.blue },
    { id: "Released",           nodeColor: palette.orange },
  ],
  links: [
    { source: "Postponed In",          target: "Summoned",              value: 911_000 },
    { source: "Newly Summoned",        target: "Summoned",              value: 10_800_000 },
    { source: "Summoned",              target: "Qualified & Available", value: 5_200_000 },
    { source: "Summoned",              target: "Unavailable",           value: 6_500_000 },
    { source: "Qualified & Available", target: "In Person",             value: 1_600_000 },
    { source: "Qualified & Available", target: "On Call",               value: 3_500_000 },
    { source: "In Person",             target: "Sent For Selection",    value: 653_000 },
    { source: "In Person",             target: "Not Selected",          value: 1_000_000 },
    { source: "Sent For Selection",    target: "Sworn on Jury",         value: 102_000 },
    { source: "Sent For Selection",    target: "Released",              value: 551_000 },
  ],
};

export const formatValue = (n: number): string => {
  if (n >= 1_000_000) return (n / 1_000_000).toFixed(1).replace(/\.0$/, "") + "M";
  if (n >= 1_000)     return Math.round(n / 1_000) + "K";
  return String(n);
};
