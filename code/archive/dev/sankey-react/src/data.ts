// Node ids match the labels used in the original Plotly sankey.
// Colors: blue = "in process / advancing", orange = "exited the funnel".

export type NodeDatum = {
  id: string;
  label: string;          // display label (may be multi-line via \n)
  color: "blue" | "orange";
  value: number;          // total flow through this node
  navigateTo?: string;    // if set, node is clickable for routing
  side: "top" | "bottom"; // label placement (above or below the chart)
};

export type LinkDatum = {
  source: string;
  target: string;
  value: number;
  color: "blue" | "orange" | "lightOrange";
};

export type SankeyData = {
  nodes: NodeDatum[];
  links: LinkDatum[];
};

export const juryData: SankeyData = {
  nodes: [
    { id: "postin",        label: "Postponed In",      color: "blue",   value: 911_000,    side: "bottom" },
    { id: "newly",         label: "Newly Summoned",    color: "blue",   value: 10_800_000, side: "top" },
    { id: "summoned",      label: "Summoned",          color: "blue",   value: 11_700_000, side: "top",
      navigateTo: "Summoning Jurors" },
    { id: "qa",            label: "Qualified &\nAvailable", color: "blue", value: 5_200_000, side: "top" },
    { id: "unavailable",   label: "Unavailable",       color: "orange", value: 6_500_000,  side: "bottom" },
    { id: "inperson",      label: "In Person",         color: "blue",   value: 1_600_000,  side: "top",
      navigateTo: "Jurors In Court" },
    { id: "oncall",        label: "On Call",           color: "orange", value: 3_500_000,  side: "bottom" },
    { id: "sfs",           label: "Sent For\nSelection", color: "orange", value: 653_000,  side: "top" },
    { id: "notselected",   label: "Not Selected",      color: "orange", value: 1_000_000,  side: "bottom" },
    { id: "sworn",         label: "Sworn on\nJury",    color: "blue",   value: 102_000,    side: "top" },
    { id: "released",      label: "Released",          color: "orange", value: 551_000,    side: "bottom" },
  ],
  links: [
    { source: "postin",   target: "summoned",    value: 911_000,    color: "blue" },
    { source: "newly",    target: "summoned",    value: 10_800_000, color: "blue" },
    { source: "summoned", target: "qa",          value: 5_200_000,  color: "blue" },
    { source: "summoned", target: "unavailable", value: 6_500_000,  color: "orange" },
    { source: "qa",       target: "inperson",    value: 1_600_000,  color: "blue" },
    { source: "qa",       target: "oncall",      value: 3_500_000,  color: "orange" },
    { source: "inperson", target: "sfs",         value: 653_000,    color: "blue" },
    { source: "inperson", target: "notselected", value: 1_000_000,  color: "lightOrange" },
    { source: "sfs",      target: "sworn",       value: 102_000,    color: "blue" },
    { source: "sfs",      target: "released",    value: 551_000,    color: "orange" },
  ],
};

export const formatValue = (n: number): string => {
  if (n >= 1_000_000) return (n / 1_000_000).toFixed(1).replace(/\.0$/, "") + "M";
  if (n >= 1_000)     return Math.round(n / 1_000) + "K";
  return String(n);
};
