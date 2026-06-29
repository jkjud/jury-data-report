import { useMemo } from "react";
import createPlotlyComponent from "react-plotly.js/factory";
// `plotly.js-dist-min` ships a prebuilt bundle (no need for plotly.js + d3 build chain).
import Plotly from "plotly.js-dist-min";
import { sankey, formatValue } from "./data";

// react-plotly.js exports a factory; we wire it to the dist bundle.
const Plot = createPlotlyComponent(Plotly as any);

type Props = {
  onNavigate?: (target: string) => void;
};

export const Sankey = ({ onNavigate }: Props) => {
  // Build node labels with the value baked in (matches screenshot: "Name\n1.6M").
  // We compute each node's total throughput (max of inflow / outflow) the way
  // Plotly does internally for hover, so the displayed number matches.
  const nodeTotals = useMemo(() => {
    const totals = sankey.node.label.map(() => ({ inflow: 0, outflow: 0 }));
    sankey.link.source.forEach((s, i) => {
      totals[s].outflow += sankey.link.value[i];
      totals[sankey.link.target[i]].inflow += sankey.link.value[i];
    });
    return totals.map((t) => Math.max(t.inflow, t.outflow));
  }, []);

  const labels = sankey.node.label.map(
    (name, i) => `${name}<br><b>${formatValue(nodeTotals[i])}</b>`
  );

  const data: any = [
    {
      type: "sankey",
      arrangement: "snap", // honor node.x / node.y while still preventing overlap
      orientation: "h",
      valueformat: ",.0f",
      // Hover behavior: Plotly's built-in hover dims non-related links, just like nivo's
      // nodeHoverOthersOpacity / linkHoverOthersOpacity.
      node: {
        label: labels,
        color: sankey.node.color,
        x: sankey.node.x,
        y: sankey.node.y,
        pad: 18,
        thickness: 18,
        line: { width: 0 },
        customdata: sankey.node.label, // raw name, used in hover
        hovertemplate:
          "<b>%{customdata}</b><br>Total: %{value:,.0f}<extra></extra>",
      },
      link: {
        source: sankey.link.source,
        target: sankey.link.target,
        value: sankey.link.value,
        color: sankey.link.color,
        customdata: sankey.link.formatted,
        hovertemplate:
          "<b>%{source.customdata} → %{target.customdata}</b>" +
          "<br>%{customdata}<extra></extra>",
      },
      textfont: {
        family: "-apple-system, system-ui, Segoe UI, Roboto, sans-serif",
        size: 12,
        color: "#1f3a5f",
      },
    },
  ];

  const layout: any = {
    margin: { l: 60, r: 90, t: 30, b: 30 },
    font: { family: "-apple-system, system-ui, Segoe UI, Roboto, sans-serif" },
    hoverlabel: { bgcolor: "#fff", bordercolor: "#999", font: { size: 12 } },
    paper_bgcolor: "white",
    plot_bgcolor: "white",
    autosize: true,
  };

  const config: any = {
    displayModeBar: false,
    responsive: true,
  };

  return (
    <Plot
      data={data}
      layout={layout}
      config={config}
      style={{ width: "100%", height: "100%" }}
      useResizeHandler
      onClick={(e: any) => {
        // Plotly fires click on a clicked node *or* link. For node clicks, the
        // event has `points[0].pointNumber` and the trace's node arrays.
        const pt = e?.points?.[0];
        if (!pt) return;
        const navIdx = pt.pointNumber;
        const navList = sankey.node.navigateTo;
        if (typeof navIdx === "number" && navList[navIdx] && onNavigate) {
          onNavigate(navList[navIdx] as string);
        }
      }}
    />
  );
};
