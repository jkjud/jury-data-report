import { useMemo, useState } from "react";
import {
  sankey as d3sankey,
  sankeyLinkHorizontal,
  sankeyJustify,
  SankeyGraph,
} from "d3-sankey";
import { SankeyData, NodeDatum, LinkDatum, formatValue } from "./data";

type SankeyProps = {
  width: number;
  height: number;
  data: SankeyData;
  onNavigate?: (target: string) => void;
};

const MARGIN = { top: 70, right: 90, bottom: 70, left: 60 };

const NODE_FILL = {
  blue: "rgba(47,117,181,0.95)",
  orange: "#ED7D31",
} as const;

const LINK_STROKE = {
  blue: "rgba(91,155,213,0.45)",
  orange: "rgba(237,125,49,0.45)",
  lightOrange: "rgba(244,177,131,0.55)",
} as const;

type SNode = NodeDatum & {
  x0: number; x1: number; y0: number; y1: number;
};
type SLink = Omit<LinkDatum, "source" | "target"> & {
  source: SNode; target: SNode; width: number;
};

type Tooltip =
  | { kind: "node"; x: number; y: number; node: SNode }
  | { kind: "link"; x: number; y: number; link: SLink }
  | null;

export const Sankey = ({ width, height, data, onNavigate }: SankeyProps) => {
  const [tooltip, setTooltip] = useState<Tooltip>(null);

  const { nodes, links } = useMemo(() => {
    const generator = d3sankey<NodeDatum, LinkDatum>()
      .nodeId((d) => d.id)
      .nodeAlign(sankeyJustify)
      .nodeWidth(20)
      .nodePadding(28)
      .extent([
        [MARGIN.left, MARGIN.top],
        [width - MARGIN.right, height - MARGIN.bottom],
      ]);

    // d3-sankey mutates the input — clone first.
    const graph: SankeyGraph<NodeDatum, LinkDatum> = {
      nodes: data.nodes.map((n) => ({ ...n })),
      links: data.links.map((l) => ({ ...l })),
    };
    const result = generator(graph);
    return {
      nodes: result.nodes as unknown as SNode[],
      links: result.links as unknown as SLink[],
    };
  }, [data, width, height]);

  const linkPath = sankeyLinkHorizontal<SNode, SLink>();

  return (
    <div className="sankey-wrap" style={{ width, height }}>
      <svg width={width} height={height}>
        {/* Links first so nodes render on top */}
        <g>
          {links.map((link, i) => (
            <path
              key={`link-${i}`}
              className="sankey-link"
              d={linkPath(link) ?? undefined}
              stroke={LINK_STROKE[link.color]}
              strokeOpacity={0.45}
              strokeWidth={Math.max(1, link.width)}
              onMouseEnter={(e) =>
                setTooltip({
                  kind: "link",
                  x: e.nativeEvent.offsetX,
                  y: e.nativeEvent.offsetY,
                  link,
                })
              }
              onMouseMove={(e) =>
                setTooltip({
                  kind: "link",
                  x: e.nativeEvent.offsetX,
                  y: e.nativeEvent.offsetY,
                  link,
                })
              }
              onMouseLeave={() => setTooltip(null)}
            />
          ))}
        </g>

        {/* Nodes */}
        <g>
          {nodes.map((node) => {
            const w = node.x1 - node.x0;
            const h = node.y1 - node.y0;
            const clickable = !!node.navigateTo;
            return (
              <g key={node.id}>
                <rect
                  className={`sankey-node${clickable ? " clickable" : ""}`}
                  x={node.x0}
                  y={node.y0}
                  width={w}
                  height={h}
                  fill={NODE_FILL[node.color]}
                  stroke="#fff"
                  strokeWidth={1}
                  onMouseEnter={(e) =>
                    setTooltip({
                      kind: "node",
                      x: e.nativeEvent.offsetX,
                      y: e.nativeEvent.offsetY,
                      node,
                    })
                  }
                  onMouseMove={(e) =>
                    setTooltip({
                      kind: "node",
                      x: e.nativeEvent.offsetX,
                      y: e.nativeEvent.offsetY,
                      node,
                    })
                  }
                  onMouseLeave={() => setTooltip(null)}
                  onClick={() => clickable && onNavigate?.(node.navigateTo!)}
                />
              </g>
            );
          })}
        </g>

        {/* Labels — placed above (top side) or below (bottom side) the chart */}
        <g>
          {nodes.map((node) => {
            const cx = (node.x0 + node.x1) / 2;
            const isTop = node.side === "top";
            const labelY = isTop ? MARGIN.top - 38 : height - MARGIN.bottom + 24;
            const valueY = isTop ? MARGIN.top - 8  : height - MARGIN.bottom + 56;
            const lines = node.label.split("\n");
            return (
              <g key={`lbl-${node.id}`} className={`sankey-label ${node.color}`}>
                {lines.map((line, i) => (
                  <text
                    key={i}
                    x={cx}
                    y={labelY + i * 14 - (lines.length - 1) * (isTop ? 14 : 0)}
                    textAnchor="middle"
                  >
                    {line}
                  </text>
                ))}
                <text x={cx} y={valueY} textAnchor="middle">
                  {formatValue(node.value)}
                </text>
              </g>
            );
          })}
        </g>
      </svg>

      {tooltip && (
        <div
          className="sankey-tooltip"
          style={{ left: tooltip.x, top: tooltip.y }}
        >
          {tooltip.kind === "node" ? (
            <>
              <strong>{tooltip.node.label.replace("\n", " ")}</strong>
              <br />
              {formatValue(tooltip.node.value)} people
              {tooltip.node.navigateTo && (
                <>
                  <br />
                  <em>click to open: {tooltip.node.navigateTo}</em>
                </>
              )}
            </>
          ) : (
            <>
              <strong>
                {tooltip.link.source.label.replace("\n", " ")} →{" "}
                {tooltip.link.target.label.replace("\n", " ")}
              </strong>
              <br />
              {formatValue(tooltip.link.value as number)} people
            </>
          )}
        </div>
      )}
    </div>
  );
};
