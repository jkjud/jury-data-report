import { ResponsiveSankey } from "@nivo/sankey";
import { formatValue, type NivoNode, type SankeyData } from "./data";

type Props = {
  data: SankeyData;
  onNavigate?: (target: string) => void;
};

export const Sankey = ({ data, onNavigate }: Props) => (
  <ResponsiveSankey
    data={data}
    margin={{ top: 40, right: 160, bottom: 40, left: 50 }}
    align="justify"
    // Use the per-node `nodeColor` field from data.ts; fall back to a scheme.
    colors={(node: any) => (node as NivoNode).nodeColor ?? "#1f77b4"}
    nodeOpacity={1}
    nodeHoverOthersOpacity={0.35}
    nodeThickness={18}
    nodeSpacing={24}
    nodeBorderWidth={0}
    nodeBorderColor={{ from: "color", modifiers: [["darker", 0.8]] }}
    nodeBorderRadius={3}
    linkOpacity={0.5}
    linkHoverOthersOpacity={0.1}
    linkContract={3}
    enableLinkGradient
    labelPosition="outside"
    labelOrientation="horizontal"
    labelPadding={16}
    labelTextColor={{ from: "color", modifiers: [["darker", 1]] }}
    valueFormat={(v) => formatValue(v as number)}
    onClick={(item) => {
      // Nivo passes either a node or a link depending on what was clicked.
      const node = item as unknown as NivoNode;
      if (node && node.navigateTo && onNavigate) onNavigate(node.navigateTo);
    }}
    legends={[
      {
        anchor: "bottom-right",
        direction: "column",
        translateX: 130,
        itemWidth: 100,
        itemHeight: 14,
        itemDirection: "right-to-left",
        itemsSpacing: 2,
        itemTextColor: "#999",
        symbolSize: 14,
      },
    ]}
  />
);
