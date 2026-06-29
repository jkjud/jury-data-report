library(readr)
library(lubridate)
library(dplyr)
library(tidyr)
library(stringr)
library(scales)
library(ggtext)
library(here)
library(ggsankeyfier)


jdr <- read_csv("./data/processed/jury_data_transformed.csv")

sankey_data <- jdr |>
  select(
    end_date,
    county,
    summons,
    postin,
    tqa,
    unavailable,
    told_to_report,
    sent_for_selection,
    not_selected,
    oncall,
    rel_challenge,
    rel_hardship,
    rel_perempt,
    jurors_sworn,
    not_reached
  ) |>
  mutate(end_year = year(ymd(end_date))) |>
  select(end_year, everything(), -end_date) 

s4s_combined <- sankey_data |>
  mutate(dismissed = rel_challenge +
           rel_hardship + rel_perempt + not_reached) |>
  select(
         -rel_challenge,
         -rel_hardship,
         -rel_perempt, 
         -not_reached
         )



sankey_aggregated <- s4s_combined %>%
  group_by(end_year) %>%
  summarise(
    across(
      where(is.numeric) & !any_of("county"),
      ~ sum(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  ) |>
  filter(end_year == 2025) |>
  mutate(new_summons = summons,
         summons = new_summons + postin)

# --- Raw data ---
data <- tibble(
  summons = 10781995, postin = 911055, tqa = 5155919,
  unavailable = 6488247, told_to_report = 1590295,
  sent_for_selection = 652672, not_selected = 1025357,
  oncall = 3515104, jurors_sworn = 101877
)



# --- Step 1: Build wide-format edge list (each row = one from→to flow) ---
flows_wide <- tribble(
  ~source,                      ~target,                        ~magnitude,
  "Postin",                   "Summoned",                 data$postin,
  "Newly Summoned",           "Summoned",                 data$summons,
  "Summoned",                 "Qualified and Available",  data$tqa,
  "Summoned",                 "Unavailable",              data$unavailable,
  "Qualified and Available",  "In Person",                data$told_to_report,
  "Qualified and Available",  "On Call",                  data$oncall,
  "In Person",                "Sent For Selection",       data$sent_for_selection,
  "In Person",                "Not Selected",             data$not_selected,
  "Sent For Selection",       "Sworn on Jury",            data$jurors_sworn,
  "Sent For Selection",       "Released",                 data$sent_for_selection - data$jurors_sworn
)

# --- Step 2: networkD3 Sankey ---
library(networkD3)
library(htmlwidgets)

# Build nodes — order determines column placement via iterations
nodes <- data.frame(
  name = c(
    "Postin",                    # 0
    "Newly Summoned",            # 1
    "Summoned",                  # 2
    "Qualified and Available",   # 3
    "Unavailable",               # 4
    "In Person",                 # 5
    "On Call",                   # 6
    "Sent For Selection",        # 7
    "Not Selected",              # 8
    "Sworn on Jury",             # 9
    "Released"                   # 10
  ),
  stringsAsFactors = FALSE
)

# Build links from flows_wide (already defined above)
links <- data.frame(
  source = match(flows_wide$source, nodes$name) - 1,
  target = match(flows_wide$target, nodes$name) - 1,
  value  = flows_wide$magnitude,
  group  = flows_wide$source  # color links by source node
)

# Format helper for value labels
fmt_num <- function(x) {
  ifelse(x >= 1e6,
    paste0(format(round(x / 1e6, 1), nsmall = 1), "M"),
    ifelse(x >= 1e3,
      paste0(format(round(x / 1e3, 1), nsmall = 1), "K"),
      as.character(x)
    )
  )
}

# Color scale — blues for main flow, reds for attrition, green for outcome
color_js <- JS(
  'd3.scaleOrdinal()
    .domain(["Postin", "Newly Summoned", "Summoned",
             "Qualified and Available", "Unavailable",
             "In Person", "On Call",
             "Sent For Selection", "Not Selected",
             "Sworn on Jury", "Released"])
    .range(["#5B9BD5", "#2F75B5", "#2E75B6",
            "#4BACC6", "#C0504D",
            "#F4B183", "#A9D18E",
            "#ED7D31", "#BFBFBF",
            "#548235", "#C0504D"])'
)

# Build the sankey
sn <- sankeyNetwork(
  Links     = links,
  Nodes     = nodes,
  Source    = "source",
  Target   = "target",
  Value    = "value",
  NodeID   = "name",
  LinkGroup = "group",
  colourScale = color_js,
  fontSize  = 13,
  fontFamily = "Arial",
  nodeWidth = 20,
  nodePadding = 25,
  sinksRight = TRUE,
  iterations = 32,
  units = "people"
)

# Post-render JS: add value labels above nodes, style like reference image
sn <- htmlwidgets::onRender(sn, '
function(el, x) {
  // Format number helper
  function fmt(n) {
    if (n >= 1e6) return (n / 1e6).toFixed(1) + "M";
    if (n >= 1e3) return (n / 1e3).toFixed(1) + "K";
    return n.toString();
  }

  // Compute node totals (max of incoming vs outgoing)
  var nodes = x.nodes;
  var links = x.links;
  nodes.forEach(function(d, i) {
    var outgoing = 0, incoming = 0;
    links.forEach(function(l) {
      if (l.source.index === i) outgoing += l.value;
      if (l.target.index === i) incoming += l.value;
    });
    d.totalValue = Math.max(outgoing, incoming);
  });

  // Style links: semi-transparent, colored by source
  d3.select(el).selectAll(".link")
    .style("stroke-opacity", 0.35);

  // Add value labels above each node
  var nodeGroup = d3.select(el).selectAll(".node");
  nodeGroup.each(function(d) {
    var g = d3.select(this);
    // Value label above node
    g.append("text")
      .attr("x", d.dx / 2)
      .attr("y", -6)
      .attr("text-anchor", "middle")
      .style("font-size", "11px")
      .style("font-weight", "bold")
      .style("fill", "#333")
      .text(fmt(d.totalValue));
  });

  // Title
  var svg = d3.select(el).select("svg");
  svg.append("text")
    .attr("x", 15)
    .attr("y", 25)
    .style("font-size", "20px")
    .style("font-weight", "bold")
    .style("font-family", "Arial")
    .style("fill", "#222")
    .text("Jury Selection Process \\u2014 FY2025");

  svg.append("text")
    .attr("x", 15)
    .attr("y", 45)
    .style("font-size", "12px")
    .style("font-family", "Arial")
    .style("fill", "#888")
    .text("From summons to sworn jurors");
}
')

sn

####################################
# ggsankey version
####################################
library(ggsankey)

# --- Build long-format data for ggsankey ---
# ggsankey needs: x, next_x, node, next_node, value
# We define each edge manually with numeric stage positions.

released_n <- data$sent_for_selection - data$jurors_sworn

edges <- tribble(
  ~from,                      ~to,                        ~value, ~x, ~next_x,
  "Postin",                   "Summoned",                 data$postin,                   1, 2,
  "Newly Summoned",           "Summoned",                 data$summons,                  1, 2,
  "Summoned",                 "Qualified and Available",  data$tqa,                      2, 3,
  "Summoned",                 "Unavailable",              data$unavailable,              2, 3,
  "Qualified and Available",  "In Person",                data$told_to_report,           3, 4,
  "Qualified and Available",  "On Call",                  data$oncall,                   3, 4,
  "In Person",                "Sent For Selection",       data$sent_for_selection,       4, 5,
  "In Person",                "Not Selected",             data$not_selected,             4, 5,
  "Sent For Selection",       "Sworn on Jury",            data$jurors_sworn,             5, 6,
  "Sent For Selection",       "Released",                 released_n,                    5, 6
)

# Terminal nodes need rows where they appear as `node` with NA next
# (otherwise ggsankey won't draw their node rectangles)
terminal_nodes <- tribble(
  ~from,                  ~to,           ~value, ~x, ~next_x,
  "Unavailable",          NA_character_, data$unavailable,  3, NA_real_,
  "On Call",              NA_character_, data$oncall,        4, NA_real_,
  "Not Selected",         NA_character_, data$not_selected,  5, NA_real_,
  "Sworn on Jury",        NA_character_, data$jurors_sworn,  6, NA_real_,
  "Released",             NA_character_, released_n,         6, NA_real_
)

all_edges <- bind_rows(edges, terminal_nodes)

sankey_df <- data.frame(
  x         = all_edges$x,
  next_x    = all_edges$next_x,
  node      = all_edges$from,
  next_node = all_edges$to,
  value     = all_edges$value
)

# --- Node value lookup for labels ---
node_totals <- c(
  "Postin"                  = data$postin,
  "Newly Summoned"          = data$summons,
  "Summoned"                = data$summons + data$postin,
  "Qualified and Available" = data$tqa,
  "Unavailable"             = data$unavailable,
  "In Person"               = data$told_to_report,
  "On Call"                 = data$oncall,
  "Sent For Selection"      = data$sent_for_selection,
  "Not Selected"            = data$not_selected,
  "Sworn on Jury"           = data$jurors_sworn,
  "Released"                = released_n
)

fmt <- function(x) {
  dplyr::case_when(
    x >= 1e6 ~ paste0(
      formatC(x / 1e6, format = "f", digits = 1), "M"
    ),
    x >= 1e3 ~ paste0(
      formatC(x / 1e3, format = "f", digits = 0), "K"
    ),
    TRUE ~ as.character(x)
  )
}

# --- Node colors ---
node_colors <- c(
  "Postin"                  = "#5B9BD5",
  "Newly Summoned"          = "#2F75B5",
  "Summoned"                = "#2E75B6",
  "Qualified and Available" = "#4BACC6",
  "Unavailable"             = "#BF4B4B",
  "In Person"               = "#F4B183",
  "On Call"                 = "#A9D18E",
  "Sent For Selection"      = "#ED7D31",
  "Not Selected"            = "#A0A0A0",
  "Sworn on Jury"           = "#548235",
  "Released"                = "#BF4B4B"
)

# --- Plot ---
ggplot(sankey_df,
       aes(x = x, next_x = next_x,
           node = node, next_node = next_node,
           fill = factor(node),
           value = value)) +
  geom_sankey(
    flow.alpha = 0.45,
    node.color = "grey30",
    width = 0.03,
    smooth = 8,
    show.legend = FALSE
  ) +
  geom_sankey_label(
    aes(label = after_stat(
      paste0(node, "\n", fmt(node_totals[node]))
    )),
    size = 3.8,
    fontface = "bold",
    color = "grey15",
    fill = NA,
    hjust = 0.5,
    vjust = 0.3,
    show.legend = FALSE
  ) +
  scale_fill_manual(values = node_colors) +
  scale_x_continuous(
    breaks = 1:6,
    labels = c("Origin", "Summoned", "Qualification",
               "Reporting", "Selection", "Outcome"),
    expand = expansion(add = c(0.3, 0.3))
  ) +
  theme_sankey(base_size = 16) +
  theme(
    plot.title    = element_text(
      face = "bold", size = 22, hjust = 0,
      margin = margin(b = 5)
    ),
    plot.subtitle = element_text(
      color = "grey50", size = 13, hjust = 0,
      margin = margin(b = 15)
    ),
    plot.margin     = margin(25, 30, 20, 20),
    plot.background = element_rect(fill = "white", color = NA),
    axis.text.x     = element_text(
      face = "bold", size = 11, color = "grey40"
    )
  ) +
  labs(
    title    = "Jury Selection Process \u2014 FY2025",
    subtitle = "From summons to sworn jurors"
  )

####################################
# Plotly interactive sankey
####################################
library(plotly)

# Node list — order determines index (0-based)
p_nodes <- c(
  "Postin",                    # 0

"Newly Summoned",            # 1
  "Summoned",                  # 2
  "Qualified and Available",   # 3
  "Unavailable",               # 4
  "In Person",                 # 5
  "On Call",                   # 6
  "Sent For Selection",        # 7
  "Not Selected",              # 8
  "Sworn on Jury",             # 9
  "Released"                   # 10
)

# Node colors (matching ggsankey palette)
p_node_colors <- c(
  "#5B9BD5", "#2F75B5", "#2E75B6",
  "#4BACC6", "#BF4B4B",
  "#F4B183", "#A9D18E",
  "#ED7D31", "#A0A0A0",
  "#548235", "#BF4B4B"
)

# Link colors — semi-transparent versions of source node color
p_link_colors <- c(
  "rgba(91,155,213,0.4)",   # Postin → Summoned
  "rgba(47,117,181,0.4)",   # Newly Summoned → Summoned
  "rgba(46,117,182,0.4)",   # Summoned → Q&A
  "rgba(46,117,182,0.4)",   # Summoned → Unavailable
  "rgba(75,172,198,0.4)",   # Q&A → In Person
  "rgba(75,172,198,0.4)",   # Q&A → On Call
  "rgba(244,177,131,0.4)",  # In Person → SFS
  "rgba(244,177,131,0.4)",  # In Person → Not Selected
  "rgba(237,125,49,0.4)",   # SFS → Sworn
  "rgba(237,125,49,0.4)"    # SFS → Released
)

# Node x/y positions — spread out to leave room for annotations
p_node_x <- c(0.12, 0.12, 0.28, 0.46, 0.46,
               0.62, 0.62, 0.78, 0.78, 0.88, 0.88)
p_node_y <- c(0.1, 0.5, 0.4, 0.25, 0.85,
               0.2, 0.7, 0.15, 0.55, 0.1, 0.35)

# Node values for annotation text
p_node_values <- c(
  data$postin, data$summons - data$postin,
  data$summons, data$tqa, data$unavailable,
  data$told_to_report, data$oncall,
  data$sent_for_selection, data$not_selected,
  data$jurors_sworn, released_n
)

# Keep hover labels but hide built-in text (annotations replace them)
p_node_labels <- paste0(
  p_nodes, "<br>", fmt(p_node_values)
)

fig <- plot_ly(
  type = "sankey",
  orientation = "h",
  arrangement = "fixed",
  node = list(
    pad       = 20,
    thickness = 25,
    line      = list(color = "grey30", width = 0.8),
    label     = p_node_labels,
    color     = p_node_colors,
    x         = p_node_x,
    y         = p_node_y,
    hovertemplate = "%{label}<extra></extra>"
  ),
  link = list(
    source = c(0, 1, 2, 2, 3, 3, 5, 5, 7, 7),
    target = c(2, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    value  = c(
      data$postin,
      data$summons - data$postin,
      data$tqa,
      data$unavailable,
      data$told_to_report,
      data$oncall,
      data$sent_for_selection,
      data$not_selected,
      data$jurors_sworn,
      released_n
    ),
    color = p_link_colors,
    hovertemplate = paste0(
      "%{source.label} \u2192 %{target.label}",
      "<br>%{value:,.0f} people<extra></extra>"
    )
  )
)

# --- Layout: hide default node labels, widen margins for annotations ---
fig <- fig |>
  layout(
    title = list(
      text = paste0(
        "<b>Jury Selection Process \u2014 FY2025</b>",
        "<br><span style='font-size:13px;color:grey'>",
        "From summons to sworn jurors</span>"
      ),
      x = 0.01,
      xanchor = "left"
    ),
    font = list(
      family = "Arial", size = 1,
      color = "rgba(0,0,0,0)"
    ),
    paper_bgcolor = "white",
    margin = list(l = 150, r = 150, t = 80, b = 30)
  )

# --- Add annotations per node using add_annotations() ---
# Left side (col 1): text to the LEFT of node
# Right side (col 6): text to the RIGHT of node
# Middle (cols 2-5): text ABOVE the node

for (i in seq_along(p_nodes)) {
  # Determine anchor/offset based on column position
  if (p_node_x[i] <= 0.15) {
    # Left column — label to the left
    xa <- "right"
    ya <- "middle"
    xs <- -20
    ys <- 0
  } else if (p_node_x[i] >= 0.85) {
    # Right column — label to the right
    xa <- "left"
    ya <- "middle"
    xs <- 20
    ys <- 0
  } else {
    # Middle columns — label above
    xa <- "center"
    ya <- "bottom"
    xs <- 0
    ys <- 20
  }

  fig <- fig |>
    add_annotations(
      x         = p_node_x[i],
      y         = p_node_y[i],
      xref      = "paper",
      yref      = "paper",
      xanchor   = xa,
      yanchor   = ya,
      ax        = xs,
      ay        = -ys,
      text      = paste0(
        "<b>", p_nodes[i], "</b><br>",
        "<b>", fmt(p_node_values[i]), "</b>"
      ),
      font      = list(
        family = "Arial",
        size   = 15,
        color  = p_node_colors[i]
      ),
      showarrow = FALSE
    )
}

fig
