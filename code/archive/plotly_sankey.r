####################################
# Plotly interactive sankey
####################################
library(plotly)

# ============================================================
# CONFIG — edit these values and re-run to tweak the chart
# ============================================================

# Node names, colors, and positions
cfg <- data.frame(stringsAsFactors = FALSE,
  name   = c("Postin",  "Newly Summoned",       "Summoned",    "Qualified and Available",   "Unavailable",    "In Person",     "On Call",     "Sent For Selection",   "Not Selected",     "Sworn on Jury",    "Released"), # nolint
  color  = c("rgba(47,117,181,0.85)", "rgba(47,117,181,0.85)",         "rgba(47,117,181,0.85)",   "rgba(47,117,181,0.85)",                  "#ED7D31",      "rgba(47,117,181,0.85)",    "#ED7D31",  "rgba(47,117,181,0.85)",             "#ED7D31",        "rgba(47,117,181,0.85)",         "#ED7D31"), # nolint
  node_x = c(0.135,        0.10,                 0.27,          0.50,                         0.45,             0.65,           0.63,         0.80,                     0.76,               0.90,               0.87), # nolint
  node_y = c(0.88,        0.40,                0.50,           0.39,                         0.80,             0.185,          0.52,          0.10,                     0.32,              0.055,               0.17) # nolint
)

# Link colors — semi-transparent versions of source node color
p_link_colors <- c(
  "rgba(91,155,213,0.4)",   # Postin -> Summoned
  "rgba(47,117,181,0.4)",   # Newly Summoned -> Summoned
  "rgba(46,117,182,0.4)",   # Summoned -> Q&A
  "rgba(237,125,49,0.4)",   # Summoned -> Unavailable
  "rgba(47,117,181,0.4)",   # Q&A -> In Person
  "rgba(237,125,49,0.4)",   # Q&A -> On Call
  "rgba(47,117,181,0.4)",  # In Person -> SFS
  "rgba(244,177,131,0.4)",  # In Person -> Not Selected
  "rgba(47,117,181,0.4)",   # SFS -> Sworn
  "rgba(237,125,49,0.4)"    # SFS -> Released
)

# Layout settings
p_margin <- list(l = 0, r = 80, t = 60, b = 40)
p_width  <- 960
p_height <- 540

# ============================================================
# BUILD
# ============================================================

build_sankey <- function() {

  nv <- c(
    data$postin, data$summons,
    data$summons + data$postin, data$tqa, data$unavailable,
    data$told_to_report, data$oncall,
    data$sent_for_selection, data$not_selected,
    data$jurors_sworn, released_n
  )

  node_labels <- paste0(cfg$name, "<br>", fmt(nv))

  fig <- plot_ly(
    width = p_width,
    height = p_height,
    type = "sankey",
    orientation = "h",
    arrangement = "fixed",
    textfont = list(color = "rgba(0,0,0,0)", size = 1),
    node = list(
      pad       = 35,
      thickness = 20,
      line      = list(color = "white", width = 1),
      label     = node_labels,
      color     = cfg$color,
      x         = cfg$node_x,
      y         = cfg$node_y,
      hovertemplate = "%{label}<extra></extra>"
    ),
    link = list(
      source = c(0, 1, 2, 2, 3, 3, 5, 5, 7, 7),
      target = c(2, 2, 3, 4, 5, 6, 7, 8, 9, 10),
      value  = c(
        data$postin,
        data$summons,
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

  fig <- fig |>
    layout(
      hovermode = "x",
      # title = list(
      #   text = paste0(
      #     "<b>Jury Selection Process \u2014 FY2025</b>",
      #     "<br><span style='font-size:13px;color:grey'>",
      #     "From summons to sworn jurors</span>"
      #   ),
      #   x = 0.01,
      #   xanchor = "left"
      # ),
      font = list(family = "Arial", size = 10, color = "white"),
      paper_bgcolor = "white",
      margin = p_margin,
      shapes = list(
        list(
          type = "rect",
          xref = "paper", yref = "paper",
          x0 = 0, x1 = 1, y0 = 0, y1 = 1,
          line = list(color = "red", width = 2),
          fillcolor = "rgba(0,0,0,0)"
        )
      )
    )

  # ============================================================
  # ANNOTATIONS — adjust x/y positions by eye for each node
  # Each node gets a name annotation and a value annotation
  # x/y are in the plot coordinate space (same as node positions)
  # ============================================================

  # Postin (node_y = 0.65, which is lower on screen since sankey y is inverted)
  fig <- fig |>
    add_annotations(x = 0.07, y = 0.115, showarrow = FALSE,
      text = "<b>Postponed<br>In</b>",
      font = list(color = "#5B9BD5", size = 13)) |>
    add_annotations(x = 0.085, y = 0.08, showarrow = FALSE,
      text = paste0("<b>", fmt(nv[1]), "</b>"),
      font = list(color = "#5B9BD5", size = 13))

  # Newly Summoned (node_y = 0.25, which is higher on screen)
  fig <- fig |>
    add_annotations(x = 0.05, y = 0.80, showarrow = FALSE,
      text = "<b>Newly<br>Summoned</b>",
      font = list(color = "#2F75B5", size = 12)) |>
    add_annotations(x = 0.05, y = 0.855, showarrow = FALSE,
      text = paste0("<b>", fmt(nv[2]), "</b>"),
      font = list(color = "#2F75B5", size = 12))

  # Summoned
  fig <- fig |>
    add_annotations(x = 0.30, y = 0.95, showarrow = FALSE,
      text = "<b>Summoned</b>",
      font = list(color = "#2E75B6", size = 12)) |>
    add_annotations(x = 0.30, y = 0.90, showarrow = FALSE,
      text = paste0("<b>", fmt(nv[3]), "</b>"),
      font = list(color = "#2E75B6", size = 12))

  # Qualified and Available
  fig <- fig |>
    add_annotations(x = 0.45, y = 0.95, showarrow = FALSE,
      text = "<b>Qualified &<br>Available</b>",
      font = list(color = "#4BACC6", size = 12)) |>
    add_annotations(x = 0.45, y = 0.88, showarrow = FALSE,
      text = paste0("<b>", fmt(nv[4]), "</b>"),
      font = list(color = "#4BACC6", size = 12))

  # Unavailable
  fig <- fig |>
    add_annotations(x = 0.45, y = 0.15, showarrow = FALSE,
      text = "<b>Unavailable</b>",
      font = list(color = "#BF4B4B", size = 12)) |>
    add_annotations(x = 0.45, y = 0.10, showarrow = FALSE,
      text = paste0("<b>", fmt(nv[5]), "</b>"),
      font = list(color = "#BF4B4B", size = 12))

  # In Person
  fig <- fig |>
    add_annotations(x = 0.60, y = 0.95, showarrow = FALSE,
      text = "<b>In Person</b>",
      font = list(color = "#F4B183", size = 12)) |>
    add_annotations(x = 0.60, y = 0.90, showarrow = FALSE,
      text = paste0("<b>", fmt(nv[6]), "</b>"),
      font = list(color = "#F4B183", size = 12))

  # On Call
  fig <- fig |>
    add_annotations(x = 0.60, y = 0.15, showarrow = FALSE,
      text = "<b>On Call</b>",
      font = list(color = "#A9D18E", size = 12)) |>
    add_annotations(x = 0.60, y = 0.10, showarrow = FALSE,
      text = paste0("<b>", fmt(nv[7]), "</b>"),
      font = list(color = "#A9D18E", size = 12))

  # Sent For Selection
  fig <- fig |>
    add_annotations(x = 0.855, y = 0.95, showarrow = FALSE,
      text = "<b>Sent For<br>Selection</b>",
      font = list(color = "#ED7D31", size = 12)) |>
    add_annotations(x = 0.855, y = 0.88, showarrow = FALSE,
      text = paste0("<b>", fmt(nv[8]), "</b>"),
      font = list(color = "#ED7D31", size = 12))

  # Not Selected
  fig <- fig |>
    add_annotations(x = 0.855, y = 0.15, showarrow = FALSE,
      text = "<b>Not Selected</b>",
      font = list(color = "#A0A0A0", size = 12)) |>
    add_annotations(x = 0.855, y = 0.10, showarrow = FALSE,
      text = paste0("<b>", fmt(nv[9]), "</b>"),
      font = list(color = "#A0A0A0", size = 12))

  # Sworn on Jury
  fig <- fig |>
    add_annotations(x = 0.95, y = 0.80, showarrow = FALSE,
      text = "<b>Sworn on<br>Jury</b>",
      font = list(color = "#548235", size = 12)) |>
    add_annotations(x = 0.95, y = 0.853, showarrow = FALSE,
      text = paste0("<b>", fmt(nv[10]), "</b>"),
      font = list(color = "#548235", size = 12))

  # Released
  fig <- fig |>
    add_annotations(x = 0.95, y = 0.50, showarrow = FALSE,
      text = "<b>Released</b>",
      font = list(color = "#BF4B4B", size = 12)) |>
    add_annotations(x = 0.95, y = 0.45, showarrow = FALSE,
      text = paste0("<b>", fmt(nv[11]), "</b>"),
      font = list(color = "#BF4B4B", size = 12))

  fig
}

# Run it
fig <- build_sankey()
fig

htmlwidgets::saveWidget(fig, "sankey.html", selfcontained = FALSE)
browseURL("sankey.html")