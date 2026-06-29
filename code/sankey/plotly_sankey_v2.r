####################################
# Plotly interactive sankey (v2 - adjusted annotations)
####################################
library(plotly)

# ============================================================
# CONFIG — edit these values and re-run to tweak the chart
# ============================================================

# Node names, colors, and positions
cfg <- data.frame(stringsAsFactors = FALSE,
  name   = c("Postin",                "Newly Summoned",       "Summoned",               "Qualified and Available",   "Unavailable",    "In Person",           "On Call",     "Sent For Selection",   "Not Selected",     "Sworn on Jury",    "Released"), # nolint
  color  = c("rgba(47,117,181,0.85)", "rgba(47,117,181,0.85)","rgba(47,117,181,0.85)", "rgba(47,117,181,0.85)",  "#ED7D31",      "rgba(47,117,181,0.85)",    "#ED7D31",  "rgba(47,117,181,0.85)",             "#ED7D31",        "rgba(47,117,181,0.85)",         "#ED7D31"), # nolint
  node_x = c(0.135,                   0.05,                     0.27,                   0.50,                         0.45,             0.65,           0.63,         0.80,                     0.76,               0.90,               0.87), # nolint
  node_y = c(0.88,                    0.40,                     0.50,                   0.39,                         0.80,             0.185,          0.52,          0.10,                     0.32,              0.055,               0.17) # nolint
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
p_margin <- list(l = 60, r = 110, t = 90, b = 80)
p_width  <- 1040
p_height <- 620

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
      font = list(family = "Arial", size = 10, color = "white"),
      paper_bgcolor = "white",
      margin = p_margin
    )

  # ============================================================
  # ANNOTATIONS
  # Annotation paper coords: x in [-0.07, 1.07], y in [-0.15, 1.29]
  # y is INVERTED relative to node y (y=1.29 is TOP of plot)
  # Rule: push annotations AWAY from sankey center; value sits BELOW text.
  # ============================================================

  # Postin (bottom-left, bottom row)
  fig <- fig |>
    add_annotations(x = 0.055, y = 0.02, showarrow = FALSE,
      text = "<b>Postponed In</b>",
      font = list(color = "#2F75B5", size = 18)) |>
    add_annotations(x = 0.093, y = -0.035, showarrow = FALSE,
      text = paste0("<b>", fmt(nv[1]), "</b>"),
      font = list(color = "#2F75B5", size = 18))

  # Newly Summoned (top-left)
  fig <- fig |>
    add_annotations(x = -0.05, y = 1.06, showarrow = FALSE,
      text = "<b>Newly Summoned</b>",
      font = list(color = "#2F75B5", size = 18)) |>
    add_annotations(x = 0.01, y = 1.008, showarrow = FALSE,
      text = paste0("<b>", fmt(nv[2]), "</b>"),
      font = list(color = "#2F75B5", size = 18))

  # Summoned (top)
  fig <- fig |>
    add_annotations(x = 0.21, y = 0.98, showarrow = FALSE,
      text = "<b>Summoned</b>",
      font = list(color = "#2F75B5", size = 18)) |>
    add_annotations(x = 0.235, y = 0.925, showarrow = FALSE,
      text = paste0("<b>", fmt(nv[3]), "</b>"),
      font = list(color = "#2F75B5", size = 18))

  # Qualified and Available (top)
  fig <- fig |>
    add_annotations(x = 0.50, y = 0.95, showarrow = FALSE,
      text = "<b>Qualified &<br>Available</b>",
      font = list(color = "#2F75B5", size = 18)) |>
    add_annotations(x = 0.50, y = 0.84, showarrow = FALSE,
      text = paste0("<b>", fmt(nv[4]), "</b>"),
      font = list(color = "#2F75B5", size = 18))

  # Unavailable (bottom)
  fig <- fig |>
    add_annotations(x = 0.54, y = 0.19, showarrow = FALSE,
      text = "<b>Unavailable</b>",
      font = list(color = "#ED7D31", size = 18)) |>
    add_annotations(x = 0.539, y = 0.135, showarrow = FALSE,
      text = paste0("<b>", fmt(nv[5]), "</b>"),
      font = list(color = "#ED7D31", size = 18))

  # In Person (top)
  fig <- fig |>
    add_annotations(x = 0.65, y = 0.977, showarrow = FALSE,
      text = "<b>In Person</b>",
      font = list(color = "#2F75B5", size = 16)) |>
    add_annotations(x = 0.65, y = 0.93, showarrow = FALSE,
      text = paste0("<b>", fmt(nv[6]), "</b>"),
      font = list(color = "#2F75B5", size = 16))

  # On Call (bottom)
  fig <- fig |>
    add_annotations(x = 0.725, y = 0.5, showarrow = FALSE,
      text = "<b>On Call</b>",
      font = list(color = "#ED7D31", size = 16)) |>
    add_annotations(x = 0.7165, y = 0.455, showarrow = FALSE,
      text = paste0("<b>", fmt(nv[7]), "</b>"),
      font = list(color = "#ED7D31", size = 16))

  # Sent For Selection (top)
  fig <- fig |>
    add_annotations(x = 0.84, y = 1.07, showarrow = FALSE,
      text = "<b>Sent For<br>Selection</b>",
      font = list(color = "#2F75B5", size = 15)) |>
    add_annotations(x = 0.825, y = 0.98, showarrow = FALSE,
      text = paste0("<b>", fmt(nv[8]), "</b>"),
      font = list(color = "#2F75B5", size = 15))

  # Not Selected (bottom)
  fig <- fig |>
    add_annotations(x = 0.89, y = 0.695, showarrow = FALSE,
      text = "<b>Not Selected</b>",
      font = list(color = "#ED7D31", size = 15)) |>
    add_annotations(x = 0.855, y = 0.625, showarrow = FALSE,
      text = paste0("<b>", fmt(nv[9]), "</b>"),
      font = list(color = "#ED7D31", size = 15))

  # Sworn on Jury (top-right)
  fig <- fig |>
    add_annotations(x = 1, y = 1.05, showarrow = FALSE,
      text = "<b>Sworn on<br>Jury</b>",
      font = list(color = "#2F75B5", size = 15)) |>
    add_annotations(x = 0.983, y = 0.955, showarrow = FALSE,
      text = paste0("<b>", fmt(nv[10]), "</b>"),
      font = list(color = "#2F75B5", size = 15))

  # Released (bottom-right)
  fig <- fig |>
    add_annotations(x = 0.97, y = .863, showarrow = FALSE,
      text = "<b>Released</b>",
      font = list(color = "#ED7D31", size = 15)) |>
    add_annotations(x = 0.953, y = .819, showarrow = FALSE,
      text = paste0("<b>", fmt(nv[11]), "</b>"),
      font = list(color = "#ED7D31", size = 15))

  fig
}

# Run it
fig <- build_sankey()
fig

htmlwidgets::saveWidget(fig, "sankey.html", selfcontained = FALSE)
browseURL("sankey.html")
