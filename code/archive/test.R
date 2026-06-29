library(tidyverse)
library(ggsankeyfier)
library(plotly)
library(grid)


# =============================================================================
# TRANSFORM sankey_aggregated → ggsankeyfier long format
# =============================================================================
#
# ggsankeyfier's pivot_stages_longer() expects WIDE data where:
#   - Each ROW is one complete pathway through the system
#   - Each COLUMN is a stage (in order, left to right)
#   - One column holds the numeric flow value
#   - Paths that end early get NA in later stage columns
#
# Your data is aggregated counts at each node, so we need to:
#   1. Compute any derived values (not_selected, released)
#   2. Build all 10 unique pathways (5 endpoints × 2 sources)
#   3. Proportionally split downstream flows between Postin & Newly Summoned
#   4. Pivot with pivot_stages_longer()
# =============================================================================

# --- Step 1: Compute derived flow values ------------------------------------

sankey_aggregated <- sankey_aggregated |>
  mutate(
    # "In Person" who were NOT sent for selection
    not_sent_for_selection = told_to_report - sent_for_selection,
    # "Sent for Selection" who were released (not sworn)
    released = sent_for_selection - jurors_sworn
  )

# --- Step 2: Build the wide "itinerary" table --------------------------------
#
# There are 5 terminal destinations:
#   Unavailable, On Call, Not Selected, Sworn on Jury, Released
# And 2 sources:
#   Postin, Newly Summoned
# So 10 rows total. Each row traces one complete path through the system.

# Proportional weight of each source feeding into Summoned
p_postin <- sankey_aggregated$postin / sankey_aggregated$summons
p_new    <- sankey_aggregated$new_summons / sankey_aggregated$summons

# Terminal flow values (before source split)
terminal_flows <- c(
  unavail      = sankey_aggregated$unavailable,
  oncall       = sankey_aggregated$oncall,
  not_selected = sankey_aggregated$not_sent_for_selection,
  sworn        = sankey_aggregated$jurors_sworn,
  released     = sankey_aggregated$released
)

sankey_wide <- tibble(
  # --- Stage 1: Source ---
  source = rep(c("Postin", "Newly Summoned"), each = 5),
  
  # --- Stage 2: Summoned pool (everyone merges here) ---
  pool = "Summoned",
  
  # --- Stage 3: Screening outcome ---
  screen = rep(c(
    "Unavailable",
    "Qualified and Available",
    "Qualified and Available",
    "Qualified and Available",
    "Qualified and Available"
  ), 2),
  
  # --- Stage 4: Assignment (NA if path ended at screening) ---
  assign = rep(c(
    NA_character_,
    "On Call",
    "In Person",
    "In Person",
    "In Person"
  ), 2),
  
  # --- Stage 5: Selection (NA if path ended at assignment) ---
  select = rep(c(
    NA_character_,
    NA_character_,
    "Not Selected",
    "Sent For Selection",
    "Sent For Selection"
  ), 2),
  
  # --- Stage 6: Final outcome (NA if path ended at selection) ---
  final = rep(c(
    NA_character_,
    NA_character_,
    NA_character_,
    "Sworn on Jury",
    "Released"
  ), 2),
  
  # --- Flow values: proportionally split by source ---
  flow = c(
    terminal_flows * p_postin,
    terminal_flows * p_new
  )
)

# Quick sanity check: do the flows for each source sum to that source's total?
sankey_wide |>
  group_by(source) |>
  summarise(total_flow = sum(flow), .groups = "drop")
# Should be ~911,055 for Postin and ~10,781,995 for Newly Summoned
# (approximate due to small discrepancies in the raw data)

# --- Step 3: Pivot to long format -------------------------------------------

sankey_long <- sankey_wide |>
  pivot_stages_longer(
    stages_from  = c("source", "pool", "screen", "assign", "select", "final"),
    values_from  = "flow"
  )

# Inspect the result — you should see columns:
#   flow, edge_id, connector ("from"/"to"), node, stage
head(sankey_long, 10)

# --- Step 4: Relabel stages for display (optional) --------------------------

sankey_long <- sankey_long |>
  mutate(
    stage = fct_recode(stage,
                       "Source"        = "source",
                       "Summoned"      = "pool",
                       "Screening"     = "screen",
                       "Assignment"    = "assign",
                       "Selection"     = "select",
                       "Outcome"       = "final"
    )
  )

# --- Step 5: Plot! ----------------------------------------------------------

pos      <- position_sankey(v_space = "auto", align = "justify")
pos_text <- position_sankey(v_space = "auto", align = "justify", nudge_x = 0.1)

p <- ggplot(sankey_long,
       aes(x = stage, y = flow,
           group = node,
           connector = connector,
           edge_id = edge_id,
           fill = node)) +
  geom_sankeyedge(position = pos, alpha = 0.5) +
  geom_sankeynode(position = pos) +
  geom_text(
    aes(label = node),
    stat     = "sankeynode",
    position = pos_text,
    hjust    = 0,
    size     = 3
  ) +
  scale_fill_viridis_d(option = "D", alpha = 0.9) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.text.y     = element_blank(),
    axis.title      = element_blank(),
    panel.grid      = element_blank()
  ) +
  labs(title = "Jury Selection Process Flow (2025)")




