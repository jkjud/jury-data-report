library(tidyverse)
library(plotly)

# --- Raw data ---
data <- tibble(
  summons          = 10781995,
  postin           = 911055,
  tqa              = 5155919,
  unavailable      = 6488247,
  told_to_report   = 1590295,
  sent_for_selection = 652672,
  not_selected     = 1025357,
  oncall           = 3515104,
  jurors_sworn     = 101877
)

# --- Step 1: Define the flows (each pipe in the system) ---
# Think of this as a recipe: each row is one connection with its value.
flows <- tribble(
  ~source,                  ~target,                    ~value,
  "Postin",                 "Summoned",                 data$postin,
  "Newly Summoned",         "Summoned",                 data$summons - data$postin,
  "Summoned",               "Qualified and Available",  data$tqa,
  "Summoned",               "Unavailable",              data$unavailable,
  "Qualified and Available", "In Person",               data$told_to_report,
  "Qualified and Available", "On Call",                  data$oncall,
  "In Person",              "Sent For Selection",       data$sent_for_selection,
  "In Person",              "Not Selected",             data$not_selected,
  "Sent For Selection",     "Sworn on Jury",            data$jurors_sworn,
  "Sent For Selection",     "Released",                 data$sent_for_selection - data$jurors_sworn
)

# --- Step 2: Build the label lookup (the node index map) ---
# Every unique node gets a 0-based index — plotly needs this.
labels <- flows %>%
  pivot_longer(cols = c(source, target), values_to = "node") %>%
  distinct(node) %>%
  mutate(index = row_number() - 1)  # 0-based for plotly

# --- Step 3: Join indices back onto flows ---
sankey_data <- flows %>%
  left_join(labels, by = c("source" = "node")) %>%
  rename(source_idx = index) %>%
  left_join(labels, by = c("target" = "node")) %>%
  rename(target_idx = index)

# --- Inspect the tidy result ---
cat("=== Node Labels ===\n")
print(labels)

cat("\n=== Sankey-Ready Flows ===\n")
print(sankey_data)

# --- Step 4: Build the Sankey with plotly ---
fig <- plot_ly(
  type = "sankey",
  orientation = "h",
  node = list(
    pad       = 15,
    thickness = 20,
    line      = list(color = "black", width = 0.5),
    label     = labels$node,
    color     = "#4A90D9"
  ),
  link = list(
    source = sankey_data$source_idx,
    target = sankey_data$target_idx,
    value  = sankey_data$value
  )
) %>%
  layout(
    title = list(text = "Jury Selection Process — 2025"),
    font  = list(size = 12)
  )

# Save as interactive HTML
htmlwidgets::saveWidget(fig, "/home/claude/jury_sankey.html", selfcontained = TRUE)
cat("\nSankey saved to jury_sankey.html\n")
