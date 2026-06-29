devtools::install_github("davidsjoberg/ggsankey")

library(tidyverse)
library(hrbrthemes)
library(viridis)
library(ggsankey)

df <- read_csv("./data/processed/sankey_data.csv")

df_sankey <- df |>
  make_long(Source,
            Destination, value = value)

p <- ggplot(df_sankey, aes(
                           x = x,
                           next_x = next_x,
                           node = node,
                           next_node = next_node,
                           fill = factor(node),
                           label = node,
                           value = value
                           ))

p_sankey <- p + geom_sankey(
                             flow.alpha = 0.9,
                             show.legend = F
                             )

p_label1 <- p_sankey + geom_sankey_label()


df_sankey <- sankey_data_transformed |>
  make_long(TQA,
            `Told To Report`,
            `Sent For Selection`,
            value = value)

p <- ggplot(df_sankey, aes(
  x = x,
  next_x = next_x,
  node = node,
  next_node = next_node,
  fill = factor(node),
  label = node,
  value = value
))

p_sankey <- p + geom_sankey(
  flow.alpha = 0.9,
  show.legend = F,
  na.rm = TRUE
)

p_label1 <- p_sankey + geom_sankey_label()

?geom_sankey()
