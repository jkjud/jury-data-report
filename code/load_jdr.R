library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(here)
library(gt)
library(scales)
library(ggtext)
library(sysfonts)
library(showtext)
library(camcorder)
library(plotly)

source(here("code/fonts.R"))

showtext::showtext_opts(dpi = 300)
camcorder::gg_record(
  dir = "img"
  , dpi = 300
  , width = 6.5
  , height = 4
  , units = "in"
)

jdr <- read_csv("./data/processed/jury_data_transformed.csv")
this_year <- year(today())
jdr_last_five <- jdr |>
  filter(year(end_date) >= this_year - 5)

jdr_last_ten <- jdr |>
  filter(year(end_date) >= this_year - 10)