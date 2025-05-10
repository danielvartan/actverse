# Load packages -----

# library(actverse)
# library(beepr)
library(downlit)
# library(cffr)
# library(codemetar)
# library(fs)
library(ggplot2)
# library(groomr) # https://github.com/danielvartan/groomr
# library(here)
library(httpgd)
library(magrittr)
library(ragg)
# library(readr)
library(rlang)
library(rutils) # https://github.com/danielvartan/rutils
library(showtext)
library(sysfonts)
library(xml2)
# library(yaml)

# Set general options -----

options(
  dplyr.print_min = 6,
  dplyr.print_max = 6,
  pillar.max_footer_lines = 2,
  pillar.min_chars = 15,
  scipen = 10,
  digits = 10,
  stringr.view_n = 6,
  pillar.bold = TRUE,
  width = 77 # 80 - 3 for #> Comment
)

# Set variables -----

set.seed(2025)

# Set knitr -----

knitr::clean_cache() |> rutils::shush()

knitr::opts_chunk$set(
  comment = "#>",
  collapse = TRUE,
  root.dir = here::here(),
  dev = "ragg_png",
  fig.showtext = TRUE
)

# Set and load graph fonts -----

sysfonts::font_paths(here::here("ttf")) |> invisible()

sysfonts::font_add(
  family = "lexend-deca",
  regular = here::here("ttf", "lexenddeca-regular.ttf"),
  bold = here::here("ttf", "lexenddeca-bold.ttf"),
  symbol = NULL
)

sysfonts::font_add(
  family = "lexend-exa",
  regular = here::here("ttf", "lexendexa-regular.ttf"),
  bold = here::here("ttf", "lexendexa-bold.ttf"),
  symbol = NULL
)

sysfonts::font_add(
  family = "roboto-mono",
  regular = here::here("ttf", "robotomono-regular.ttf"),
  bold = here::here("ttf", "robotomono-bold.ttf"),
  italic = here::here("ttf", "robotomono-italic.ttf"),
  bolditalic = here::here("ttf", "robotomono-bolditalic.ttf"),
  symbol = NULL
)

showtext::showtext_auto()

# Set `ggplot2` theme -----

ggplot2::theme_set(
  ggplot2::theme(
    text = ggplot2::element_text(
      color = brandr::get_brand_color("black"),
      family = "lexend-deca",
      face = "plain"
    )
  ) +
    actverse:::get_actverse_theme()
)
