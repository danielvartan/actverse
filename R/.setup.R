# Load Packages -----

library(brandr)
library(downlit)
library(ggplot2)
library(here)
library(knitr)
library(magrittr)
library(ragg)
library(rutils) # https://github.com/danielvartan/rutils
library(showtext)
library(sysfonts)
library(xml2)

# Set Options -----

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

# Set Variables -----

set.seed(2025)

# Set `knitr`` -----

clean_cache()

opts_chunk$set(
  comment = "#>",
  collapse = TRUE,
  root.dir = here(),
  dev = "ragg_png",
  fig.showtext = TRUE
)

# Set `brandr` -----

options(BRANDR_BRAND_YML = here("_brand.yml"))

# Set and Load Fonts -----

font_paths(here("ttf"))

font_add(
  family = "lexend-deca",
  regular = here("ttf", "lexenddeca-regular.ttf"),
  bold = here("ttf", "lexenddeca-bold.ttf"),
  symbol = NULL
)

font_add(
  family = "lexend-exa",
  regular = here("ttf", "lexendexa-regular.ttf"),
  bold = here("ttf", "lexendexa-bold.ttf"),
  symbol = NULL
)

font_add(
  family = "roboto-mono",
  regular = here("ttf", "robotomono-regular.ttf"),
  bold = here("ttf", "robotomono-bold.ttf"),
  italic = here("ttf", "robotomono-italic.ttf"),
  bolditalic = here("ttf", "robotomono-bolditalic.ttf"),
  symbol = NULL
)

showtext_auto()

# Set `ggplot2` Theme -----

theme_set(
  # theme(
  #   text = element_text(
  #     color = "black",
  #     family = "lexend-deca",
  #     face = "plain"
  #   )
  # ) +
    actverse:::get_actverse_theme()
)
