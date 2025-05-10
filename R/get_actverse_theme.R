get_actverse_theme <- function() {
  ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.frame = ggplot2::element_blank(),
      legend.ticks = ggplot2::element_line(color = "white")
    )
}
