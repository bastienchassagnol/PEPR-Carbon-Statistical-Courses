# Shared ggplot theme and saver for every slide figure.
# Sourced by generate-slide-figures.R and generate-data-figures.R.

library(ggplot2)
library(ragg)

theme_slide <- function() {
  theme_minimal(base_size = 18) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "#e6e1d8"),
      plot.title = element_text(
        face = "bold", colour = "#0e4d3a", size = 18
      ),
      plot.subtitle = element_text(colour = "#3d4a42", size = 13),
      axis.title = element_text(colour = "#1c2b24"),
      strip.text = element_text(face = "bold", colour = "#0e4d3a"),
      legend.position = "bottom",
      plot.background = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA)
    )
}

save_fig <- function(plot, filename, width = 9, height = 5.2,
                     dir = "figures/generated") {
  ggsave(
    filename = file.path(dir, filename),
    plot = plot,
    width = width,
    height = height,
    dpi = 200,
    device = ragg::agg_png,
    bg = "white"
  )
}
