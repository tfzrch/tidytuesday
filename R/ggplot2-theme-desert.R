
# theme_desert() ----------------------------------------------------------
theme_desert <- function(palette = desert_palette, 
                         size.min = 9, 
                         size.title.min = 10, 
                         size.title.max = 15) {
  theme_classic() %+replace%
    # Basic elements ----
    theme(
      text = element_text(colour = palette[1], size = size.min),
      line = element_line(colour = palette[5])
    ) %+replace%
    theme(
      # Backgrounds ----
      panel.background = element_rect(fill = palette[3], colour = palette[3]),
      plot.background = element_rect(fill = palette[3], colour = palette[3]),
      # Axes ----
      axis.ticks = element_line(colour = palette[5]),
      axis.line = element_line(colour = palette[5]),
      # Axis Labelling ----
      axis.text = element_text(size = size.min, colour = "black"),
      axis.title = element_text(size = size.title.min, colour = palette[5], face = "bold"),
      # Plot Labelling ----
      plot.title = element_text(
        colour = palette[1],
        size = size.title.max,
        face = "bold"),
      plot.subtitle = element_text(
        colour = palette[1],
        size = size.title.max * .8,
        face = "italic"),
      plot.caption.position = "plot",
      plot.caption = element_text(hjust = 0),
      # Legends ----
      legend.background = element_rect(fill = palette[3], colour = palette[3]),
      legend.title = element_text(
        colour = palette[1],
        size = size.title.min,
        face = "bold"),
      legend.text = element_text(
        colour = palette[1],
        size = size.title.min * 0.8,
        face = "italic"),
      legend.key.width = unit(4, "mm"),
      legend.key.height = unit(4, "mm"),
      # Strips ----
      strip.background = element_rect(
        colour = palette[3],
        fill = palette[3]),
      strip.text = element_text(
        colour = palette[1],
        size = size.title.min,
        face = "bold")
    )
}

# theme_desert_map() ------------------------------------------------------
theme_desert_map <- function() {
  theme_desert() %+replace%
    theme(
      axis.text = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank()
    )
}
