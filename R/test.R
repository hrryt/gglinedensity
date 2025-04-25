GeomLineTest <- ggplot2::ggproto(
  "GeomLineTest", ggplot2::GeomLine,
  setup_data = function(data, params) {
    line_data <<- data
    data$flipped_aes <- params$flipped_aes
    data <- ggplot2::flip_data(data, params$flipped_aes)
    data <- data[order(data$PANEL, data$group, data$x), ]
    ggplot2::flip_data(data, params$flipped_aes)
  },
  draw_panel = function(data, panel_params, coord, self = self) {
    draw_data <<- data
    ggplot2::ggproto_parent(ggplot2::GeomLine, self)$draw_panel(data, panel_params, coord)
  }
)

geom_line_test <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity",
                            na.rm = FALSE, orientation = NA, show.legend = NA, inherit.aes = TRUE,
                            ...)
{
  ggplot2::layer(data = data, mapping = mapping, stat = stat, geom = GeomLineTest,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = rlang::list2(na.rm = na.rm, orientation = orientation,
                       ...))
}
