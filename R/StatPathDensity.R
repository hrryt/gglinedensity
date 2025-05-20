#' @name stat_line_density
#' @usage NULL
#' @format NULL
#' @export
StatPathDensity <- ggplot2::ggproto(
  "StatPathDensity", StatLineDensity,
  default_aes = ggplot2::aes(fill = after_stat(count)),
  setup_data = function(data, params) {
    ggplot2::flip_data(data, params$flipped_aes)
  }
)
