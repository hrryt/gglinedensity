#' @name stat_line_density
#' @usage NULL
#' @format NULL
#' @export
StatPathDensity <- ggplot2::ggproto(
  "StatPathDensity", StatLineDensity,
  setup_data = function(data, params) {
    ggplot2::flip_data(data, params$flipped_aes)
  }
)
