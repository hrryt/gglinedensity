#' Create a DenseLines Heatmap
#'
#' A 'ggplot2' statistic implementing the DenseLines algorithm
#' described by Moritz and Fisher (2018).
#'
#' @aliases gglinedensity
#' @inheritParams ggplot2::stat_bin_2d
#' @inheritParams ggplot2::stat_identity
#' @inheritParams ggplot2::stat_count
#' @param normalise if `TRUE`, the default, density is normalised per group
#' by the sum in each bin vertically, or horizontally if
#' `orientation` is set to `"y"`.
#' @returns A [ggplot2::layer()].
#'
#' @section Aesthetics:
#' `stat_line_density()` understands the following aesthetics
#' (required aesthetics are in bold):
#' * **`x`**
#' * **`y`**
#' * `group`
#'
#' @section Computed variables:
#' These are calculated by the 'stat' part of layers and can be accessed with
#' [delayed evaluation][ggplot2::aes_eval].
#' * `after_stat(density)` \cr
#' density estimate.
#'
#' @inheritSection ggplot2::stat_count Orientation
#'
#' @seealso [ggplot2::stat_bin_2d()], [ggplot2::geom_line()], [ggplot2::geom_raster()].
#'
#' @references Moritz, D. & Fisher, D. (2018).
#' Visualizing a Million Time Series with the Density Line Chart.
#' arXiv preprint arXiv:1409.0473.
#' <https://doi.org/10.48550/arxiv.1808.06019>.
#'
#' @examples
#' library(ggplot2)
#'
#' p <- ggplot(txhousing, aes(date, median, group = city))
#'
#' p +
#'   stat_line_density(na.rm = TRUE)
#'
#' p +
#'   stat_line_density(
#'     # map density to colour rather than fill
#'     aes(colour = after_stat(density)),
#'     geom = "point", size = 5, na.rm = TRUE
#'   ) +
#'   stat_line_density(
#'     aes(
#'       # add a label where density > 7
#'       label = after_stat(ifelse(density > 7, round(density, 2), NA)),
#'       # label background fill
#'       fill = after_stat(density)
#'     ),
#'     geom = "label", na.rm = TRUE
#'   ) +
#'   scale_colour_viridis_c(trans = "log10") +
#'   scale_fill_viridis_c(trans = "log10")
#'
#' p +
#'   stat_line_density(
#'     # convert to factor for a discrete scale
#'     aes(fill = after_stat(as.factor(density))),
#'     normalise = FALSE, drop = FALSE, na.rm = TRUE
#'   ) +
#'   geom_text( # equivalent to stat_line_density(geom = "text")
#'     aes(label = after_stat(ifelse(density > 20, density, NA)), fill = NULL),
#'     stat = "line_density", # or stat = StatLineDensity
#'     normalise = FALSE, na.rm = TRUE
#'   ) +
#'   scale_fill_ordinal(name = "count")
#'
#' ggplot(txhousing, aes(median, date, group = city)) +
#'   stat_line_density(
#'     # scale the maximum density to 1
#'     aes(fill = after_stat(density / max(density))),
#'     bins = 50, orientation = "y", na.rm = TRUE
#'   ) +
#'   scale_fill_continuous(name = "density") +
#'   scale_y_reverse()
#'
#' @export
stat_line_density <- function(mapping = NULL, data = NULL, geom = "raster",
                              position = "identity", ..., bins = 30,
                              binwidth = NULL, drop = TRUE, normalise = TRUE,
                              orientation = NA, na.rm = FALSE, show.legend = NA,
                              inherit.aes = TRUE) {
  ggplot2::layer(
    stat = StatLineDensity, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = rlang::list2(
      bins = bins, binwidth = binwidth, drop = drop, normalise = normalise,
      orientation = orientation, na.rm = na.rm, ...
    )
  )
}
