#' Create a DenseLines Heatmap
#'
#' `stat_line_density()` is a 'ggplot2' statistic implementing the DenseLines
#' algorithm described by Moritz and Fisher (2018). `stat_path_density()` is to
#' `stat_line_density()` as `geom_path()` is to `geom_line()`.
#'
#' `stat_line_density()` provides the `density` variable, which normalises
#' `count` by its sum in each column of bins with the same value
#' of the variable on the `orientation` axis. This is also provided by
#' `stat_path_density()`, but should be used with caution as the DenseLines
#' algorithm assumes lines are connected in order of the variable on the
#' `orientation` axis. `stat_path_density()` therefore defaults to
#' `aes(fill = after_stat(count))` rather than `after_stat(density)`.
#'
#' @aliases gglinedensity
#' @inheritParams ggplot2::stat_bin_2d
#' @inheritParams ggplot2::stat_identity
#' @inheritParams ggplot2::stat_count
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
#' * `after_stat(count)` \cr
#' number of lines in bin.
#' * `after_stat(density)` \cr
#' density of lines in bin. The result of the DenseLines algorithm.
#' * `after_stat(ncount)` \cr
#' count, scaled to maximum of 1.
#' * `after_stat(ndensity)` \cr
#' density, scaled to a maximum of 1.
#'
#' @inheritSection ggplot2::stat_count Orientation
#'
#' @seealso [ggplot2::stat_bin_2d()], [ggplot2::geom_line()], [ggplot2::geom_raster()].
#'
#' @references Moritz, D. & Fisher, D. (2018).
#' Visualizing a Million Time Series with the Density Line Chart.
#' arXiv preprint arXiv:1409.0473. \doi{10.48550/arxiv.1808.06019}.
#'
#' @examples
#' library(ggplot2)
#'
#' p <- ggplot(txhousing, aes(date, median, group = city))
#'
#' p +
#'   stat_line_density(drop = FALSE, na.rm = TRUE)
#'
#' p +
#'   aes(fill = after_stat(count)) +
#'   stat_line_density(
#'     aes(colour = after_stat(count)),
#'     geom = "point", size = 10, bins = 15, na.rm = TRUE
#'   ) +
#'   stat_line_density(
#'     aes(label = after_stat(ifelse(count > 25, count, NA))),
#'     geom = "label", size = 6, bins = 15, na.rm = TRUE
#'   )
#'
#' ggplot(txhousing, aes(median, date, group = city)) +
#'   stat_line_density(
#'     aes(fill = after_stat(ndensity)),
#'     bins = 50, orientation = "y", na.rm = TRUE
#'   )
#'
#' m <- ggplot(economics, aes(unemploy/pop, psavert, group = date < as.Date("2000-01-01")))
#' m + geom_path(aes(colour = after_stat(group)))
#' m + stat_path_density()
#'
#' @export
stat_line_density <- function(mapping = NULL, data = NULL, geom = "raster",
                              position = "identity", ..., bins = 30,
                              binwidth = NULL, drop = TRUE, orientation = NA,
                              na.rm = FALSE, show.legend = NA,
                              inherit.aes = TRUE) {
  ggplot2::layer(
    stat = StatLineDensity, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = rlang::list2(
      bins = bins, binwidth = binwidth, drop = drop,
      orientation = orientation, na.rm = na.rm, ...
    )
  )
}


#' @rdname stat_line_density
#' @export
stat_path_density <- function(mapping = NULL, data = NULL, geom = "raster",
                              position = "identity", ..., bins = 30,
                              binwidth = NULL, drop = TRUE, orientation = NA,
                              na.rm = FALSE, show.legend = NA,
                              inherit.aes = TRUE) {
  ggplot2::layer(
    stat = StatPathDensity, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = rlang::list2(
      bins = bins, binwidth = binwidth, drop = drop,
      orientation = orientation, na.rm = na.rm, ...
    )
  )
}
