#' @name stat_line_density
#' @usage NULL
#' @format NULL
#' @export
StatLineDensity <- ggplot2::ggproto(
  "StatLineDensity", ggplot2::Stat,
  required_aes = c("x", "y"),
  default_aes = ggplot2::aes(fill = after_stat(density)),
  extra_params = c("na.rm", "orientation"),
  setup_params = function(data, params) {
    params$flipped_aes <- ggplot2::has_flipped_aes(
      data, params, ambiguous = TRUE
    )
    if (is.character(params$drop)) {
      params$drop <- !identical(params$drop, "none")
    }
    params <- fix_bin_params(params, fun = "stat_line_density")
    vars <- c("origin", "binwidth", "breaks", "center", "boundary")
    params[vars] <- lapply(params[vars], dual_param, default = NULL)
    params$closed <- dual_param(params$closed, list(x = "right", y = "right"))
    params
  },
  setup_data = function(data, params) {
    data <- ggplot2::flip_data(data, params$flipped_aes)
    data <- data[order(data$x), ]
    data
  },
  compute_layer = function(data, params, layout, self = self) {
    check_required_aesthetics(
      self$required_aes, c(names(data), names(params)), "stat_line_density"
    )
    required_aes <- intersect(
      names(data), unlist(strsplit(self$required_aes, "|", fixed = TRUE))
    )
    ggplot2::remove_missing(
      data, params$na.rm,
      c(required_aes, self$non_missing_aes), "stat_line_density", finite = TRUE
    )
    params <- params[intersect(names(params), self$parameters())]
    args <- c(list(data = quote(data), scales = quote(scales)), params)
    dapply(data, "PANEL", function(data) {
      scales <- layout$get_scales(data$PANEL[1])
      rlang::try_fetch(
        rlang::inject(self$compute_panel(data = data, scales = scales, !!!params)),
        error = function(cnd) {
          cli::cli_warn("Computation failed in {.fn stat_line_density}", parent = cnd)
          data_frame0()
        }
      )
    })
  },
  compute_panel = function(data, scales, binwidth = NULL, bins = 30, breaks = NULL,
                           origin = NULL, drop = TRUE, boundary = NULL, closed = NULL,
                           center = NULL, normalise = TRUE, flipped_aes = FALSE) {
    boundary <- boundary %||% if (is.null(center)) list(x = 0, y = 0)
    bins <- dual_param(bins, list(x = 30, y = 30))

    if(flipped_aes) {
      scales_x <- scales$y
      scales_y <- scales$x
    } else {
      scales_x <- scales$x
      scales_y <- scales$y
    }

    xbin <- compute_bins(
      data$x, scales_x, breaks$x, binwidth$x,
      bins$x, center$x, boundary$x, closed$x
    )
    ybin <- compute_bins(
      data$y, scales_y, breaks$y, binwidth$y,
      bins$y, center$y, boundary$y, closed$y
    )

    width <- length(xbin$breaks) - 1L
    height <- length(ybin$breaks) - 1L

    x_binned <- (data$x - min(xbin$breaks)) * width / (max(xbin$breaks) - min(xbin$breaks))
    y_binned <- (data$y - min(ybin$breaks)) * height / (max(ybin$breaks) - min(ybin$breaks))

    xy <- lapply(split(cbind(x_binned, y_binned), data$group), as.double)
    rast <- line_density(xy, width, height, !normalise)

    new_x <- xbin$breaks[-1] - diff(xbin$breaks) / 2
    new_y <- ybin$breaks[-1] - diff(ybin$breaks) / 2

    xy <- expand.grid(x = new_x, y = new_y)
    xy$density <- rast

    if(drop) xy <- xy[rast != 0, ]

    xy$flipped_aes <- flipped_aes
    ggplot2::flip_data(xy, flipped_aes)
  }
)
