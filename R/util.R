dual_param <- function (x, default = list(x = NULL, y = NULL), flipped_aes) {
  if (is.null(x)) {
    default
  }
  else if (length(x) == 2) {
    if (is.list(x) && !is.null(names(x))) {
      x
    }
    else {
      list(x = x[[1]], y = x[[2]])
    }
  }
  else {
    list(x = x, y = x)
  }
}

check_required_aesthetics <- function (required, present, name, call = rlang::caller_env()) {
  if (is.null(required))
    return()
  required <- strsplit(required, "|", fixed = TRUE)
  if (any(lengths(required) > 1)) {
    required <- lapply(required, rep_len, 2)
    required <- list(vapply(required, `[`, character(1),
                            1), vapply(required, `[`, character(1), 2))
  }
  else {
    required <- list(unlist(required))
  }
  missing_aes <- lapply(required, setdiff, present)
  if (any(lengths(missing_aes) == 0))
    return()
  message <- "{.fn {name}} requires the following missing aesthetics: {.field {missing_aes[[1]]}}"
  if (length(missing_aes) > 1) {
    message <- paste0(message, " {.strong or} {.field {missing_aes[[2]]}}")
  }
  cli::cli_abort(message, call = call)
}

fix_bin_params <- function (params, fun, version)
{
  if (!is.null(params$origin)) {
    args <- paste0(fun, c("(origin)", "(boundary)"))
    deprecate_warn0(version, args[1], args[2])
    params$boundary <- params$origin
    params$origin <- NULL
  }
  if (!is.null(params$right)) {
    args <- paste0(fun, c("(right)", "(closed)"))
    deprecate_warn0(version, args[1], args[2])
    params$closed <- if (isTRUE(params$right))
      "right"
    else "left"
    params$right <- NULL
  }
  if (is.null(params$breaks %||% params$binwidth %||% params$bins)) {
    cli::cli_inform("{.fn {fun}} using {.code bins = 30}. Pick better value {.arg binwidth}.")
    params$bins <- 30
  }
  params
}

deprecate_warn0 <- function(...) {
  user_env <- getOption("ggplot2_plot_env") %||% rlang::caller_env(2)
  lifecycle::deprecate_warn(..., user_env = user_env)
}

dapply <- function (df, by, fun, ..., drop = TRUE) {
  grouping_cols <- .subset(df, by)
  fallback_order <- unique0(c(by, names(df)))
  apply_fun <- function(x) {
    res <- fun(x, ...)
    if (is.null(res))
      return(res)
    if (length(res) == 0)
      return(data_frame0())
    vars <- lapply(`names<-`(by, by), function(col) .subset2(x,
                                                            col)[1])
    if (is.matrix(res))
      res <- split_matrix(res)
    if (is.null(names(res)))
      names(res) <- paste0("V", seq_along(res))
    if (all(by %in% names(res)))
      return(data_frame0(!!!unclass(res)))
    res <- modify_list(unclass(vars), unclass(res))
    res <- res[intersect(c(fallback_order, names(res)),
                         names(res))]
    data_frame0(!!!res)
  }
  has_single_group <- all(vapply(grouping_cols, function(x) identical(as.character(levels(x) %||%
                                                                                     attr(x, "n")), "1"), logical(1)))
  if (has_single_group) {
    return(apply_fun(df))
  }
  ids <- id(grouping_cols, drop = drop)
  group_rows <- split_with_index(seq_len(nrow(df)), ids)
  result <- lapply(seq_along(group_rows), function(i) {
    cur_data <- vctrs::vec_slice(df, group_rows[[i]])
    apply_fun(cur_data)
  })
  vec_rbind0(!!!result)
}

id <- function (.variables, drop = FALSE) {
  nrows <- NULL
  if (is.data.frame(.variables)) {
    nrows <- nrow(.variables)
    .variables <- unclass(.variables)
  }
  lengths <- lengths(.variables)
  .variables <- .variables[lengths != 0]
  if (length(.variables) == 0) {
    n <- nrows %||% 0L
    id <- seq_len(n)
    attr(id, "n") <- n
    return(id)
  }
  if (length(.variables) == 1) {
    return(id_var(.variables[[1]], drop = drop))
  }
  ids <- rev(lapply(.variables, id_var, drop = drop))
  p <- length(ids)
  ndistinct <- vapply(ids, attr, "n", FUN.VALUE = numeric(1),
                      USE.NAMES = FALSE)
  n <- prod(ndistinct)
  if (n > 2^31) {
    char_id <- rlang::inject(paste(!!!ids, sep = "\r"))
    res <- match(char_id, unique0(char_id))
  }
  else {
    combs <- c(1, cumprod(ndistinct[-p]))
    mat <- rlang::inject(cbind(!!!ids))
    res <- c((mat - 1L) %*% combs + 1L)
  }
  if (drop) {
    id_var(res, drop = TRUE)
  }
  else {
    res <- as.integer(res)
    attr(res, "n") <- n
    res
  }
}

id_var <- function (x, drop = FALSE) {
  if (length(x) == 0) {
    id <- integer()
    n <- 0L
  }
  else if (!is.null(attr(x, "n")) && !drop) {
    return(x)
  }
  else if (is.factor(x) && !drop) {
    x <- addNA(x, ifany = TRUE)
    id <- as.integer(x)
    n <- nlevels(x)
  }
  else {
    levels <- sort(unique0(x), na.last = TRUE)
    id <- match(x, levels)
    n <- max(id)
  }
  attr(id, "n") <- n
  id
}

unique0 <- function(x) {
  x %||% vctrs::vec_unique(x)
}

data_frame0 <- function(...) {
  vctrs::data_frame(..., .name_repair = "minimal")
}

split_matrix <- function(x, col_names = colnames(x)) {
  force(col_names)
  x <- lapply(seq_len(ncol(x)), function(i) x[, i])
  if (!is.null(col_names))
    names(x) <- col_names
  x
}

modify_list <- function (old, new) {
  for (i in names(new)) old[[i]] <- new[[i]]
  old
}

split_with_index <- function (x, f, n = max(f)) {
  if (n == 1)
    return(list(x))
  f <- as.integer(f)
  attributes(f) <- list(levels = as.character(seq_len(n)),
                        class = "factor")
  unname(split(x, f))
}

vec_rbind0 <- function (..., .error_call = rlang::current_env(), .call = rlang::caller_env()) {
  with_ordered_restart(vctrs::vec_rbind(..., .error_call = .error_call),
                       .call)
}

with_ordered_restart <- function (expr, .call) {
  withCallingHandlers(expr, vctrs_error_incompatible_type = function(cnd) {
    x <- cnd[["x"]]
    y <- cnd[["y"]]
    class_x <- class(x)[1]
    class_y <- class(y)[1]
    restart <- FALSE
    if (is.ordered(x) || is.ordered(y)) {
      restart <- TRUE
      if (is.ordered(x)) {
        x <- factor(as.character(x), levels = levels(x))
      }
      if (is.ordered(y)) {
        y <- factor(as.character(y), levels = levels(y))
      }
    }
    else if (is.character(x) || is.character(y)) {
      restart <- TRUE
      if (is.character(x)) {
        y <- as.character(y)
      }
      else {
        x <- as.character(x)
      }
    }
    else if (is.factor(x) || is.factor(y)) {
      restart <- TRUE
      lev <- c()
      if (is.factor(x)) {
        lev <- c(lev, levels(x))
      }
      if (is.factor(y)) {
        lev <- c(lev, levels(y))
      }
      x <- factor(as.character(x), levels = unique(lev))
      y <- factor(as.character(y), levels = unique(lev))
    }
    if (!restart) {
      return(rlang::zap())
    }
    msg <- paste0("Combining variables of class <", class_x,
                  "> and <", class_y, ">")
    desc <- paste0("Please ensure your variables are compatible before plotting (location: ",
                   rlang::format_error_call(.call), ")")
    deprecate_soft0("3.4.0", I(msg), details = desc)
    x_arg <- cnd[["x_arg"]]
    y_arg <- cnd[["y_arg"]]
    call <- cnd[["call"]]
    if (inherits(cnd, "vctrs_error_ptype2")) {
      out <- vctrs::vec_ptype2(x, y, x_arg = x_arg, y_arg = y_arg, call = call)
      restart <- "vctrs_restart_ptype2"
    }
    else if (inherits(cnd, "vctrs_error_cast")) {
      out <- vctrs::vec_cast(x, y, x_arg = x_arg, to_arg = y_arg, call = call)
      restart <- "vctrs_restart_cast"
    }
    else {
      return(rlang::zap())
    }
    try_restart <- function(restart, ...) {
      if (!rlang::is_null(findRestart(restart))) {
        invokeRestart(restart, ...)
      }
    }
    try_restart(restart, out)
  })
}

deprecate_soft0 <- function(...) {
  user_env <- getOption("ggplot2_plot_env") %||% rlang::caller_env(2)
  lifecycle::deprecate_soft(..., user_env = user_env)
}

compute_bins <- function (x, scale = NULL, breaks = NULL, binwidth = NULL, bins = NULL,
                          center = NULL, boundary = NULL, closed = c("right", "left")) {
  range <- if (ggplot2::is_scale(scale))
    scale$dimension()
  else range(x)
  check_length(range, 2L)
  if (!is.null(breaks)) {
    breaks <- allow_lambda(breaks)
    if (is.function(breaks)) {
      breaks <- breaks(x)
    }
    if (ggplot2::is_scale(scale) && !scale$is_discrete()) {
      breaks <- scale$transform(breaks)
    }
    check_numeric(breaks)
    bins <- bin_breaks(breaks, closed)
    return(bins)
  }
  check_number_decimal(boundary, allow_infinite = FALSE, allow_null = TRUE)
  check_number_decimal(center, allow_infinite = FALSE, allow_null = TRUE)
  if (!is.null(boundary) && !is.null(center)) {
    cli::cli_abort("Only one of {.arg boundary} and {.arg center} may be specified.")
  }
  if (!is.null(binwidth)) {
    binwidth <- allow_lambda(binwidth)
    if (is.function(binwidth)) {
      binwidth <- binwidth(x)
    }
    check_number_decimal(binwidth, min = 0, allow_infinite = FALSE)
    bins <- bin_breaks_width(range, binwidth, center = center,
                             boundary = boundary, closed = closed)
    return(bins)
  }
  bins <- allow_lambda(bins)
  if (is.function(bins)) {
    bins <- bins(x)
  }
  check_number_whole(bins, min = 1, allow_infinite = FALSE)
  bin_breaks_bins(range, bins, center = center, boundary = boundary,
                  closed = closed)
}

check_length <- function (x, length = integer(), ..., min = 0, max = Inf, arg = rlang::caller_arg(x),
                          call = rlang::caller_env()) {
  if (missing(x)) {
    stop_input_type(x, "a vector", arg = arg, call = call)
  }
  n <- length(x)
  if (n %in% length) {
    return(invisible(NULL))
  }
  fmt <- if (inherits(arg, "AsIs"))
    identity
  else function(x) sprintf("`%s`", x)
  if (length(length) > 0) {
    type <- paste0("a vector of length ", oxford_comma(length))
    if (length(length) == 1) {
      type <- switch(sprintf("%d", length), `0` = "an empty vector",
                     `1` = "a scalar of length 1", type)
    }
    msg <- sprintf("%s must be %s, not length %d.", fmt(arg),
                   type, n)
    cli::cli_abort(msg, call = call, arg = arg)
  }
  range <- pmax(range(min, max, na.rm = TRUE), 0)
  if (n >= min & n <= max) {
    return(invisible(NULL))
  }
  if (identical(range[1], range[2])) {
    check_length(x, range[1], arg = arg, call = call)
    return(invisible(NULL))
  }
  type <- if (range[2] == 1)
    "scalar"
  else "vector"
  what <- paste0("a length between ", range[1], " and ", range[2])
  if (identical(range[2], Inf)) {
    what <- paste0("at least length ", range[1])
  }
  if (identical(range[1], 0)) {
    what <- paste0("at most length ", range[2])
  }
  msg <- sprintf("`%s` must be a %s with %s, not length %d.",
                 fmt(arg), type, what, n)
  cli::cli_abort(msg, call = call, arg = arg)
}

allow_lambda <- function(x) {
  if (rlang::is_formula(x)) rlang::as_function(x) else x
}

bin_breaks <- function(breaks, closed = c("right", "left"), fuzz = NULL) {
  check_numeric(breaks)
  closed <- rlang::arg_match0(closed, c("right", "left"))
  breaks <- sort(breaks)
  fuzz <- fuzz %||% 1e-08 * stats::median(diff(breaks[is.finite(breaks)]))
  if (!is.finite(fuzz)) {
    fuzz <- .Machine$double.eps * 1000
  }
  if (closed == "right") {
    fuzzes <- c(-fuzz, rep.int(fuzz, length(breaks) - 1))
  }
  else {
    fuzzes <- c(rep.int(-fuzz, length(breaks) - 1), fuzz)
  }
  structure(list(breaks = breaks, fuzzy = breaks + fuzzes,
                 right_closed = closed == "right"), class = "ggplot2_bins")
}

bin_breaks_width <- function (x_range, width = NULL, center = NULL, boundary = NULL,
                              closed = c("right", "left")) {
  if (is.null(boundary)) {
    if (is.null(center)) {
      boundary <- width/2
    }
    else {
      boundary <- center - width/2
    }
  }
  shift <- floor((x_range[1] - boundary)/width)
  origin <- boundary + shift * width
  max_x <- x_range[2] + (1 - 1e-08) * width
  if (isTRUE((max_x - origin)/width > 1e+06)) {
    cli::cli_abort(c("The number of histogram bins must be less than 1,000,000.",
                     i = "Did you make {.arg binwidth} too small?"))
  }
  breaks <- seq(origin, max_x, width)
  if (length(breaks) == 1) {
    breaks <- c(breaks, breaks + width)
  }
  bin_breaks(breaks, closed = closed)
}

bin_breaks_bins <- function (x_range, bins = 30, center = NULL, boundary = NULL,
                             closed = c("right", "left")) {
  if (scales::zero_range(x_range)) {
    width <- 0.1
  }
  else if (bins == 1) {
    width <- diff(x_range)
    boundary <- x_range[1]
    center <- NULL
  }
  else {
    width <- (x_range[2] - x_range[1])/(bins - 1)
    if (is.null(center)) {
      boundary <- boundary %||% x_range[1] - width/2
    }
  }
  bin_breaks_width(x_range, width, boundary = boundary, center = center,
                   closed = closed)
}

check_numeric <- function (x, what = "a {.cls numeric} vector", ..., arg = rlang::caller_arg(x),
                           call = rlang::caller_env()) {
  check_object(x, is.numeric, what, ..., arg = arg, call = call)
}

check_object <- function (x, check_fun, what, ..., allow_na = FALSE, allow_null = FALSE,
                          arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (!missing(x)) {
    if (check_fun(x)) {
      return(invisible(NULL))
    }
    if (allow_null && rlang::is_null(x)) {
      return(invisible(NULL))
    }
    if (allow_na && all(is.na(x))) {
      return(invisible(NULL))
    }
  }
  stop_input_type(x, as_cli(what), ..., allow_null = allow_null,
                  arg = arg, call = call)
}

as_cli <- function (..., env = rlang::caller_env()) {
  cli::cli_fmt(cli::cli_text(..., .envir = env))
}

stop_input_type <- function (x, what, ..., allow_na = FALSE, allow_null = FALSE,
                             show_value = TRUE, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  cli <- rlang::env_get_list(nms = c("format_arg", "format_code"),
                      last = topenv(), default = function(x) sprintf("`%s`",
                                                                     x), inherit = TRUE)
  if (allow_na) {
    what <- c(what, cli$format_code("NA"))
  }
  if (allow_null) {
    what <- c(what, cli$format_code("NULL"))
  }
  if (length(what)) {
    what <- oxford_comma(what)
  }
  if (inherits(arg, "AsIs")) {
    format_arg <- identity
  }
  else {
    format_arg <- cli$format_arg
  }
  message <- sprintf("%s must be %s, not %s.", format_arg(arg),
                     what, obj_type_friendly(x, value = show_value))
  rlang::abort(message, ..., call = call, arg = arg)
}

oxford_comma <- function (chr, sep = ", ", final = "or") {
  n <- length(chr)
  if (n < 2) {
    return(chr)
  }
  head <- chr[seq_len(n - 1)]
  last <- chr[n]
  head <- paste(head, collapse = sep)
  if (n > 2) {
    paste0(head, sep, final, " ", last)
  }
  else {
    paste0(head, " ", final, " ", last)
  }
}

obj_type_friendly <- function (x, value = TRUE)
{
  if (rlang::is_missing(x)) {
    return("absent")
  }
  if (is.object(x)) {
    if (inherits(x, "quosure")) {
      type <- "quosure"
    }
    else {
      type <- class(x)[[1L]]
    }
    return(sprintf("a <%s> object", type))
  }
  if (!rlang::is_vector(x)) {
    return(.rlang_as_friendly_type(typeof(x)))
  }
  n_dim <- length(dim(x))
  if (!n_dim) {
    if (!rlang::is_list(x) && length(x) == 1) {
      if (rlang::is_na(x)) {
        return(switch(typeof(x), logical = "`NA`", integer = "an integer `NA`",
                      double = if (is.nan(x)) {
                        "`NaN`"
                      } else {
                        "a numeric `NA`"
                      }, complex = "a complex `NA`", character = "a character `NA`",
                      .rlang_stop_unexpected_typeof(x)))
      }
      show_infinites <- function(x) {
        if (x > 0) {
          "`Inf`"
        }
        else {
          "`-Inf`"
        }
      }
      str_encode <- function(x, width = 30, ...) {
        if (nchar(x) > width) {
          x <- substr(x, 1, width - 3)
          x <- paste0(x, "...")
        }
        encodeString(x, ...)
      }
      if (value) {
        if (is.numeric(x) && is.infinite(x)) {
          return(show_infinites(x))
        }
        if (is.numeric(x) || is.complex(x)) {
          number <- as.character(round(x, 2))
          what <- if (is.complex(x))
            "the complex number"
          else "the number"
          return(paste(what, number))
        }
        return(switch(typeof(x), logical = if (x) "`TRUE`" else "`FALSE`",
                      character = {
                        what <- if (nzchar(x)) "the string" else "the empty string"
                        paste(what, str_encode(x, quote = "\""))
                      }, raw = paste("the raw value", as.character(x)),
                      .rlang_stop_unexpected_typeof(x)))
      }
      return(switch(typeof(x), logical = "a logical value",
                    integer = "an integer", double = if (is.infinite(x)) show_infinites(x) else "a number",
                    complex = "a complex number", character = if (nzchar(x)) "a string" else "\"\"",
                    raw = "a raw value", .rlang_stop_unexpected_typeof(x)))
    }
    if (length(x) == 0) {
      return(switch(typeof(x), logical = "an empty logical vector",
                    integer = "an empty integer vector", double = "an empty numeric vector",
                    complex = "an empty complex vector", character = "an empty character vector",
                    raw = "an empty raw vector", list = "an empty list",
                    .rlang_stop_unexpected_typeof(x)))
    }
  }
  vec_type_friendly(x)
}

vec_type_friendly <- function (x, length = FALSE) {
  if (!rlang::is_vector(x)) {
    rlang::abort("`x` must be a vector.")
  }
  type <- typeof(x)
  n_dim <- length(dim(x))
  add_length <- function(type) {
    if (length && !n_dim) {
      paste0(type, sprintf(" of length %s", length(x)))
    }
    else {
      type
    }
  }
  if (type == "list") {
    if (n_dim < 2) {
      return(add_length("a list"))
    }
    else if (is.data.frame(x)) {
      return("a data frame")
    }
    else if (n_dim == 2) {
      return("a list matrix")
    }
    else {
      return("a list array")
    }
  }
  type <- switch(type, logical = "a logical %s", integer = "an integer %s",
                 numeric = , double = "a double %s", complex = "a complex %s",
                 character = "a character %s", raw = "a raw %s", type = paste0("a ",
                                                                               type, " %s"))
  if (n_dim < 2) {
    kind <- "vector"
  }
  else if (n_dim == 2) {
    kind <- "matrix"
  }
  else {
    kind <- "array"
  }
  out <- sprintf(type, kind)
  if (n_dim >= 2) {
    out
  }
  else {
    add_length(out)
  }
}

.rlang_as_friendly_type <- function(type) {
  switch(type, list = "a list", `NULL` = "`NULL`", environment = "an environment",
         externalptr = "a pointer", weakref = "a weak reference",
         S4 = "an S4 object", name = , symbol = "a symbol", language = "a call",
         pairlist = "a pairlist node", expression = "an expression vector",
         char = "an internal string", promise = "an internal promise",
         ... = "an internal dots object", any = "an internal `any` object",
         bytecode = "an internal bytecode object", primitive = ,
         builtin = , special = "a primitive function", closure = "a function",
         type)
}

.rlang_stop_unexpected_typeof <- function(x, call = rlang::caller_env()) {
  rlang::abort(sprintf("Unexpected type <%s>.", typeof(x)), call = call)
}

check_number_decimal <- function (x, ..., min = NULL, max = NULL, allow_infinite = TRUE,
                                  allow_na = FALSE, allow_null = FALSE, arg = rlang::caller_arg(x),
                                  call = rlang::caller_env()) {
  if (missing(x)) {
    exit_code <- 1
  }
  else if (0 == (exit_code <- .standalone_types_check_dot_call(rlang::ffi_standalone_check_number_1.0.7,
                                                               x, allow_decimal = TRUE, min, max, allow_infinite, allow_na,
                                                               allow_null))) {
    return(invisible(NULL))
  }
  .stop_not_number(x, ..., exit_code = exit_code, allow_decimal = TRUE,
                   min = min, max = max, allow_na = allow_na, allow_null = allow_null,
                   arg = arg, call = call)
}

.standalone_types_check_dot_call <- .Call

.stop_not_number <- function (x, ..., exit_code, allow_decimal, min, max, allow_na,
                              allow_null, arg, call) {
  if (allow_decimal) {
    what <- "a number"
  }
  else {
    what <- "a whole number"
  }
  if (exit_code == 2) {
    min <- min %||% -Inf
    max <- max %||% Inf
    if (min > -Inf && max < Inf) {
      what <- sprintf("%s between %s and %s", what, min,
                      max)
    }
    else if (x < min) {
      what <- sprintf("%s larger than or equal to %s",
                      what, min)
    }
    else if (x > max) {
      what <- sprintf("%s smaller than or equal to %s",
                      what, max)
    }
    else {
      rlang::abort("Unexpected state in OOB check", .internal = TRUE)
    }
  }
  stop_input_type(x, what, ..., allow_na = allow_na, allow_null = allow_null,
                  arg = arg, call = call)
}

check_number_whole <- function (x, ..., min = NULL, max = NULL, allow_infinite = FALSE,
                                allow_na = FALSE, allow_null = FALSE, arg = rlang::caller_arg(x),
                                call = rlang::caller_env()) {
  if (missing(x)) {
    exit_code <- 1
  }
  else if (0 == (exit_code <- .standalone_types_check_dot_call(rlang::ffi_standalone_check_number_1.0.7,
                                                               x, allow_decimal = FALSE, min, max, allow_infinite, allow_na,
                                                               allow_null))) {
    return(invisible(NULL))
  }
  .stop_not_number(x, ..., exit_code = exit_code, allow_decimal = FALSE,
                   min = min, max = max, allow_na = allow_na, allow_null = allow_null,
                   arg = arg, call = call)
}
