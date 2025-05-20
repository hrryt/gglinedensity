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

data_frame0 <- function(...) {
  vctrs::data_frame(..., .name_repair = "minimal")
}
