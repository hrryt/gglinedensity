test_that("stat_line_density works", {
  library(ggplot2)
  p <- ggplot(txhousing, aes(date, median, group = city))
  vdiffr::expect_doppelganger(
    "DenseLines heatmap",
    p + stat_line_density(bins = 10, drop = FALSE, na.rm = TRUE)
  )
  vdiffr::expect_doppelganger(
    "Normalised counts",
    p + stat_line_density(aes(fill = after_stat(ncount)), na.rm = TRUE)
  )
})

test_that("stat_line_density is orientable", {
  library(ggplot2)
  p <- ggplot(txhousing, aes(median, date, group = city))
  vdiffr::expect_doppelganger(
    "Flipped DenseLines heatmap",
    p + stat_line_density(aes(fill = after_stat(ndensity)), na.rm = TRUE)
  )
})

test_that("stat_path_density works", {
  library(ggplot2)
  m <- ggplot(economics, aes(unemploy/pop, psavert, group = date < as.Date("2000-01-01")))
  m + geom_path(aes(colour = after_stat(group)))
  vdiffr::expect_doppelganger(
    "Overlapping paths",
    m + stat_path_density()
  )
})
