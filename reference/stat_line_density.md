# Create a DenseLines Heatmap

`stat_line_density()` is a 'ggplot2' statistic implementing the
DenseLines algorithm described by Moritz and Fisher (2018).
`stat_path_density()` is to `stat_line_density()` as
[`geom_path()`](https://ggplot2.tidyverse.org/reference/geom_path.html)
is to
[`geom_line()`](https://ggplot2.tidyverse.org/reference/geom_path.html).

## Usage

``` r
stat_line_density(
  mapping = NULL,
  data = NULL,
  geom = "raster",
  position = "identity",
  ...,
  bins = 30,
  binwidth = NULL,
  drop = TRUE,
  orientation = NA,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

stat_path_density(
  mapping = NULL,
  data = NULL,
  geom = "raster",
  position = "identity",
  ...,
  bins = 30,
  binwidth = NULL,
  drop = TRUE,
  orientation = NA,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html). If
  specified and `inherit.aes = TRUE` (the default), it is combined with
  the default mapping at the top level of the plot. You must supply
  `mapping` if there is no plot mapping.

- data:

  The data to be displayed in this layer. There are three options:

  If `NULL`, the default, the data is inherited from the plot data as
  specified in the call to
  [`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

  A `data.frame`, or other object, will override the plot data. All
  objects will be fortified to produce a data frame. See
  [`fortify()`](https://ggplot2.tidyverse.org/reference/fortify.html)
  for which variables will be created.

  A `function` will be called with a single argument, the plot data. The
  return value must be a `data.frame`, and will be used as the layer
  data. A `function` can be created from a `formula` (e.g.
  `~ head(.x, 10)`).

- geom:

  The geometric object to use to display the data for this layer. When
  using a `stat_*()` function to construct a layer, the `geom` argument
  can be used to override the default coupling between stats and geoms.
  The `geom` argument accepts the following:

  - A `Geom` ggproto subclass, for example `GeomPoint`.

  - A string naming the geom. To give the geom as a string, strip the
    function name of the `geom_` prefix. For example, to use
    [`geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html),
    give the geom as `"point"`.

  - For more information and other ways to specify the geom, see the
    [layer
    geom](https://ggplot2.tidyverse.org/reference/layer_geoms.html)
    documentation.

- position:

  A position adjustment to use on the data for this layer. This can be
  used in various ways, including to prevent overplotting and improving
  the display. The `position` argument accepts the following:

  - The result of calling a position function, such as
    [`position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.html).
    This method allows for passing extra arguments to the position.

  - A string naming the position adjustment. To give the position as a
    string, strip the function name of the `position_` prefix. For
    example, to use
    [`position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.html),
    give the position as `"jitter"`.

  - For more information and other ways to specify the position, see the
    [layer
    position](https://ggplot2.tidyverse.org/reference/layer_positions.html)
    documentation.

- ...:

  Other arguments passed on to
  [`layer()`](https://ggplot2.tidyverse.org/reference/layer.html)'s
  `params` argument. These arguments broadly fall into one of 4
  categories below. Notably, further arguments to the `position`
  argument, or aesthetics that are required can *not* be passed through
  `...`. Unknown arguments that are not part of the 4 categories below
  are ignored.

  - Static aesthetics that are not mapped to a scale, but are at a fixed
    value and apply to the layer as a whole. For example,
    `colour = "red"` or `linewidth = 3`. The geom's documentation has an
    **Aesthetics** section that lists the available options. The
    'required' aesthetics cannot be passed on to the `params`. Please
    note that while passing unmapped aesthetics as vectors is
    technically possible, the order and required length is not
    guaranteed to be parallel to the input data.

  - When constructing a layer using a `stat_*()` function, the `...`
    argument can be used to pass on parameters to the `geom` part of the
    layer. An example of this is
    `stat_density(geom = "area", outline.type = "both")`. The geom's
    documentation lists which parameters it can accept.

  - Inversely, when constructing a layer using a `geom_*()` function,
    the `...` argument can be used to pass on parameters to the `stat`
    part of the layer. An example of this is
    `geom_area(stat = "density", adjust = 0.5)`. The stat's
    documentation lists which parameters it can accept.

  - The `key_glyph` argument of
    [`layer()`](https://ggplot2.tidyverse.org/reference/layer.html) may
    also be passed on through `...`. This can be one of the functions
    described as [key
    glyphs](https://ggplot2.tidyverse.org/reference/draw_key.html), to
    change the display of the layer in the legend.

- bins:

  Number of bins. Overridden by `binwidth`. Defaults to 30.

- binwidth:

  The width of the bins. Can be specified as a numeric value or as a
  function that takes x after scale transformation as input and returns
  a single numeric value. When specifying a function along with a
  grouping structure, the function will be called once per group. The
  default is to use the number of bins in `bins`, covering the range of
  the data. You should always override this value, exploring multiple
  widths to find the best to illustrate the stories in your data.

  The bin width of a date variable is the number of days in each time;
  the bin width of a time variable is the number of seconds.

- drop:

  if `TRUE` removes all cells with 0 counts.

- orientation:

  The orientation of the layer. The default (`NA`) automatically
  determines the orientation from the aesthetic mapping. In the rare
  event that this fails it can be given explicitly by setting
  `orientation` to either `"x"` or `"y"`. See the *Orientation* section
  for more detail.

- na.rm:

  If `FALSE`, the default, missing values are removed with a warning. If
  `TRUE`, missing values are silently removed.

- show.legend:

  logical. Should this layer be included in the legends? `NA`, the
  default, includes if any aesthetics are mapped. `FALSE` never
  includes, and `TRUE` always includes. It can also be a named logical
  vector to finely select the aesthetics to display. To include legend
  keys for all levels, even when no data exists, use `TRUE`. If `NA`,
  all levels are shown in legend, but unobserved levels are omitted.

- inherit.aes:

  If `FALSE`, overrides the default aesthetics, rather than combining
  with them. This is most useful for helper functions that define both
  data and aesthetics and shouldn't inherit behaviour from the default
  plot specification, e.g.
  [`annotation_borders()`](https://ggplot2.tidyverse.org/reference/annotation_borders.html).

## Details

`stat_line_density()` provides the `density` variable, which normalises
`count` by its sum in each column of bins with the same value of the
variable on the `orientation` axis. This is also provided by
`stat_path_density()`, but should be used with caution as the DenseLines
algorithm assumes lines are connected in order of the variable on the
`orientation` axis. `stat_path_density()` therefore defaults to
`aes(fill = after_stat(count))` rather than `after_stat(density)`.

## Aesthetics

`stat_line_density()` understands the following aesthetics (required
aesthetics are in bold):

- **`x`**

- **`y`**

- `group`

## Computed variables

These are calculated by the 'stat' part of layers and can be accessed
with [delayed
evaluation](https://ggplot2.tidyverse.org/reference/aes_eval.html).

- `after_stat(count)`  
  number of lines in bin.

- `after_stat(density)`  
  density of lines in bin. The result of the DenseLines algorithm.

- `after_stat(ncount)`  
  count, scaled to maximum of 1.

- `after_stat(ndensity)`  
  density, scaled to a maximum of 1.

## Orientation

This geom treats each axis differently and, thus, can thus have two
orientations. Often the orientation is easy to deduce from a combination
of the given mappings and the types of positional scales in use. Thus,
ggplot2 will by default try to guess which orientation the layer should
have. Under rare circumstances, the orientation is ambiguous and
guessing may fail. In that case the orientation can be specified
directly using the `orientation` parameter, which can be either `"x"` or
`"y"`. The value gives the axis that the geom should run along, `"x"`
being the default orientation you would expect for the geom.

## References

Moritz, D. & Fisher, D. (2018). Visualizing a Million Time Series with
the Density Line Chart. arXiv preprint arXiv:1409.0473.
[doi:10.48550/arxiv.1808.06019](https://doi.org/10.48550/arxiv.1808.06019)
.

## See also

[`ggplot2::stat_bin_2d()`](https://ggplot2.tidyverse.org/reference/geom_bin_2d.html),
[`ggplot2::geom_line()`](https://ggplot2.tidyverse.org/reference/geom_path.html),
[`ggplot2::geom_raster()`](https://ggplot2.tidyverse.org/reference/geom_tile.html).

## Examples

``` r
library(ggplot2)

p <- ggplot(txhousing, aes(date, median, group = city))

p +
  stat_line_density(drop = FALSE, na.rm = TRUE)


p +
  aes(fill = after_stat(count)) +
  stat_line_density(
    aes(colour = after_stat(count)),
    geom = "point", size = 10, bins = 15, na.rm = TRUE
  ) +
  stat_line_density(
    aes(label = after_stat(ifelse(count > 25, count, NA))),
    geom = "label", size = 6, bins = 15, na.rm = TRUE
  )


ggplot(txhousing, aes(median, date, group = city)) +
  stat_line_density(
    aes(fill = after_stat(ndensity)),
    bins = 50, orientation = "y", na.rm = TRUE
  )


m <- ggplot(economics, aes(unemploy/pop, psavert, group = date < as.Date("2000-01-01")))
m + geom_path(aes(colour = after_stat(group)))

m + stat_path_density()

```
