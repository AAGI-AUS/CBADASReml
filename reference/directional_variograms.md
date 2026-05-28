# A Spatial Variogram Function for Single Site Data

This function allows you to observe directional variograms in the 0
(vertical) and 90 (horizontal) directions for gridded small-plot trial
data.

## Usage

``` r
directional_variograms(model)
```

## Arguments

- model:

  `asreml` A model with some residual structure.

## Value

[`gstat::variogram`](https://r-spatial.github.io/gstat/reference/variogram.html)
A Gstat variogram.

## Examples

``` r
if (FALSE) { # requireNamespace("asreml", quietly = TRUE)
library(asreml)
model <- asreml(
    fixed = yield ~ weed,
    random = ~idv(Variety),
    residual = ~ar1v(Row):id(Column),
    data = wheat
)
mod <- directional_variograms(model)
plot(mod, multipanel = FALSE)

if (FALSE) { # \dontrun{
# You can also plot using ggplot if you wish
ggplot(
    mod,
    aes(x = dist, y = gamma, group = dir.hor, colour = factor(dir.hor))
) +
    geom_point() +
    geom_line() +
    facet_grid(~dir.hor, scales = "free_x")
} # }
}
```
