# Rotate a georeferenced data frame

Rotate a provided georeferenced dataframe by a specified angle in
degrees.

## Usage

``` r
ofe_rotate_data(data, angle)
```

## Arguments

- data:

  `data.frame`. The dataframe to be rotated. This should be
  georeferenced per the [sf](https://CRAN.R-project.org/package=sf)
  package.

- angle:

  `numeric`. Clockwise angle in degrees to rotate the dataframe.

## Value

`data.frame`. `data` with geometry now defined in rotated coordinates.
Contains the same columns as the input with the following additions:

- x_original:

  The original x-coordinates before rotation.

- y_original:

  The original y-coordinates before rotation.

## Author

Braden Thorne, <braden.thorne@curtin.edu.au>

## Examples

``` r
if (FALSE) { # requireNamespace("asreml", quietly = TRUE)
library(CBADASReml)

data <- agridat::lasrosas.corn |>
filter(year == 2001) |>
rename(
  Yield = yield,
  Treatment = nf,
  Rep = rep
 ) |>
 dplyr::select(Yield, Treatment, Rep, long, lat) |>
 st_as_sf(coords = c("long", "lat"), crs = 4326) |>
 st_transform(3395)

ofe_rotate_data(data, -60)
}
```
