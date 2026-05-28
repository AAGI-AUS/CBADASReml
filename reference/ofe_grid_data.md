# Grid data ready for ASReml analysis

Optimally rotate and grid a georeferenced dataframe

## Usage

``` r
ofe_grid_data(data_in, rotation_angle, nrow, npe, ncol, trim_ends = TRUE)
```

## Arguments

- data_in:

  `data.frame`. The dataframe to be rotated. This should be
  georeferenced per the [sf](https://CRAN.R-project.org/package=sf)
  package.

  We assume there are only three column of actual interest in the
  dataframe, which are:

  Yield

  :   The observed yield values.

  Treatment

  :   The treatment factor to assess.

  Rep

  :   The blocking structure to be preserved

- rotation_angle:

  `numeric`. Clockwise angle in degrees to rotate the dataframe.

- nrow:

  `numeric`. The number of rows in the final grid. This should be as
  large as possible without introducing spurious NA values.

- npe:

  `numeric`. Number of pseudo-environments to be generated along the
  strips. PEs will be evenly spaced, so may not be appropriate in all
  cases. Always assess PEs visually before use.

- ncol:

  `numeric`. The actual number of strips in the data. This should be
  counted/confirmed before hand. If there is a gap between strips, count
  the number of strips in this gap when determining this number.

- trim_ends:

  `bool`. Boolean determining whether the gridded data should be trimmed
  to remove NAs occurring at the ends of columns. Defaults to FALSE.

## Value

`gridded.ofe`. `list` containing the following items:

- gridded_data `data.frame`:

  The georeferenced gridded data.

- data_original `data.frame`:

  The input data with additional columns for visualising the rotation
  process.

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

ofe_grid_data(data, -60, 80, 5, 18, trim_ends=FALSE)
}
```
