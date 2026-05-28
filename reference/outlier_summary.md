# Detect outliers for small-plot trial analysis

Provides a summary of the outliers present in the asreml model. Gives
context to the outliers by showing the responses for the same factor
combinations as the outliers

## Usage

``` r
outlier_summary(model, cutoff = 3.5)
```

## Arguments

- model:

  The model object to detect outliers in.

  The value may be:

  - `asreml`

  - `glmmTMB` (not yet implemented)

- cutoff:

  `numeric` The magnitude of a point's residual to be considered an
  outlier.

## Value

`NULL` Prints:

- The number of outliers.

- A table of the outliers, if any.

- A table of relevant context to some factor combinations, if there are
  outliers.

## Examples

``` r
if (FALSE) { # requireNamespace("asreml", quietly = TRUE)
library(asreml)
model <- asreml(
    fixed = weight ~ littersize + Dose + Sex + Dose:Sex,
    random = ~ idv(Dam),
    residual = ~units,
    data = rats
)
outlier_summary(model)
}
```
