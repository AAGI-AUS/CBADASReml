# Create an LSD table from an ASReml model

Generates a table of least significant differences (LSDs) for a given
model.

## Usage

``` r
lsd_table(model, classify, alpha = 0.05, ...)
```

## Arguments

- model:

  The model object to calculate LSDs from.

  The value may be:

  - `asreml`

  - `glmmTMB` (not yet implemented)

- classify:

  `character` A string specifying which variables to predict and
  calculate LSDs from.

- alpha:

  significance level `numeric`.

- ...:

  Arguments to pass to `predictPlus.asreml`

## Value

A `data.frame` with the LSD values.

## Examples

``` r
if (FALSE) { # requireNamespace("asreml", quietly = TRUE)
library(asreml)
model <- asreml(
    fixed = yield ~ Variety + Nitrogen + Variety:Nitrogen,
    random = ~ idv(Blocks) + idv(Blocks):idv(Wplots),
    residual = ~ idv(units),
    data = oats
)
lsd_table(model, classify = "Variety")
}
```
