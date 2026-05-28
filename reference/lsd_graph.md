# Create an LSD graph from an ASReml model

Generates a table of least significant differences (LSDs) for a given
model.

## Usage

``` r
lsd_graph(model, classify, ...)
```

## Arguments

- model:

  The model to generate LSDs from.

  The value may be:

  - `asreml`

  - `glmmTMB` (not yet implemented)

- classify:

  `character` vector A string specifying which variables to predict and
  calculate LSDs from.

  For `asreml` models, it is passed to `classify` argument of
  `predictPlus.asreml`.

- ...:

  Arguments to pass to `predictPlus.asreml`

## Value

`ggplot` object Returns a ggplot2 plot of the LSDs.

## Examples

``` r
if (FALSE) { # requireNamespace("asreml", quietly = TRUE)
library(asreml)
model <- asreml(
    fixed = yield ~ Variety + Nitrogen + Variety:Nitrogen,
    random = ~idv(Blocks) + idv(Blocks):idv(Wplots),
    residual = ~idv(units),
    data = oats
)
lsd_graph(model, classify = "Variety")
}
```
