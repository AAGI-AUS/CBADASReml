# Group treatments using results of paired t-test

Intended as a replacement of the
[`agricolae::orderPvalue()`](https://rdrr.io/pkg/agricolae/man/orderPvalue.html)
function, but with (maybe) a better algorithm.

## Usage

``` r
lsd_group(treatments, means, alpha, pvalues)
```

## Arguments

- treatments:

  `character` vector Character vector of the treatment names

- means:

  `numeric` Vector of the treatment means/fitted values

- alpha:

  `numeric` Significant difference threshold

- pvalues:

  `matrix` of `numeric` Matrix of pvalues calculated via pairwise
  t-tests

## Value

data.frame Dataframe of each treatment and their associated LSD group

## Examples

``` r
if (FALSE) { # requireNamespace("asreml", quietly = TRUE) & requireNamespace("asremlPlus", quietly = TRUE)
library(asreml)
model <- asreml(
    fixed = yield ~ Variety + Nitrogen + Variety:Nitrogen,
    random = ~idv(Blocks) + idv(Blocks):idv(Wplots),
    residual = ~idv(units),
    data = oats
)

pred <- asremlPlus::predictPlus.asreml(
    model,
    classify = "Variety",
    wald.tab = as.data.frame(asreml::wald(model, denDF = "algebraic")$Wald)
)

prob.matrix <- ifelse(is.na(pred$p.differences), 1, pred$p.differences)
treatments <- colnames(prob.matrix)
means <- pred$predictions$predicted.value
alpha <- 0.05

lsd_group(
    treatments,
    means,
    alpha,
    prob.matrix
)
}
```
