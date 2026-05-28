# An ANOVA table function

This function allows you to observe the ANOVA table for multiple ASReml
or [glmmTMB](https://CRAN.R-project.org/package=glmmTMB) models.

## Usage

``` r
anova_table(..., n_digits = 3)
```

## Arguments

- ...:

  The models to use in the ANOVA table comparison

  Their values may be:

  - `asreml`

  - `glmmTMB`

- n_digits:

  `numeric` The number of digits to round results to.

## Value

`data.frame` A dataframe containing all ANOVA tables. Can be used with
xtable to produce a LaTeX table.

## Examples

``` r
if (FALSE) { # requireNamespace("asreml", quietly = TRUE)
library(CBADASReml)
library(asreml)
test_data <- oats
test_data["yield2"] <- oats["yield"] * runif(nrow(oats["yield"]))
mod1 <- asreml(
    fixed = yield ~ Variety + Nitrogen + Variety:Nitrogen,
    random = ~ idv(Blocks) + idv(Blocks):idv(Wplots),
    residual = ~ idv(units),
    data = test_data
)
mod2 <- asreml(
    fixed = yield2 ~ Variety + Nitrogen + Variety:Nitrogen,
    random = ~ idv(Blocks) + idv(Blocks):idv(Wplots),
    residual = ~ idv(units),
    data = test_data
)
anova_table(mod1, mod2)

## With glmmTMB models
library(glmmTMB)
Salamanders$count2 <- runif(nrow(Salamanders), 0, 10)
mod3 <- glmmTMB( # Zero inflated model
    count ~ spp * mined + (1 | site),
    zi = ~ spp * mined,
    data = Salamanders,
    family = nbinom2
)
mod4 <- glmmTMB( # Hurdle model
    count2 ~ spp * mined + (1 | site),
    zi = ~ spp * mined,
    data = Salamanders,
    family = truncated_nbinom2
)
anova_table(mod3, mod4)
}
```
