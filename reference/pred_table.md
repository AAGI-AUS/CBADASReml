# Generate prediction table from model

Generate a table of predicted values and CIs from an asreml or
[glmmTMB](https://rdrr.io/pkg/glmmTMB/man/glmmTMB.html) model, for the
specified variables `asreml` or `glmmTMB` object.

## Usage

``` r
pred_table(
  mod,
  classify,
  link_fun = "identity",
  tmb_component = "cond",
  tmb_type = "response",
  factor_combine = TRUE,
  trt_col_label = "Treatment"
)
```

## Arguments

- mod:

  The model object to get predictions from.

  The value may be:

  - `asreml`

  - `glmmTBM`

- classify:

  `character` vector. Variable for which predictions will be made.

  For `asreml::asreml()` models, it is passed to `classify` argument of
  [`asremlPlus::predictPlus.asreml()`](https://rdrr.io/pkg/asremlPlus/man/predictPlus.asreml.html).
  For [glmmTMB](https://CRAN.R-project.org/package=glmmTMB) models it is
  used in the `specs` argument of
  [`emmeans::emmeans()`](https://rvlenth.github.io/emmeans/reference/emmeans.html)

- link_fun:

  `character` vector. Specifies the transformation function to apply
  over the predictions. Only applies for `asreml::asreml()` objects.

  The value may be:

  - `"identity"` (default)

  - `"log"`

  - `"inverse"`

  - `"sqrt"`

  - `"logit"`

  - `"probit"`

  - `"cloglog"`

- tmb_component:

  `character` vector. Specifies the component of the
  [glmmTMB](https://CRAN.R-project.org/package=glmmTMB) model from which
  to get predictions.

  The value may be:

  - `"cond"` (default)

  - `"zi"`

  - `"cmean"`

  - `"response"`

- tmb_type:

  `character` vector. Specifies the prediction type for
  [glmmTMB](https://CRAN.R-project.org/package=glmmTMB) models.

  - Only `"response"` is supported at the moment.

- factor_combine:

  Logical Whether or not to combine the factors in `classify` into a
  single column in the output table. If `TRUE` (default), the factor
  levels are concatenated and labelled using `trt_col_label`.

- trt_col_label:

  `character` vector. Specifies the label for the combined factor column
  when `factor_combine` is `TRUE`. Defaults to `"Treatment"`

## Value

`data.frame`. Contains the predicted means, standard errors, and
confidence intervals for the specified variables. Includes the following
columns:

- Treatment:

  The combined factor levels, if `factor_combine` is `TRUE`.

- Mean:

  The predicted mean values.

- Standard Error:

  The standard errors of the predicted means.

- Lower CL:

  The lower confidence limits.

- Upper CL:

  The upper confidence limits.

If `factor_combine` is `FALSE`, the factors specified in `classify`
remain as separate columns.

## Author

Matthew Nguyen, <matthew.nguyen@curtin.edu.au>

## Examples

``` r
if (FALSE) { # requireNamespace("asreml", quietly = TRUE)
library(CBADASReml)
library(asreml)
library(glmmTMB)
mod1 <- asreml(
    fixed = yield ~ Variety + Nitrogen + Variety:Nitrogen,
    random = ~ idv(Blocks) + idv(Blocks):idv(Wplots),
    residual = ~ idv(units),
    data = oats
)
# Zero inflated model
mod2 <- glmmTMB(
    count ~ spp * mined + (1|site),
    zi = ~ spp * mined,
    data = Salamanders,
    family = nbinom2
)
pred_table(mod1, classify = "Variety")
}
```
