# A prediction function for models with more than one variable to predict on

This function outputs the predictions for each factor for a single
model. Each factor is put on a separate Excel sheet.

## Usage

``` r
report_tables(model, classify)
```

## Arguments

- model:

  The model to generate predictions for.

  The value may be:

  - `asreml`

  - `glmmTBM` (not yet implemented)

- classify:

  `character` A string specifying the factors in the model to predict
  on. If multiple are specified separate with either `*` or `:`. For
  example `Nitrogen:Variety` or `Nitrogen*Variety`.

## Value

`list` of `data.frame` A list of data frames. The first data frame is
the ANOVA for the model. The remaining data frames are the prediction
tables from the classify object.

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

report_tables(
    model,
    classify = "Variety:Nitrogen"
)

if (FALSE) { # \dontrun{
# Using it to write with writexl
tables <- excel_prediction_file(
    model,
    classify = "Variety:Nitrogen",
)

writexl::write_xlsx(x = tables, path = "Prediction_Tables.xlsx")
} # }
}
```
