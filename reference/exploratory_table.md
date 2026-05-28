# Produces a summary table for the data with Mean, Median, SD, CV and Range

This function outputs the summary statistics of the given data.

## Usage

``` r
exploratory_table(data, response, groupby)
```

## Arguments

- data:

  `data.frame` A data set to be summarised

- response:

  `character` String variable name in the data of the response

- groupby:

  `character` vector Vector of strings of the names of the grouping
  variables in the data

## Value

`data.frame` Summary table

## Examples

``` r
exploratory_table(mtcars, response = "mpg", groupby = c("cyl", "gear"))
#>   cyl gear   mean median                sd               cv reps  min  max
#> 1   4    3   21.5   21.5              <NA>             <NA>    1 21.5 21.5
#> 2   4    4 26.925  25.85  4.80736042810546 17.8546348304752    8 21.4 33.9
#> 3   4    5   28.2   28.2  3.11126983722081 11.0328717631944    2   26 30.4
#> 4   6    3  19.75  19.75   2.3334523779156 11.8149487489398    2 18.1 21.4
#> 5   6    4  19.75   20.1    1.552417469626  7.8603416183595    4 17.8   21
#> 6   6    5   19.7   19.7              <NA>             <NA>    1 19.7 19.7
#> 7   8    3  15.05   15.2  2.77439592114621 18.4345243929981   12 10.4 19.2
#> 8   8    5   15.4   15.4 0.565685424949239 3.67328198018986    2   15 15.8
#>         range
#> 1 21.5 - 21.5
#> 2 21.4 - 33.9
#> 3   26 - 30.4
#> 4 18.1 - 21.4
#> 5   17.8 - 21
#> 6 19.7 - 19.7
#> 7 10.4 - 19.2
#> 8   15 - 15.8
```
