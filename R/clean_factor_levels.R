#' Function to clean factor levels similar to janitor:::old_make_clean_names,
#' but for factors instead of names. Most of the code is taken directly from
#' janitor:::old_make_clean_names.
#'
#' @param x character or factor vector
#' @export
#' @return character or factor vector depending on the input type. Will contain
#'     cleaned versions of the strings in x.

clean_factor_levels <- function(x) {
    ## Ensure x is factor or character vector
    if (!(is.character(x) | is.factor(x))) stop("x must be factor or character")

    old_names <- as.character(x)

    new_names <-
        old_names |>
        gsub(pattern = "'", replacement = "") |>
        gsub(pattern = "\"", replacement = "") |>
        gsub(pattern = "%", replacement = "percent") |>
        gsub(pattern = "^[ ]+", replacement = "") |>
        make.names() |>
        gsub(pattern = "[.]+", replacement = "_") |>
        gsub(pattern = "[_]+", replacement = "_") |>
        tolower() |>
        gsub(pattern = "_$", replacement = "")

    if (is.factor(x)) return(factor(new_names))
    return(new_names)
}
