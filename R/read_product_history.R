#' Read and prepare product history file.
#'
#' @param filepath A character vector representing the file path to the
#'     <farm_name>_product_history.csv
#' @export
#' @return A long dataframe with columns: Farm, Field, year, product

read_product_history <- function(filepath) {
    if (!file.exists(filepath)) {
        stop("
             File does not exists.
             Is the path correct? Have you combined the metadata files?
             ")
    }

    key_raw <- utils::read.csv(filepath)
    key_raw <- key_raw[, !(names(key_raw) %in% c("Area", "TotalYears"))]
    key_long <- tidyr::pivot_longer(
                           key_raw,
                           cols = 3:ncol(key_raw),
                           names_to = "year",
                           values_to = "product"
                       )
    key_long$year <- as.numeric(substr(key_long$year, 6, 10))

    return(key_long)
}
