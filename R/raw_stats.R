#' Produces a summary table for the data with Mean, Median, SD, CV and Range.
#'
#' This function outputs the summary statistics of the given data
#' @param data, a data set to be summarised
#' @param response, variable(s) to get summary statistics of. If there are multiple responses then put it as example c("Nitrogen","Variety").
#' @param ... variables (without quotes) to group and arrange the data in. For example Nitrogen, Variety
#' @param decimal_places, number of decimal places to show
#' @keywords Raw statistics
#' @import dplyr
#' @export
#' @return a list of the raw stats tables
#' @examples \dontrun{
#' .data <- data.frame(A = as.factor(rep(1:5, 2)), B = rnorm(10), C = as.factor(rep(1:2, 5)))
#' raw_stats(.data, response = "B", C, A)
#' }
raw_stats <- function(data, response, ..., decimal_places = 3) {
    stats_ <- list()

    for (i in 1:length(response)) {
        print(response[i])
        .r_stat <- expr(!!sym(response[i]))
        stats_[[i]] <- data %>%
            group_by(!!!rlang::ensyms(...)) %>%
            mutate(
                Mean = mean(!!.r_stat, na.rm = T),
                Median = median(!!.r_stat, na.rm = T),
                SD = stats::sd(!!.r_stat, na.rm = T),
                CV = 100 * stats::sd(!!.r_stat, na.rm = T) / mean(!!.r_stat, na.rm = T),
                Reps = n(),
                Range = paste0(
                    round(range(!!.r_stat, na.rm = T)[1], decimal_places), " - ",
                    round(range(!!.r_stat, na.rm = T)[2], decimal_places)
                )
            ) %>%
            distinct(!!!rlang::ensyms(...), Mean, Median, SD, CV, Range, Reps) %>%
            arrange(!!!rlang::ensyms(...)) %>%
            mutate(across(c(Mean, Median, SD, CV), ~ round(.x, decimal_places))) %>%
            select(!!!rlang::ensyms(...), Reps, Range, Mean, Median, SD, CV)

        print(xtable::xtable(stats_[[i]]), include.rownames = FALSE)
    }

    return(stats_)
}
