#' Produces a summary table for the data with Mean, Median, SD, CV and Range.
#'
#' This function outputs the summary statistics of the given data
#' @param data, a data set to be summarised
#' @param response, variable (without quotes) to get summary statistics of.
#' @param decimal_places, number of decimal places to show
#' @param ... variables (without quotes) to group and arrange the data in.
#'
#' @keywords Raw statistics
#' @import dplyr
#'
#' @return a data.frame
#' @export
#' @examples \dontrun{
#' exploratory_table(mtcars, disp, cyl, gear)
#' }
exploratory_table <- function(data, response, decimal_places = 3, ...) {
    resp <- enquo(resp)

    table <- data %>%
        group_by(!!!rlang::ensyms(...)) %>%
        mutate(
            Mean = mean(!!resp, na.rm = T),
            Median = median(!!resp, na.rm = T),
            SD = stats::sd(!!resp, na.rm = T),
            CV = 100 * stats::sd(!!resp, na.rm = T) / mean(!!resp, na.rm = T),
            Reps = n(),
            Range = paste0(
                round(range(!!resp, na.rm = T)[1], decimal_places),
                " - ",
                round(range(!!resp, na.rm = T)[2], decimal_places)
            )
        ) %>%
        distinct(!!!rlang::ensyms(...), Mean, Median, SD, CV, Range, Reps) %>%
        arrange(!!!rlang::ensyms(...)) %>%
        mutate(across(c(Mean, Median, SD, CV), ~ round(.x, decimal_places))) %>%
        select(!!!rlang::ensyms(...), Reps, Range, Mean, Median, SD, CV) %>%
        ungroup()

    return(table)
}
