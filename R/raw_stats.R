#' Produces a summary table for the data with Mean, Median, SD, CV and Range.
#'
#' This function outputs the summary statistics of the given data
#' @param data a data set to be summarised
#' @param response string variable name in the data of the response
#' @param groupby vector of strings of the names of the grouping variables in
#'     the data
#'
#' @returns data.frame
#' @export
#'
#' @examples
#' exploratory_table(mtcars, response = "mpg", groupby = c("cyl", "gear"))

exploratory_table <- function(data, response, groupby) {
    if (!response %in% names(data)) {
        stop("response must be a column in the data")
    }
    if (!all(groupby %in% names(data))) {
        stop("grouping variables must be columns in the data")
    }

    compute_stats <- function(x) {
        mn <- mean(x, na.rm = TRUE)
        med <- stats::median(x, na.rm = TRUE)
        sd <- sd(x, na.rm = TRUE)
        cv <- 100 * sd / mn
        reps <- length(x)
        rnge <- range(x, na.rm = TRUE)

        return(c(
            mean = mn,
            median = med,
            sd = sd,
            cv = cv,
            reps = reps,
            min = rnge[1],
            max = rnge[2],
            range = paste(rnge, collapse = " - ")
        ))
    }

    # Use aggregate to compute statistics per group
    aggregated_data <- stats::aggregate(
        x = data[[response]],
        by = data[groupby],
        FUN = \(x) list(compute_stats(x))
    )

    # Extract computed statistics and unnest data frames
    stats_list <- do.call(rbind, aggregated_data$x)
    summary_df <- cbind(aggregated_data[groupby], stats_list)

    # Order by grouping variables
    summary_df <- summary_df[do.call(order, summary_df[groupby]), ]
    rownames(summary_df) <- NULL

    return(summary_df)
}
