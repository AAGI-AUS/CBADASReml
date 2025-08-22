#' Detect outliers for small-plot trial analysis
#'
#' Provides a summary of the outliers present in the \link[asreml]{asreml}
#' model. Gives context to the outliers by showing the responses for the same
#' factor combinations as the outliers
#'
#' @param model
#'   The model object to detect outliers in.
#'
#'   The value may be:
#'   * `asreml`
#'   * `glmmTMB` (not yet implemented)
#' @param cutoff `numeric`
#'   The magnitude of a point's residual to be considered an outlier.
#'
#' @returns `NULL`
#'   Prints:
#'   * The number of outliers.
#'   * A table of the outliers, if any.
#'   * A table of relevant context to some factor combinations, if there are
#'     outliers.
#'
#' @examplesIf requireNamespace("asreml", quietly = TRUE)
#' library(asreml)
#' model <- asreml(
#'     fixed = weight ~ littersize + Dose + Sex + Dose:Sex,
#'     random = ~ idv(Dam),
#'     residual = ~units,
#'     data = rats
#' )
#' outlier_summary(model)
#' @autoglobal
#' @export
outlier_summary <- function(model, cutoff = 3.5) {
    data <- as.data.frame(model$mf)

    # Add standardised residuals to the model (if not already in the model)
    if (is.null(model$aom)) {
        cli::cli_alert_warning("No aom = T, updating model")
        model <- asreml::update.asreml(model, aom = TRUE, trace = FALSE)
    }

    # Finding the factors used in the model if not supplied by the user
    factors <- model$formulae$fixed
    factors <- as.character(attr(factors, "variables"))
    factors <- factors[3:length(factors)]
    ## factors_sym <- rlang::syms(factors)

    data$combined_trt <- do.call(paste, c(data[, factors], sep = "_"))

    # identify which treatment combinations have an outlier
    data$residuals <- model$aom$R[, 2]
    outlier_trt <- data[abs(data$residuals) > 3.5, "combined_trt"]
    outlier_table <- data[abs(data$residuals) > 3.5, ]

    # show table only with those treatment combinations
    unordered_table <- data[data$combined_trt %in% outlier_trt, ]
    ordered_table <- unordered_table[
        order(unordered_table$combined_trt, -abs(unordered_table$residuals)),
    ]

    # Results to help the user identify outliers and their context
    if (sum(abs(data$residuals) > cutoff) > 0) {
        cli::cli_h1("Outliers detected: {sum(abs(data$residuals) > cutoff)}")
        print(outlier_table, 3)
        cli::cli_h2("Data table")
        print(ordered_table, 3)
    } else {
        cli::cli_alert_info("No outliers detected")
    }
    return(invisible(NULL))
}
