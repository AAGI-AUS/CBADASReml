#' Provides a summary of the outliers present in the asreml model.
#' Gives context to the outliers by showing the responses for the same factor
#' combinations as the outliers
#'
#' @param model an ASReml Model.
#' @param cutoff the point beyond which points are considered as Outliers
#' @keywords Outlier
#' @returns prints to the console the number of outliers and two tables (one with just the outliers, one giving the context relevant to same factor combinations).
#' @export
#' @examples
#'
#' library(asreml)
#' model <- asreml(
#'     fixed = weight ~ littersize + Dose + Sex + Dose:Sex,
#'     random = ~idv(Dam),
#'     residual = ~units,
#'     data = rats
#' )
#'
#' outlier_summary(model)
#'
#' @autoglobal
outlier_summary <- function(model, cutoff = 3.5) {
    data <- as.data.frame(model$mf)

    # Add standardised residuals to the model (if not already in the model)
    if (is.null(model$aom)) {
        print("No aom = T, Updating Model")
        model <- update.asreml(model, aom = TRUE)
    }

    # Finding the factors used in the model if not supplied by the user
    factors <- model$formulae$fixed
    factors <- as.character(attr(factors, "variables"))
    factors <- factors[3:length(factors)]
    factors_sym <- rlang::syms(factors)

    data$combined_trt = do.call(paste, c(data[, factors], sep = "_"))

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
        print(paste("Outliers detected:", sum(abs(data$residuals) > cutoff)))
        print(outlier_table, 3)
        print(ordered_table, 3)
    } else {
        print("No outliers detected")
    }
}
