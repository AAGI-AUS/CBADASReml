#' An outlier Function
#'
#' This function produces a table of the outliers as well the other points from that same treatment configuration.
#' @param model an ASReml Model.
#' @param cutoff the point beyond which points are considered as Outliers
#' @param factors the treatment factors used in the model
#' @param data the data used in the Model. Can be found automatically from the model.
#' @keywords Outlier
#' @import tidyr DT dplyr
#' @export
#' @examples
#' \dontrun{
#' outlier_table(model)
#' }
outlier_table <- function(model, cutoff = 3.5, factors = NULL, data = NULL) {
    Response <- toString(model$formulae$fixed[[2]])

    # Add standardised residuals to the model (if not provided)
    if (is.null(model$aom)) {
        model <- update(model, aom = T)
    }

    Std_Residuals <- model$aom$R[, 2]

    # Finding the data if not supplied
    if (is.null(data)) {
        data <- as.data.frame(model$mf)
    }

    # Finding the factors used in the model if not supplied by the user
    if (is.null(factors)) {
        factors <- model$formulae$fixed
        factors <- as.character(attr(factors, "variables"))
        factors <- factors[3:length(factors)]
    }

    factors_sym <- rlang::syms(factors)

    data <- data %>% unite(TMT, factors, remove = F)

    data$Residual <- round(Std_Residuals, 3)
    data$outlier <- ifelse(abs(Std_Residuals) > cutoff, 1, 0)

    outliers <- which(abs(Std_Residuals) > cutoff)

    if (length(outliers) == 0) {
        outliers <- c(order(abs(Std_Residuals), decreasing = T)[1:3])
    }

    top_TMT <- unlist(data %>% filter(units %in% outliers) %>% select(TMT))
    table <- data %>%
        filter(TMT %in% c(top_TMT)) %>%
        group_by(!!!factors_sym) %>%
        arrange(!!!factors_sym)

    datatable(table, options = list(pageLength = 10), caption = htmltools::tags$caption(
        style = "caption-side: top; text-align: left;",
        "Outlier Results: ", htmltools::em(Response)
    )) %>% formatStyle(
        "outlier",
        target = "row",
        backgroundColor = styleEqual(c(0, 1), c("#FFFFFF", "#7BF79A")),
    )
}
