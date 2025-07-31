#' A prediction function for models with more than one variable to predict on
#'
#' This function outputs the predictions for each factor for a single model.
#' Each factor is put on a separate Excel sheet.
#'
#' @param model
#'   The model to generate predictions for.
#'
#'   The value may be:
#'   * `asreml`
#'   * `glmmTBM` (not yet implemented)
#' @param classify `character`
#'   A string specifying the factors in the model to predict on. If multiple
#'   are specified separate with either \code{*} or \code{:}. For example
#'   \code{Nitrogen:Variety} or \code{Nitrogen*Variety}.
#'
#' @returns `list` of `data.frame`
#'   A list of data frames. The first data frame is the ANOVA for the model.
#'   The remaining data frames are the prediction tables from the classify
#'   object.
#'
#' @examplesIf requireNamespace("asreml", quietly = TRUE)
#' library(asreml)
#' model <- asreml(
#'     fixed = yield ~ Variety + Nitrogen + Variety:Nitrogen,
#'     random = ~idv(Blocks) + idv(Blocks):idv(Wplots),
#'     residual = ~idv(units),
#'     data = oats
#' )
#'
#' report_tables(
#'     model,
#'     classify = "Variety:Nitrogen"
#' )
#'
#' \dontrun{
#' # Using it to write with writexl
#' tables <- excel_prediction_file(
#'     model,
#'     classify = "Variety:Nitrogen",
#' )
#'
#' writexl::write_xlsx(x = tables, path = "Prediction_Tables.xlsx")
#' }
#'
#' @export
report_tables <- function(model, classify) {
    if (!inherits(model, "asreml")) {
        stop("Model should be an asreml object")
    }

    if (!is.character(classify)) {
        stop("classify should be a string")
    }

    # Find all classify treatments levels to use
    factors <- strsplit(classify, ":|\\*")[[1]]
    classify_vector <- unlist(
        lapply(seq_along(factors), function(i) {
            utils::combn(factors, i, paste, collapse = ":")
        })
    )

    # Create the tables for each
    pred_list <- lapply(
        classify_vector,
        \(x) {
            lsd_table(
                model,
                classify = x
            )
        }
    )

    names(pred_list) <- classify_vector

    final_list <- append(list(anova_table(model)), pred_list)

    names(final_list)[[1]] <- "Anova"

    return(final_list)
}
