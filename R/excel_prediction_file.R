#' A prediction function for models with more than one variable to predict on.
#'
#' This function outputs the predictions for each factor for a single model.
#' Each factor is put on a separate excel sheet.
#' @param model an ASReml model. Can be a model object, or a string referencing
#'     a model object.
#' @param classify A string specifying the factors in the model to predict on.
#'     For example "Nitrogen:Variety".
#' @param filename A string for the filename.
#' @param sed logical: should sed be included in the predictions?
#' @keywords wald xtable latex
#' @import dplyr
#' @export
#' @examples
#' \dontrun{
#' excel_prediction_file(
#' model,
#' classify = "N:V",
#' filename = "Yield_Prediction.xlsx"
#' )
#' }
excel_prediction_file <- function(model, classify, filename, sed = F) {
    if (is.character(model)) {
        model <- eval(sym(model))
    }

    # Find all classify treatments levels to use
    factors <- strsplit(classify, ":")[[1]]
    classify_vector <- unlist(
        lapply(1:length(factors), function(i) {
            utils::combn(factors, i, paste, collapse = ":")
        })
    )

    table_list <- list()

    for (i in 1:length(classify_vector)) {
        table_list[[i]] <- pred_se_avsed_lsd(
            model,
            classify = classify_vector[i],
            sed = sed
        )
    }

    names(table_list) <-
        paste0(
            "Pred.",
            stringr::str_replace_all(classify_vector, "[[:punct:]]", " ")
        )

    # Anova table
    anova_table <- as.data.frame(
        cbind(rownames(asreml::wald(model)), asreml::wald(model))
    )
    anova_table <- anova_table %>% mutate(across(2:5, as.numeric))
    colnames(anova_table)[1] <- "Source"
    table_list <- table_list[lapply(table_list, length) > 0]
    ## Add the anova table to the list
    final_list <- append(list(anova_table), table_list)

    ## Add an appropriate name for the sheet
    names(final_list)[1] <- "Wald Table of Significance"

    # Write the final excel file
    writexl::write_xlsx(final_list, filename)
}
