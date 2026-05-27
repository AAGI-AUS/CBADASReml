#' Create an LSD table from an ASReml model
#'
#' Generates a table of least significant differences (LSDs) for a given model.
#'
#' @param model
#'   The model object to calculate LSDs from.
#'
#'   The value may be:
#'   * `asreml`
#'   * `glmmTMB` (not yet implemented)
#' @param classify `character`
#'   A string specifying which variables to predict and calculate LSDs from.
#' @param alpha xxxxxxx
#' @param ...
#'   Arguments to pass to `predictPlus.asreml`
#'
#' @returns A `data.frame` with the LSD values.
#'
#' @examplesIf requireNamespace("asreml", quietly = TRUE)
#' library(asreml)
#' model <- asreml(
#'     fixed = yield ~ Variety + Nitrogen + Variety:Nitrogen,
#'     random = ~ idv(Blocks) + idv(Blocks):idv(Wplots),
#'     residual = ~ idv(units),
#'     data = oats
#' )
#' lsd_table(model, classify = "Variety")
#' @autoglobal
#' @export

lsd_table <- function(model, classify, alpha = 0.05, ...) {
    is_asreml_installed()

    suppressMessages(
        pred <- asremlPlus::predictPlus.asreml(
            model,
            classify = classify,
            wald.tab = as.data.frame(
                asreml::wald(model, denDF = "algebraic")$Wald
            ),
            ...
        )
    )

    lsd <- pred$LSD$assignedLSD

    prob_matrix <- pred$p.differences
    prob_matrix[is.na(prob_matrix)] <- 1

    treatments <- colnames(prob_matrix)
    means <- pred$predictions$predicted.value

    lsdmeantab <- lsd_group(treatments, means, alpha, prob_matrix)
    lsdmeantab$lsd <- lsd
    lsdmeantab$means <- sort(means, decreasing = TRUE, na.last = TRUE)
    return(lsdmeantab)
}
