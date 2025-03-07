#' Create an LSD table from an ASReml model
#'
#' Generates a table of least significant differences (LSDs) for a given model.
#'
#' @param model An \pkg{ASReml-R} model.
#' @param classify A string specifying which variables to predict and calculate
#'   LSDs from.
#' @returns A `data.frame` with the LSD values.
#' @examplesIf requireNamespace("asreml", quietly = TRUE)
#' library(asreml)
#' model <- asreml(
#'     fixed = yield ~ Variety + Nitrogen + Variety:Nitrogen,
#'     random = ~idv(Blocks) + idv(Blocks):idv(Wplots),
#'     residual = ~idv(units),
#'     data = oats
#' )
#' lsd_table(model, classify = "Variety")
#' @export
lsd_table <- function(model, classify) {
    ## Suppress all prints
    capture.output(
        pred <- asremlPlus::predictPlus.asreml(
                                model,
                                classify = classify,
                                wald.tab = as.data.frame(asreml::wald(model))
                            )
    )

    response <- model[["call"]][["fixed"]][[2]]
    response <- toString(response)

    # LSD Value
    lsd <- pred$LSD$assignedLSD

    prob.matrix <- ifelse(is.na(pred$p.differences), 1, pred$p.differences)

    treatments <- colnames(prob.matrix)
    means <- pred$predictions$predicted.value

    alpha <- 0.05

    lsdmeantab <-
        agricolae::orderPvalue(
                       treatments,
                       means,
                       alpha,
                       prob.matrix,
                       console = TRUE
                   )

    lsdmeantab$Treatment <- rownames(lsdmeantab)

    lsdmeantab$LSD <- lsd

    return(lsdmeantab)
}
