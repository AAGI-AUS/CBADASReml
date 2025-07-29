#' Create an LSD plot from an ASReml model
#'
#' Generates a table of least significant differences (LSDs) for a given model.
#'
#' @param model
#'   The model to generate LSDs from.
#'
#'   The value may be:
#'   * `asreml`
#'   * `glmmTMB` (not yet implemented)
#' @param classify `character` vector
#'   A string specifying which variables to predict and calculate LSDs from.
#'
#'   For `asreml` models, it is passed to `classify` argument of
#'   `predictPlus.asreml`.
#' @param ...
#'   Arguments to pass to `predictPlus.asreml`
#'
#' @returns `ggplot` object
#'   Returns a ggplot2 plot of the LSDs.
#'
#' @examplesIf requireNamespace("asreml", quietly = TRUE)
#' library(asreml)
#' model <- asreml(
#'     fixed = yield ~ Variety + Nitrogen + Variety:Nitrogen,
#'     random = ~idv(Blocks) + idv(Blocks):idv(Wplots),
#'     residual = ~idv(units),
#'     data = oats
#' )
#' lsd_graph(model, classify = "Variety")
#' @autoglobal
#' @export
lsd_graph <- function(model, classify, ...) {
    response <- model[["call"]][["fixed"]][[2]]
    response <- toString(response)

    lsdmeantab <- lsd_table(model, classify)

    # Defining the min and max for the graphs
    y_min <- 0.95 * (min(lsdmeantab$means) - lsdmeantab$lsd[[1]])
    y_max <- 1.05 * (max(lsdmeantab$means) + lsdmeantab$lsd[[1]])

    g <-
        ggplot2::ggplot(
            lsdmeantab,
            ggplot2::aes(
                x = stats::reorder(treatment, means),
                y = means
                # fill = means
            )
        ) +
        ggplot2::geom_bar(
            color = "black",
            position = ggplot2::position_dodge(0.9),
            stat = "identity",
            fill = "lightblue"
        ) +
        ggplot2::geom_text(
            ggplot2::aes(label = group, y = (means + lsd)),
            position = ggplot2::position_dodge(0.5),
            vjust = -0.2,
            ## size = 5
            size = 8
        ) +
        ggplot2::geom_errorbar(
            ggplot2::aes(ymin = means - lsd, ymax = means + lsd),
            width = 0.1
        ) +
        ggplot2::theme_bw() +
        ggplot2::theme(
            axis.line = ggplot2::element_line(linewidth = 3, colour = "grey80")
        ) +
        ggplot2::labs(x = classify, y = response, fill = classify) +
        ggplot2::coord_cartesian(ylim = c(y_min, y_max))

    return(g)
}
