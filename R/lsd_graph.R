#' A LSD graph making function
#'
#' This function needs the library agricolae, asremlPlus and ggplot2
#' @param model, an ASReml model.
#' @param classify, A string specifying which variables to predict and calculate LSDs from.
#' @param ... Arguments to pass to `predictPlus.asreml`
#' @keywords LSD
#' @importFrom ggplot2 ggplot aes geom_bar geom_text geom_errorbar labs coord_cartesian theme_bw theme position_dodge
#' @export
#' @returns return a ggplot2 object.
#' @examples
#' \dontrun{
#' LSD_wrapper(model, classify = "Nitrogen*Variety", sep_line = T)
#' }
lsd_graph <- function(model, classify, ...) {
  capture.output(
    pred <- asremlPlus::predictPlus.asreml(
      model,
      classify = classify,
      wald.tab = as.data.frame(asreml::wald(model)),
      ...
    )
  )

  response <- model[["call"]][["fixed"]][[2]]
  response <- toString(response)

  # LSD Value
  lsd <- pred$LSD$assignedLSD

  # Standard Error
  std.error <- pred$predictions$standard.error

  prob.matrix <- ifelse(is.na(pred$p.differences), 1, pred$p.differences)

  treatments <- colnames(prob.matrix)
  means <- pred$predictions$predicted.value

  alpha <- 0.05

  lsdmeantab <- agricolae::orderPvalue(
    treatments,
    means,
    alpha,
    prob.matrix,
    console = TRUE
  )

  lsdmeantab$Treatment <- rownames(lsdmeantab)

  # Defining the min and max for the graphs
  y_min <- 0.95 * (min(lsdmeantab$means) - lsd)
  y_max <- 1.05 * (max(lsdmeantab$means) + lsd)

  g <-
    ggplot(
      lsdmeantab,
      aes(
        x = stats::reorder(Treatment, means),
        y = means
        # fill = means
      )
    ) +
    geom_bar(
      color = "black",
      position = position_dodge(.9),
      stat = "identity",
      fill = "lightblue"
    ) +
    geom_text(
      aes(label = groups, y = (means + lsd)),
      position = position_dodge(0.5),
      vjust = -0.2,
      ## size = 5
      size = 8
    ) +
    geom_errorbar(aes(ymin = means - lsd, ymax = means + lsd), width = .1) +
    theme_bw() +
    theme(axis.line = element_line(linewidth = 3, colour = "grey80")) +
    labs(x = classify, y = response, fill = classify) +
    coord_cartesian(ylim = c(y_min, y_max))
  ## }

  return(g)
}
