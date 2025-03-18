#' A plotting function for Single-Site and MET Analysis
#'
#' This function allows you to observe outliers more easily.
#' @param model, an ASReml model.
#' @param stndResid Use standardised residuals?
#' @keywords Outlier
#' @export
#' @import   stringr ggplot2 ggpubr
#' @examples
#' \dontrun{
#' plot_sagi(model)
#' }
plot_sagi <- function(model, stndResid = TRUE) {
    env_names <- names(model$R.param)
    TEMP <- as.data.frame(model$mf)
    TEMP$ID <- 1:nrow(TEMP)

    if (stndResid) {
        model <- update(model, aom = T)
        TEMP$Residuals <- model$aom$R[, 2]
    } else {
        TEMP$Residuals <- stats::resid(model)
    }

    if (length(env_names) > 1) {
        env <- attr(model$formulae$residual, "term.labels")

        env_name <- str_extract_all(env, "(?<=\\|)[^[[:punct:]]]{1,}")
        env_name <- str_extract_all(env_name, "[^\\s]{1,}", simplify = TRUE)[1]

        residuals <- TEMP
        residuals <- as.data.frame(residuals)

        fit <- model[["linear.predictors"]]

        residuals$fitted <- fit
        residuals$units <- 1:nrow(residuals)

        for (i in env_names) {
            data <- residuals %>% filter(!!rlang::sym(env_name) == i)

            a <-
                ggplot(data, aes(x = Residuals)) +
                geom_histogram(fill = "#0072b2", colour = "white")
            b <-
                ggplot(data, aes(x = fitted, y = Residuals)) +
                geom_point(shape = 1) +
                geom_hline(yintercept = 0) +
                geom_text(
                    data = subset(data, abs(Residuals) > 3),
                    aes(label = ID, x = fitted, y = Residuals),
                    hjust = 1.2,
                    colour = "red"
                )
            c <-
                ggplot(data, aes(sample = Residuals)) +
                stat_qq(shape = 1) +
                stat_qq_line(colour = "#0072b2")
            d <-
                ggplot(data, aes(x = units, y = Residuals)) +
                geom_point(shape = 1) +
                geom_hline(yintercept = 0)

            plot_env <- ggarrange(a, b, c, d, nrow = 2, ncol = 2)

            print(annotate_figure(plot_env, top = paste0(i, " Diagnostics")))
        }
    } else {
        residuals <- TEMP$Residuals
        residuals <- as.data.frame(residuals)

        fit <- model[["linear.predictors"]]

        residuals$fitted <- fit

        residuals$units <- 1:nrow(residuals)

        a <- ggplot(residuals, aes(x = residuals)) +
            geom_histogram(fill = "#0072b2", colour = "white")
        b <- ggplot(residuals, aes(x = fitted, y = residuals)) +
            geom_point(shape = 1) +
            geom_hline(yintercept = 0)
        c <- ggplot(residuals, aes(sample = residuals)) +
            stat_qq(shape = 1) +
            stat_qq_line(colour = "#0072b2")
        d <- ggplot(residuals, aes(x = units, y = residuals)) +
            geom_point(shape = 1) +
            geom_hline(yintercept = 0)

        plot_env <- ggarrange(a, b, c, d, nrow = 2, ncol = 2)

        print(annotate_figure(plot_env, top = paste0(" Diagnostics")))
    }
}
