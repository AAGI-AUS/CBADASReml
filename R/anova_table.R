#' An ANOVA table Function
#'
#' This function allows you to observe the anova table for multiple ASReml
#' Models. It outputs a table as well as a printed xtable to the console.
#' @param ... `asreml`, any number of asreml models.
#' @param ndig `numeric`, number of digits to round results to.
#' @export
#' @return a dataframe containing all anova tables
#' @examples
#' library(asreml)
#' test_data <- oats
#' test_data["yield2"] <- test_data["yield"] * runif(nrow(test_data["yield"]))
#' mod1 <- asreml(
#'     fixed = yield ~ Variety + Nitrogen + Variety:Nitrogen,
#'     random = ~idv(Blocks) + idv(Blocks):idv(Wplots),
#'     residual = ~idv(units),
#'     data = test_data
#' )
#' mod2 <- asreml(
#'     fixed = yield2 ~ Variety + Nitrogen + Variety:Nitrogen,
#'     random = ~idv(Blocks) + idv(Blocks):idv(Wplots),
#'     residual = ~idv(units),
#'     data = test_data
#' )
#' anova_table(mod1, mod2, ndig = 5)
anova_table <- function(..., ndig = 3) {
    models <- list(...)

    response_names <- unlist(
        lapply(models, function(x) toString(x[["call"]][["fixed"]][[2]]))
    )

    if (length(unique(response_names)) != length(models)) {
        stop(paste("Models should have different response names:", response_names))
    }

    wald_list <- lapply(models, function(mod) {
        as.data.frame(asreml::wald.asreml(mod))
    })

    significance_list <- lapply(wald_list, function(x) x["Pr(Chisq)"])

    pval_table <- significance_list %>%
        purrr::map(function(x) tibble::rownames_to_column(x, "rn")) %>%
        purrr::reduce(full_join, by = "rn")

    colnames(pval_table) <- c("Source of Variation", response_names)

    pval_table <- pval_table %>%
        filter(
            !(`Source of Variation` %in% c("(Intercept)", "residual (MS)"))
        ) %>%
        mutate(across(
            all_of(response_names),
            ~ round(.x, digits = ndig)
        ))
    print(xtable::xtable(pval_table), include.rownames = F)
    return(pval_table)
}
