#' An ANOVA table Function
#'
#' This function allows you to observe the ANOVA table for multiple \pkg{ASReml}
#' or \CRANpkg{glmmTMB} models.
#'
#' @param ...
#'   The models to use in the ANOVA table comparison
#'
#'   Their values may be:
#'   * `asreml`
#'   * `glmmTMB`
#' @param n_digits `numeric`
#'   The number of digits to round results to.
#'
#' @returns `data.frame`
#'   A dataframe containing all ANOVA tables. Can be used with xtable to
#'   produce a LaTeX table.
#'
#' @examplesIf requireNamespace("asreml", quietly = TRUE)
#' library(CBADASReml)
#' library(asreml)
#' test_data <- oats
#' test_data["yield2"] <- oats["yield"] * runif(nrow(oats["yield"]))
#' mod1 <- asreml(
#'     fixed = yield ~ Variety + Nitrogen + Variety:Nitrogen,
#'     random = ~ idv(Blocks) + idv(Blocks):idv(Wplots),
#'     residual = ~ idv(units),
#'     data = test_data
#' )
#' mod2 <- asreml(
#'     fixed = yield2 ~ Variety + Nitrogen + Variety:Nitrogen,
#'     random = ~ idv(Blocks) + idv(Blocks):idv(Wplots),
#'     residual = ~ idv(units),
#'     data = test_data
#' )
#' anova_table(mod1, mod2)
#'
#' ## With glmmTMB models
#' library(glmmTMB)
#' Salamanders$count2 <- runif(nrow(Salamanders), 0, 10)
#' mod3 <- glmmTMB( # Zero inflated model
#'     count ~ spp * mined + (1 | site),
#'     zi = ~ spp * mined,
#'     data = Salamanders,
#'     family = nbinom2
#' )
#' mod4 <- glmmTMB( # Hurdle model
#'     count2 ~ spp * mined + (1 | site),
#'     zi = ~ spp * mined,
#'     data = Salamanders,
#'     family = truncated_nbinom2
#' )
#' anova_table(mod3, mod4)
#' @autoglobal
#' @export
anova_table <- function(..., n_digits = 3) {
    if (!is.numeric(n_digits)) {
        stop(paste0("n_digits must be numeric: ", n_digits))
    }
    UseMethod("anova_table")
}

#' @method anova_table asreml
#' @export
anova_table.asreml <- function(..., n_digits = 3) {
    models <- list(...)

    response_names <- sapply(
        models,
        \(x) as.character(x[["call"]][["fixed"]][[2]])
    )

    if (length(unique(response_names)) != length(models)) {
        stop(paste(
            "Models should have different response names:",
            response_names
        ))
    }

    wald_list <- lapply(
        models,
        \(mod) as.data.frame(asreml::wald.asreml(mod))
    )

    sig_list <- lapply(
        wald_list,
        \(x) {
            data.frame(
                Effect = rownames(x),
                pvalue = as.numeric(x[, "Pr(Chisq)"]),
                row.names = NULL
            )
        }
    )

    ## lapply returns list of dfs, which is reduced by full outer joins
    pval_table <- Reduce(
        \(x, y) merge(x, y, by = "Effect", all = TRUE),
        sig_list
    )

    ## Return to original order
    pval_table <- pval_table[
        match(sig_list[[1]]$Effect, pval_table$Effect),
    ]

    names(pval_table) <- c("Effect", response_names)

    ## Filter out intercept and residual pvalues
    pval_table <- pval_table[
        !(pval_table[["Effect"]] %in% c("(Intercept)", "residual (MS)")),
    ]

    ## Round pvals to n_digits
    pval_table[, response_names] <-
        lapply(
            pval_table[, response_names, drop = FALSE],
            \(x) round(x, digits = n_digits)
        )

    rownames(pval_table) <- NULL

    return(pval_table)
}

#' @method anova_table glmmTMB
#' @export
anova_table.glmmTMB <- function(..., n_digits = 3, zi = FALSE) {
    models <- list(...)

    response_names <- sapply(
        models,
        \(x) {
            as.character(attr(
                x$modelInfo$reTrms$cond$terms$fixed,
                "variables"
            )[[2]])
        }
    )

    anova_list <- lapply(
        models,
        \(mod) {
            as.data.frame(
                car::Anova(
                    mod,
                    component = ifelse(zi, "zi", "cond")
                )
            )
        }
    )

    sig_list <- lapply(
        anova_list,
        \(x) {
            data.frame(
                Effect = rownames(x),
                pvalue = as.numeric(x[, "Pr(>Chisq)"]),
                row.names = NULL
            )
        }
    )

    ## lapply returns list of dfs, which is reduced by full outer joins
    pval_table <- Reduce(
        \(x, y) merge(x, y, by = "Effect", all = TRUE),
        sig_list
    )

    ## Return to original order
    pval_table <- pval_table[
        match(sig_list[[1]]$Effect, pval_table$Effect),
    ]

    names(pval_table) <- c("Effect", response_names)

    ## Filter out intercept and residual pvalues
    pval_table <- pval_table[
        !(pval_table[["Effect"]] %in% c("(Intercept)", "residual (MS)")),
    ]

    ## Round pvals to n_digits
    pval_table[, response_names] <-
        lapply(
            pval_table[, response_names, drop = FALSE],
            \(x) round(x, digits = n_digits)
        )

    rownames(pval_table) <- NULL

    return(pval_table)
}
