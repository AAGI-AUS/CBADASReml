#' A function that returns tablulated predictions from a model
#'
#' This function returns the predictions and confidence intervals of an
#' `asreml` or `glmmTMB` object.
#'
#' @param mod
#'     The model object to get predictions from.
#'
#'     The value may be:
#'     * `asreml`
#'     * `glmmTBM`
#' @param classify
#'     Variable to make predictions on
#' @param asrLink
#'     Something
#' @param tmb_component
#'     Something
#' @param factor_combine
#'     Something
#' @param trt_col_label
#'     Something
#'
#' @return
#'     Something
#'
#' @examplesIf
#'     Something
#' @export
tab_pred <- function(
    mod,
    classify,
    asrLink = "identity",
    TMBcomponent = "cond",
    TMBtype = "response",
    facCombine = TRUE,
    trtColLbl = "Treatment"
) {
    modtype <- class(mod)

    stopifnot(
        "Model must be of class asreml or glmmTMB" = modtype == "asreml" |
            modtype == "glmmTMB",
        "classify must be type character" = is.character(classify),
        'asrLink must be one of "identity", "log", "inverse", "sqrt",
        "logit", "probit", "cloglog"' = asrLink %in%
            c(
                "identity",
                "log",
                "inverse",
                "sqrt",
                "logit",
                "probit",
                "cloglog"
            ),
        'TMBcomponent must be one of "cond", "cmean", "zi", "response"' = TMBcomponent %in%
            c("cond", "cmean", "zi", "response"),
        'TMBtype must be one of "response"' = TMBtype %in% c("response"),
        "facCombine must be type logical" = is.logical(facCombine),
        "trtColLbl must be type character" = is.character(trtColLbl)
        ## Check/extend conditions
    )

    if (modtype == "asreml") {
        invisible(
            pred <- asremlPlus::predictPlus.asreml(
                mod,
                classify = classify,
                wald.tab = as.data.frame(asreml::wald(mod)),
                pairwise = TRUE,
                transform.function = asrLink
            )
        )
        if ("backtransformed.predictions" %in% names(pred$backtransforms)) {
            tab <- pred$backtransforms
            names(tab)[names(tab) == "backtransformed.predictions"] <-
                "predicted.value"
        } else {
            tab <- pred$predictions
        }
        tab <- tab[, -ncol(tab)]
        names(tab)[
            names(tab) %in%
                c(
                    "predicted.value",
                    "standard.error",
                    "upper.Confidence.limit",
                    "lower.Confidence.limit"
                )
        ] <-
            c("Mean", "Standard Error", "Upper CL", "Lower CL")
        tab <- tab[, c(1:(ncol(tab) - 2), ncol(tab), ncol(tab) - 1)]
    } else if (modtype == "glmmTMB") {
        ## Vectorise components of classify for emmeans
        spec <- unlist(strsplit(classify, "\\*"))
        pred <-
            as.data.frame(
                emmeans::emmeans(
                    mod,
                    specs = spec,
                    type = TMBtype,
                    component = TMBcomponent
                )
            )
        tab <- pred[, names(pred) != "df"]
        ## if ("lower.CL" %in% names(tab)) {
        ##     names(tab)[names(tab) %in% c("emmean", "response", "SE", "lower.CL", "upper.CL")] <-
        ##         c("Mean", "Standard Error", "Lower CL", "Upper CL")
        ## } else if ("asymp.LCL" %in% names(tab)) {
        ##     names(tab)[names(tab) %in% c("emmean", "response", "SE", "asymp.LCL", "asymp.UCL")] <-
        ##         c("Mean", "Standard Error", "Lower CL", "Upper CL")
        ## }
        names(tab)[
            names(tab) %in%
                c(
                    "emmean",
                    "response",
                    "SE",
                    "lower.CL",
                    "asymp.LCL",
                    "upper.CL",
                    "asymp.UCL"
                )
        ] <-
            c("Mean", "Standard Error", "Lower CL", "Upper CL")
    }

    ## Format pred table
    if (facCombine == TRUE) {
        ## Last four columns are always present - combine first ncol - 4 names
        if (ncol(tab) - 4 > 1) {
            tab[[trtColLbl]] <-
                as.factor(
                    apply(
                        tab[, 1:(ncol(tab) - 4)],
                        1,
                        \(x) paste0(x, collapse = ",\n")
                    )
                )
        } else {
            tab[[trtColLbl]] <- as.factor(
                tab[, 1]
            )
        }
        tab <- tab[, (ncol(tab) - 4):ncol(tab)]
        tab <- tab[, c(
            trtColLbl,
            "Mean",
            "Standard Error",
            "Lower CL",
            "Upper CL"
        )]
    }

    ## Add option to sort by means, desc or asc

    return(tab)
}
