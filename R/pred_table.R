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
#' @param link_fun
#'     Something
#' @param tmb_component
#'     Something
#' @param tmb_type
#'     Something
#' @param factor_combine
#'     Something
#' @param trt_col_label
#'     Something
#'
#' @return
#'     Something
#'
#' @export
pred_table <- function(
    mod,
    classify,
    link_fun = "identity",
    tmb_component = "cond",
    tmb_type = "response",
    factor_combine = TRUE,
    trt_col_label = "Treatment"
) {
    modtype <- class(mod)

    stopifnot(
        "Model must be of class asreml or glmmTMB" = modtype == "asreml" |
            modtype == "glmmTMB",
        "classify must be type character" = is.character(classify),
        'link_fun must be one of "identity", "log", "inverse", "sqrt",
        "logit", "probit", "cloglog"' = link_fun %in%
            c(
                "identity",
                "log",
                "inverse",
                "sqrt",
                "logit",
                "probit",
                "cloglog"
            ),
        'tmb_component must be one of "cond", "cmean", "zi", "response"' = tmb_component %in%
            c("cond", "cmean", "zi", "response"),
        'tmb_type must be one of "response"' = tmb_type %in% c("response"),
        "factor_combine must be type logical" = is.logical(factor_combine),
        "trt_col_label must be type character" = is.character(trt_col_label)
        ## Check/extend conditions
    )

    if (modtype == "asreml") {
        invisible(
            pred <- asremlPlus::predictPlus.asreml(
                mod,
                classify = classify,
                wald.tab = as.data.frame(asreml::wald(mod)),
                pairwise = TRUE,
                transform.function = link_fun
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
                    type = tmb_type,
                    component = tmb_component
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
    if (factor_combine == TRUE) {
        ## Last four columns are always present - combine first ncol - 4 names
        if (ncol(tab) - 4 > 1) {
            tab[[trt_col_label]] <-
                as.factor(
                    apply(
                        tab[, 1:(ncol(tab) - 4)],
                        1,
                        \(x) paste0(x, collapse = ",\n")
                    )
                )
        } else {
            tab[[trt_col_label]] <- as.factor(
                tab[, 1]
            )
        }
        tab <- tab[, (ncol(tab) - 4):ncol(tab)]
        tab <- tab[, c(
            trt_col_label,
            "Mean",
            "Standard Error",
            "Lower CL",
            "Upper CL"
        )]
    }

    ## Add option to sort by means, desc or asc

    return(tab)
}
