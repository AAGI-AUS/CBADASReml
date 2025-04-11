#' Generate Prediction Table from Model
#'
#' Generater a table of predicted values and CIs from an
#' \code{\link[asreml]{asreml}} or \code{\link[glmmTMB]{glmmTMB}} model, for
#' the specified variables `asreml` or `glmmTMB` object. Created by Matthew
#' Nguyen.
#'
#' @param mod
#'     The model object to get predictions from.
#'
#'     The value may be:
#'     * \code{asreml}
#'     * \code{glmmTBM}
#' @param classify \code{character} vector.
#'     Variable for which predictions will be made.
#'
#'     For \code{asreml} models, it is passed to \code{classify} argument of
#'     \code{predictPlus.asreml}.
#'     For \code{glmmTMB} models it is used in the \code{specs} argument of
#'     \code{\link[emmeans]{emmeans}}
#' @param link_fun \code{character} vector.
#'     Specifies the transformation function to apply over the predictions.
#'     Only applies for \code{asreml} objects.
#'
#'     The value may be:
#'     * \code{"identity"} (default)
#'     * \code{"log"}
#'     * \code{"inverse"}
#'     * \code{"sqrt"}
#'     * \code{"logit"}
#'     * \code{"probit"}
#'     * \code{"cloglog"}
#' @param tmb_component \code{character} vector.
#'     Specifies the component of the \code{glmmTMB} model from which to get
#'     predictions.
#'
#'     The value may be:
#'     * \code{"cond"} (default)
#'     * \code{"zi"}
#'     * \code{"cmean"}
#'     * \code{"response"}
#' @param tmb_type `character` vector.
#'     Specifies  the prediction type for \code{glmmTMB} models.
#'
#'     Only \code{"response"} is supported at the moment.
#' @param factor_combine Logical
#'     Whether or not to combine the factors in \code{classify} into a single
#'     column in the output table. If \code{TRUE} (default), the factor levels
#'     are concatenated and labelled using \code{trt_col_label}.
#'
#' @param trt_col_label \code{character} vector.
#'     Specifies the label for the combined factor column when
#'     \code{factor_combine} is \code{TRUE}. Defaults to \code{"Treatment"}
#'
#' @returns \code{data.frame}.
#'     Contains the predicted means, standard errors, and confidence intervals
#'     for the specified variables. Includes the following columns:
#' \describe{
#'   \item{Treatment}{The combined factor levels, if \code{factor_combine} is \code{TRUE}.}
#'   \item{Mean}{The predicted mean values.}
#'   \item{Standard Error}{The standard errors of the predicted means.}
#'   \item{Lower CL}{The lower confidence limits.}
#'   \item{Upper CL}{The upper confidence limits.}
#' }
#'     If \code{factor_combine} is \code{FALSE}, the factors specified in
#'     \code{classify} remain as separate columns.
#'
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
