#' Generate Prediction Table from Model
#'
#' Generate a table of predicted values and CIs from an
#' \link[asreml]{asreml} or \link[glmmTMB]{glmmTMB} model, for
#' the specified variables `asreml` or `glmmTMB` object.
#'
#' @param mod
#'   The model object to get predictions from.
#'
#'   The value may be:
#'   * `asreml`
#'   * `glmmTBM`
#' @param classify `character` vector.
#'   Variable for which predictions will be made.
#'
#'   For [asreml::asreml()] models, it is passed to `classify` argument of
#'   [asremlPlus::predictPlus.asreml()].
#'   For \CRANpkg{glmmTMB} models it is used in the `specs` argument of
#'   [emmeans::emmeans()]
#' @param link_fun `character` vector.
#'   Specifies the transformation function to apply over the predictions.
#'   Only applies for [asreml::asreml()] objects.
#'
#'   The value may be:
#'
#'   * `"identity"` (default)
#'   * `"log"`
#'   * `"inverse"`
#'   * `"sqrt"`
#'   * `"logit"`
#'   * `"probit"`
#'   * `"cloglog"`
#' @param tmb_component `character` vector.
#'   Specifies the component of the \CRANpkg{glmmTMB} model from which to get
#'   predictions.
#'
#'   The value may be:
#'
#'   * `"cond"` (default)
#'   * `"zi"`
#'   * `"cmean"`
#'   * `"response"`
#' @param tmb_type `character` vector.
#'   Specifies  the prediction type for \CRANpkg{glmmTMB} models.
#'
#'   * Only `"response"` is supported at the moment.
#' @param factor_combine Logical
#'     Whether or not to combine the factors in `classify` into a single
#'     column in the output table. If `TRUE` (default), the factor levels
#'     are concatenated and labelled using `trt_col_label`.
#' @param trt_col_label `character` vector.
#'     Specifies the label for the combined factor column when
#'     `factor_combine` is `TRUE`. Defaults to `"Treatment"`
#'
#' @returns `data.frame`.
#'     Contains the predicted means, standard errors, and confidence intervals
#'     for the specified variables. Includes the following columns:
#' \describe{
#'   \item{Treatment}{The combined factor levels, if `factor_combine` is
#'                    `TRUE`.}
#'   \item{Mean}{The predicted mean values.}
#'   \item{Standard Error}{The standard errors of the predicted means.}
#'   \item{Lower CL}{The lower confidence limits.}
#'   \item{Upper CL}{The upper confidence limits.}
#' }
#'     If `factor_combine` is `FALSE`, the factors specified in
#'     `classify` remain as separate columns.
#'
#' @examplesIf requireNamespace("asreml", quietly = TRUE)
#' library(CBADASReml)
#' library(asreml)
#' mod1 <- asreml(
#'     fixed = yield ~ Variety + Nitrogen + Variety:Nitrogen,
#'     random = ~ idv(Blocks) + idv(Blocks):idv(Wplots),
#'     residual = ~ idv(units),
#'     data = oats
#' )
#' pred_table(mod1, classify = "Variety")
#' @autoglobal
#' @author Matthew Nguyen, \email{matthew.nguyen@@curtin.edu.au}
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
        utils::capture.output(
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
            tab <- as.data.frame(pred$predictions)
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
