#' Group treatments using results of paired t-test
#'
#' Intended as a replacement of the [agricolae::orderPvalue()] function, but
#' with (maybe) a better algorithm.
#'
#' @param treatments `character` vector
#'   Character vector of the treatment names
#' @param means `numeric`
#'   Vector of the treatment means/fitted values
#' @param alpha `numeric`
#'   Significant difference threshold
#' @param pvalues `matrix` of `numeric`
#'   Matrix of pvalues calculated via pairwise t-tests
#'
#' @returns data.frame
#'   Dataframe of each treatment and their associated LSD group
#'
#' @examplesIf "&"(requireNamespace("asreml", quietly = TRUE), requireNamespace("asremlPlus", quietly = TRUE))
#' library(asreml)
#' model <- asreml(
#'     fixed = yield ~ Variety + Nitrogen + Variety:Nitrogen,
#'     random = ~idv(Blocks) + idv(Blocks):idv(Wplots),
#'     residual = ~idv(units),
#'     data = oats
#' )
#'
#' pred <- asremlPlus::predictPlus.asreml(
#'     model,
#'     classify = "Variety",
#'     wald.tab = as.data.frame(asreml::wald(model, denDF = "algebraic")$Wald)
#' )
#'
#' prob.matrix <- ifelse(is.na(pred$p.differences), 1, pred$p.differences)
#' treatments <- colnames(prob.matrix)
#' means <- pred$predictions$predicted.value
#' alpha <- 0.05
#'
#' lsd_group(
#'     treatments,
#'     means,
#'     alpha,
#'     prob.matrix
#' )
#' @autoglobal
#' @export
lsd_group <- function(treatments, means, alpha, pvalues) {
    ## Order everything by descending mean
    ord <- order(means, decreasing = TRUE)
    mns <- means[ord]
    trts <- treatments[ord]
    pvals <- pvalues[ord, ord]

    n_trt <- length(trts)
    grp <- rep("", n_trt)
    ## Index of letter to use for current group
    ltr <- 0

    ## Iterate over all treatments
    for (trt_i in 1:n_trt) {
        ## Check trt i with all other treatments
        for (trt_j in (trt_i:n_trt + 1)) {
            ## Is their pval significant?
            ## Is trt_j not at the end?
            if ((trt_j <= n_trt) && (pvals[trt_i, trt_j] >= alpha)) {
                ## Continue checking
                next
            } else {
                ## Stop checking and assign labels
                ## If trt_j has no label, we need to iterate the label
                if (grp[trt_j - 1] == "") {
                    ltr <- ltr + 1
                }
                ## All trts from trt_i to trt_j-1 are in one group
                grp_idx <- trt_i:(trt_j - 1)
                letter <- letters[ltr]
                ## If already in group don't add, else add
                grp[grp_idx] <- ifelse(
                    grepl(paste0(letter, "$"), grp[grp_idx]),
                    grp[grp_idx],
                    paste0(grp[grp_idx], letter)
                )
                ## If trt_j > n_trt, we have nothing left to check
                ## so we can exit
                if (trt_j > n_trt) {
                    return(data.frame(treatment = trts, group = grp))
                }
                break
            }
        }
    }
    stop("Something bad happened...")
}
