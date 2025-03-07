#' Group treatments using results of paired t-test
#'
#' @param treatments character vector of the treatment names
#' @param means numeric vector of the treatment means/fitted values
#' @param alpha numeric significant difference threshold
#' @param pvalues symmetric numeric matrix of pvalues calculated via pairwise
#'     t-tests
#' @return data.frame of treatments and their associated group
#' @export
#' @examplesIf requireNamespace("asreml", quietly = TRUE)
#' library(asreml)
#' model <- asreml(
#'     fixed = yield ~ Variety + Nitrogen + Variety:Nitrogen,
#'     random = ~idv(Blocks) + idv(Blocks):idv(Wplots),
#'     residual = ~idv(units),
#'     data = oats
#' )
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
    for (trt_i in 1:(n_trt - 1)) {
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
                if (grp[trt_j - 1] == "") ltr <- ltr + 1
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
