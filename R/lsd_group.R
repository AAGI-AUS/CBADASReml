#' Group treatments using results of paired t-test
#'
#' @param treatments
#' @param means
#' @param alpha
#' @param pvalues
#' @return data.frame of treatments and their associated group
#'
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
            if ((trt_j != n_trt + 1) && (pvals[trt_i, trt_j] >= alpha)) {
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
                break
            }
        }
    }
    return(data.frame(treatment = trts, group = grp))
}
