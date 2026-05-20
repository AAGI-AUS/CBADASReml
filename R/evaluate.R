#' Calculate the statistical power for an experimental design.
#'
#' Calculate pairwise treatment-comparison power from the Fisher information of
#' the provided experimental design using the treatment information matrix
#' induced by an AR1 x AR1 spatial dependence structure. This function
#' evaluates how much "power" the design has to detect a specified treatment
#' difference after adjusting for nuisance effects (e.g. blocks, etc.)
#'
#' The calculation is based on the treatment information matrix
#' \deqn{
#'   I_T = X_1^\intercal L_R X_1,
#' }
#' where \eqn{X_1} is the treatment indicator matrix and \eqn{L_R} is the
#' covariance-adjusted projection matrix that removes nuisance effects:
#' \deqn{
#'   L
#'   =
#'   \Sigma^{-1}
#'   -
#'   \Sigma^{-1} X_2
#'   \left( X_2^\intercal \Sigma^{-1} X_2 \right)^{-1}
#'   \Sigma^{-1}
#' }
#' For a treatment contrast \eqn{c^\intercal \tau}, the contrast standard error
#' is computed from
#' \deqn{
#'   \operatorname{SE}(c^\intercal \hat{\tau})
#'   =
#'   \sqrt{\sigma^2 c^\intercal I_T^+ c},
#' }
#' where \eqn{I_T^+} is the Moore-Penrose pseudoinverse of the treatment
#' information matrix. The effect size \code{delta} is then converted into a
#' noncentrality parameter,
#' \deqn{
#'   \lambda
#'   =
#'   \frac{\delta}{\operatorname{SE}(c^\intercal \hat{\tau})},
#' }
#' and power is calculated using a two-sided known-covariance Z-test
#' approximation.
#'
#' @param design \code{data.frame}.
#'        The design data frame, one row per experimental unit.
#' @param treatment_cols \code{character} vector.
#'        Column name of the treatment in \code{design}. If multiple columns
#'        are provided, each unique combination is treated as a distinct
#'        treatment combination.
#' @param row_col \code{character} vector.
#'        Column name of the experiment's rows in \code{design}.
#' @param column_col \code{character} vector.
#'        Column name of the experiment's columns in \code{design}.
#' @param block_col \code{character} vector or \code{NULL}.
#'        Column name of the experiment's blocking factor in \code{design}.
#'        Use \code{NULL} for unblocked designs.
#' @param rho_row \code{numeric}.
#'        AR1 correlation parameter in the row direction.
#' @param rho_col \code{numeric}.
#'        AR1 correlation parameter in the column direction.
#' @param sigma2 \code{numeric}.
#'        Assumed residual variance.
#' @param delta \code{numeric}.
#'        Treatment difference that is "worth detecting". The minimum
#'        difference between treatments that we care about.
#' @param alpha \code{numeric}.
#'        Significance/p-value threshold for the two-sided z-tests.
#' @param tolerance \code{numeric}.
#'        Tolerance for numerical stability.
#'
#' @return A list containing:
#' \describe{
#'   \item{contrast_power}{Data frame of pairwise treatment-comparison standard
#'     errors, noncentrality parameters, power values, and effective
#'     replicates.}
#'   \item{design_power}{The minimum pairwise power across all estimable
#'     treatment comparisons.}
#'   \item{average_power}{The average pairwise power across all estimable
#'     treatment comparisons.}
#'   \item{worst_comparison}{The treatment comparison with the lowest power.}
#'   \item{fisher_info}{The treatment information matrix.}
#'   \item{treatment_cov}{The model-based covariance matrix of treatment
#'     estimates, equal to \eqn{\sigma^2 I_T^+}.}
#'   \item{eigenvalues}{Positive eigenvalues of the treatment information
#'     matrix.}
#'   \item{rank}{Numerical rank of the treatment information matrix.}
#'   \item{assumptions}{List of covariance, effect-size, and testing
#'     assumptions used.}
#' }
#'
#' @details
#' The spatial covariance parameters \code{rho_row}, \code{rho_col}, and
#' \code{sigma2} are treated as assumed planning values, not estimated from the
#' data. The returned power is therefore conditional on the supplied covariance
#' structure, the chosen effect size \code{delta}, and the significance level
#' \code{alpha}.
#'
#' Thus, you as the statistician must select four parameters based on previous
#' trials, domain knowledge, or \emph{conservative} assumptions:
#' \describe{
#'   \item{\eqn{\rho_{\text{row}}}}{
#'     AR1 row dependence parameter.
#'     How correlated are observations that are one row apart? This is hard to
#'     select, I would recommend testing multiple different values such as
#'     \eqn{0.0} for no dependence, \eqn{0.3} for moderate, and \eqn{0.5} for
#'     strong dependence, and reporting each.
#'   }
#'   \item{\eqn{\rho_{\text{col}}}}{
#'     AR1 column dependence parameter.
#'     How correlated are observations that are one column apart? This is hard
#'     to select, I would recommend testing multiple different values such as
#'     \eqn{0.0} for no dependence, \eqn{0.3} for moderate, and \eqn{0.5} for
#'     strong dependence, and reporting each.
#'     }
#'   \item{\eqn{\sigma^2}}{
#'     The residual variance.
#'     This should be based on previous similar experiments, or a conservative
#'     estimate. Underestimating \eqn{\sigma^2} will result in \emph{overstated
#'     power}.
#'     }
#'   \item{\eqn{\delta}}{
#'     Detectable treatment difference.
#'     Choose \eqn{\delta} as the \emph{smallest} treatment
#'     difference that would matter scientifically, agronomically,
#'     commercially, etc. So if yield differences of less than \eqn{0.1} t/ha
#'     are not important to the farmer, then use \eqn{\delta = 0.1}. Otherwise,
#'     test multiple deltas and report the power of each of them.
#'     }
#' }
#'
#' @examples
#' \dontrun{
#' # RCBD
#' df <- data.frame(
#'     row = rep(1:6, each = 4),
#'     col = rep(1:4, times = 6),
#'     treatment = rep(LETTERS[1:8], 3),
#'     block = rep(1:3, each = 8)
#' )
#'
#' # Optimise while respecting blocks
#' result <- speed::speed(df,
#'     "treatment",
#'     swap_within = "block",
#'     iterations = 5000,
#'     seed = 42
#' )
#'
#' ## test on latin square
#' latinsquare <- data.frame(
#'     row = rep(1:4, each = 4),
#'     col = rep(1:4, times = 4),
#'     treatment = c(
#'         "A", "B", "C", "D",
#'         "B", "C", "D", "A",
#'         "C", "D", "A", "B",
#'         "D", "A", "B", "C"
#'     )
#' )
#'
#' res_power <- design_power(
#'     design = latinsquare,
#'     treatment_cols = "treatment",
#'     row_col = "row",
#'     column_col = "col",
#'     block_col = NULL,
#'     rho_row = 0.3,
#'     rho_col = 0.3,
#'     sigma2 = 1,
#'     delta = 1,
#'     alpha = 0.05
#' )
#' }
#'
#' @export
design_power <- function(
  design,
  treatment_cols = "treatment",
  row_col = "row",
  column_col = "col",
  block_col = NULL,
  rho_row = 0.1,
  rho_col = 0.1,
  sigma2 = 1,
  delta = 1,
  alpha = 0.05,
  tolerance = 1e-10
) {
    # arg checks
    if (sigma2 <= 0) stop("sigma2 must be positive.")
    if (delta <= 0) stop("delta must be positive.")

    ti <- compute_treatment_info(
        design = design,
        treatment_cols = treatment_cols,
        row_col = row_col,
        column_col = column_col,
        block_col = block_col,
        rho_row = rho_row,
        rho_col = rho_col,
        alpha = alpha,
        tolerance = tolerance
    )

    trt_levels <- ti$trt_levels
    v <- length(trt_levels)

    # Pairwise treatment stat power
    pairs <- combn(trt_levels, 2, simplify = FALSE)

    out <- lapply(pairs, \(pair) {
        contrast <- numeric(v)
        names(contrast) <- trt_levels
        contrast[pair[1]] <- 1
        contrast[pair[2]] <- -1

        # Estimable?
        # contrast must lie in the row space of the information
        # use a property of the pseudoinverse:
        # A A+ maps all column vectors of A to themselves
        projected <- as.numeric(contrast %*% ti$info_inv %*% ti$info)
        estimable <- sqrt(sum((contrast - projected)^2)) <=
            tolerance * (1 + sqrt(sum(contrast^2)))

        if (!estimable) {
            return(data.frame(
                comparison = paste(pair, collapse = " vs "),
                estimable = FALSE,
                se = NA,
                lambda = NA,
                power = NA,
                effective_replicates = NA
            ))
        }

        contrast_var <-
            sigma2 * as.numeric(t(contrast) %*% ti$info_inv %*% contrast)
        se <- sqrt(contrast_var)
        ## effect size that we want to have the power to detect in units of SE
        lambda <- abs(delta / se)

        ## Fisher-information z power
        ### this is called the known-covariance z-test approximation
        crit <- stats::qnorm(1 - alpha / 2)
        power <- stats::pnorm(-crit, mean = lambda, sd = 1) +
            (1 - stats::pnorm(crit, mean = lambda, sd = 1))

        ## iid-equivalent replication for this contrast
        ## converts contrast variance into number of balanced iid replicates
        ## that would give the same pairwise standard error
        effective_replicates <- 2 * sigma2 / se^2

        data.frame(
            comparison = paste(pair, collapse = " vs "),
            estimable = TRUE,
            se = se,
            lambda = lambda,
            power = power,
            effective_replicates = effective_replicates
        )
    })

    # return power for all comparisons
    contrast_power <- do.call(rbind, out)
    return(list(
        contrast_power = contrast_power,
        design_power = min(contrast_power$power, na.rm = TRUE),
        average_power = mean(contrast_power$power, na.rm = TRUE),
        worst_comparison = contrast_power$comparison[which.min(contrast_power$power)],
        fisher_info = ti$info,
        treatment_cov = sigma2 * ti$info_inv,
        eigenvalues = ti$eigenvalues,
        rank = ti$rank,
        assumptions = list(
            sigma2 = sigma2,
            delta = delta,
            alpha = alpha,
            rho_row = rho_row,
            rho_col = rho_col,
            block_col = block_col,
            treatment_cols = treatment_cols,
            power_method = "known-covariance Fisher-information z power"
        )
    ))
}

#' Calculate the design efficiency/optimality of a small-plot trial design.
#'
#' Calculates model-based efficiency for a design using the treatment
#' information matrix induced by an AR1 x AR1 spatial dependence structure.
#' All this function does is evaluate how much information the design contains
#' about treatment contrasts, after adjusting for nuisance effects (e.g. blocks
#' , etc.)
#'
#' The function computes the treatment information matrix
#' \deqn{
#'   I_T = X_1^\top L_R X_1,
#' }
#' where \eqn{X_1} is the treatment indicator matrix and \eqn{L_R} is the
#' covariance-adjusted projection matrix that removes nuisance effects:
#' \deqn{
#'   L
#'   =
#'   \Sigma^{-1}
#'   -
#'   \Sigma^{-1} X_2
#'   \left( X_2^\intercal \Sigma^{-1} X_2 \right)^{-1}
#'   \Sigma^{-1}
#' }
#' Pairwise treatment contrast variances are then calculated from \eqn{I_T^+},
#' the Moore-Penrose pseudoinverse of the treatment information matrix.
#'
#' @param design \code{data.frame}.
#'        The design data frame, one row per experimental unit.
#' @param treatment_cols \code{character} vector.
#'        Column name of the treatment in \code{design}. If multiple columns
#'        are provided, each unique combination is treated as a distinct
#'        treatment combination.
#' @param row_col \code{character} vector.
#'        Column name of the experiment's rows in \code{design}.
#' @param column_col \code{character} vector.
#'        Column name of the experiment's columns in \code{design}.
#' @param block_col \code{character} vector or \code{NULL}.
#'        Column name of the experiment's blocking factor in \code{design}.
#'        Use \code{NULL} for unblocked designs.
#' @param rho_row \code{numeric}.
#'        AR1 correlation parameter in the row direction.
#' @param rho_col \code{numeric}.
#'        AR1 correlation parameter in the column direction.
#' @param tolerance \code{numeric}.
#'        Tolerance for numerical stability.
#'
#' @return A list containing:
#' \describe{
#'   \item{fisher_info}{The treatment information matrix, \eqn{I_T}.}
#'   \item{eigenvalues}{Positive eigenvalues of the treatment information
#'     matrix.}
#'   \item{rank}{Numerical rank of the treatment information matrix.}
#'   \item{estimable_design}{Whether all treatment contrasts are estimable. For
#'     \eqn{v} treatment levels, this requires rank at least \eqn{v - 1}.}
#'   \item{a_optimality}{A-optimality score, calculated as the sum of inverse
#'     positive eigenvalues. Smaller values indicate better average treatment
#'     contrast precision.}
#'   \item{d_optimality}{D-optimality score, calculated as the negative sum of
#'     log positive eigenvalues. Smaller values indicate greater overall
#'     information volume.}
#'   \item{pairwise_efficiency}{Data frame containing pairwise treatment
#'     comparisons, estimability indicators, contrast variances, and
#'     iid-equivalent effective replicates.}
#'   \item{assumptions}{List of spatial correlation and design assumptions used
#'     in the calculation.}
#' }
#'
#' @examples
#' \dontrun{
#' # RCBD
#' df <- data.frame(
#'     row = rep(1:6, each = 4),
#'     col = rep(1:4, times = 6),
#'     treatment = rep(LETTERS[1:8], 3),
#'     block = rep(1:3, each = 8)
#' )
#'
#' # Optimise while respecting blocks
#' result <- speed::speed(df,
#'     "treatment",
#'     swap_within = "block",
#'     iterations = 5000,
#'     seed = 42
#' )
#'
#' ## test on latin square
#' latinsquare <- data.frame(
#'     row = rep(1:4, each = 4),
#'     col = rep(1:4, times = 4),
#'     treatment = c(
#'         "A", "B", "C", "D",
#'         "B", "C", "D", "A",
#'         "C", "D", "A", "B",
#'         "D", "A", "B", "C"
#'     )
#' )
#'
#' res_eff <- design_efficiency(
#'     design = latinsquare,
#'     treatment_cols = "treatment",
#'     row_col = "row",
#'     column_col = "col",
#'     block_col = NULL,
#'     rho_row = 0.3,
#'     rho_col = 0.3
#' )
#' }
#'
#' @return
#' A list with a lot of info in it.
#'
#' @export
design_efficiency <- function(
    design,
    treatment_cols = c("treatment"),
    row_col = "row",
    column_col = "col",
    block_col = NULL,
    rho_row = 0.1,
    rho_col = 0.1,
    alpha = 0.05,
    tolerance = 1e-10
) {
    ti <- compute_treatment_info(
        design = design,
        treatment_cols = treatment_cols,
        row_col = row_col,
        column_col = column_col,
        block_col = block_col,
        rho_row = rho_row,
        rho_col = rho_col,
        alpha = alpha,
        tolerance = tolerance
    )

    v <- length(ti$trt_levels)

    if (ti$rank < v - 1) {
        a_val <- Inf
        d_val <- Inf
    } else {
        a_val <- sum(1 / ti$eigenvalues)
        d_val <- -sum(log(ti$eigenvalues))
    }

    # pairwise treatment comparisons
    pairs <- combn(ti$trt_levels, 2, simplify = FALSE)

    out <- lapply(pairs, \(pair) {
        contrast <- numeric(v)
        names(contrast) <- ti$trt_levels
        contrast[pair[1]] <- 1
        contrast[pair[2]] <- -1

        # Estimable?
        # contrast must lie in the row space of the information
        # use a property of the pseudoinverse:
        # A A+ maps all column vectors of A to themselves
        # the contrast should be theoretically unchanged if in info row space
        projected <- as.numeric(contrast %*% ti$info_inv %*% ti$info)

        estimable <- sqrt(sum((contrast - projected)^2)) <=
            tolerance * (1 + sqrt(sum(contrast^2)))

        if (!estimable) {
            return(data.frame(
                comparison = paste(pair, collapse = " vs "),
                estimable = FALSE,
                variance = NA,
                effective_replicates = NA
            ))
        }

        contrast_var <- as.numeric(t(contrast) %*% ti$info_inv %*% contrast)

        return(data.frame(
            comparison = paste(pair, collapse = " vs "),
            estimable = TRUE,
            variance = contrast_var,
            effective_replicates = 2 / contrast_var
        ))
    })

    contrast_eff <- do.call(rbind, out)
    return(list(
        fisher_info = ti$info,
        eigenvalues = ti$eigenvalues,
        rank = ti$rank,
        estimable_design = ti$rank >= v-1,
        a_optimality = a_val,
        d_optimality = d_val,
        pairwise_efficiency = contrast_eff,
        assumptions = list(
            rho_row = rho_row,
            rho_col = rho_col,
            block = block_col,
            treatment_cols = treatment_cols
        )
    ))
}


## Helper functions

combine_treatments <- function(design, treatment_cols) {
    vals <-
        apply(design[, treatment_cols, drop = FALSE], 1, paste, collapse = ":")
    return(factor(vals, levels = unique(vals)))
}

## moore-penrose inverse/pseudoinverse
mpinv <- function(mat, tol = 1e-10) {
    s <- svd(mat)
    keep <- s$d > tol * max(dim(mat)) * max(s$d)
    d_inv <- ifelse(keep, 1 / s$d, 0)

    s$v %*% diag(d_inv, length(d_inv)) %*% t(s$u)
}

## ar1 x ar1 correlation structure
cor_ar1_ar1 <- function(row, column, rho_row, rho_col, alpha) {
    if (alpha <= 0 || alpha >= 1) stop("alpha must be between 0 and 1.")
    if (rho_row < 0 || rho_row >= 1) stop("rho_row must be in [0, 1).")
    if (rho_col < 0 || rho_col >= 1) stop("rho_col must be in [0, 1).")
    if (!is.numeric(row)) row <- as.numeric(factor(row))
    if (!is.numeric(column)) column <- as.numeric(factor(column))

    if (anyNA(row) || anyNA(column)) stop("NAs in row or col")

    row_dist <- abs(outer(row, row, "-"))
    col_dist <- abs(outer(column, column, "-"))

    R <- (rho_row^row_dist) * (rho_col^col_dist)

    return((R + t(R)) / 2)
}

build_treatment_matrix <- function(treatments) {
    treatments <- factor(treatments)
    trt_levels <- levels(treatments)
    x1 <- stats::model.matrix(~ 0 + treatments)
    colnames(x1) <- trt_levels
    return(list(x1 = x1, trt_levels = trt_levels))
}

build_nuisance_matrix <- function(design, block = NULL) {
    ## if non-blocked, intercept is nuisance
    if (is.null(block)) {
        x2 <- matrix(1, nrow = nrow(design), ncol = 1)
        colnames(x2) <- "intercept"
        return(x2)
    }

    if (!block %in% names(design)) {
        stop(sprintf("block column '%s' not in design", block))
    }

    ## if blocked, no intercept since block span is confounded with it
    stats::model.matrix(
        stats::as.formula(sprintf("~ 0 + factor(%s)", block)),
        data = design
    )
}

compute_treatment_info <- function(
    design,
    treatment_cols = c("treatment"),
    row_col = "row",
    column_col = "col",
    block_col = NULL,
    rho_row = 0.1,
    rho_col = 0.1,
    alpha = alpha,
    tolerance = 1e-10
) {
    ## combine multiple treatment columns into a treatment-combo factor
    treatments <- combine_treatments(design, treatment_cols)
    trt <- build_treatment_matrix(treatments)
    ## x1 is the treatment indicator
    x1 <- trt$x1
    trt_levels <- trt$trt_levels

    ## x2 is the nuisance matrix, intercept and block indicators
    x2 <- build_nuisance_matrix(design, block = block_col)

    ## spatial structure
    r <- cor_ar1_ar1(
        row = design[[row_col]],
        column = design[[column_col]],
        rho_row = rho_row,
        rho_col = rho_col,
        alpha = alpha
    )
    r_inv <- solve(r)
    r_inv_x2 <- r_inv %*% x2

    ## GLS projection that removes nuisance effects
    ## L = I - X2 (X2t X2)^{-1} X2t
    ## under spatial R structure's geometry:
    ## L = R^{-1} - R^{-1} X2 (X2t R^{-1} X2)^{-1} X2t R^{-1}
    mid <- t(x2) %*% r_inv_x2
    mid_inv <- mpinv(mid, tol = tolerance)
    proj <- r_inv - r_inv_x2 %*% mid_inv %*% t(r_inv_x2)

    ## fisher information in GLS nuisanceless geometry
    info <- t(x1) %*% proj %*% x1
    info <- (info + t(info)) / 2
    info_inv <- mpinv(info, tol = tolerance)

    ## eigenvals for contrasts and optimality
    eig <- eigen(info, symmetric = TRUE, only.values = TRUE)$values
    max_eig <- max(eig)

    pos_eig <- if (max_eig <= 0) {
        numeric(0)
    } else {
        eig[eig > tolerance * max_eig]
    }

    return(list(
        info = info,
        info_inv = info_inv,
        L = proj,
        R = r,
        X1 = x1,
        X2 = x2,
        trt_levels = trt_levels,
        eigenvalues = sort(pos_eig, decreasing = TRUE),
        rank = length(pos_eig)
    ))
}
