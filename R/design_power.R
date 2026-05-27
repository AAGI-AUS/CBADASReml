#' Calculate the statistical power for an experimental design
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
#' information matrix. The effect size `delta` is then converted into a
#' noncentrality parameter,
#' \deqn{
#'   \lambda
#'   =
#'   \frac{\delta}{\operatorname{SE}(c^\intercal \hat{\tau})},
#' }
#' and power is calculated using a two-sided known-covariance Z-test
#' approximation.
#'
#' @param design `data.frame`.
#'        The design data frame, one row per experimental unit.
#' @param treatment_cols `character` vector.
#'        Column name of the treatment in `design`. If multiple columns
#'        are provided, each unique combination is treated as a distinct
#'        treatment combination.
#' @param row_col `character` vector.
#'        Column name of the experiment's rows in `design`.
#' @param column_col `character` vector.
#'        Column name of the experiment's columns in `design`.
#' @param block_col `character` vector or `NULL`.
#'        Column name of the experiment's blocking factor in `design`.
#'        Use `NULL` for unblocked designs.
#' @param rho_row `numeric`.
#'        AR1 correlation parameter in the row direction.
#' @param rho_col `numeric`.
#'        AR1 correlation parameter in the column direction.
#' @param sigma2 `numeric`.
#'        Assumed residual variance.
#' @param delta `numeric`.
#'        Treatment difference that is "worth detecting". The minimum
#'        difference between treatments that we care about.
#' @param alpha `numeric`.
#'        Significance/p-value threshold for the two-sided z-tests.
#' @param tolerance `numeric`.
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
#' The spatial covariance parameters `rho_row`, `rho_col`, and
#' `sigma2` are treated as assumed planning values, not estimated from the
#' data. The returned power is therefore conditional on the supplied covariance
#' structure, the chosen effect size `delta`, and the significance level
#' `alpha`.
#'
#' Thus, you as the statistician must select four parameters based on previous
#' trials, domain knowledge, or *conservative* assumptions:
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
#'     estimate. Underestimating \eqn{\sigma^2} will result in *overstated
#'     power*.
#'     }
#'   \item{\eqn{\delta}}{
#'     Detectable treatment difference.
#'     Choose \eqn{\delta} as the *smallest* treatment
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
    if (sigma2 <= 0) {
        cli::cli_abort("sigma2 must be positive.")
    }
    if (delta <= 0) {
        cli::cli_abort("delta must be positive.")
    }

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
    pairs <- utils::combn(trt_levels, 2, simplify = FALSE)

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
            return(
                data.frame(
                    c(
                        comparison = paste(pair, collapse = " vs "),
                        estimable = FALSE,
                        se = NA,
                        lambda = NA,
                        power = NA,
                        effective_replicates = NA
                    ),
                    stringsAsFactors = FALSE
                ),
            )
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
        worst_comparison = contrast_power$comparison[which.min(
            contrast_power$power
        )],
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
