#' Calculate the design efficiency/optimality of a small-plot trial design
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
#' @param alpha significance level `numeric`.
#' @param tolerance `numeric`.
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
    treatment_cols = "treatment",
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
    pairs <- utils::combn(ti$trt_levels, 2, simplify = FALSE)

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
                c(
                    comparison = paste(pair, collapse = " vs "),
                    estimable = FALSE,
                    variance = NA,
                    effective_replicates = NA
                ),
                stringsAsFactors = FALSE
            ))
        }

        contrast_var <- as.numeric(t(contrast) %*% ti$info_inv %*% contrast)

        return(data.frame(
            c(
                comparison = paste(pair, collapse = " vs "),
                estimable = TRUE,
                variance = contrast_var,
                effective_replicates = 2 / contrast_var,
                stringsAsFactors = FALSE
            )
        ))
    })

    contrast_eff <- do.call(rbind, out)
    return(list(
        fisher_info = ti$info,
        eigenvalues = ti$eigenvalues,
        rank = ti$rank,
        estimable_design = ti$rank >= v - 1,
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
    if (alpha <= 0 || alpha >= 1) {
        cli::cli_abort("alpha must be between 0 and 1.")
    }
    if (rho_row < 0 || rho_row >= 1) {
        cli::cli_abort("rho_row must be in [0, 1).")
    }
    if (rho_col < 0 || rho_col >= 1) {
        cli::cli_abort("rho_col must be in [0, 1).")
    }
    if (!is.numeric(row)) {
        row <- as.numeric(factor(row))
    }
    if (!is.numeric(column)) {
        column <- as.numeric(factor(column))
    }

    if (anyNA(row) || anyNA(column)) {
        cli::cli_abort("NAs in row or col")
    }

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
        cli::cli_abort(sprintf("block column '%s' not in design", block))
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
