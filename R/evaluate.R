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
#'   I_T = X_1^\intercal L_V X_1,
#' }
#' where \eqn{X_1} is the treatment indicator matrix, the observational
#' covariance is \eqn{\sigma^2 V}, and \eqn{L_V} is the relative
#' covariance-adjusted projection matrix that removes nuisance effects:
#' \deqn{
#'   L_V
#'   =
#'   V^{-1}
#'   -
#'   V^{-1} X_2
#'   \left( X_2^\intercal V^{-1} X_2 \right)^{-1}
#'   X_2^\intercal
#'   V^{-1}
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
#' and power is calculated using a two-sided Wald test. A noncentral
#' t-distribution is used when finite denominator degrees of freedom are
#' available; otherwise, the known-covariance normal approximation is used.
#'
#' @param design \code{data.frame}.
#'        The design data frame, one row per experimental unit.
#' @param treatment_cols \code{character} vector.
#'        Column name of the treatment in \code{design}. If multiple columns
#'        are provided, each unique combination is treated as a distinct
#'        treatment combination.
#' @param block_col A single \code{character} column name or \code{NULL}.
#'        Column name of the experiment's blocking factor in \code{design}.
#'        Use \code{NULL} for unblocked designs.
#' @param covariance_structure Covariance structure created with
#'        [cov_iid()], [cov_ar1()], [cov_ar1ar1()], or another covariance
#'        helper.
#' @param sigma2 \code{numeric}.
#'        Assumed residual variance.
#' @param delta \code{numeric}.
#'        Treatment difference that is "worth detecting". The minimum
#'        difference between treatments that we care about.
#' @param alpha \code{numeric}.
#'        Significance threshold for the two-sided Wald tests.
#' @param denom_df Positive denominator degrees of freedom for a noncentral
#'        t-test, or \code{Inf} for the known-covariance normal approximation.
#'        If \code{NULL}, residual degrees of freedom are calculated from the
#'        fixed-effect design matrix. This residual value is only approximate
#'        when covariance parameters will be estimated.
#' @param tolerance \code{numeric}.
#'        Tolerance for numerical stability.
#'
#' @return A list containing:
#' \describe{
#'   \item{contrast_power}{Data frame of pairwise treatment-comparison standard
#'     errors, noncentrality parameters, power values, and effective
#'     replicates.}
#'   \item{design_power}{The minimum pairwise power across all estimable
#'     treatment comparisons, or \code{NA} if none are estimable.}
#'   \item{average_power}{The average pairwise power across all estimable
#'     treatment comparisons, or \code{NA} if none are estimable.}
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
#' The covariance structure and \code{sigma2} are treated as planning values.
#' The returned power is therefore conditional on the supplied covariance
#' structure, the scientifically relevant difference \code{delta}, and the
#' significance level \code{alpha}. Underestimating \code{sigma2} overstates
#' power. Evaluate plausible covariance structures when their parameters are
#' uncertain.
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
#' # RCBD with AR1 x AR1 structure
#' res_power <- design_power(
#'     design = latinsquare,
#'     treatment_cols = "treatment",
#'     block_col = "rep",
#'     covariance_structure = cov_ar1ar1(
#'         row_col = "row",
#'         column_col = "range",
#'         rho_row = 0.1,
#'         rho_col = 0.1
#'     ),
#'     sigma2 = 1,
#'     delta = 1,
#'     alpha = 0.05
#' )
#'
#' # Split plot design
#' res_power <- design_power(
#'     design = latinsquare,
#'     treatment_cols = "treatment",
#'     block_col = "rep",
#'     covariance_structure =
#'         cov_ar1ar1("row", "range", rho_row = 0.1, rho_col = 0.1) +
#'         cov_random(cov_iid("whole_plot"), sd_ratio = 1),
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
    block_col = NULL,
    covariance_structure = cov_iid(),
    sigma2 = 1,
    delta = 1,
    alpha = 0.05,
    denom_df = NULL,
    tolerance = 1e-10
) {
    if (sigma2 <= 0) stop("sigma2 must be positive.")
    if (delta <= 0) stop("delta must be positive.")

    ti <- compute_treatment_info(
        design = design,
        treatment_cols = treatment_cols,
        block_col = block_col,
        covariance_structure = covariance_structure,
        tolerance = tolerance
    )

    trt_levels <- ti$trt_levels
    v <- length(trt_levels)
    ## pairwise power requires at least one treatment contrast
    if (v < 2) {
        stop("design must contain at least two treatment levels.")
    }

    ## use fixed-effect residual df unless we have denominator df
    if (is.null(denom_df)) {
        df <- ti$resid_df
        ## fall back to normal power when residual df are unavailable
        if (!is.finite(df) || df <= 0) {
            warning(
                "No residual degrees of freedom; using normal approximation."
            )
            df <- Inf
        } else if (ti$has_random) {
            ## warn that mixed-model denominator df usually need refinement
            warning(
                "Residual degrees of freedom may be optimistic when ",
                "covariance parameters are estimated; set denom_df ",
                "explicitly if known."
            )
        }
    } else {
        df <- denom_df
    }

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

        ## Fisher-information Wald power
        power <- wald_power(lambda, alpha, df)

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

    ## power for all comparisons
    contrast_power <- do.call(rbind, out)
    estimable <- contrast_power$estimable %in% TRUE
    ## summarise estimable contrasts
    if (any(estimable)) {
        estimable_power <- contrast_power$power[estimable]
        design_power_value <- min(estimable_power)
        average_power_value <- mean(estimable_power)
        estimable_rows <- which(estimable)
        worst_comparison <- contrast_power$comparison[
            estimable_rows[which.min(estimable_power)]
        ]
    } else {
        ## else return NA when there are none
        design_power_value <- NA_real_
        average_power_value <- NA_real_
        worst_comparison <- NA_character_
    }

    ## record which reference distribution was used for the wald test
    power_method <- if (is.finite(df)) {
        "Fisher-information noncentral-t Wald power"
    } else {
        "known-covariance Fisher-information normal power"
    }

    return(structure(list(
        contrast_power = contrast_power,
        design_power = design_power_value,
        average_power = average_power_value,
        worst_comparison = worst_comparison,
        fisher_info = ti$info,
        treatment_cov = sigma2 * ti$info_inv,
        eigenvalues = ti$eigenvalues,
        rank = ti$rank,
        assumptions = list(
            sigma2 = sigma2,
            delta = delta,
            alpha = alpha,
            covariance_structure = format_covariance_structure(
                covariance_structure
            ),
            block_col = block_col,
            treatment_cols = treatment_cols,
            denom_df = df,
            power_method = power_method
        )
    ), class = c("cbadas_design_power", "list")))
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
#'     block_col = NULL,
#'     covariance_structure = cov_ar1ar1(
#'         row_col = "row",
#'         column_col = "range",
#'         rho_row = 0.1,
#'         rho_col = 0.1
#'     )
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
    block_col = NULL,
    covariance_structure = cov_iid(),
    alpha = 0.05,
    tolerance = 1e-10
) {
    ti <- compute_treatment_info(
        design = design,
        treatment_cols = treatment_cols,
        block_col = block_col,
        covariance_structure = covariance_structure,
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
    return(structure(list(
        fisher_info = ti$info,
        eigenvalues = ti$eigenvalues,
        rank = ti$rank,
        estimable_design = ti$rank >= v - 1,
        a_optimality = a_val,
        d_optimality = d_val,
        pairwise_efficiency = contrast_eff,
        assumptions = list(
            covariance_structure = format_covariance_structure(
                covariance_structure
            ),
            block = block_col,
            treatment_cols = treatment_cols
        )
    ), class = c("cbadas_design_efficiency", "list")))
}

#' @export
print.cbadas_design_power <- function(x, ..., n = 5) {
    contrast_power <- x$contrast_power
    estimable <- contrast_power$estimable %in% TRUE
    n_estimable <- sum(estimable)
    n_total <- nrow(contrast_power)

    ui_h1("Design power")
    ui_text(paste0(
        "Design power: ",
        format_ui_percent(x$design_power),
        "  |  Average power: ",
        format_ui_percent(x$average_power)
    ))
    ui_text(paste0(
        "Worst comparison: ",
        format_ui_value(x$worst_comparison),
        "  |  Estimable comparisons: ",
        n_estimable,
        "/",
        n_total
    ))
    ui_text(paste0(
        "Assumptions: delta = ",
        format_ui_number(x$assumptions$delta),
        ", sigma2 = ",
        format_ui_number(x$assumptions$sigma2),
        ", alpha = ",
        format_ui_number(x$assumptions$alpha),
        ", denominator df = ",
        format_ui_number(x$assumptions$denom_df),
        ", covariance = ",
        x$assumptions$covariance_structure
    ))

    if (n_estimable > 0) {
        ui_h2("Lowest-power comparisons")
        power_table <- contrast_power[estimable, , drop = FALSE]
        power_table <- power_table[
            order(power_table$power, power_table$se),
            ,
            drop = FALSE
        ]
        power_table <- utils::head(power_table, n)
        print(format_power_table(power_table), row.names = FALSE)
    } else {
        ui_alert("No estimable pairwise comparisons.")
    }

    invisible(x)
}

#' @export
print.cbadas_design_efficiency <- function(x, ..., n = 5) {
    pairwise_efficiency <- x$pairwise_efficiency
    estimable <- pairwise_efficiency$estimable %in% TRUE
    n_estimable <- sum(estimable)
    n_total <- nrow(pairwise_efficiency)

    ui_h1("Design efficiency")
    ui_text(paste0(
        "Estimable design: ",
        if (isTRUE(x$estimable_design)) "yes" else "no",
        "  |  Rank: ",
        x$rank,
        "  |  Estimable comparisons: ",
        n_estimable,
        "/",
        n_total
    ))
    ui_text(paste0(
        "A-optimality: ",
        format_ui_number(x$a_optimality),
        "  |  D-optimality: ",
        format_ui_number(x$d_optimality)
    ))
    ui_text(paste0(
        "Assumptions: covariance = ",
        x$assumptions$covariance_structure
    ))

    if (n_estimable > 0) {
        ui_h2("Least-precise comparisons")
        efficiency_table <- pairwise_efficiency[estimable, , drop = FALSE]
        efficiency_table <- efficiency_table[
            order(-efficiency_table$variance),
            ,
            drop = FALSE
        ]
        efficiency_table <- utils::head(efficiency_table, n)
        print(format_efficiency_table(efficiency_table), row.names = FALSE)
    } else {
        ui_alert("No estimable pairwise comparisons.")
    }

    invisible(x)
}

## Covariance structure builders

#' Build a covariance structure
#'
#' Builds the "relative" covariance for a design:
#' \deqn{
#'   V_{\text{rel}} = R + ZGZ
#' }
build_covariance_matrix <- function(design, covariance_structure) {
    components <- as_cov_components(covariance_structure)

    n <- nrow(design)
    V <- matrix(0, n, n)

    for (component in components) {
        if (component$type == "iid") {
            V <- V + diag(n)
        } else if (component$type == "ar1") {
            V <- V + apply_group_mask(
                cor_ar1(
                    index = get_design_column(
                        design,
                        component$index_col,
                        "index"
                    ),
                    rho = component$rho
                ),
                design = design,
                group = component$group
            )
        } else if (component$type == "ar1ar1") {
            V <- V + apply_group_mask(
                cor_ar1ar1(
                    row = get_design_column(
                        design,
                        component$row_col,
                        "row"
                    ),
                    column = get_design_column(
                        design,
                        component$column_col,
                        "column"
                    ),
                    rho_row = component$rho_row,
                    rho_col = component$rho_col
                ),
                design = design,
                group = component$group
            )
        } else if (component$type == "cs") {
            V <- V + cor_cs(
                group = get_design_column(
                    design,
                    component$group_col,
                    "group"
                ),
                rho = component$rho
            )
        } else if (component$type == "exponential") {
            y <- if (is.null(component$y_col)) {
                NULL
            } else {
                get_design_column(design, component$y_col, "y")
            }
            V <- V + apply_group_mask(
                cor_exponential(
                    x = get_design_column(design, component$x_col, "x"),
                    y = y,
                    range = component$range
                ),
                design = design,
                group = component$group
            )
        } else if (component$type == "random") {
            group <- component$component$group

            if (is.null(group)) {
                stop("random() covariance components need a grouping column")
            }
            get_design_column(design, group, "group")

            form <- stats::as.formula(paste0("~ 0 + factor(", group, ")"))
            Z <- stats::model.matrix(form, data = design)

            G <- diag(ncol(Z))
            V <- V + (component$sd_ratio^2) * Z %*% G %*% t(Z)
        } else {
            stop("Unknown covariance component: ", component$type)
        }
    }

    if (nrow(V) != n || ncol(V) != n) {
        stop(
            "Covariance matrix must have ",
            "dimension = nrow(design) x nrow(design)"
        )
    }

    if (!isSymmetric(V, tol = 1e-10)) {
        stop("Covariance matrix must be symmetric")
    }

    return((V + t(V)) / 2)
}

as_cov_components <- function(x) {
    if (inherits(x, "cov_structure")) {
        return(x$components)
    }

    if (inherits(x, "cov_component")) {
        return(list(x))
    }

    stop("Invalid covariance structure.")
}

#' Independent, identically distributed covariance component
#'
#' @param group Optional grouping column used when wrapped in [cov_random()].
#'
#' @rdname covariance_helpers
#' @export
cov_iid <- function(group = NULL) {
    structure(
        list(
            type = "iid",
            group = group
        ),
        class = "cov_component"
    )
}

#' One-dimensional AR1 covariance component
#'
#' @param index_col Column containing the ordered coordinate.
#' @param rho AR1 correlation parameter.
#' @param group Optional grouping column. When supplied, covariance is zero
#'   between different groups.
#'
#' @rdname covariance_helpers
#' @export
cov_ar1 <- function(index_col, rho = 0.1, group = NULL) {
    structure(
        list(
            type = "ar1",
            index_col = index_col,
            rho = rho,
            group = group
        ),
        class = "cov_component"
    )
}

#' Two-dimensional separable AR1 covariance component
#'
#' @param row_col Column containing row coordinates.
#' @param column_col Column containing column coordinates.
#' @param rho_row AR1 correlation parameter in the row direction.
#' @param rho_col AR1 correlation parameter in the column direction.
#' @param group Optional grouping column. When supplied, covariance is zero
#'   between different groups.
#'
#' @rdname covariance_helpers
#' @export
cov_ar1ar1 <- function(
    row_col,
    column_col,
    rho_row = 0.1,
    rho_col = 0.1,
    group = NULL
) {
    structure(
        list(
            type = "ar1ar1",
            row_col = row_col,
            column_col = column_col,
            rho_row = rho_row,
            rho_col = rho_col,
            group = group
        ),
        class = "cov_component"
    )
}

#' Compound-symmetry covariance component
#'
#' @param group_col Column defining independent compound-symmetry groups.
#' @param rho Common within-group correlation.
#'
#' @rdname covariance_helpers
#' @export
cov_cs <- function(group_col, rho = 0.1) {
    structure(
        list(
            type = "cs",
            group_col = group_col,
            rho = rho
        ),
        class = "cov_component"
    )
}

#' Exponential-distance covariance component
#'
#' @param x_col Column containing x coordinates.
#' @param y_col Optional column containing y coordinates.
#' @param range Positive distance scale.
#' @param group Optional grouping column. When supplied, covariance is zero
#'   between different groups.
#'
#' @rdname covariance_helpers
#' @export
cov_exponential <- function(x_col, y_col = NULL, range = 1, group = NULL) {
    structure(
        list(
            type = "exponential",
            x_col = x_col,
            y_col = y_col,
            range = range,
            group = group
        ),
        class = "cov_component"
    )
}

#' Transform a random covariance structure into observation space
#'
#' @param x Covariance component or grouping column name.
#' @param sd_ratio Standard deviation ratio relative to the residual scale.
#'
#' @rdname covariance_helpers
#' @export
cov_random <- function(x, sd_ratio = 1) {
    if (is.character(x) && length(x) == 1) {
        x <- cov_iid(x)
    }

    if (!inherits(x, "cov_component")) {
        stop(
            "cov_random() must wrap a covariance component or grouping ",
            "variable name"
        )
    }

    structure(
        list(
            type = "random",
            component = x,
            sd_ratio = sd_ratio
        ),
        class = "cov_component"
    )
}

#' @export
`+.cov_component` <- function(e1, e2) {
    structure(
        list(components = c(as_cov_components(e1), as_cov_components(e2))),
        class = "cov_structure"
    )
}

#' @export
`+.cov_structure` <- function(e1, e2) {
    structure(
        list(components = c(as_cov_components(e1), as_cov_components(e2))),
        class = "cov_structure"
    )
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

## Two sided power of a Wald test at lambda noncentrality
## noncentral t at finite df, else normal
wald_power <- function(lambda, alpha, df = Inf) {
    if (is.finite(df)) {
        crit <- stats::qt(1 - alpha / 2, df)
        return(
            stats::pt(-crit, df, ncp = lambda) +
                stats::pt(crit, df, ncp = lambda, lower.tail = FALSE)
        )
    } else {
        crit <- stats::qnorm(1 - alpha / 2)
        return(
            stats::pnorm(-crit, mean = lambda, sd = 1) +
                stats::pnorm(
                    crit,
                    mean = lambda,
                    sd = 1,
                    lower.tail = FALSE
                )
        )
    }
}

format_power_table <- function(x) {
    data.frame(
        comparison = x$comparison,
        power = format_ui_percent(x$power),
        se = format_ui_number(x$se),
        lambda = format_ui_number(x$lambda),
        effective_replicates = format_ui_number(x$effective_replicates),
        check.names = FALSE
    )
}

format_efficiency_table <- function(x) {
    data.frame(
        comparison = x$comparison,
        variance = format_ui_number(x$variance),
        effective_replicates = format_ui_number(x$effective_replicates),
        check.names = FALSE
    )
}

format_covariance_structure <- function(covariance_structure) {
    components <- as_cov_components(covariance_structure)

    paste(
        vapply(
            components,
            format_covariance_component,
            character(1)
        ),
        collapse = " + "
    )
}

format_covariance_component <- function(component) {
    if (component$type == "iid") {
        if (is.null(component$group)) {
            return("iid")
        }

        return(paste0("iid(", component$group, ")"))
    }

    if (component$type == "ar1") {
        out <- paste0(
            "ar1(index = ",
            component$index_col,
            ", rho = ",
            format_ui_number(component$rho)
        )
        if (!is.null(component$group)) {
            out <- paste0(out, ", group = ", component$group)
        }

        return(paste0(out, ")"))
    }

    if (component$type == "ar1ar1") {
        out <- paste0(
            "ar1ar1(row = ",
            component$row_col,
            ", column = ",
            component$column_col,
            ", rho_row = ",
            format_ui_number(component$rho_row),
            ", rho_col = ",
            format_ui_number(component$rho_col)
        )
        if (!is.null(component$group)) {
            out <- paste0(out, ", group = ", component$group)
        }

        return(paste0(out, ")"))
    }

    if (component$type == "cs") {
        return(paste0(
            "cs(group = ",
            component$group_col,
            ", rho = ",
            format_ui_number(component$rho),
            ")"
        ))
    }

    if (component$type == "exponential") {
        out <- paste0(
            "exponential(x = ",
            component$x_col
        )
        if (!is.null(component$y_col)) {
            out <- paste0(out, ", y = ", component$y_col)
        }
        out <- paste0(
            out,
            ", range = ",
            format_ui_number(component$range)
        )
        if (!is.null(component$group)) {
            out <- paste0(out, ", group = ", component$group)
        }

        return(paste0(out, ")"))
    }

    if (component$type == "random") {
        return(paste0(
            "random(",
            format_covariance_component(component$component),
            ", sd_ratio = ",
            format_ui_number(component$sd_ratio),
            ")"
        ))
    }

    component$type
}

format_ui_value <- function(x) {
    if (length(x) == 0 || is.na(x)) {
        return("NA")
    }

    as.character(x)
}

format_ui_number <- function(x, digits = 3) {
    if (length(x) == 0) {
        return("NA")
    }

    vapply(
        x,
        \(value) {
            if (is.na(value)) {
                return("NA")
            }

            if (is.infinite(value)) {
                return(as.character(value))
            }

            formatC(value, digits = digits, format = "f")
        },
        character(1)
    )
}

format_ui_percent <- function(x, digits = 1) {
    if (length(x) == 0) {
        return("NA")
    }

    vapply(
        x,
        \(value) {
            if (is.na(value)) {
                return("NA")
            }

            if (is.infinite(value)) {
                return(as.character(value))
            }

            paste0(formatC(100 * value, digits = digits, format = "f"), "%")
        },
        character(1)
    )
}

ui_h1 <- function(text) {
    if (requireNamespace("cli", quietly = TRUE)) {
        cli::cli_h1(text)
    } else {
        cat("\n", text, "\n", sep = "")
    }
}

ui_h2 <- function(text) {
    if (requireNamespace("cli", quietly = TRUE)) {
        cli::cli_h2(text)
    } else {
        cat("\n", text, "\n", sep = "")
    }
}

ui_text <- function(text) {
    if (requireNamespace("cli", quietly = TRUE)) {
        cli::cli_text(text)
    } else {
        cat(text, "\n", sep = "")
    }
}

ui_alert <- function(text) {
    if (requireNamespace("cli", quietly = TRUE)) {
        cli::cli_alert_info(text)
    } else {
        cat(text, "\n", sep = "")
    }
}

get_design_column <- function(design, column, label) {
    if (!column %in% names(design)) {
        stop(sprintf("%s column '%s' not in design", label, column))
    }

    design[[column]]
}

apply_group_mask <- function(mat, design, group = NULL) {
    if (is.null(group)) {
        return(mat)
    }

    group_values <- get_design_column(design, group, "group")
    if (anyNA(group_values)) {
        stop("NAs in grouping column")
    }

    same_group <- outer(group_values, group_values, "==")
    mat * same_group
}

validate_correlation <- function(x, name = "rho") {
    invalid <- !is.numeric(x) ||
        length(x) != 1 ||
        is.na(x) ||
        x < 0 ||
        x >= 1

    if (invalid) {
        stop(sprintf("%s must be in [0, 1).", name))
    }
}

validate_positive_number <- function(x, name) {
    invalid <- !is.numeric(x) ||
        length(x) != 1 ||
        is.na(x) ||
        x <= 0

    if (invalid) {
        stop(sprintf("%s must be positive.", name))
    }
}

## one-dimensional ar1 correlation structure
cor_ar1 <- function(index, rho) {
    validate_correlation(rho)
    if (length(index) == 0) stop("index values must not be empty")
    if (!is.numeric(index)) index <- as.numeric(factor(index))
    if (anyNA(index)) stop("NAs in index")

    index_dist <- abs(outer(index, index, "-"))
    R <- rho^index_dist

    return((R + t(R)) / 2)
}

## ar1 x ar1 correlation structure
cor_ar1ar1 <- function(row, column, rho_row, rho_col) {
    validate_correlation(rho_row, "rho_row")
    validate_correlation(rho_col, "rho_col")
    if (!is.numeric(row)) row <- as.numeric(factor(row))
    if (!is.numeric(column)) column <- as.numeric(factor(column))

    if (anyNA(row) || anyNA(column)) stop("NAs in row or col")

    row_dist <- abs(outer(row, row, "-"))
    col_dist <- abs(outer(column, column, "-"))

    R <- (rho_row^row_dist) * (rho_col^col_dist)

    return((R + t(R)) / 2)
}

## compound-symmetry correlation structure
cor_cs <- function(group, rho) {
    validate_correlation(rho)
    if (length(group) == 0) stop("group values must not be empty")
    if (anyNA(group)) stop("NAs in group")

    same_group <- outer(group, group, "==")
    R <- ifelse(same_group, rho, 0)
    diag(R) <- 1

    return((R + t(R)) / 2)
}

## exponential spatial correlation structure
cor_exponential <- function(x, y = NULL, range) {
    validate_positive_number(range, "range")
    if (length(x) == 0) stop("x values must not be empty")
    if (!is.numeric(x)) stop("x must be numeric")

    if (is.null(y)) {
        if (anyNA(x)) stop("NAs in x")
        dist <- abs(outer(x, x, "-"))
    } else {
        if (!is.numeric(y)) stop("y must be numeric")
        if (length(y) != length(x)) {
            stop("x and y must have the same length")
        }
        if (anyNA(x) || anyNA(y)) stop("NAs in x or y")

        x_dist <- outer(x, x, "-")
        y_dist <- outer(y, y, "-")
        dist <- sqrt(x_dist^2 + y_dist^2)
    }

    R <- exp(-dist / range)

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
    block_col = NULL,
    covariance_structure = cov_iid(),
    tolerance = 1e-10
) {
    ## combine multiple treatment columns into a treatment-combo factor
    treatments <- combine_treatments(design, treatment_cols)
    if (nlevels(treatments) < 2) {
        stop("design must contain at least two treatment levels.")
    }
    trt <- build_treatment_matrix(treatments)
    ## x1 is the treatment indicator
    x1 <- trt$x1
    trt_levels <- trt$trt_levels

    ## x2 is the nuisance matrix, intercept and block indicators
    x2 <- build_nuisance_matrix(design, block = block_col)

    ## random covariance terms for the denominator df warning
    components <- as_cov_components(covariance_structure)
    has_random <- any(vapply(
        components,
        \(cmp) identical(cmp$type, "random"),
        FALSE
    ))

    ## spatial structure
    V <- build_covariance_matrix(
        design = design,
        covariance_structure = covariance_structure
    )
    V_inv <- solve(V)
    V_inv_x2 <- V_inv %*% x2

    ## GLS projection that removes nuisance effects
    ## L = I - X2 (X2t X2)^{-1} X2t
    ## under spatial R structure's geometry:
    ## L = R^{-1} - R^{-1} X2 (X2t R^{-1} X2)^{-1} X2t R^{-1}
    mid <- t(x2) %*% V_inv_x2
    mid_inv <- mpinv(mid, tol = tolerance)
    proj <- V_inv - V_inv_x2 %*% mid_inv %*% t(V_inv_x2)

    ## fisher information in GLS nuisanceless geometry
    info <- t(x1) %*% proj %*% x1
    info <- (info + t(info)) / 2

    ## only keep positive information eigenvalues above tolerance
    eig <- eigen(info, symmetric = TRUE)
    vals <- eig$values
    max_val <- max(vals)
    keep <- if (max_val <= 0) {
        rep(FALSE, length(vals))
    } else {
        vals > tolerance * max(dim(info)) * max_val
    }
    ## invert only the estimable positive information eigenspace
    if (any(keep)) {
        vectors <- eig$vectors[, keep, drop = FALSE]
        info_inv <- sweep(vectors, 2, vals[keep], "/") %*% t(vectors)
    } else {
        info_inv <- matrix(0, nrow = nrow(info), ncol = ncol(info))
    }
    ## force symmetric after floating point ops
    info_inv <- (info_inv + t(info_inv)) / 2
    pos_eig <- vals[keep]

    ## provide the default denominator df for fixed effect wald tests
    resid_df <- nrow(design) - qr(cbind(x1, x2))$rank

    return(list(
        info = info,
        info_inv = info_inv,
        L = proj,
        V = V,
        X1 = x1,
        X2 = x2,
        resid_df = resid_df,
        has_random = has_random,
        trt_levels = trt_levels,
        eigenvalues = sort(pos_eig, decreasing = TRUE),
        rank = length(pos_eig)
    ))
}
