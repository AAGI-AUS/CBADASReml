#' Calculate the statistical power for an experimental design
#'
#' Calculate pairwise treatment-comparison power from the Fisher information of
#' the provided experimental design using the treatment information matrix
#' induced by a specified relative covariance structure. This function
#' evaluates how much power the design has to detect a specified treatment
#' difference after adjusting for nuisance effects such as blocks.
#' Supply the difference worth detecting (\code{delta}) and the expected
#' residual standard deviation (\code{sd}). The headline is the lowest
#' pairwise power, with Bonferroni adjustment by default.
#'
#' @param design \code{data.frame}.
#'        The design data frame, one row per independently randomised
#'        experimental unit. Do not count repeated measurements or subsamples
#'        as independent replication.
#' @param treatment_cols \code{character} vector.
#'        Column name of the treatment in \code{design}. If multiple columns
#'        are provided, each unique combination is treated as a distinct
#'        treatment combination. Missing treatment assignments are not allowed.
#' @param block_col A single \code{character} column name or \code{NULL}.
#'        Column name of the experiment's blocking factor in \code{design}.
#'        Use \code{NULL} for unblocked designs.
#' @param covariance_structure Covariance structure created with
#'        [cov_iid()], [cov_ar1()], [cov_ar1ar1()], or another covariance
#'        helper. General correlated structures provide precision estimates,
#'        but power is unavailable unless the simple test described below
#'        applies.
#' @param sigma2 \code{numeric}.
#'        Optional residual variance, retained for compatibility. Supply
#'        either \code{sd} or \code{sigma2}, not both.
#' @param delta \code{numeric}.
#'        Treatment difference that is "worth detecting". The minimum
#'        difference between treatments that we care about. Must be supplied,
#'        finite and positive, and expressed in the response's units.
#' @param sd \code{numeric}.
#'        Required finite positive residual standard deviation, unless
#'        \code{sigma2} is supplied. Use variation remaining after treatment
#'        and block effects, in the same units as \code{delta}. An
#'        upper-plausible value is appropriate for cautious planning.
#'        With covariance helpers, the observational covariance is
#'        \code{sd^2 * V}; for \code{cov_cs()}, this is the marginal scale,
#'        whereas \code{cov_iid() + cov_random()} uses the independent
#'        residual scale.
#' @param alpha \code{numeric}.
#'        Family-wise significance threshold for the two-sided tests when
#'        \code{alpha_adjustment = "bonferroni"}; otherwise the per-comparison
#'        significance threshold. Must be a finite number between 0 and 1.
#' @param alpha_adjustment Pairwise multiplicity adjustment. The default,
#'        \code{"bonferroni"}, uses \code{alpha / choose(v, 2)} for \code{v}
#'        treatment levels. Use \code{"none"} only for unadjusted marginal
#'        power.
#'
#' @return A list containing:
#' \describe{
#'   \item{contrast_power}{Data frame of pairwise treatment-comparison standard
#'     errors, noncentrality parameters, power values, and effective
#'     replicates.}
#'   \item{design_power}{The minimum pairwise power when every pairwise
#'     treatment comparison is estimable and power is supported, otherwise
#'     \code{NA}.}
#'   \item{average_power}{The average pairwise power when every pairwise
#'     treatment comparison is estimable and power is supported, otherwise
#'     \code{NA}.}
#'   \item{worst_comparison}{The comparison with the lowest power when every
#'     pairwise comparison is estimable and power is supported, otherwise
#'     \code{NA}.}
#'   \item{fisher_info}{The treatment information matrix.}
#'   \item{treatment_cov}{The model-based covariance matrix of treatment
#'     estimates on the estimable treatment-contrast space.}
#'   \item{eigenvalues}{Positive eigenvalues of the treatment information
#'     matrix.}
#'   \item{rank}{Numerical rank of the treatment information matrix.}
#'   \item{assumptions}{List of covariance, effect-size, and testing
#'     assumptions used.}
#' }
#'
#' @details
#' The mathematics has three steps: calculate a comparison's standard error
#' after adjusting for blocks, form \code{delta / SE}, and use a two-sided
#' noncentral t distribution to calculate rejection probability. The error
#' degrees of freedom are the number of observations minus the rank of the
#' treatment-and-block model; users do not need to supply them.
#'
#' This test is justified for normally distributed, independent errors with
#' a common variance, including fixed-block designs. Random intercepts and
#' compound-symmetry components are also supported when their group effects
#' are entirely absorbed by the fixed blocks (or the intercept), leaving
#' independent errors with a common variance. No general mixed-model or
#' spatial power approximation is made. Unsupported covariance structures
#' return \code{NA} power with a warning, while retaining model-based standard
#' errors. These standard errors treat the covariance as supplied, without
#' allowing for its estimation from future data.
#'
#' Bonferroni adjustment and the weakest-comparison summary are cautious
#' choices, not a guarantee against incorrect noise or independence
#' assumptions. Power is conditional on the supplied planning values.
#' The headline is not the probability of detecting every difference.
#' Multiple treatment columns define comparisons between treatment
#' combinations, not tests of main effects or interactions.
#'
#' Overall power is unavailable when any comparison is confounded with blocks
#' or there is insufficient replication to estimate the residual variance.
#' Estimable comparisons remain available in the contrast-level output.
#'
#' @examples
#' trial <- data.frame(treatment = rep(c("A", "B"), each = 12))
#' design_power(trial, delta = 5, sd = 10)
#'
#' rcbd <- expand.grid(treatment = LETTERS[1:3], block = 1:8)
#' design_power(
#'     rcbd,
#'     delta = 5,
#'     sd = 10,
#'     block_col = "block"
#' )
#'
#' @export
design_power <- function(
    design,
    treatment_cols = "treatment",
    block_col = NULL,
    covariance_structure = cov_iid(),
    sigma2 = NULL,
    delta = NULL,
    alpha = 0.05,
    alpha_adjustment = c("bonferroni", "none"),
    sd = NULL
) {
    ## require a meaningful difference and noise scale instead of guessing
    if (is.null(delta)) {
        stop("Supply delta: the smallest treatment difference worth detecting.")
    }
    if (is.null(sd) && is.null(sigma2)) {
        stop("Supply sd: the expected residual standard deviation.")
    }
    if (!is.null(sd) && !is.null(sigma2)) {
        stop("Supply either sd or sigma2, not both.")
    }
    ## accept standard deviation in response units or the legacy variance
    if (!is.null(sd)) {
        validate_positive_number(sd, "sd")
        sigma2 <- sd^2
        if (!is.finite(sigma2) || sigma2 == 0) {
            stop("sd is too large or too small; rescale the response units.")
        }
    }
    validate_positive_number(sigma2, "sigma2")
    validate_positive_number(delta, "delta")
    if (
        !is.numeric(alpha) || is.complex(alpha) || length(alpha) != 1 ||
            !is.finite(alpha) || alpha <= 0 || alpha >= 1
    ) {
        stop("alpha must be a finite number between 0 and 1.")
    }
    ## resolve the default adjustment and reject unknown options
    alpha_adjustment <- match.arg(alpha_adjustment)
    ## keep numerical tuning internal to the user-facing power calculation
    tolerance <- 1e-10

    ti <- compute_treatment_info(
        design = design,
        treatment_cols = treatment_cols,
        block_col = block_col,
        covariance_structure = covariance_structure,
        tolerance = tolerance
    )

    trt_levels <- ti$trt_levels
    v <- length(trt_levels)

    n_comparisons <- choose(v, 2)
    comparison_alpha <- if (alpha_adjustment == "bonferroni") {
        alpha / n_comparisons
    } else {
        alpha
    }

    ## only use a t reference distribution when the simple error model applies
    reference <- power_reference(design, ti, covariance_structure, tolerance)
    df <- reference$df
    if (!is.null(reference$reason)) {
        warning(reference$reason, call. = FALSE)
    }

    # Pairwise treatment stat power
    pairs <- utils::combn(trt_levels, 2, simplify = FALSE)

    out <- lapply(pairs, \(pair) {
        contrast <- numeric(v)
        names(contrast) <- trt_levels
        contrast[pair[1]] <- 1
        contrast[pair[2]] <- -1

        ## check design connectivity independently of covariance
        projected <- as.numeric(contrast %*% ti$estimability)
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

        ## use the covariance factor to avoid cancellation in contrast variances
        relative_var <- sum(crossprod(contrast, ti$variance_factor)^2)
        contrast_var <- sigma2 * relative_var
        ## refuse numerical overflow or underflow instead of reporting certainty
        if (!is.finite(contrast_var) || contrast_var <= 0) {
            stop("Comparison variance is unstable; rescale the response units.")
        }
        se <- sqrt(contrast_var)
        ## effect size that we want to have the power to detect in units of SE
        lambda <- abs(delta / se)

        ## two-sided t-test power under the supported error model
        power <- if (is.na(df)) {
            NA_real_
        } else {
            wald_power(lambda, comparison_alpha, df)
        }

        ## iid-equivalent replication for this contrast
        ## converts contrast variance into number of balanced iid replicates
        ## that would give the same pairwise standard error
        effective_replicates <- 2 / relative_var

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
    ## only summarise designs where every pairwise contrast is estimable
    if (all(estimable) && all(is.finite(contrast_power$power))) {
        estimable_power <- contrast_power$power
        design_power_value <- min(estimable_power)
        average_power_value <- mean(estimable_power)
        worst_comparison <- contrast_power$comparison[
            which.min(estimable_power)
        ]
    } else {
        ## leave overall power undefined for unsupported or disconnected designs
        design_power_value <- NA_real_
        average_power_value <- NA_real_
        worst_comparison <- NA_character_
    }

    ## record the supported test and explain unavailable overall power
    power_note <- reference$reason
    if (is.null(power_note) && !all(estimable)) {
        power_note <- paste(
            "Overall power is unavailable:",
            "some treatment comparisons are confounded with blocks."
        )
    }
    power_method <- if (is.na(df)) {
        "None"
    } else {
        "Exact noncentral-t power under the stated Gaussian error model"
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
            sd = sqrt(sigma2),
            delta = delta,
            alpha = alpha,
            comparison_alpha = comparison_alpha,
            alpha_adjustment = alpha_adjustment,
            covariance_structure = format_covariance_structure(
                covariance_structure
            ),
            block_col = block_col,
            treatment_cols = treatment_cols,
            denom_df = df,
            denom_df_method = reference$method,
            power_method = power_method,
            power_note = power_note
        )
    ), class = c("cbadas_design_power", "list")))
}

#' Calculate the design efficiency/optimality of a small-plot trial design
#'
#' Calculates model-based efficiency for a design using the treatment
#' information matrix induced by a supplied covariance structure.
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
#'   X_2^\intercal
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
#' @param block_col \code{character} vector or \code{NULL}.
#'        Column name of the experiment's blocking factor in \code{design}.
#'        Use \code{NULL} for unblocked designs.
#' @param covariance_structure Covariance structure created with [cov_iid()]
#'        or another covariance helper. Precision is conditional on these
#'        supplied values, without accounting for covariance estimation.
#' @param alpha Unused; retained for compatibility.
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
#'         column_col = "col",
#'         rho_row = 0.1,
#'         rho_col = 0.1
#'     )
#' )
#' }
#'
#' @export
design_efficiency <- function(
    design,
    treatment_cols = "treatment",
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
    pairs <- utils::combn(ti$trt_levels, 2, simplify = FALSE)

    out <- lapply(pairs, \(pair) {
        contrast <- numeric(v)
        names(contrast) <- ti$trt_levels
        contrast[pair[1]] <- 1
        contrast[pair[2]] <- -1

        ## use the same covariance-independent estimability check as power
        projected <- as.numeric(contrast %*% ti$estimability)

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

        ## use the same stable variance calculation as power
        contrast_var <- sum(crossprod(contrast, ti$variance_factor)^2)
        if (!is.finite(contrast_var) || contrast_var <= 0) {
            stop("Comparison variance is outside a safe numerical range.")
        }

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
    available <- estimable & is.finite(contrast_power$power)
    n_estimable <- sum(estimable)
    n_total <- nrow(contrast_power)

    ui_h1("Design power")
    ## headline the weakest comparison, not an average that can hide weak pairs
    ui_text(paste0(
        "Weakest-comparison power: ",
        format_ui_percent(x$design_power)
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
        "Assumptions: difference = ",
        format_ui_number(x$assumptions$delta),
        ", noise SD (base scale) = ",
        format_ui_number(sqrt(x$assumptions$sigma2)),
        ", significance level = ",
        format_ui_number(x$assumptions$alpha),
        " (",
        x$assumptions$alpha_adjustment,
        ")",
        ", covariance = ",
        x$assumptions$covariance_structure
    ))
    ## show limitations even when some individual comparisons are available
    if (!is.null(x$assumptions$power_note)) {
        ui_alert(x$assumptions$power_note)
    }
    ui_text(paste(
        "Conditional on the stated noise and Gaussian error model.",
        "Not the probability of detecting every difference."
    ))

    if (any(available)) {
        ui_h2("Lowest-power comparisons")
        power_table <- contrast_power[available, , drop = FALSE]
        power_table <- power_table[
            order(power_table$power, power_table$se),
            ,
            drop = FALSE
        ]
        power_table <- utils::head(power_table, n)
        print(format_power_table(power_table), row.names = FALSE)
    } else if (n_estimable == 0) {
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
#'   V_{\text{rel}} = R + ZGZ^\intercal
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
            ## validate the wrapper so unsupported covariance is never ignored
            component <- cov_random(component$component, component$sd_ratio)
            Z <- build_group_matrix(design, component$component$group)

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

    ## reject overflow or non-finite covariance entries before inversion
    if (any(!is.finite(V))) {
        stop("Covariance matrix must contain only finite values.")
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

#' Build covariance components for design evaluation
#'
#' Helpers for composing the relative covariance matrix used by
#' [design_power()] and [design_efficiency()].
#'
#' @param group Optional grouping column. For spatial covariance components,
#'   covariance is zero between groups. For [cov_iid()], this is used when
#'   the component is wrapped in [cov_random()].
#'
#' @details
#' Components can be added with \code{+}. For example, an iid residual plus a
#' whole-plot random component can be written as
#' \code{cov_iid() + cov_random("whole_plot", sd_ratio = 1)}.
#'
#' @name covariance_helpers
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
#' @param rho AR1 or within-group correlation parameter in \eqn{[0, 1)}.
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
#' @param range Finite positive distance scale.
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

#' Independent random-intercept covariance component
#'
#' @param x Grouping column name or a [cov_iid()] component with a grouping
#'   column. Other covariance components are not supported.
#' @param sd_ratio Finite non-negative standard deviation ratio relative to the
#'   residual scale. Zero removes this component's contribution to covariance.
#'   General random-effect models provide precision estimates, but power is
#'   only available when the group effects are absorbed by fixed blocks.
#'
#' @rdname covariance_helpers
#' @export
cov_random <- function(x, sd_ratio = 1) {
    if (is.character(x) && length(x) == 1) {
        x <- cov_iid(x)
    }

    ## only independent random intercepts are implemented
    if (!inherits(x, "cov_component") || !identical(x$type, "iid")) {
        stop(
            "cov_random() only supports a grouping column name or ",
            "cov_iid(group); other covariance components are not supported."
        )
    }
    validate_column_name(x$group, "random grouping")

    ## allow zero variance, but reject invalid standard deviation ratios
    if (
        !is.numeric(sd_ratio) || is.complex(sd_ratio) ||
            length(sd_ratio) != 1 || !is.finite(sd_ratio) || sd_ratio < 0
    ) {
        stop("sd_ratio must be a finite non-negative number.")
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
    ## require a non-empty design with unambiguous column selections
    if (!is.data.frame(design) || nrow(design) == 0) {
        stop("design must be a non-empty data frame.")
    }
    if (
        anyNA(names(design)) || any(!nzchar(names(design))) ||
            anyDuplicated(names(design))
    ) {
        stop("design must have unique, non-empty column names.")
    }
    if (
        !is.character(treatment_cols) || length(treatment_cols) == 0 ||
            anyNA(treatment_cols) || any(!nzchar(treatment_cols)) ||
            anyDuplicated(treatment_cols)
    ) {
        stop("treatment_cols must contain unique, non-empty column names.")
    }
    columns <- lapply(
        unname(treatment_cols),
        \(column) get_design_column(design, column, "treatment")
    )

    ## identify combinations using integer codes so labels cannot merge them
    codes <- lapply(columns, \(values) match(values, unique(values)))
    keys <- do.call(paste, c(codes, sep = ":"))
    first <- !duplicated(keys)
    ids <- match(keys, keys[first])

    ## keep readable, unique labels in first-observed treatment order
    labels <- do.call(paste, c(lapply(columns, as.character), sep = ":"))
    labels <- make.unique(labels[first])
    factor(ids, levels = seq_along(labels), labels = labels)
}

## two-sided noncentral-t power at lambda noncentrality
wald_power <- function(lambda, alpha, df) {
    ## an overflowing signal-to-noise ratio has limiting power one
    if (is.infinite(lambda)) return(1)
    crit <- stats::qt(1 - alpha / 2, df)
    stats::pt(-crit, df, ncp = lambda) +
        stats::pt(crit, df, ncp = lambda, lower.tail = FALSE)
}

format_power_table <- function(x) {
    data.frame(
        comparison = x$comparison,
        power = format_ui_percent(x$power),
        standard_error = format_ui_number(x$se),
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

## validate column names before using them to select design variables
validate_column_name <- function(column, label) {
    if (
        !is.character(column) || length(column) != 1 ||
            is.na(column) || !nzchar(column)
    ) {
        stop(sprintf(
            "%s column must be a single non-empty column name.",
            label
        ))
    }
}

get_design_column <- function(design, column, label) {
    validate_column_name(column, label)
    if (!column %in% names(design)) {
        stop(sprintf("%s column '%s' not in design", label, column))
    }

    ## reject missing assignments before factors can drop or relabel rows
    values <- design[[column]]
    if (!is.atomic(values) || !is.null(dim(values)) || is.complex(values)) {
        stop(sprintf("%s column '%s' must be a vector.", label, column))
    }
    if (anyNA(values)) {
        stop(sprintf(
            "%s column '%s' must not contain missing values.",
            label, column
        ))
    }
    ## blank labels are missing assignments too
    if (
        (is.character(values) || is.factor(values)) &&
            any(!nzchar(trimws(as.character(values))))
    ) {
        stop(sprintf(
            "%s column '%s' must not contain empty values.",
            label, column
        ))
    }
    if (is.numeric(values) && any(!is.finite(values))) {
        stop(sprintf(
            "%s column '%s' must contain only finite values.",
            label, column
        ))
    }
    values
}

## build group indicators without parsing column names or requiring two levels
build_group_matrix <- function(design, column, label = "group") {
    group <- factor(get_design_column(design, column, label))
    z <- matrix(0, nrow = nrow(design), ncol = nlevels(group))
    z[cbind(seq_along(group), as.integer(group))] <- 1
    colnames(z) <- levels(group)
    z
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
        is.complex(x) ||
        length(x) != 1 ||
        !is.finite(x) ||
        x < 0 ||
        x >= 1

    if (invalid) {
        stop(sprintf("%s must be in [0, 1).", name))
    }
}

validate_positive_number <- function(x, name) {
    invalid <- !is.numeric(x) ||
        is.complex(x) ||
        length(x) != 1 ||
        !is.finite(x) ||
        x <= 0

    if (invalid) {
        stop(sprintf("%s must be a finite positive number.", name))
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

    ## if blocked, no intercept since block span is confounded with it
    build_group_matrix(design, block, "block")
}

## identify models that reduce to independent errors after block adjustment
power_reference <- function(design, ti, covariance_structure, tolerance) {
    nuisance <- qr(ti$X2, tol = tolerance)
    supported <- vapply(as_cov_components(covariance_structure), \(component) {
        if (component$type == "iid") return(TRUE)
        if (component$type == "random" && component$sd_ratio == 0) return(TRUE)

        ## independent components need no extra variance-component approximation
        component_cov <- build_covariance_matrix(design, component)
        if (all(component_cov == diag(component_cov[1, 1], nrow(design)))) {
            return(TRUE)
        }

        ## group intercepts disappear when already spanned by the fixed blocks
        group <- switch(
            component$type,
            random = component$component$group,
            cs = component$group_col,
            NULL
        )
        if (is.null(group)) return(FALSE)
        z <- build_group_matrix(design, group)
        max(abs(qr.resid(nuisance, z))) <= tolerance
    }, logical(1))

    ## do not guess degrees of freedom for general mixed or spatial models
    if (!all(supported)) {
        return(list(
            df = NA_real_,
            method = "unavailable for this covariance structure",
            reason = paste(
                "Power unavailable: this covariance structure requires a",
                "mixed-model or spatial test outside the simple supported",
                "method. Standard errors are conditional on the supplied",
                "covariance; they do not account for estimating it."
            )
        ))
    }
    if (ti$resid_df <= 0) {
        return(list(
            df = NA_real_,
            method = "unavailable without residual replication",
            reason = "Insufficient independent replication to estimate power."
        ))
    }

    ## use the residual degrees of freedom of the ordinary linear-model test
    list(df = ti$resid_df, method = "fixed-effect residual df", reason = NULL)
}

compute_treatment_info <- function(
    design,
    treatment_cols = c("treatment"),
    block_col = NULL,
    covariance_structure = cov_iid(),
    tolerance = 1e-10
) {
    ## validate numerical tolerance for both power and efficiency calculations
    validate_positive_number(tolerance, "tolerance")
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

    ## determine estimable contrasts before covariance can affect numerical rank
    raw_residual <- qr.resid(qr(x2, tol = tolerance), x1)
    structural <- svd(raw_residual, nu = 0)
    cutoff <- tolerance * max(dim(x1)) * sqrt(max(colSums(x1^2)))
    basis <- structural$v[, structural$d > cutoff, drop = FALSE]
    rank <- ncol(basis)
    estimability <- tcrossprod(basis)

    ## whiten with a Cholesky factor instead of explicitly inverting covariance
    V <- build_covariance_matrix(
        design = design,
        covariance_structure = covariance_structure
    )
    scale <- max(diag(V))
    root <- tryCatch(chol(V / scale), error = function(e) NULL)
    if (is.null(root) || any(!is.finite(root))) {
        stop(
            "Covariance must be positive definite; check duplicated ",
            "coordinates and covariance components."
        )
    }
    ## reject nearly singular covariance before calculating precision
    if (rcond(root) <= sqrt(tolerance)) {
        stop("Covariance is too ill-conditioned to evaluate safely.")
    }

    ## zero structural rank stays zero even when covariance arithmetic is noisy
    info <- info_inv <- matrix(0, ncol(x1), ncol(x1))
    variance_factor <- matrix(0, ncol(x1), 0)
    pos_eig <- numeric(0)
    if (rank > 0) {
        whitened_blocks <- forwardsolve(t(root), x2)
        whitened_treatments <- forwardsolve(t(root), x1 %*% basis)
        block_qr <- qr(whitened_blocks, tol = tolerance)
        if (block_qr$rank != ncol(x2)) {
            stop("Block adjustment is too ill-conditioned to evaluate safely.")
        }
        ## remove blocks without subtracting nearly equal information matrices
        adjusted <- qr.resid(block_qr, whitened_treatments)
        decomposition <- svd(adjusted, nu = 0)
        values <- decomposition$d
        if (any(values <= tolerance * max(dim(adjusted)) * max(values))) {
            stop(
                "Treatment information is too ill-conditioned ",
                "to evaluate safely."
            )
        }

        ## invert only the structurally estimable treatment space
        vectors <- basis %*% decomposition$v
        pos_eig <- (values / sqrt(scale))^2
        if (any(!is.finite(pos_eig)) || any(pos_eig <= 0)) {
            stop("Treatment information is outside a safe numerical range.")
        }
        variance_factor <- sweep(vectors, 2, 1 / sqrt(pos_eig), "*")
        info <- tcrossprod(sweep(vectors, 2, sqrt(pos_eig), "*"))
        info_inv <- tcrossprod(variance_factor)
        if (any(!is.finite(info_inv))) {
            stop("Treatment information is outside a safe numerical range.")
        }
    }
    dimnames(info) <- dimnames(info_inv) <- list(trt_levels, trt_levels)

    return(list(
        info = info,
        info_inv = info_inv,
        variance_factor = variance_factor,
        estimability = estimability,
        V = V,
        X1 = x1,
        X2 = x2,
        resid_df = nrow(x1) - ncol(x2) - rank,
        trt_levels = trt_levels,
        eigenvalues = sort(pos_eig, decreasing = TRUE),
        rank = rank
    ))
}
