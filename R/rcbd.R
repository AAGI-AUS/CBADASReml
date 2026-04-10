#' Generate an Optimal Randomised Complete Block Design
#'
#' @param n_treatments Number of treatments.
#' @param n_blocks Number of complete blocks.
#' @param block_size Defaults to n_treatments (complete blocks / RCBD).
#' @param lambda An n x n covariance matrix, where n = n_blocks * block_size. Defaults to the identity.
#' @param block_map Optional block map. Defaults to assuming plots are ordered
#'     sequentially. I.e. block1 gets indices 1:k, block2 gets (k+1):(2k), etc.
#' @param criterion Character, either A or D.
#' @param n_starts Number of random restarts for the interchange algorithm (only for incomplete blocks).
#' @param max_iter Maximum interchange sweeps per restart.
#' @param seed Optional integer seed for reproducibility.
#' @param trt_labels Character vector of treatment names (length n_treatments). Defaults to T1, T2, etc.
#' @param block_labels  Character vector of block names (length n_blocks). Defaults to B1, B2, etc.
#' @param verbose Boolean. Prints progress.
#'
#' @return An S3 object of class \code{"optimal_rcbd"} containing:
#'     \item{design}{Data frame with columns Block, Plot, Treatment.}
#'     \item{design_matrix}{Integer matrix (blocks x plots) of treatment
#'          indices.}
#'     \item{incidence_matrix}{N matrix (treatments x blocks).}
#'     \item{info_matrix}{Nuisance adjusted information matrix C.}
#'     \item{criterion}{Criterion used ("A" or "D").}
#'     \item{criterion_value}{Achieved criterion value (lower is better).}
#'     \item{eigenvalues}{Non-zero eigenvalues of C (descending).}
#'     \item{efficiency_pct}{Design efficiency relative to theoretical optimum.}
#'     \item{replications}{Named vector of per-treatment replication counts.}
#'     \item{params}{List of input parameters for reference.}
#'
#' @examples
#' des <- generate_optimal_rcbd(
#'     n_treatments = 5,
#'     n_blocks = 4,
#'     criterion = "A"
#' )
#' print(des)
#' plot(des)
#'
generate_rcbd <- function(
    n_treatments,
    n_blocks,
    block_size = n_treatments,
    lambda = NULL,
    block_map = NULL,
    criterion = c("A", "D"),
    n_starts = 10L,
    max_iter = 100L,
    seed = NULL,
    trt_labels = NULL,
    block_labels = NULL,
    verbose = FALSE
) {
    # a million arg checks!!!!!!!
    criterion <- match.arg(criterion)
    if (!is.null(seed)) set.seed(seed)

    if (n_treatments %% 1 != 0) stop("n_treatments must be a whole number.")
    if (n_blocks %% 1 != 0) stop("n_blocks must be a whole number.")
    if (block_size %% 1 != 0) stop("block_size must be a whole number.")
    if (n_starts %% 1 != 0 || n_starts < 1) stop("n_starts must be a positive integer.")
    if (max_iter %% 1 != 0 || max_iter < 1) stop("max_iter must be a positive integer.")

    v <- as.integer(n_treatments)
    b <- as.integer(n_blocks)
    k <- as.integer(block_size)
    is_complete <- (k == v)

    if (is.null(trt_labels)) trt_labels <- paste0("T", seq_len(v))
    if (is.null(block_labels)) block_labels <- paste0("B", seq_len(b))
    stopifnot(length(trt_labels) == v, length(block_labels) == b)

    if (length(unique(trt_labels)) != length(trt_labels)) stop("trt_labels must be unique.")
    if (length(unique(block_labels)) != length(block_labels)) stop("block_labels must be unique.")
    if (any(!nzchar(trt_labels))) stop("trt_labels must be non-empty strings.")
    if (any(!nzchar(block_labels))) stop("block_labels must be non-empty strings.")

    n_plots <- b * k
    if (is.null(lambda)) {
        # identity
        has_spatial <- FALSE
        lambda <- diag(n_plots)
    } else {
        has_spatial <- TRUE
        stopifnot(
            is.matrix(lambda),
            nrow(lambda) == n_plots,
            ncol(lambda) == n_plots,
            isSymmetric(lambda, tol = 1e-8)
        )
    }

    if (is.null(block_map)) {
        # sequential
        block_map <- matrix(seq_len(n_plots), nrow = b, ncol = k, byrow = TRUE)
    } else {
        stopifnot(
            is.matrix(block_map),
            nrow(block_map) == b,
            ncol(block_map) == k,
            setequal(as.vector(block_map), seq_len(n_plots))
        )
    }


    # helper functions

    # incidence matrix: takes the design matrix and returns the number of times
    # treatment i appears in block j.
    incidence <- function(dm) {
        N <- matrix(0L, v, b)
        for (j in seq_len(b)) {
            for (i in seq_len(k)) {
                N[dm[j, i], j] <- N[dm[j, i], j] + 1L
            }
        }
        return(N)
    }

    # adjusted fisher information: tells us how much information we have left
    # to estimate treatment contrasts AFTER nuking the block effect
    info_mat <- function(N) {
        r_vec <- rowSums(N)
        R <- diag(as.numeric(r_vec), nrow = v)
        k_vec <- colSums(N)
        K_inv <- diag(1 / k_vec, nrow = b)
        return(R - N %*% K_inv %*% t(N))
    }

    # get positive eigenvalues from a SYMMETRIC matrix
    pos_eig <- function(C) {
        ev <- eigen(C, symmetric = TRUE, only.values = TRUE)$values
        return(ev[ev > 1e-10])
    }

    # criterion value to MINIMISE
    crit <- function(dm) {
        if (has_spatial) {
            C <- spatial_info(dm, v, block_map, mat_L, n_plots)
        } else {
            C <- info_mat(incidence(dm))
        }
        pe <- pos_eig(C)
        if (length(pe) < v - 1L) return(Inf) # design is disconnected
        return(switch(
            criterion,
            A = sum(1 / pe),
            D = -sum(log(pe))
        ))
    }

    # random design: each block gets k distinct treatments
    rand_design <- function() {
        dm <- matrix(0L, b, k)
        for (j in seq_len(b)) dm[j, ] <- sample.int(v, k)
        return(dm)
    }

    # projection matrix
    ## L projects away nuisance fixed effects (intercept + blocks) from the GLS
    ## precision. This only depends on lambda and the block structure, so
    ## precompute this mfer
    build_L <- function(lambda_inv, bmap, b_count, n) {
        X2 <- matrix(0, n, b_count)
        for (j in seq_len(b_count)) {
            X2[bmap[j, ], j] <- 1 # block indicators
        }
        LiX2 <- lambda_inv %*% X2
        lambda_inv - LiX2 %*% solve(crossprod(X2, LiX2)) %*% t(LiX2)
    }

    # information matrix but more spatial
    spatial_info <- function(dm, v, bmap, mat_L, n) {
        X1 <- matrix(0, n, v)
        for (j in seq_len(nrow(dm))) {
            for (i in seq_len(ncol(dm))) {
                X1[bmap[j, i], dm[j, i]] <- 1
            }
        }
        crossprod(X1, mat_L) %*% X1
    }


    # pairwise interchange algorithm
    ## for each block we swapping every within-block treatment with every
    ## without-block treatment. choose the swap that improves the most. then
    ## repeat until a full sweep produces no improvement or max_iter

    optimise <- function(dm) {
        val <- crit(dm)

        # control max iter
        for (iter in seq_len(max_iter)) {
            improved <- FALSE

            # for each block
            for (j in sample.int(b)) {
                if (has_spatial && is_complete) {
                    # spatial optimiser
                    ## swap positions within block
                    # For two positions in a block
                    for (p1 in seq_len(k - 1L)) {
                        for (p2 in (p1 + 1L):k) {
                            # swap them and eval the criterion
                            dm[j, c(p1, p2)] <- dm[j, c(p2, p1)]
                            trial <- crit(dm)
                            if (trial < val - 1e-12) {
                                val <- trial
                                improved <- TRUE
                            } else {
                                # swap back if not improved
                                dm[j, c(p1, p2)] <- dm[j, c(p2, p1)]
                            }
                        }
                    }
                } else {
                    in_blk <- dm[j, ]
                    out_blk <- setdiff(seq_len(v), in_blk)
                    # skip if block is complete
                    if (length(out_blk) == 0L) next

                    # for each eu within block j
                    for (pos in seq_len(k)) {
                        old_trt <- dm[j, pos]
                        best_new <- NA_integer_

                        # for each cand outside of the block
                        for (cand in out_blk) {
                            # test the candidate
                            dm[j, pos] <- cand
                            trial <- crit(dm)
                            if (trial < val - 1e-12) {
                                val <- trial
                                best_new <- cand
                                improved <- TRUE
                            }
                            # replace after testing
                            dm[j, pos] <- old_trt
                        }

                        if (!is.na(best_new)) {
                            dm[j, pos] <- best_new
                            out_blk <- setdiff(seq_len(v), dm[j, ])
                        }
                    }
                }
            }
            if (!improved) break
        }

        list(dm = dm, val = val, iters = iter)
    }


    # precompute spatial structure
    lambda_inv <- solve(lambda)
    mat_L <- build_L(lambda_inv, block_map, b, n_plots)


    # search the design space for best dessy
    if (is_complete && !has_spatial) {
        # every valid rcbd has the same information matrix, so a single random
        # permutation within each block is already optimal
        dm <- matrix(0L, b, k)
        for (j in seq_len(b)) dm[j, ] <- sample.int(v)
        N <- incidence(dm)
        val <- crit(dm)
        best <- list(dm = dm, val = val, N = N, iters = 0L)
        if (verbose) {
            message("Every valid RCBD is equally ", criterion, "-optimal.")
        }
    } else {
        # if not complete
        best <- NULL
        for (s in seq_len(n_starts)) {
            res <- optimise(rand_design())
            if (verbose) {
                message(sprintf(
                    "  Start %d/%d  %s = %.6f  (%d iters)",
                    s,
                    n_starts,
                    criterion,
                    res$val,
                    res$iters
                ))
            }
            if (is.null(best) || res$val < best$val) best <- res
        }
    }


    # efficiency criteria
    best$N <- incidence(best$dm)
    if (has_spatial) {
        C <- spatial_info(best$dm, v, block_map, mat_L, n_plots)
    } else {
        C <- info_mat(best$N)
    }
    pe <- pos_eig(C)
    r_vec <- rowSums(best$N)
    r_bar <- mean(r_vec)

    # A = trace(C^-) = (v-1)^2 * k  / (r * v * (k-1))
    # D = log det(C) = (v-1) * log(alpha)
    alpha_opt <- r_bar * v * (k - 1) / (k * (v - 1))

    if (is_complete && !has_spatial) {
        # since complete designs are equally efficient
        eff_pct <- 100
    } else if (has_spatial) {
        eff_pct <- NA_real_
    } else {
        if (criterion == "A") {
            opt_trace <- (v - 1) / alpha_opt
            achieved <- sum(1 / pe)
            eff_pct <- 100 * opt_trace / achieved
        } else {
            opt_log_det <- (v - 1) * log(alpha_opt)
            ach_log_det <- sum(log(pe))
            eff_pct <- 100 * exp((ach_log_det - opt_log_det) / (v - 1))
        }
    }


    # make design df
    design_df <- data.frame(
        Block = factor(rep(block_labels, each = k), levels = block_labels),
        Plot = rep(seq_len(k), times = b),
        Treatment = factor(trt_labels[as.vector(t(best$dm))], levels = trt_labels),
        stringsAsFactors = FALSE
    )


    # fancy S3 output
    out <- list(
        design = design_df,
        design_matrix = best$dm,
        incidence_matrix = best$N,
        info_matrix = C,
        criterion = criterion,
        criterion_value = best$val,
        eigenvalues = sort(pe, decreasing = TRUE),
        efficiency_pct = eff_pct,
        replications = setNames(r_vec, trt_labels),
        params = list(
            n_treatments = v,
            n_blocks = b,
            block_size = k,
            has_spatial = has_spatial,
            lambda = if (has_spatial) lambda else NULL,
            block_map = block_map,
            is_complete = is_complete,
            seed = seed,
            n_starts = n_starts,
            max_iter = max_iter,
            trt_labels = trt_labels,
            block_labels = block_labels
        )
    )
    class(out) <- "optimal_rcbd"
    out
}

# methods

# made some ai generate this, too lazy to do it myself lel
plot.optimal_rcbd <- function(x, ...) {
    p  <- x$params
    dm <- x$design_matrix
    v  <- p$n_treatments;  b <- p$n_blocks;  k <- p$block_size

    pal <- if (v <= 12) {
        c(
            "#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14F", "#EDC948",
            "#B07AA1", "#FF9DA7", "#9C755F", "#BAB0AC", "#D37295", "#FABFD2"
        )[seq_len(v)]
    } else {
        hcl.colors(v, palette = "Set2")
    }

    old_par <- par(mar = c(4, 6, 3.5, 9), xpd = TRUE)
    on.exit(par(old_par))

    plot(NULL,
        xlim = c(0.5, k + 0.5), ylim = c(0.5, b + 0.5),
        xlab = "Plot within block", ylab = "",
        main = sprintf("RCBD Field Layout  (%s-optimal,  eff = %.1f%%)",
            x$criterion, x$efficiency_pct),
        axes = FALSE, asp = 1)

    axis(1, at = seq_len(k), cex.axis = 0.9)
    axis(2, at = seq_len(b), labels = p$block_labels,
        las = 1, tick = FALSE, cex.axis = 0.9)

    for (j in seq_len(b)) {
        for (i in seq_len(k)) {
            trt <- dm[j, i]
            rect(i - 0.46, j - 0.42, i + 0.46, j + 0.42,
                col = pal[trt], border = "grey30", lwd = 0.7)
            text(i, j, p$trt_labels[trt], cex = max(0.55, 1 - 0.03 * v), font = 2)
        }
    }

    legend("right", inset = c(-0.22, 0),
        legend = p$trt_labels, fill = pal,
        title = "Treatment", cex = 0.75, bty = "n")
}

# convenient covariance structure makers
cor_ar1 <- function(n, rho) {
    rho^abs(outer(seq_len(n), seq_len(n), "-"))
}

cor_ar1_ar1 <- function(n_rows, n_cols, rho_row, rho_col) {
    kronecker(
        cor_ar1(n_rows, rho_row),
        cor_ar1(n_cols, rho_col)
    )
}
