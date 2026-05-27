skip_if_no_asreml <- function() {
    testthat::skip_if_not(
        requireNamespace("asreml", quietly = TRUE),
        "asreml is not installed"
    )
}

.oats_model_cache <- NULL
get_oats_model <- function() {
    if (!requireNamespace("asreml", quietly = TRUE)) {
        testthat::skip("asreml is not installed")
    }
    if (is.null(.oats_model_cache)) {
        .oats_model_cache <<- asreml::asreml(
            fixed = yield ~ Variety + Nitrogen + Variety:Nitrogen,
            random = ~ asreml::idv(Blocks) +
                asreml::idv(Blocks):asreml::idv(Wplots),
            residual = ~ asreml::idv(units),
            data = asreml::oats,
            trace = FALSE
        )
    }
    .oats_model_cache
}

round_pred_table <- function(pt, digits = 4) {
    pt[, 2:5] <- lapply(pt[, 2:5], round, digits = digits)
    pt
}
