skip_on_cran()
set.seed(123)
library(asreml)
test_data <- oats
test_data["yield2"] <- oats["yield"] * runif(nrow(oats["yield"]))
test_data["yield3"] <- oats["yield"] * runif(nrow(oats["yield"])) * 2

mod1 <- asreml(
    fixed = yield ~ Variety + Nitrogen + Variety:Nitrogen,
    random = ~ idv(Blocks) + idv(Blocks):idv(Wplots),
    residual = ~ idv(units),
    data = test_data,
    trace = FALSE
)
mod2 <- asreml(
    fixed = yield2 ~ Variety + Nitrogen + Variety:Nitrogen,
    random = ~ idv(Blocks) + idv(Blocks):idv(Wplots),
    residual = ~ idv(units),
    data = test_data,
    trace = FALSE
)
mod3 <- asreml(
    fixed = yield3 ~ Variety + Nitrogen + Variety:Nitrogen,
    random = ~ idv(Blocks) + idv(Blocks):idv(Wplots),
    residual = ~ idv(units),
    data = test_data,
    trace = FALSE
)
out <- data.frame(
    Effect = c("Variety", "Nitrogen", "Variety:Nitrogen"),
    yield = c(0.226, 0.000, 0.936)
)

## Tests
test_that("appropriate errors are given for incorrect parameters given", {
    expect_error(
        anova_table(),
        "^no applicable method.*$"
    )
    expect_error(
        anova_table("spaghetti"),
        "^no applicable method.*$"
    )

    ## stop(paste0("n_digits must be numeric: ", n_digits))
    expect_error(
        anova_table(mod1, n_digits = "rigatoni"),
        "^n_digits.*rigatoni$"
    )
})

test_that("one model works", {
    expect_identical(anova_table(mod1), out)
})
test_that("two models work", {
    out2 <- cbind(out, yield2 = c(0.059, 0.004, 0.562))
    expect_identical(anova_table(mod1, mod2), out2)
})
test_that("two+ models work", {
    out3 <- cbind(
        out,
        yield2 = c(0.059, 0.004, 0.562),
        yield3 = c(0.978, 0.000, 0.171)
    )
    expect_identical(anova_table(mod1), out)
})

test_that("n_digits returns correct digits", {
    real_num <- anova_table(mod1, n_digits = 10)$yield
    test_num <- anova_table(mod1)$yield
    expect_true(all(test_num == round(real_num, 3)))

    test_nd <- 5
    test_num <- anova_table(mod1, n_digits = test_nd)$yield
    expect_true(all(test_num == round(real_num, test_nd)))
})
