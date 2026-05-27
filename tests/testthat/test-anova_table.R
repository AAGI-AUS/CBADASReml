# Shared setup at top level
skip_if_no_asreml()
set.seed(123)

test_data <- asreml::oats
test_data$yield2 <- asreml::oats$yield * runif(nrow(asreml::oats))
test_data$yield3 <- asreml::oats$yield * runif(nrow(asreml::oats)) * 2

mod1 <- asreml::asreml(
    fixed = yield ~ Variety + Nitrogen + Variety:Nitrogen,
    random = ~ idv(Blocks) + idv(Blocks):idv(Wplots),
    residual = ~ idv(units),
    data = test_data,
    trace = FALSE
)
mod2 <- asreml::asreml(
    fixed = yield2 ~ Variety + Nitrogen + Variety:Nitrogen,
    random = ~ idv(Blocks) + idv(Blocks):idv(Wplots),
    residual = ~ idv(units),
    data = test_data,
    trace = FALSE
)
mod3 <- asreml::asreml(
    fixed = yield3 ~ Variety + Nitrogen + Variety:Nitrogen,
    random = ~ idv(Blocks) + idv(Blocks):idv(Wplots),
    residual = ~ idv(units),
    data = test_data,
    trace = FALSE
)

out <- data.frame(
    Effect = c("Variety", "Nitrogen", "Variety:Nitrogen"),
    yield = c(0.226, 0.000, 0.936),
    stringsAsFactors = FALSE
)

test_that("appropriate errors for incorrect parameters", {
    skip_if_no_asreml()
    expect_error(anova_table(), "^no applicable method.*$")
    expect_error(anova_table("spaghetti"), "^no applicable method.*$")
    expect_error(
        anova_table(mod1, n_digits = "rigatoni"),
        "^n_digits.*rigatoni$"
    )
})

test_that("one model works", {
    skip_if_no_asreml()
    expect_identical(anova_table(mod1), out)
})

test_that("two models work", {
    skip_if_no_asreml()
    out2 <- cbind(out, yield2 = c(0.059, 0.004, 0.562))
    expect_identical(anova_table(mod1, mod2), out2)
})

test_that("two+ models work", {
    skip_if_no_asreml()
    out3 <- cbind(
        out,
        yield2 = c(0.059, 0.004, 0.562),
        yield3 = c(0.978, 0.000, 0.171)
    )
    expect_identical(anova_table(mod1, mod2, mod3), out3)
})

test_that("n_digits returns correct digits", {
    skip_if_no_asreml()
    real_num <- anova_table(mod1, n_digits = 10)$yield
    test_num <- anova_table(mod1)$yield
    expect_true(all(test_num == round(real_num, 3)))

    test_nd <- 5
    test_num <- anova_table(mod1, n_digits = test_nd)$yield
    expect_true(all(test_num == round(real_num, test_nd)))
})
