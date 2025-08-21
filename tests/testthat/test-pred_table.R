skip_on_cran()
library(asreml)
library(glmmTMB)

test_that("we get expected pred table for simple data with ASReml", {
    mod1 <- asreml(
        fixed = yield ~ Variety + Nitrogen + Variety:Nitrogen,
        random = ~ idv(Blocks) + idv(Blocks):idv(Wplots),
        residual = ~ idv(units),
        data = oats,
        trace = FALSE
    )
    p_tab <- data.frame(
        Treatment = c("Golden_rain", "Marvellous", "Victory"),
        Mean = c(104.5000, 109.7917, 97.6250),
        `Standard Error` = c(7.7975, 7.7975, 7.7975),
        `Lower CL` = c(87.1260, 92.4177, 80.2510),
        `Upper CL` = c(121.8740, 127.1657, 114.9990)
    )
    pt <- pred_table(mod1, classify = "Variety")
    pt[, 2] <- round(pt[, 2], 4)
    pt[, 3] <- round(pt[, 3], 4)
    pt[, 4] <- round(pt[, 4], 4)
    pt[, 5] <- round(pt[, 5], 4)

    expect_true(all(pt == p_tab))
})

test_that("we get expected pred table for simple data with glmmTMB", {
    mod2 <- glmmTMB(
        count ~ spp * mined + (1 | site),
        zi = ~ spp * mined,
        data = Salamanders,
        family = nbinom2
    )
    p_tab <- data.frame(
        Treatment = c("GP", "PR", "DM", "EC-A", "EC-L", "DES-L", "DF"),
        Mean = c(1.1604, 0.7013, 1.6350, 0.7079, 1.0028, 2.1342, 1.3426),
        `Standard Error` = c(
            0.8044,
            0.3745,
            0.4390,
            0.4248,
            0.2000,
            0.5614,
            0.3418
        ),
        `Lower CL` = c(0.2982, 0.2463, 0.9660, 0.2184, 0.6783, 1.2744, 0.8151),
        `Upper CL` = c(4.5151, 1.9972, 2.7673, 2.2951, 1.4824, 3.5740, 2.2115)
    )
    pt <- pred_table(mod2, classify = "spp")
    pt[, 2] <- round(pt[, 2], 4)
    pt[, 3] <- round(pt[, 3], 4)
    pt[, 4] <- round(pt[, 4], 4)
    pt[, 5] <- round(pt[, 5], 4)

    expect_true(all(pt == p_tab))
})
