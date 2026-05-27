test_that("pred_table returns expected output for asreml model", {
    skip_if_no_asreml()
    p_tab <- data.frame(
        Treatment = c("Golden_rain", "Marvellous", "Victory"),
        Mean = c(104.5000, 109.7917, 97.6250),
        Standard.Error = c(7.7975, 7.7975, 7.7975),
        Lower.CL = c(87.1260, 92.4177, 80.2510),
        Upper.CL = c(121.8740, 127.1657, 114.9990)
    )
    pt <- round_pred_table(pred_table(get_oats_model(), classify = "Variety"))
    expect_equal(pt, p_tab)
})

test_that("pred_table returns expected output for glmmTMB model", {
    skip_if_not_installed("glmmTMB")
    p_tab <- data.frame(
        Treatment = factor(
            c("GP", "PR", "DM", "EC-A", "EC-L", "DES-L", "DF"),
            levels = c("GP", "PR", "DM", "EC-A", "EC-L", "DES-L", "DF")
        ),
        Mean = c(1.1739, 0.2935, 1.4783, 0.5435, 2.1848, 2.3152, 1.2717),
        `Standard Error` = c(
            0.2240,
            0.0744,
            0.2746,
            0.1180,
            0.3917,
            0.4132,
            0.2403
        ),
        `Lower CL` = c(0.8076, 0.1786, 1.0271, 0.3551, 1.5375, 1.6318, 0.8781),
        `Upper CL` = c(1.7064, 0.4822, 2.1276, 0.8319, 3.1046, 3.2849, 1.8418),
        check.names = FALSE
    )

    mod <- glmmTMB::glmmTMB(
        count ~ spp,
        data = glmmTMB::Salamanders,
        family = glmmTMB::nbinom2
    )
    pt <- round_pred_table(pred_table(mod, classify = "spp"))
    expect_equal(pt, p_tab)
})
