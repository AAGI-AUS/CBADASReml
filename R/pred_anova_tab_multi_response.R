pred_anova_tab_multi_response <- function(
    ...,
    classify,
    all_pval = T,
    tabcaption = NULL
) {
    # Number of factors
    n <- as.numeric(length(strsplit(classify, "[*]")[[1]]))

    models <- list(...)
    pval_table <- rownames(asreml::wald(models[[1]]))
    for (i in models) {
        response <- round(asreml::wald(i)[, 4], 3)
        pval_table <- cbind(pval_table, response)
    }
    pval_table <- as.data.frame(pval_table)
    pval_table <- pval_table[, -1]

    print(pval_table)

    k <- nrow(pval_table)
    print(k)
    empty <- rep(" ", k)

    pval_table <- cbind(rownames(pval_table), pval_table)

    for (i in 1:(n - 1)) {
        pval_table <- cbind(empty, pval_table)
    }

    testo <- models[[1]]

    pred_table <- asreml::predict.asreml(
        object = testo,
        classify = classify,
        sed = TRUE
    )$pvals[, 1:n]

    avsed.vec <- rep(NA, c(length(models) + 4))

    temp_num <- 4

    for (i in models) {
        response <- rownames(attr(i$formulae$fixed, "factors"))[1]
        temp_colnames <- colnames(pred_table)
        predtemp <- asreml::predict.asreml(i, classify = classify, sed = TRUE)
        prediction <- round(predtemp$pvals[, n + 1], 2) # predtemp$pvals[,n+1] #
        avsed.vec[temp_num] <- as.numeric(predtemp$avsed[2])
        pred_table <- cbind(pred_table, prediction)
        colnames(pred_table) <- append(temp_colnames, response)
        temp_num <- temp_num + 1
    }

    a <- colnames(pred_table)
    colnames(pval_table) <- a

    pred_table <- stats::na.omit(pred_table)
    pval_table <- stats::na.omit(pval_table)

    print(avsed.vec)
    print(pred_table)

    pred_table <- rbind(pred_table, round(avsed.vec, 3))

    print(pred_table)

    joint_table <- rbind(pred_table, pval_table)

    print(xtable(joint_table, include.rownames = F))
}
