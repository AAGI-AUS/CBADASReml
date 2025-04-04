#' A function for Predictions, SE, AVSED and possibly LSD
#'
#' This function needs the library asreml, tidyr and dplyr
#' @param model, an ASReml model.
#' @param classify, A string specifying the variables to consider.
#' @param LSD logical. Do you include the mean LSD value?
#' @param sed logical. Do you include the sed matrix?
#' @keywords Predictions
#' @import dplyr tidyr
#' @examples
#' \dontrun{pred_se_avsed_lsd(model = mod1, classify = "Nitrogen", LSD = T)}
pred_se_avsed_lsd <- function(model, classify, LSD = FALSE, sed = F) {
    prediction_table <- asreml::predict.asreml(
        model,
        classify = classify,
        sed = sed
    )

    prediction_table$pvals <- prediction_table$pvals %>%
        filter(!is.na(predicted.value)) %>%
        select(-status)

    # prediction_table$pvals <- prediction_table$pvals
    pvals <- prediction_table$pvals
    if (dim(pvals)[1] == 0) return(NULL)
    pvals[nrow(pvals) + 1, ] <- NA

    if (sed) avsed <- prediction_table$avsed["mean"] else
        avsed <- prediction_table$avsed["overall"]

    # Adding AvSED to the data.frame
    # avsed <- predict.asreml(model, classify = classify)$avsed

    name1 <- colnames(pvals)[1]
    name2 <- colnames(pvals)[2]

    av_sed_df <- data.frame(
        placeholder1 = "Average SED:",
        placeholder2 = as.character(avsed)
    )

    colnames(av_sed_df)[1] <- name1
    colnames(av_sed_df)[2] <- name2

    pvals[, 2] <- as.character(pvals[, 2])

    combined_table_sorted <- bind_rows(pvals, av_sed_df)

    # Adding LSD to the data.frame
    if (LSD) {
        pred <- invisible(asremlPlus::predictPlus.asreml(
            model,
            classify,
            wald.tab = as.data.frame(asreml::wald(model))
        ))

        LSD <- invisible(pred$LSD$meanLSD)

        LSD_df <- data.frame(
            placeholder1 = "Average LSD:",
            placeholder2 = as.character(LSD)
        )

        colnames(LSD_df)[1] <- name1
        colnames(LSD_df)[2] <- name2

        combined_table_sorted <- bind_rows(combined_table_sorted, LSD_df)
    }

    combined_table_sorted$predicted.value <- as.numeric(
        combined_table_sorted$predicted.value
    )
    combined_table_sorted$std.error <- as.numeric(
        combined_table_sorted$std.error
    )

    return(combined_table_sorted)
}
