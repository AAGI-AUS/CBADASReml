#' A Standard Residual to detect outliers using aom
#'
#' This function outputs the any potential outliers in a asreml model
#' @param model, an asreml model object
#' @param threshold, threshold at which the standard residual is cut to default 3.5
#' @param printThres, The number of closer to threshold values to print default 2, max length of data
#' @keywords Standard Residuals
#' @export
#' @examples
#' \dontrun{
#' outlier_detect(mod1, printThres = 5)
#' }
outlier_detect <- function(model, threshold = 3.5, printThres = 2) {
    temp.datframe <- as.data.frame(model$mf)

    if (!any(names(model) == "aom")) {
        print("No aom = T, Updating Model")
        update.model <- asreml::update.asreml(model, aom = T)
        update.model <- asreml::update.asreml(model, aom = T)

        temp.stdres <- update.model$aom$R[, 2]
    } else {
        temp.stdres <- model$aom$R[, 2]
    }

    temp.stdres <- as.data.frame(
        cbind(as.numeric(names(temp.stdres)), temp.stdres)
    )

    temp.datframe$StdRes <-
        temp.stdres$temp.stdres[match(as.numeric(temp.datframe$units), temp.stdres$V1)]

    temp.datframe <- temp.datframe[!is.na(as.numeric(temp.datframe$StdRes)), ]

    if (any(abs(temp.datframe$StdRes) > threshold)) {
        print(paste0(
            "Outliers found: ",
            sum(as.numeric(abs(temp.datframe$StdRes) > threshold))
        ))
        print(temp.datframe[which(abs(temp.datframe$StdRes) > threshold), ])
    } else {
        print("No outliers.")
        order.temp.datframe <-
            temp.datframe[rev(order(abs(temp.datframe$StdRes))), ]
        if (printThres > nrow(order.temp.datframe)) {
            print("Invalid printThres value. Defaulting to 2")
            printThres <- 2
        }

        print(order.temp.datframe[c(1:printThres), ])
    }
}
