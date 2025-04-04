#' An Updater Function for Asreml
#'
#' This function allows you to quickly update an Asreml model multiple times. It will stop if it hasn't converged after niter iterations.
#' @param model an ASReml Model.
#' @param niter Number of iterations to run before stopping.
#' @keywords Outlier
#' @export
#' @examples
#' \dontrun{
#' model <- updater(model)
#' }
#'
updater <- function(model, niter = 10) {
    runs <- 0

    while (
        class(tryCatch(
            {
                model <- asreml::update.asreml(model)
            },
            warning = function(w) {
                1
            }
        )) !=
            "asreml"
    ) {
        runs <- runs + 1

        model <- asreml::update.asreml(model)

        if (runs == niter) {
            break
        }
    }

    return(model)
}
