#' A variogram graph making function
#'
#' This function outputs (and saves) variogram with scaled labels
#' @param asreml.mod, an ASReml model.
#' @param saveplot Should the output plot be saved?
#' @param filename Where to save the plot if saveplot=TRUE
#' @keywords met.asreml variogram
#' @export
#' @examples
#' \dontrun{
#' met.asr.vario(T2.Yield.VIC, saveplot = T, filename = "Trial2.Vic.Variogram")
#' }
met_asr_vario <- function(asreml.mod, saveplot = FALSE, filename = NULL) {
    temp.asr <- asreml.mod
    b <- ASExtras4::met.asreml(temp.asr)

    b[[1]][[2]]$panel.args.common$xlab$cex <- 1.5
    b[[1]][[2]]$panel.args.common$ylab$cex <- 1.5
    b[[1]][[2]]$panel.args.common$scales.3d$x.scales$cex <- .75
    b[[1]][[2]]$panel.args.common$scales.3d$y.scales$cex <- .75
    b[[1]][[2]]$panel.args.common$scales.3d$z.scales$cex <- .75

    if (saveplot) {
        grDevices::pdf(paste0(filename, ".pdf"), width = 5.5, height = 5.5)
        print(b[[1]][[2]])
        grDevices::dev.off()
    } else {
        print(b[[1]][[2]])
    }
}
