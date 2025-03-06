#' Produces a plot border for an experimental design.
#'
#'
#' @param deslist experimental design listing.
#' @param nameborder name of the blocking variable. For example, "Rep".
#' @keywords design border
#' @import dplyr xtable
#' @export
#' @return a list of the raw stats tables
#' @examples \dontrun{
#' .data <- data.frame(A = as.factor(rep(1:5, 2)), B = rnorm(10), C = as.factor(rep(1:2, 5)))
#' raw_stats(.data, response = "B", C, A)
#' }
ggplotborder <- function(deslist, nameborder = NULL) {
    if (is.null(nameborder)) {
        stop("Specify Column of deslist to plot")
    }

    df <- deslist

    namenum <- which(names(df) == nameborder)

    group_dat <- data.frame(Mainplot = sort(unique(df[, namenum])), xmin = 0, xmax = 0, ymin = 0, ymax = 0)

    for (i in group_dat$Mainplot) {
        tmp <- df[df[, namenum] == i, ]
        group_dat[which(group_dat$Mainplot == i), "xmin"] <- min(tmp$Range) - 0.5
        group_dat[which(group_dat$Mainplot == i), "xmax"] <- max(tmp$Range) + 0.5
        group_dat[which(group_dat$Mainplot == i), "ymin"] <- min(tmp$Row) - 0.5
        group_dat[which(group_dat$Mainplot == i), "ymax"] <- max(tmp$Row) + 0.5
    }

    group_dat
}
