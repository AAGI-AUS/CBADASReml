#' A spatial variogram function for Single Site Data
#'
#' This function needs the library stringr, asreml and ggplot2
#' @param model, an ASReml model.
#' @keywords spatial
#' @export
#' @examples
#' \dontrun{
#' spatial_ar1(model)
#' }
spatial_ar1 <- function(model) {
    variogram_fun <- function(x, rho, var) {
        return(var * (1 - rho^x))
    }

    # Find the resiudal names
    a <- as.character(attr(model$formulae$residual, "variables"))

    dir1 <- stringr::str_match(a[2], "\\((.*?)\\)")[, 2]
    dir2 <- stringr::str_match(a[3], "\\((.*?)\\)")[, 2]

    # Find the pararemeters rho1, rho2 and var
    v <- summary(model)$varcomp

    f <- rownames(v)
    f <- stringr::str_match(f, "\\!(.*?)\\!")[, 2]

    rho1 <- v[which(f == dir1), "component"]
    rho2 <- v[which(f == dir2), "component"]

    f <- rownames(v)
    f <- stringr::str_match_all(f, "R(?!.)")

    var <- v[which(f == "R"), "component"]

    # Create the variograms
    d <- as.data.frame(asreml::varioGram(model))

    colnames(d) <- c("Row", "Range", "gamma")

    g1 <-
        ggplot2::ggplot(
                     data = subset(d, Range == 0),
                     ggplot2::aes(x = Row, y = gamma)
                 ) +
        ggplot2::geom_point() +
        ggplot2::stat_function(
                     fun = variogram_fun,
                     args = list(rho = rho1, var = var)
                 )
    g2 <-
        ggplot2::ggplot(
                     data = subset(d, Row == 0),
                     ggplot2::aes(x = Range, y = gamma)
                 ) +
        ggplot2::geom_point() +
        ggplot2::stat_function(
                     fun = variogram_fun,
                     args = list(rho = rho2, var = var)
                 )

    print(g1)
    print(g2)
}
