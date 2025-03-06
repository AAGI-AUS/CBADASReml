#' A spatial variogram function for Single Site Data
#'
#' This function allows you to observe outliers more easily
#' @param model `asreml`, an ASReml model with some residual structure.
#' @param tol `numeric`, tolerance for each angle that is allowed.
#' @return A `gstat::variogram` object.
#' @export
#' @examplesIf requireNamespace("asreml", quietly = TRUE)
#' library(asreml)
#' model <- wheat1.asr <- asreml(
#'     fixed = yield ~ weed,
#'     random = ~idv(Variety),
#'     residual = ~ar1v(Row):id(Column),
#'     data = wheat
#' )
#' spatial_2D_variogram(model)
spatial_2D_variogram <- function(model, tol = 0) {
    data <- as.data.frame(model$mf)

    # Retrieving the names of the Two directions for the model
    resid <- as.character(attr(model$formulae$residual, "variables"))
    # Match the entire string, but capture everything inside the parentheses
    dirs <- sub(".*\\(([^)]*)\\).*", "\\1", resid[2:3])

    dir1_range <- range(as.numeric(data[, dirs[1]]))
    dir2_range <- range(as.numeric(data[, dirs[2]]))

    spatial_data <- expand.grid(
        dir1 = seq(min(dir1_range), max(dir1_range)),
        dir2 = seq(min(dir2_range), max(dir2_range))
    )

    spatial_data$residual <- stats::resid(model)

    m_spatial_data <- sf::st_as_sf(
                              na.omit(spatial_data),
                              coords = c("dir1", "dir2")
                          )

    vario_mod <- gstat::variogram(
                            residual ~ 1,
                            data = m_spatial_data,
                            alpha = c(0, 90),
                            width = 1,
                            cutoff = max(dir1_range, dir2_range),
                            tol.hor = tol
    )

    return(vario_mod)
}
