#' A spatial variogram function for Single Site Data
#'
#' This function allows you to observe directional variograms in the 0 (vertical) and 90 (horizontal) directions for gridded small-plot trial data.
#' @param model `asreml`, an ASReml model with some residual structure.
#' @returns A `gstat::variogram` object.
#' @export
#' @examplesIf requireNamespace("asreml", quietly = TRUE)
#' library(asreml)
#' model <- asreml(
#'     fixed = yield ~ weed,
#'     random = ~idv(Variety),
#'     residual = ~ar1v(Row):id(Column),
#'     data = wheat
#' )
#' mod <- directional_variograms(model)
#' plot(mod, multipanel = F)
#'
#' \dontrun{
#' # You can also plot using ggplot if you wish
#' ggplot(
#'     mod,
#'     aes(x = dist, y = gamma, group = dir.hor, colour = factor(dir.hor))
#' ) +
#'     geom_point() +
#'     geom_line() +
#'     facet_grid(~dir.hor, scales = "free_x")
#' }
directional_variograms <- function(model) {
    data <- as.data.frame(model$mf)

    # Retrieving the names of the two directions for the model
    resid <- as.character(attr(model$formulae$residual, "variables"))

    # Find the individual direction names
    dirs <- sub(".*\\(([^)]*)\\).*", "\\1", resid[2:3])

    data$residual <- stats::resid(model)

    m_spatial_data <- sf::st_as_sf(
        stats::na.omit(data),
        coords = c(dirs[1], dirs[2])
    )

    vario_mod <- gstat::variogram(
        residual ~ 1,
        data = m_spatial_data,
        alpha = c(0, 90),
        width = 1,
        cutoff = max(m_spatial_data %>% sf::st_coordinates()),
        tol.hor = 0,
        tol.ver = 0
    )

    return(vario_mod)
}
