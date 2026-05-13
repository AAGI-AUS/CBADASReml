#' Rotate a georeferenced data frame
#'
#' Rotate a provided georeferenced dataframe by a specified angle in degrees.
#'
#' @param data `data.frame`.
#'   The dataframe to be rotated. 
#'   This should be goereferenced per the `sf` pacakge.
#'   
#' @param angle `numeric`.
#'   Clockwise angle in degrees to rotate the dataframe.
#'
#' @returns `data.frame`.
#'     `data` with geometry now defined in rotated coordinates.
#'     Contains the same columns as the input with the following additions:
#' \describe{
#'   \item{x_original}{The original x-coordinates before rotation.}
#'   \item{y_original}{The original y-coordinates before rotation.}
#' }
#'
#' @examplesIf requireNamespace("asreml", quietly = TRUE)
#' library(CBADASReml)
#' library(dplyr)
#' library(sf)
#' library(spdep)
#' 
#' data <- agridat::lasrosas.corn |>
#' filter(year == 2001) |>
#' rename(
#'   Yield = yield,
#'   Treatment = nf,
#'   Rep = rep
#'  ) |>
#'  dplyr::select(Yield, Treatment, Rep, long, lat) |>
#'  st_as_sf(coords = c("long", "lat"), crs = 4326) |>
#'  st_transform(3395)
#'  
#' ofe_rotate_data(data, -60)
#' @autoglobal
#' @author Braden Thorne, \email{braden.thorne@@curtin.edu.au}
#' @export
ofe_rotate_data <- function(data, angle){
  rotation_angle <- angle*pi/180
  rotation_matrix <- matrix(c(
    cos(rotation_angle),
    sin(rotation_angle),
    -sin(rotation_angle),
    cos(rotation_angle)
  ), nrow=2, ncol=2)
  
  original_coordinates <- sf::st_coordinates(data)
  translation_matrix <- apply(st_coordinates(data), 2, mean)
  
  rotated_coordinates <- (
    rotation_matrix %*% (
      original_coordinates |> 
        as.matrix() |> 
        t() - 
        translation_matrix
    )
  ) + translation_matrix
  
  return(
    data |> 
      sf::st_drop_geometry() |> 
      dplyr::mutate(
        x_original = as.numeric(original_coordinates[,1]),
        y_original = as.numeric(original_coordinates[,2]),
        x = rotated_coordinates[1,],
        y = rotated_coordinates[2,]
      ) |> 
      sf::st_as_sf(
        coords = c("x", "y"),
        crs = sf::st_crs(data)
      )
  )
}


#' Grid data ready for ASReml analysis.
#'
#' Optimally rotate and grid a georeferenced dataframe
#'
#' @param data_in `data.frame`.
#'   The dataframe to be rotated. 
#'   This should be goereferenced per the `sf` pacakge.
#'   
#'   We assume there are only three column of actual interest in the dataframe,
#'   which are:
#' \describe{
#'   \item{Yield}{The observed yield values.}
#'   \item{Treatment}{The treatment factor to assess.}
#'   \item{Rep}{The blocking structure to be preserved}
#' }
#'   
#' @param rotation_angle `numeric`.
#'   Clockwise angle in degrees to rotate the dataframe.
#'   
#' @param nrow `numeric`.
#'   The number of rows in the final grid.
#'   This should be as large as possible without introducing spurious NA values.
#'   
#' @param npe `numeric`.
#'   Number of pseudo-environments to be generated along the strips.
#'   PEs will be evenly spaced, so may not be appropriate in all cases.
#'   Always assess PEs visually before use.
#'   
#' @param ncol `numeric`.
#'   The actual number of strips in the data.
#'   This should be counted/confirmed before hand.
#'   If there is a gap between strips, count the number of strips 
#'   in this gap when determining this number.
#'   
#' @param trim_ends `bool`.
#'   Boolean determining whether the gridded data should be trimmed
#'   to remove NAs occuring at the ends of columns.
#'   Defaults to FALSE.
#'
#' @returns `gridded.ofe`.
#'     `list` containing the following items:
#' \describe{
#'   \item{gridded_data `data.frame`}{
#'     The georeferenced gridded data.
#'   }
#'   \item{data_original `data.frame`}{
#'     The input data with additional columns for visualising the
#'     rotation process.
#'   }
#' }
#'
#' @examplesIf requireNamespace("asreml", quietly = TRUE)
#' library(CBADASReml)
#' library(dplyr)
#' library(sf)
#' library(spdep)
#' 
#' data <- agridat::lasrosas.corn |>
#' filter(year == 2001) |>
#' rename(
#'   Yield = yield,
#'   Treatment = nf,
#'   Rep = rep
#'  ) |>
#'  dplyr::select(Yield, Treatment, Rep, long, lat) |>
#'  st_as_sf(coords = c("long", "lat"), crs = 4326) |>
#'  st_transform(3395)
#'  
#' ofe_grid_data(data, -60, 80, 5, 18, trim_ends=FALSE)
#' @autoglobal
#' @author Braden Thorne, \email{braden.thorne@@curtin.edu.au}
#' @export
ofe_grid_data <- function(data_in, rotation_angle, nrow, npe, ncol, trim_ends=TRUE){
  
  ### Start with the rough angle rotation to determine strip labels
  data_pre_angle <- ofe_rotate_data(data_in, rotation_angle) |> 
    dplyr::mutate(
      x = as.numeric(sf::st_coordinates(geometry)[, "X"]),
      y = as.numeric(sf::st_coordinates(geometry)[, "Y"]),
      Row = 0,
      Col = 0
    )
  
  x_gap <- (max(data_pre_angle$x)-min(data_pre_angle$x))/(ncol-1)
  col_breaks <- seq(
    min(data_pre_angle$x)-x_gap/2,
    max(data_pre_angle$x)+x_gap/2,
    length=(ncol+1)
  )
  col_mid_points <- col_breaks[1:ncol] + diff(col_breaks)
  for (i in 1:ncol){
    data_pre_angle$Col <- data_pre_angle$Col + (data_pre_angle$x>col_breaks[i])
  }
  
  ### Based on the labels, determine the actual rotation angle and rotate data.
  true_angle <- (180/pi)*atan(
    1/stats::lm(
      y_original~x_original,
      data=dplyr::filter(data_pre_angle, Col==1)
    )$coefficients[[2]]
  )
  
  data <- ofe_rotate_data(data_in, true_angle)
  
  ### Prepare output data.
  data_out <- data |>
    dplyr::mutate(
      Yield = as.numeric(Yield),
      Treatment = as.factor(Treatment),
      Rep = as.factor(Rep),
      x = as.numeric(sf::st_coordinates(geometry)[, "X"]),
      y = as.numeric(sf::st_coordinates(geometry)[, "Y"]),
      x_rough_rotation = data_pre_angle$x,
      y_rough_rotation = data_pre_angle$y,
      Row = 0,
      Col = 0
    )
  
  ### Assign strip labels
  x_gap <- (max(data_out$x)-min(data_out$x))/(ncol-1)
  col_breaks <- seq(
    min(data_out$x)-x_gap/2,
    max(data_out$x)+x_gap/2,
    length=(ncol+1)
  )
  col_mid_points <- col_breaks[1:ncol] + diff(col_breaks)
  for (i in 1:ncol){
    data_out$Col <- data_out$Col + (data_out$x>col_breaks[i])
  }
  
  ### Can trim ends to not contain NAs if required.
  if (trim_ends) {
    y_extrema <- data_out |>
      sf::st_drop_geometry() |>
      dplyr::mutate(Col=as.factor(Col)) |>
      dplyr::group_by(Col) |>
      dplyr::summarise(
        min(y),
        max(y)
      )
    data_out <- data_out |>
      dplyr::filter(
        y>=max(y_extrema[["min(y)"]]),
        y<=min(y_extrema[["max(y)"]])
      )
  }
  
  ### Assign specified number of rows.
  row_breaks <- seq(min(data_out$y)-1e-6, max(data_out$y)+1e-6, length=(nrow+1))
  row_mid_points <- row_breaks[1:nrow] + diff(row_breaks)
  for (i in 1:nrow){
    data_out$Row <- data_out$Row + (data_out$y>row_breaks[i])
  }
  
  ### Summarise and reference to the gridded coordinates.
  data_out_summary <- data_out |>
    sf::st_drop_geometry() |>
    dplyr::mutate(
      Col=as.factor(Col),
      Row=as.factor(Row)
    ) |>
    dplyr::group_by(Col, Row) |>
    dplyr::summarise(
      Yield = mean(Yield),
      Treatment = dplyr::first(Treatment),
      Rep = dplyr::first(Rep)
    ) |>
    as.data.frame() |>
    dplyr::mutate(
      temp_filter = paste(Row, Col)
    ) |>
    dplyr::arrange(Rep, Col, Row)
  
  point.reference.frame <- data.frame(
    Row = as.factor(rep(1:nrow, ncol)),
    Col = as.factor(rep(1:ncol, each = nrow))
  ) |> 
    dplyr::mutate(
      x = col_mid_points[Col],
      y = row_mid_points[Row],
      x_rotated = col_mid_points[Col]-col_mid_points[1],
      y_rotated = row_mid_points[Row]-row_mid_points[1]
    ) |> 
    sf::st_as_sf(coords = c("x", "y"), crs = sf::st_crs(data)) |> 
    ofe_rotate_data(-true_angle)
  
  structure(
    class = "gridded.ofe",
    list(
      gridded_data = point.reference.frame |> 
        dplyr::left_join(data_out_summary) |>
        dplyr::select(-temp_filter) |> 
        dplyr::mutate(
          Pe.Row = as.factor(1 + floor(npe*as.numeric(Row)/(nrow+1e-6)))
        ),
      original_data = data_out
    )
  )
}


#' Plot diagnostics of gridded OFE
#'
#' Returns a plot with the following attributes:
#' \describe{
#'   \item{Original Data}{
#'     TOP LEFT.
#'     The raw data in the projected space.
#'   }
#'   \item{Rough Rotation}{
#'     TOP RIGHT
#'     Rotated original data with alternative strips highlighted.
#'     If each strip is not a single colour, the rotation is insufficient.
#'     In practice only the first strip needs to be identified correctly, 
#'     however it is best to ensure all strips are identified.
#'   }
#'   \item{Gridded Data (Raw)}{
#'     BOTTOM LEFT
#'     The accurate rotated, gridded data.
#'     Strips should be vertically aligned.
#'     NAs will display in red.
#'     The number of NAs should be relatively small, concentrated around the
#'     edges where strips do not begin at the same point (if ends were trimmed
#'     these should not be present) and in holes where data is missing.
#'     Red horizontal lines indicate that too many rows have been specified.
#'     Large NA concentrations in all four corners indicates the rotation
#'     has likely failed.
#'   }
#'   \item{Gridded Data (Projected)}{
#'     BOTTOM RIGHT
#'     The gridded data projected back to the original data space.
#'     This should look similar to the original data - if it does not the 
#'     process has likely failed and the other plots should be inspected to
#'     establish why.
#'   }
#' }
#'
#' @param data `gridded.ofe`.
#'     `list` generated by `ofe_grid_data` containing the following items:
#' \describe{
#'   \item{gridded_data `data.frame`}{
#'     The georeferenced gridded data.
#'   }
#'   \item{data_original `data.frame`}{
#'     The input data with additional columns for visualising the
#'     rotation process.
#'   }
#' }
#'
#' @examplesIf requireNamespace("asreml", quietly = TRUE)
#' library(CBADASReml)
#' library(dplyr)
#' library(sf)
#' library(spdep)
#' 
#' data <- agridat::lasrosas.corn |>
#' filter(year == 2001) |>
#' rename(
#'   Yield = yield,
#'   Treatment = nf,
#'   Rep = rep
#'  ) |>
#'  dplyr::select(Yield, Treatment, Rep, long, lat) |>
#'  st_as_sf(coords = c("long", "lat"), crs = 4326) |>
#'  st_transform(3395)
#'  
#' gridded_data <- ofe_grid_data(data, -60, 80, 5, 18, trim_ends=FALSE)
#' plot.gridded.ofe(gridded_data)
#' @autoglobal
#' @author Braden Thorne, \email{braden.thorne@@curtin.edu.au}
#' @export
plot.gridded.ofe <- function(gridded_ofe){
  p1 <- gridded_ofe$original_data |> 
    ggplot2::ggplot(
      ggplot2::aes(x=x_original, y=y_original, colour=Yield)
    ) + 
    ggplot2::geom_point() + 
    ggplot2::labs(x="Projected Longitude", y="Projected Latitude")
  p2 <- gridded_ofe$original_data |>
    dplyr::mutate(`Odd Strip`= as.factor(dplyr::if_else(Col%%2==0, "No", "Yes"))) |>
    ggplot2::ggplot(
      ggplot2::aes(x=x_rough_rotation, y=y_rough_rotation, colour=`Odd Strip`)
    ) +
    ggplot2::geom_point() +
    ggplot2::labs(x="Roughly Rotated X", y="Roughly Rotated Y")

  p3 <- gridded_ofe$gridded_data |>
    ggplot2::ggplot(
      ggplot2::aes(x=x_rotated, y=y_rotated, colour=Yield)
    ) +
    ggplot2::geom_point() +
    ggplot2::scale_colour_continuous(na.value="red") +
    ggplot2::labs(x="Gridded X", y="Gridded Y")

  p4 <- gridded_ofe$gridded_data |>
    ggplot2::ggplot(
      ggplot2::aes(
        x=sf::st_coordinates(geometry)[, "X"],
        y=sf::st_coordinates(geometry)[, "Y"],
        colour=Yield
      )
    ) +
    ggplot2::geom_point() +
    ggplot2::scale_colour_continuous(na.value="red") +
    ggplot2::labs(x="Projected Longitude", y="Projected Latitude")

  cowplot::plot_grid(p1, p2, p3, p4, ncol=2)
}


