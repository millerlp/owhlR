#' Convert pressure data in millibar to seawater depth in meters
#'
#' Convert pressure data in millibar to seawater depth in meters
#'
#' Convert water pressure data, in units of millibar, to the
#' equivalent depth (or elevation above the pressure sensor if you
#' prefer that perspective) of seawater in meters.
#'
#' @param x A vector of pressure data, units of millibar
#' @param latitude The latitude at which the seawater pressure data were
#' collected.
#'
#' @return A vector of seawater depth in meters
#'
#' @export
#'
#' @importFrom oce swDepth


millibarToSeawater <- function(x, latitude = NULL){
  # Sanity check
  if (is.null(latitude)){
    stop('Please provide a numeric latitude for the data, such as 33.72')
  }
  # Convert input values of pressure in millibar to decibar
  x.dbar = x / 100

  # Use the oce function swDepth to estimate the height of seawater above the
  # sensor.
  swDepth.m = oce::swDepth(x.dbar, latitude = latitude)
  # Return result vector
  swDepth.m
}
