#' Find or define boundaries of sampling bursts in time series
#'
#' Find or define boundaries of sampling bursts in a time series
#'
#' This function searches for natural breaks in a time series, or
#' defines a set of breaks based on the desired length of sampling
#' bursts to be analyzed.
#'
#' @param x A vector of POSIXct time stamps
#' @param Fs Sampling rate, units of Hertz (samples per second)
#' @param burstLength Desired length, in minutes, of sampling bursts to be analyzed
#'
#' @return A vector of row indices from input vector x that define time
#' chunks to be analyzed. Each value is the start and end of a time chunk
#' (except the 1st and last values of the vector)
#'
#' @export

burstBounds <- function(x, Fs, burstLength = NULL) {
  # Sanity check
  if (class(x)[1] != 'POSIXct'){
    stop('Input data x must be POSIXct time values')
  }
  # Search for existing natural breaks in the data set. Use the
  # sampling rate to determine where longer breaks exist
  bounds = which(diff(x) > (1/Fs))

  if (length(bounds) > 0){
    # If there are natural breaks in the time series, use the
    # values in bounds
    bounds = c(1,bounds) # Add on the first row index
    bounds = c(bounds,length(x))  # Add on the final row index
  } else if (length(bounds) == 0){
    if (is.null(burstLength)){
      stop('Please enter a burstLength value (in minutes) for desired sampling burst length')
    }
    # If bounds has length 0, the dataset is continuous
    # Calculate the number of rows in a time chunk
    stepSize = burstLength * 60 * Fs #  minutes x 60 seconds x sample rate
    # Generate a set of chunk boundaries based on stepSize
    bounds = seq(from = 1, to = length(x), by = stepSize)
    bounds = c(bounds,length(x)) # Add on the final row index
  }


  # return results in a vector
  bounds
}
