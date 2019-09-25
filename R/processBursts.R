#' Generate wave statistics for multiple sampling bursts
#'
#' Generate wave statistics for multiple sampling bursts
#'
#' @param Ht A vector of sea surface heights
#' @param times A vector of POSIXct time stamps
#' @param burstLength The desired sampling burst length, in minutes
#' @param Fs The sampling rate of the data, units of Hz (samples per second)
#' @param method The wave statistic estimation method, either 'sp' for spectral
#' (default) or 'zc' for zero-crossing
#' @param tzone The time zone for input POSIXct time values. Default = 'UTC'
#'
#' @return A data frame of wave statistics (height and period) for each sampling
#' burst. The time value represents the time stamp at the end of the sampling
#' burst
#'
#' @export
#'
#' @importFrom oceanwaves waveStatsSP waveStatsZC


processBursts <- function(Ht, times, burstLength = NULL, Fs,
                          method = 'sp', tzone = NULL){
  # Determine boundaries of bursts
  bounds <- owhlR::burstBounds(times, Fs = Fs, burstLength = burstLength)

  for (i in 1: (length(bounds)-1) ){
    # Extract the chunks of contiguous data
    if (length(bounds) == 2) {
      # There is only one chunk
      tempdatHt <- Ht
      timesHt <- times
      # Check that the chunk meets the desired burst length
      chunklength <- as.numeric(difftime(timesHt[bounds[2]],
                              timesHt[bounds[1]],
                              units='mins'))
      if (chunklength < burstLength) {
        stop('Time series is too short')
      }
    } else if (length(bounds) > 2) {
      # There are at least 2 chunks
      tempdatHt <- Ht[bounds[i]:bounds[i+1]]
      timesHt <- times[bounds[i]:bounds[i+1]]
      chunklength <- as.numeric(difftime(timesHt[length(timesHt)],
                              timesHt[1],
                              units='mins'))
    }
    # Check that the length of this is within a margin of error
    # of the desired burstLength
    if (chunklength >= burstLength-0.01 &
        chunklength <= burstLength+0.01) {
      # Extract the date & time at the end of the chunk to be used
      # as the identifying time stamp for this chunk
      tempDateTime <- timesHt[length(timesHt)]
      if (method == 'sp'){
        # Use the spectral analysis method to generate wave stats
        spstats <- oceanwaves::waveStatsSP(tempdatHt,
                                           Fs = Fs)
      } else if (method == 'zc'){
        # Using the zero-crossing function to estimate wave stats
        zcstats <- oceanwaves::waveStatsZC(tempdatHt,
                                           Fs = Fs)
      }
      # Stick the wave heights and periods in lists
      if (i == 1){
        if (method == 'sp'){
          # Add DateTime to the spectral results
          spstats[['DateTime']] <- tempDateTime
          # Save spectral results as a list
          spstatsList <- spstats
        } else if (method == 'zc'){
          # Add DateTime to the zerocross results
          zcstats[['DateTime']] <- tempDateTime
          # Save zerocross results as a list
          zcstatsList <- zcstats
        }
      } else {
        if (method == 'sp'){
          # Add DateTime to the spectral results
          spstats[['DateTime']] <- tempDateTime
          spstatsList <- mapply(c, spstatsList, spstats, SIMPLIFY=FALSE)
        } else if (method == 'zc'){
          # Add DateTime to zerocross results
          zcstats[['DateTime']] <- tempDateTime
          # Add to the zerocross list
          zcstatsList <- mapply(c, zcstatsList, zcstats, SIMPLIFY=FALSE)
        }
      }
    }
  }
  if (method == 'sp'){
    # Convert list object to data frame
    results <- as.data.frame(spstatsList)
  } else if (method == 'zc'){
    # Convert list object to data frame
    results <- as.data.frame(zcstatsList)
  }
  if (!is.null(tzone)){
    # Reset time zone attribute after data frame conversion
    attr(results$DateTime,'tzone') <- tzone
  }
  # Return results data frame
  results
}

