#' Create an XTS Object from Resampled Signals
#'
#' This function takes multiple signals and their corresponding sample rates, 
#' resamples the signals to the highest sample rate among them, and creates 
#' an xts (eXtensible Time Series) object with the resampled signals aligned 
#' according to a provided start time.
#'
#' @param signals A list of numeric vectors representing the signals. 
#'                Each signal in the list should correspond to one sample rate 
#'                in the `sample_rates` argument.
#' @param sample_rates A numeric vector containing the sample rates for each 
#'                     signal in `signals`. The length of `sample_rates` must 
#'                     match the length of `signals`.
#' @param start_time The start time for the xts object. This can be a character 
#'                   string or an object that can be converted to POSIXct. The 
#'                   time is assumed to be in UTC.
#'
#' @return An xts object containing the resampled signals, with each column 
#'         representing one of the original signals, resampled to the highest 
#'         sample rate among them. The xts object's index starts from 
#'         `start_time` and increments at a rate of 1 divided by the maximum 
#'         sample rate.
#'
#' @importFrom xts xts
#' @importFrom signal resample
#' @export
#'
#' @examples
#' signals <- list(rnorm(100), rnorm(100))
#' sample_rates <- c(1, 2)
#' start_time <- "2020-01-01 00:00:00"
#' xts_data <- create_xts(signals, sample_rates, start_time)
#' plot(xts_data)
#'
create_xts <- function(signals, sample_rates, start_time) {
  
  # Check if the lengths of signals and sample_rates are equal
  if (length(signals) != length(sample_rates)) {
    stop("Length of signals and sample_rates should be equal")
  }
  
  # Convert start_time to POSIXct if it's not already
  start_time <- as.POSIXct(start_time, tz = "UTC")
  
  # Determine the highest sample rate
  max_sample_rate <- max(sample_rates)
  
  # Initialize a list to store resampled signals
  resampled_signals <- list()
  
  # Resample each signal
  for (i in seq_along(signals)) {
    # Resample the signal
    up <- max_sample_rate
    down <- sample_rates[i]
    resampled_signal <- signal::resample(signals[[i]], up, down)
    
    # Trim or extend the resampled signal to match the expected length
    expected_length <- length(signals[[i]]) * up / down
    len_diff <- expected_length - length(resampled_signal)
    if (len_diff > 0) {
      # Extend
      resampled_signal <- c(resampled_signal, rep(NA, len_diff))
    } else if (len_diff < 0) {
      # Trim
      resampled_signal <- utils::head(resampled_signal, expected_length)
    }
    
    resampled_signals[[i]] <- resampled_signal
  }
  
  # Create a time index for the xts object
  final_length <- max(sapply(resampled_signals, length))
  time_index <- seq(from = start_time, by = 1/max_sample_rate, length.out = final_length)
  
  # Combine all resampled signals into one data frame
  combined_signals <- do.call(cbind, lapply(resampled_signals, function(x) { length(x) <- final_length; x }))
  
  # Create and return an xts object
  return(xts(combined_signals, order.by = time_index))
}