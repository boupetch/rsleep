#' Split signals into a list of epochs according to an events dataframe or an epoch duration.
#' @param signals A list of numeric vectors containing signals.
#' @param sRates A vector or list of integer values of the signals sample rates.
#' @param resample The sample rate to resample all signals. Defaults to 100.
#' @param epoch Epochs reference. Can be an events dataframe or the number of seconds of each epoch. Defaults to 30.
#' @param startTime The start timestamp of the signal, used to join events to epoch.
#' @return A list of signal chunks
#' @examples
#' epochs(list(c(1:1000),c(1:1000)),100,2)
#' @export
epochs <- function(signals,
                   sRates,
                   resample = 100,
                   epoch = 30,
                   startTime = 0){

  resampled_signals <- mapply(function(x,y){
    if(y != resample){
      signal::resample(x,resample,y)
    } else{
      x
    }
  }, x = signals,
  y = sRates)

  if(is.numeric(epoch)){

    lapply(
      split(resampled_signals,
            ceiling(seq_along(resampled_signals[,1])/(resample*epoch))),
      matrix,
      ncol = dim(resampled_signals)[2])

  } else if(is.data.frame(epoch)){

    epoch$begin <- as.numeric(epoch$begin)
    epoch$end <- as.numeric(epoch$end)

    epochs <- lapply(c(1:nrow(epoch)), function(x){
      sidx <- (epoch$begin[x] - startTime)*resample
      eidx <- (epoch$end[x] - startTime)*resample-1
      max <- dim(resampled_signals)[1]
      if((sidx <=  max) & (eidx <= max)){
        resampled_signals[sidx:eidx,]
      }
    })
    epochs[sapply(epochs, is.null)] <- NULL
    epochs

  } else {

    stop("`epoch` parameter must be a numeric or a dataframe of events.")

  }
}
