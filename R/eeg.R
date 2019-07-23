#' Plot the spectrogram of signal.
#'
#' @description `spectrogram` resamples signal and use the `specgram` function from the `signal` library to compute the spectrogram. Results resolution can be then reduced to quickly plot large signals.
#' @param signal Numerical vector of the signal.
#' @param sRate Signal sample rate in Hertz.
#' @param maxFreq Maximal frequency to plot in Hertz. Signal will be resampled at maxFreq*2 sample rate.
#' @param n The size of the Fourier transform window.
#' @param window Shape of the fourier transform window, defaults to n*2.
#' @param overlap Overlap with previous window, defaults to 0.
#' @param cols Color scale used for the underlying plot function.
#' @param freq Aggregate frequency used to lower spectrogram resolution. Defaults to 4.
#' @param plot Boolean, plot or not the spectrogram.
#' @param startTime Posixct of the signal start. Adjust the x axis labels accordingly.
#' @return A spectrogram.
#' @examples
#' library(signal)
#' spectrogram(chirp(seq(-2, 15, by = 0.001), 400, 10, 100, 'quadratic'),20,n=1024)
#' @export
spectrogram <- function(signal,
                        sRate,
                        maxFreq = 25,
                        n = 1024,
                        window = n*2,
                        overlap = 0,
                        cols = c(rep("#3B9AB2",9),"#78B7C5","#EBCC2A","#E1AF00",rep("#F21A00",6)),
                        freq = 4,
                        plot = T,
                        startTime = as.POSIXct("1970/01/01 00:00:00")){

  resample_sRate <- maxFreq*2

  x <- signal::resample(signal,resample_sRate,sRate)

  spec <- signal::specgram(x = x,
                           Fs = resample_sRate,
                           n = n,
                           window = window,
                           overlap = overlap)

  spec$S <- apply(spec$S,2,Re)

  suppressWarnings(
    spec$S <- apply(spec$S,2,function(x){
      stats::aggregate(stats::ts(as.numeric(x), frequency=freq), 1, max)
    }))

  spec$f <- as.numeric(stats::aggregate(stats::ts(spec$f, frequency=freq), 1, max))

  endTime <- startTime + round(length(x)/resample_sRate)
  spec$t <- seq(startTime,endTime,(as.numeric(endTime)-as.numeric(startTime))/length(spec$t))

  if(plot){
    plot(spec, col = cols,ylab="Frequency (Hz)",xlab="")
  } else {
    return(spec)
  }
}
