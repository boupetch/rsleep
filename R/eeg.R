#' Plot the spectrogram of signal.
#'
#' @description `spectrogram` resamples signal and use the `specgram` function from the `signal` library to compute the spectrogram. Results resolution can be then reduced to quickly plot large signals.
#' @param signal Numerical vector of the signal.
#' @param sRate Signal sample rate in Hertz.
#' @param maxFreq Maximal frequency to plot in Hertz. Signal will be resampled at maxFreq*2 sample rate.
#' @param n The size of the Fourier transform window.
#' @param window Shape of the fourier transform window, defaults to n.
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
                        window = n,
                        overlap = 0,
                        cols = c(rep("#3B9AB2",9),"#78B7C5","#EBCC2A","#E1AF00",rep("#F21A00",6)),
                        freq = 4,
                        plot = TRUE,
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

#' Computes spectral power of bands listed in the bands argument.
#'
#' @description `bands_power` calculates power spectral densities estimates using Welch's method on bands. Bands are computed from spectrogram bands equal or greater than lower limit and inferior to the upper limit.
#' @param bands A list of bands to compute with lower and upper limits in the form `list(c(0,4),c(4,8))``
#' @param signal Numerical vector of the signal.
#' @param sRate Signal sample rate in Hertz.
#' @param normalize A band to normalize (divide) by. Defaults to `c(0.5,40)`. Can be set up to FALSE for raw results.
#' @return A list of bands powers.
#' @examples
#' bands_power(bands = list(c(0,4),c(4,8)),signal = sin(c(1:10000)),sRate = 200)
#' @export
bands_power <- function(bands, signal , sRate, normalize = c(0.5,40)){

  #s <- phonTools::pwelch(x = signal,fs = sRate,points = 1000, show = FALSE)
  s <- pwelch(x = signal,sRate = sRate,points = 1000, show = FALSE)
  #s[,2] <- s[,2]+abs(min(s[,2]))

  lapply(bands, function(band){

    s_filtered <- s[s$hz >= band[1] & s$hz < band[2],]

    if(length(normalize) == 2){
      s_broadband <- s[s$hz >= normalize[1] & s$hz < normalize[2],]
      sum(s_filtered$psd)/sum(s_broadband$psd)
    } else {
      sum(s_filtered$psd)
    }

  })
}

#' pwelch psd
#'
#' @description pwelch psd
#' @param x todo
#' @param sRate todo
#' @param points todo
#' @param overlap todo
#' @param padding todo
#' @param show todo
#' @return peridodogram plotted or raw
#' @examples
#' s <- pwelch(sin(c(1:10000)), 200)
#' @export
pwelch <- function(x,
                   sRate,
                   points = 0,
                   overlap = 0,
                   padding = 0,
                   show = TRUE){
  n = length(x)
  if (points == 0)
    points = ceiling(n/10)
  x = c(x, rep(0, points))
  spots = seq(1, n, points - overlap)
  if ((points + padding)%%2 == 1)
    padding = padding + 1
  n = points + padding
  psd = rep(0, n)
  for (i in 1:length(spots)) {
    tmp = x[spots[i]:(spots[i] + points - 1)] * signal::hamming(points)
    tmp = c(tmp, rep(0, padding))
    tmp = stats::fft(tmp)
    tmp = tmp * Conj(tmp)
    psd = psd + tmp
  }
  psd = psd/length(spots)
  psd = psd[1:(n/2 + 1)]
  psd = abs(psd)
  psd = log(psd)
  psd = psd - max(psd)
  hz = seq(0, sRate/2, length.out = (n/2) + 1)
  if (show == TRUE)
    plot(hz, psd, type = "l", ylab = "PSD",
         xlab = "Hz",
         xaxs = "i")
  invisible(data.frame("hz" = hz,
            "psd" = psd))
}
