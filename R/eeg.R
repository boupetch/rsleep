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

#' Compute power spectral density of bands listed in the bands argument.
#'
#' @description `bands_psd` calculates power spectral densities estimates on bands. Bands are computed from spectrogram bands equal or greater than lower limit and inferior to the upper limit.
#' @param signal Numerical vector of the signal.
#' @param sRate Signal sample rate in Hertz.
#' @param bands A list of bands to compute with lower and upper limits in the form `list(c(0,4),c(4,8))``
#' @param normalize A band to normalize (divide) by. Defaults to `c(0.5,40)`. Can be set up to FALSE for raw results. Defaults to FALSE.
#' @param method Character. Method to use to compute power spectral density. "pwelch" or "psm". Defaults to "pwelch".
#' @return A list of bands powers.
#' @examples
#' signal <- sin(seq(0,100,0.01))
#' bands_psd(bands = list(c(0,4),c(4,8)), signal = signal, sRate = 200)
#' @export
bands_psd <- function(
  signal,
  sRate,
  bands,
  normalize = FALSE,
  method= "pwelch"){

  if(method == "pwelch"){
    s <- pwelch(x = signal, sRate = sRate, points = 1000, show = FALSE)
  } else if(method == "psm"){
    s <- psm(x = signal, sRate = sRate, show = FALSE)
  } else{
    stop("Choose between \"pwelch\" and \"psm\" for psd estimation method.")
  }

  lapply(bands, function(band){

    s_filtered <-  s[s$hz >= band[1] & s$hz < band[2],]

    if(length(normalize) == 2){
      s_broadband <- s[s$hz >= normalize[1] & s$hz < normalize[2],]
      sum(s_filtered$psd)/sum(s_broadband$psd)
    } else {
      sum(s_filtered$psd)
    }

  })
}

#' Power spectral density using Welch's method.
#'
#' @description Power spectral density using Welch's method.
#' @references Welch, P. “The Use of Fast Fourier Transform for the Estimation of Power Spectra: A Method Based on Time Averaging over Short, Modified Periodograms.” IEEE Transactions on Audio and Electroacoustics 15, no. 2 (June 1967): 70–73. https://doi.org/10.1109/TAU.1967.1161901.
#' @param x Signal vector.
#' @param sRate Sample rate of the signal.
#' @param points todo
#' @param overlap todo
#' @param padding todo
#' @param show todo
#' @return peridodogram plotted or raw
#' @examples
#' x <- sin(c(1:10000))
#' psd <- pwelch(sin(c(1:10000)), 200)
#' head(psd)
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
    graphics::plot(hz, psd, type = "l", ylab = "PSD",
         xlab = "Hz",
         xaxs = "i")
  invisible(data.frame("hz" = hz,
            "psd" = psd))
}

#' Power spectral density using adaptive sine multitaper.
#'
#' @description Power spectral density using adaptive sine multitaper.
#' @references Barbour, A. J. and R. L. Parker (2014), psd: Adaptive, sine multitaper power spectral density estimation for R, Computers & Geosciences, Volume 63, February 2014, Pages 1-8, ISSN 0098-3004, http://dx.doi.org/10.1016/j.cageo.2013.09.015
#' @param x Signal vector.
#' @param sRate Sample rate of the signal.
#' @param length periodogram resolution. 0 default to not resize.
#' @param show todo
#' @return peridodogram plotted or raw.
#' @examples
#' x <- sin(c(1:10000))
#' psd <- psm(x, 200, 100)
#' head(psd)
#' @export
psm <- function(x, sRate, length=0, show = TRUE){

  options(psd.ops=list(
    tapmin = 1,
    tapcap = 1000,
    names = list(
      fft = "working_fft",
      fft.padded = "fft_even_demeaned_padded",
      last.taper = "last_taper_sequence",
      last.psdcore = "last_psdcore_psd",
      last.psdcore.extrap = "last_psdcore_psd_extrap",
      series.even = "ser_orig_even",
      var.even = "ser_even_var",
      n.even = "len_even",
      n.even.half = "len_even_half",
      series.orig = "ser_orig",
      n.orig = "len_orig"
    )
  ))

  res <- psd::pspectrum(x,plot=FALSE,verbose=FALSE)

  df <- data.frame("hz" = res$freq, "psd" = res$spec)

  df$psd <- log(df$psd)

  df$hz <- df$hz*sRate

  if(length > 0){

    psd <- signal::resample(x = df$psd,
                            p = length,
                            q = nrow(df))

    hz <- signal::resample(x = df$hz,
                           p = length,
                           q = nrow(df))

    df <- data.frame("psd" = psd,
                     "hz" = hz)
  }

  if (show == TRUE)
    graphics::plot(df$hz, df$psd, type = "l", ylab = "PSD",
                   xlab = "Hz",
                   xaxs = "i")
  invisible(df)
}

#' A7 spindle detection algorithm
#'
#' @description A sleep spindle detection algorithm that emulates human expert spindle scoring
#' @references Lacourse, K., Delfrate, J., Beaudry, J., Peppard, P., & Warby, S. C. (2019). A sleep spindle detection algorithm that emulates human expert spindle scoring. In Journal of Neuroscience Methods (Vol. 316, pp. 3–11). Elsevier BV. https://doi.org/10.1016/j.jneumeth.2018.08.014 
#' @param x EEG signal in uV.
#' @param sRate Sample rate of the signal.
#' @param window Size of the window in seconds. Default: 0.3
#' @param step Size of the step between windows in seconds. Default: 0.1
#' @param butter_order Order of the Butterworth filters. Default: 5
#' @param A7absSigPow A7absSigPow treshold. Default: 1.25
#' @param A7relSigPow A7relSigPow treshold. Default: 1.6
#' @param A7sigmaCov A7sigmaCov treshold. Default: 1.3
#' @param A7sigmaCorr A7sigmaCorr treshold. Default: 0.69
#' @return Detected spindles and associated features.
#' @details
#' A sleep spindle detection algorithm based on 4 features 
#' computed along segmented signal according to `window` size 
#' and `step` size parameters. 
#' 
#' 1. Absolute sigma power
#' \deqn{A7absSigPow = \log_{10} \left( \sum_{i=1}^{N} \frac{EEG\sigma_{i}^2}{N} \right)}
#' 2. Relative sigma power 
#' \deqn{A7relSigPow = zscore\left( \log_{10} \left( \frac{PSA_{11-16Hz}}{PSA_{4.5-30Hz}} \right) \right)}
#' 3. Sigma covariance 
#' \deqn{A7sigmaCov = zscore\left( \log_{10} \left( \frac{1}{N} \sum_{i=1}^{N} \left( EEG_{bf_i} - \mu_{EEG_{bf}} \right) \left( EEG_{\sigma_i} - \mu_{EEG_{\sigma}} \right) \right) \right)}
#' 4. Sigma correlation
#' \deqn{A7sigmaCor = \frac{\text{cov}(EEG_{bf}, EEG_{\sigma})}{sd_{EEG_{bf}} * sd_{EEG_{\sigma}}}}
#' @examples
#' fpath <- paste0(tempdir(),"c3m2_n2_200hz_uv.csv")
#' 
#' download.file(
#'   url = "https://rsleep.org/data/c3m2_n2_200hz_uv.csv",
#'   destfile = fpath)
#' 
#' # Read only a sample of the EEG signal
#' s = read.csv(fpath,header = FALSE)[,1][25000:45000]
#' 
#' file.remove(fpath)
#' 
#' a7_results = a7(s, 200)
#' 
#' # Plot the first detected spindle
#' data = data.frame(x=s,index=seq_along(s))
#' a = a7_results$spindles$idxStart[1]
#' b = a7_results$spindles$idxEnd[1]
#' data = data[(data$index <= (b+600)) & (data$index >= (a-600)), ]
#' library(ggplot2)
#' ggplot(data, aes(x = index, y = x)) +
#'  geom_line() +
#'  geom_line(data = subset(data, index >= a & index <= b), aes(x = index, y = x), color = "red") +
#'  labs(x = "Signal index", y = "C3-M2") +
#'  theme_minimal()
#'   
#' # Visualise features distribution 
#' 
#' hist(a7_results$df$absSigPow,main = "A7absSigPow")
#' 
#' hist(a7_results$df$relSigPow,main = "A7relSigPow")
#' 
#' hist(a7_results$df$sigmaCov,main = "A7sigmaCov")
#' 
#' hist(a7_results$df$sigmaCorr,main = "A7sigmaCorr") 
#' 
#' @export
a7 = function(
    x, 
    sRate, 
    window=0.3,
    step = 0.1,
    butter_order = 5,
    A7absSigPow = 1.25,
    A7relSigPow = 1.6,
    A7sigmaCov = 1.3,
    A7sigmaCorr = 0.69){
  
  results = list()
  
  # Filter EEGs
  low_freq <- 11
  high_freq <- 16
  nyquist <- sRate / 2
  low_norm <- low_freq / nyquist
  high_norm <- high_freq / nyquist
  order <- butter_order
  butter_filter <- signal::butter(order, c(low_norm, high_norm), type = "pass")
  EEGs <- signal::filtfilt(butter_filter, x)
  
  # Filter EEGbf 
  low_freq <- 0.3
  high_freq <- 30
  nyquist <- sRate / 2
  low_norm <- low_freq / nyquist
  high_norm <- high_freq / nyquist
  order <- butter_order
  butter_filter <- signal::butter(order, c(low_norm, high_norm), type = "pass")
  EEGbf <- signal::filtfilt(butter_filter, x)
  
  # Segments
  segments = rsleep::segmentation(
    list(EEGbf, EEGs),
    c(sRate, sRate),
    segments_size = window,
    step = step,
    padding = 0)
  
  results[["segments_dimensions"]] = dim(segments)
  
  # Compute A7absSigPow
  
  absSigPow = apply(segments, 1, function(x){
    sum(x[,2]^2) / length(x[,2])
  })
  
  absSigPow = log10(absSigPow)
  
  # Compute A7relSigPow
  
  psa1116 = apply(segments, 1, function(x){
    N <- length(x[,1])
    f1 <- 11
    f2 <- 16
    lower <- floor(f1 * N / sRate)
    upper <- ceiling(f2 * N / sRate)
    sum(abs(stats::fft(x[,1])[lower:upper])^2)
  })
  
  psa4530 = apply(segments, 1, function(x){
    N <- length(x[,1])
    f1 <- 4.5
    f2 <- 30
    lower <- floor(f1 * N / sRate)
    upper <- ceiling(f2 * N / sRate)
    sum(abs(stats::fft(x[,1])[lower:upper])^2)
  })
  
  logPowRat = log10(psa1116/psa4530)
  
  logPowRat_segments = rsleep::segmentation(
    list(c(unlist(logPowRat),0)),
    c(1),
    segments_size = 1,
    step = 1,
    padding = 30/window/2)
  
  relSigPow = unlist(lapply(c(1:dim(logPowRat_segments)[1]), function(i){
    sorted_v <- sort(logPowRat_segments[i,])
    p10 <- stats::quantile(sorted_v, 0.10)
    p90 <- stats::quantile(sorted_v, 0.90)
    subset_v <- sorted_v[sorted_v >= p10 & sorted_v <= p90]
    normalized_rel = (logPowRat_segments[i,] - mean(logPowRat_segments[i,]))/stats::sd(subset_v)
    normalized_rel[(30/window/2+1)]
  }))
  
  # Compute A7sigmaCov
  
  scov = apply(segments,1,function(x){
    mean_EEGbf <- mean(x[,1])
    mean_EEGsigma <- mean(x[,2])
    covariance = sum((x[,1] - mean_EEGbf) * (x[,2] - mean_EEGsigma)) / length(x[,1])
    if(covariance > 0){
      covariance
    } else {
      0
    }
  })
  logSigmaCov = unlist(lapply(scov,function(x){
    if(x > 0){
      log10(x)
    } else {
      0
    }
  }))
  
  zscore_segments = rsleep::segmentation(
    list(c(unlist(logSigmaCov),0)),
    c(1),
    segments_size = 1,
    step = 1,
    padding = 30/window/2)
  
  sigmaCov = unlist(apply(zscore_segments, 1, function(x){
    sorted_v <- sort(x)
    p10 <- stats::quantile(sorted_v, 0.10)
    p90 <- stats::quantile(sorted_v, 0.90)
    subset_v <- sorted_v[sorted_v >= p10 & sorted_v <= p90]
    normalized = (x - mean(x))/stats::sd(subset_v)
    normalized[(30/window/2+1)]
  }))
  
  # Compute A7sigmaCorr
  
  sigmaCorr = unlist(lapply(c(1:dim(segments)[1]),function(x){
    scov[[x]]/(stats::sd(segments[x,,1])*stats::sd(segments[x,,2]))
  }))
  
  # All together
  results[["df"]] = data.frame(
    logPowRat = logPowRat,
    absSigPow = absSigPow,
    relSigPow = relSigPow,
    sigmaCov = sigmaCov,
    sigmaCorr = sigmaCorr)
  
  # Detect spindles based on thresolds
  
  results[["df"]]$spindle <- unlist(
    lapply(c(1:nrow(results[["df"]])), function(i) {
      all(
        results[["df"]]$absSigPow[i] > A7absSigPow,
        results[["df"]]$relSigPow[i] > A7relSigPow,
        results[["df"]]$sigmaCov[i] > A7sigmaCov,
        results[["df"]]$sigmaCorr[i] > A7sigmaCorr)
    }))
  
  # Detect consecutive
  
  # Pass 1
  spindle = FALSE
  for(i in c(1:nrow(results[["df"]]))){
    if(spindle & (results[["df"]]$absSigPow[i] > A7absSigPow) & (results[["df"]]$sigmaCov[i] > A7sigmaCov)){
      results[["df"]]$spindle[i] = TRUE
    }
    spindle = results[["df"]]$spindle[i]
  }
  
  # Pass 2
  
  spindle = FALSE
  for(i in c(nrow(results[["df"]]):1)){
    if(spindle & (results[["df"]]$absSigPow[i] > A7absSigPow) & (results[["df"]]$sigmaCov[i] > A7sigmaCov)){
      results[["df"]]$spindle[i] = TRUE
    }
    spindle = results[["df"]]$spindle[i]
  }
  
  
  # Simplify spindles 
  
  results[["df"]]$idxStart = c(0:(nrow(results[["df"]])-1))*step*sRate+1
  
  results[["df"]]$idxEnd = results[["df"]]$idxStart + (step*sRate-1)
  
  # Spindles
  
  spindles <- list()
  start <- NULL
  end <- NULL
  for (i in seq_along(results[["df"]]$spindle)) {
    if (results[["df"]]$spindle[i]) {
      if (is.null(start)) {
        start <- i
      }
      end <- i
    } else {
      if (!is.null(start)) {
        spindles[[length(spindles) + 1]] <- list(
          start = start, 
          end = end)
        start <- NULL
      }
    }
  }
  if (!is.null(start)) {
    spindles[[length(spindles) + 1]] <- list(
      start = start, 
      end = end)
  }
  spindles = dplyr::bind_rows(spindles)
  spindles$length = (spindles$end - spindles$start + 1) * step
  spindles$idxStart = spindles$start * step * sRate
  spindles$idxEnd = spindles$end * step * sRate
  spindles$segmentStart = spindles$start
  spindles$segmentEnd = spindles$end
  spindles$start = NULL
  spindles$end = NULL
  spindles = spindles[spindles$length >= 0.3,]
  spindles = spindles[spindles$length <= 2.5,]
  
  results[["spindles"]] = spindles
  
  return(results)
}

#' Bandpass Filter Function
#'
#' This function applies a bandpass filter to a signal. 
#' It first normalizes the high and low frequencies based on the Nyquist frequency,
#' then creates a Butterworth filter using the `signal::butter` function,
#' and finally applies the filter to the signal using `signal::filtfilt`.
#'
#' @param x A numeric vector representing the signal to be filtered.
#' @param high The high cutoff frequency for the bandpass filter.
#' @param low The low cutoff frequency for the bandpass filter.
#' @param sRate The sampling rate of the signal.
#' @param order The order of the Butterworth filter, defaulting to 5.
#' @return A numeric vector representing the filtered signal.
#' @importFrom signal butter
#' @importFrom signal filtfilt
#' @examples
#' sample_signal <- sin(seq(0, 10, length.out = 1000))
#' filtered_signal <- bandpass(sample_signal, high = 0.3, low = 0.1, sRate = 100)
#' @export
#' @seealso \code{\link[signal]{butter}}, \code{\link[signal]{filtfilt}}
#' 
#' @references
#' If applicable, add references here.
#'
bandpass <- function(x, high, low, sRate, order = 5) {
  nyquist <- sRate / 2
  low_norm <- low / nyquist
  high_norm <- high / nyquist
  butter_filter <- signal::butter(
    order, c(low_norm, high_norm), 
    type = "pass")
  return(signal::filtfilt(butter_filter, x))
}

