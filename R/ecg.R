#' Detect R peaks in a raw ECG signal.
#'
#' @description `detect_rpeaks` implements the first part of the Pan & Tompkins algorithms to detect R peaks from a raw ECG signal.
#' @references Pan, Jiapu, and Willis J. Tompkins. "A real-time QRS detection algorithm." IEEE Trans. Biomed. Eng 32, no. 3 (1985): 230-236.
#' @param signal Numerical vector of ECG signal.
#' @param sRate ECG signal sample rate.
#' @param lowcut Butterworth bandpass filter low cut value.
#' @param highcut Butterworth bandpass filter high cut value.
#' @param filter_order Butterworth bandpass filter order value.
#' @param integration_window Convolution window size.
#' @param refractory Minimal space between peaks in milliseconds.
#' @param return_index If TRUE, the index for each R peak is returned instead of the timing. 
#' @return A numeric vector of detected R peaks, expressed in seconds* from the start of the signal. This vector can be used in RHRV using `RHRV::LoadBeatVector()`.
#' 
#' *(or samples if return_index is TRUE)
#' @export
#' @examples
#' path <- paste0(tempdir(),"rec_1.dat")
#' download.file("https://physionet.org/files/ecgiddb/1.0.0/Person_01/rec_1.dat?download",path)
#' ecg <- readBin(path,integer(),500*30)
#' peaks <- detect_rpeaks(ecg, sRate = 500)
#' unlink(path)
#' print(peaks)
#'
#' ecg.df <- data.frame(ECG = ecg,Seconds = c(1:length(ecg))/500)
#' library(ggplot2)
#' ggplot(ecg.df,aes(x = Seconds,y = ECG)) +
#'   geom_line() + theme_bw() +
#'   geom_vline(data.frame(p = peaks),mapping = aes(xintercept = p), linetype="dashed",color = "red")
detect_rpeaks <- function(
  signal,
  sRate,
  lowcut = 0,
  highcut = 15,
  filter_order = 1,
  integration_window = 15,
  refractory = 200,
  return_index = FALSE){

  nyquist_freq = 0.5 * sRate
  low = lowcut / nyquist_freq
  high = highcut / nyquist_freq

  # Apply bandpass butterworth filter.
  bandpass <- signal::butter(n = filter_order,
                             W = c((lowcut / (0.5 * sRate)), high),
                             type = "pass")
  signal_filt <- signal::filtfilt(
    bandpass,
    c(rep(signal[1],sRate),
      signal,rep(signal[length(signal)],sRate)))
  signal_filt <- signal_filt[(sRate+1):(length(signal_filt)-sRate)]

  # Apply diff
  signal_diff <- diff(signal_filt)

  # Squaring signal
  signal_squared <- signal_diff^2
  signal_squared <- c(signal_squared[1], signal_squared)

  # Apply convolution on chunks of signal (100000 values)
  # to avoid an unidentified bug.
  split_ecg <- split(signal_squared, ceiling(seq_along(signal_squared)/100000))
  split_ecg2 <- lapply(
    split_ecg,
    function(x){
      xc <- stats::convolve(x,rep(1,integration_window),type="open")
      difflen <- length(xc) - length(x)
      xc <- xc[(difflen/2+1):(length(xc)-(difflen/2))]
      xc
      })
  signal_conv <- unlist(split_ecg2,use.names = FALSE)

  # Detect peak on preprocessed signal.
  peaks <- c(0)
  limit <- mean(signal_conv)*3

  refractory <- sRate*(refractory/1000)
  x <- signal_conv
  for(i in c(2:(length(x)-1))){
    if((x[i] > limit) &&
       (x[i] > x[i-1]) &&
       (x[i] > x[i+1]) &&
       ((i - peaks[length(peaks)]) > refractory)){
      peaks <- c(peaks,i)
    }
  }
  peaks <- peaks[2:length(peaks)]
  
  if(return_index) return(peaks)
  
  return(peaks/sRate)

}
