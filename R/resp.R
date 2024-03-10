#' Adaptive Normalization of a Signal
#'
#' This function implements an adaptive normalization method on a given respiratory signal.
#'
#' It is designed to preserve the parts of the signal where the amplitude of respiration
#' is small, typically when the body maintains a sleeping posture for extended periods.
#'
#' @references Choi, S. H., Yoon, H., Kim, H. S., Kim, H. B., Kwon, H. B., Oh, S. M., Lee, Y. J., & Park, K. S. (2018). Real-time apnea-hypopnea event detection during sleep by convolutional neural networks. In Computers in Biology and Medicine (Vol. 100, pp. 123–131). Elsevier BV. https://doi.org/10.1016/j.compbiomed.2018.06.028 
#' @param x Numeric vector representing the input signal to be normalized.
#' @param sRate Integer value representing the sampling rate of the signal (number of samples per second).
#' @return Numeric vector representing the adaptively normalized signal.
#' @details
#' Adaptive normalization first segments the signal into 1 second window
#' before computing \eqn{A(k)} and  is based on the following equations:
#'
#' Equation (1) - Mean absolute deviation:
#' \deqn{A(k) = \frac{1}{fs} \sum_{i=k \cdot fs}^{(k+1) \cdot fs - 1} \left| x(i) \right|}{A(k) = (1/fs) * sum(abs(x[i])) for i = k*fs to (k+1)*fs - 1}
#'
#' Equation (2) - Standard deviation:
#' \deqn{\sigma(k) = \sqrt{\frac{1}{fs-1} \sum_{i=k \cdot fs}^{(k+1) \cdot fs - 1} (x(i) - \bar{x}(k))^2}}{sigma(k) = sqrt((1/(fs-1)) * sum((x[i] - mean(x))^2)) for i = k*fs to (k+1)*fs - 1}
#'
#' Equation (3) - Adaptive normalization factor, initialized to 1.
#' \deqn{F_{\text{norm}}(k) = \min\left(0.95F_{\text{norm}}(k-1) + 0.05A(k), 0.95F_{\text{norm}}(k-1) + 0.05\sigma(k)\right)}{F_norm(k) = min(0.95*F_norm(k-1) + 0.05*A(k), 0.95*F_norm(k-1) + 0.05*sigma(k))}
#'
#' @export
adanorm = function(x, sRate){
  normalized_x = list()
  seconds_idx = seq(1,length(x),sRate)
  fnorm = 1
  for(second_idx in seconds_idx){
    second = x[second_idx:(second_idx+sRate-1)]
    ak = mean(abs(second))
    sigmak = sqrt(sum((second - mean(second))^2) / (length(second) - 1))
    fnorm = min(0.95 * fnorm + 0.05 * ak, 0.95 * fnorm + 0.05 * sigmak)
    normalized_second = second/fnorm
    normalized_x[[(length(normalized_x)+1)]] = normalized_second
  }
  normalized_x = unlist(normalized_x)
  return(normalized_x)
}


#' Clean Oximetry Signal
#'
#' This function processes an oximetry signal vector to remove values below a specified threshold. 
#' It is designed to enhance the quality of oximetry data by replacing sub-threshold impossible values 
#' with the nearest valid data points.
#'
#' @param oximetry A numeric vector representing the oximetry signal. 
#'                 Each element corresponds to an oximetry reading.
#' @param threshold A numeric value setting the minimum acceptable oximetry value. 
#'                  Default is 70. Values in `oximetry` below this threshold will be 
#'                  replaced with the nearest value above the threshold or an average 
#'                  of the nearest valid values on either side.
#'
#' @return A numeric vector of the same length as `oximetry`. 
#'         Sub-threshold values are replaced based on nearby valid readings.
#'
#' @details
#' The function iterates through the `oximetry` vector. For each value below the `threshold`,
#' it searches for the nearest valid value (above the threshold) to the left and right. 
#' If both neighbors are found, it replaces the sub-threshold value with their average. 
#' If only one valid neighbor is found, it uses that value. 
#'
#' The algorithm ensures that the processed signal retains the general pattern of the 
#' original data while mitigating the impact of anomalously low readings.
#'
#' @examples
#' oximetry_data <- c(91, 92, 91, 34, 92, 93, 91)
#' clean_oximetry(oximetry_data)
#'
#' @export
clean_oximetry <- function(oximetry, threshold = 70) {
  if (length(oximetry) < 3) {
    stop("Data vector must have at least 3 elements.")
  }
  
  for (i in 1:length(oximetry)) {
    if (oximetry[i] < threshold) {
      left <- right <- NA
      
      # Search for nearest value over threshold on the left
      for (j in i:1) {
        if (oximetry[j] > threshold) {
          left <- oximetry[j]
          break
        }
      }
      
      # Search for nearest value over threshold on the right
      for (j in i:length(oximetry)) {
        if (oximetry[j] > threshold) {
          right <- oximetry[j]
          break
        }
      }
      
      # Calculate average if both neighbors are found
      if (!is.na(left) && !is.na(right)) {
        oximetry[i] <- mean(c(left, right))
      } else if (!is.na(left)) {
        oximetry[i] <- left
      } else if (!is.na(right)) {
        oximetry[i] <- right
      }
    }
  }
  
  return(oximetry)
}

#' Detect Apneic Events in SpO2 Signal Data
#'
#' This function implements the algorithm described by Jund & Al in "Real-Time Automatic Apneic Event Detection Using Nocturnal Pulse Oximetry", 2018.
#' It analyzes a given SpO2 signal to detect apneic events. It works by resampling the input signal
#' and applying a series of checks to identify potential apnea instances. The algorithm uses a state machine
#' with different blocks representing various stages of detection.
#'
#' @references Jung, D. W., Hwang, S. H., Cho, J. G., Choi, B. H., Baek, H. J., Lee, Y. J., Jeong, D.-U., & Park, K. S. (2018). Real-Time Automatic Apneic Event Detection Using Nocturnal Pulse Oximetry. In IEEE Transactions on Biomedical Engineering (Vol. 65, Issue 3, pp. 706–712). Institute of Electrical and Electronics Engineers (IEEE). https://doi.org/10.1109/tbme.2017.2715405 
#' @param spo2 A numeric vector representing the SpO2 signal data.
#' @param sRate The original sampling rate of the SpO2 signal.
#' @return A list of numeric vectors. Each vector represents a detected apneic event, containing the start and end
#'         indices of the event in the resampled signal.
#' @examples
#' # Example usage
#' spo2_sample <- c(98, 97, 96, 95, 94, 93, 92, 91, 90, 89, 88)
#' sample_rate <- 1  # Assuming 1 Hz sampling rate
#' detected_apneas <- detect_apneic_events(spo2_sample, sample_rate)
#' print(detected_apneas)
#'
#' @export
detect_apneic_events <- function(spo2, sRate) {
  
  spo2 = signal::resample(spo2, 1, sRate)
  
  apneas <- list()
  
  a = 26
  b = 0
  c = 0
  block  = "1"
  while((a <= length(spo2)) & (b <= length(spo2)) & (c <= length(spo2))  ){
    switch(
      block,
      "1" = {
        if((-3<=(spo2[a] - spo2[a-1])) & ((spo2[a] - spo2[a-1])<=-1)){
          b = a + 1
          block = "3"
        } else {
          block = "2"
        }
      },
      "2" = {
        a = a + 1
        block = "1"
      },
      "3" = {
        if(spo2[b] <= spo2[b-1]){
          if((spo2[a]-spo2[b]) >=-3){
            c = b+1
            block = "4"
          }
          else{
            b = b + 1
            block = "3"
          }
        } else {
          block = "2"
        }
      },
      "4" = {
        if(!(((spo2[a]-spo2[c]) <= 1) || ((spo2[c]-spo2[b]) >= 3))){
          block = "5"
        }
        if(!(c-a) >= 10){
          block = "5"
        }
        if(!(c-a) <= 90){
          block = "6"
        }
        apneas[[length(apneas)+1]] = c(a-25,c-25)
        block = "6"
      },
      "5" = {
        c = c + 1
        if(spo2[b] >= spo2[c-1]){
          b = c-1
        }
        block = "5"
        
      },
      "6" = {
        a = c + 1
        block = "1"
      }
      
    )
    
  }
  
  return(apneas)
}