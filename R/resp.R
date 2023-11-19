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