#' Adaptive normalization for respiratory signals
#'
#' @description Adaptive normalization for respiratory signals as described by Choi & Al (2018) is applied in order to save the part where the amplitude of respiration is small owing to the sleeping posture for a long time.
#' @references Choi, S. H., Yoon, H., Kim, H. S., Kim, H. B., Kwon, H. B., Oh, S. M., Lee, Y. J., & Park, K. S. (2018). Real-time apnea-hypopnea event detection during sleep by convolutional neural networks. In Computers in Biology and Medicine (Vol. 100, pp. 123–131). Elsevier BV. https://doi.org/10.1016/j.compbiomed.2018.06.028 
#' @param x signal vector.
#' @param sRate Sample rate of the signal.
#' @return normalized signal.
#' @export
adaptive_normalization = function(x, sRate){
  normalized_x = list()
  seconds_idx = seq(1,length(x),sRate)
  fnorm = 0
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