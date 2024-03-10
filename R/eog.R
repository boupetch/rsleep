#' Detection of Rapid-Eye Movements (REMs)
#'
#' Implements the algorithm detailed in Agarwal &Al. "Detection of Rapid-Eye Movements in Sleep Studies."
#' This function processes electrooculography (EOG) signals to detect rapid-eye 
#' movements (REMs) characteristic of REM sleep, applying filters, artifact detection, and angle-based 
#' inclusion criteria.
#'
#' @param roc Right outer canthus EOG signal vector.
#' @param loc Left outer canthus EOG signal vector.
#' @param sRate Sampling rate of the EOG signals in Hz.
#' @param l Window length in seconds for REM detection (default is 0.5).
#' @param art Artifact threshold, max amplitude allowed in EOG signals to consider the data valid (default is 500).
#' @param nip Negative Inflexion Point threshold for REM detection (default is 120).
#' @param cc Correlation coefficient threshold for inclusion of a REM event. Negative correlation 
#'   indicates potential REM (default is -0.2).
#' @param da Desired angle for REM detection (default is 45 degrees).
#' @param dadiff Acceptable deviation from the desired angle for one eye, if the other eye compensates 
#'   with a larger deviation (default is 15 degrees).
#'
#' @details The function processes the EOG signals by applying a band-pass Butterworth filter to isolate 
#'   frequencies between 1 and 5 Hz, typical for REMs. It then computes the artifact measure and evaluates 
#'   the signal for REM events based on the slope of the EOG signal segments, correlation between left and 
#'   right signals, and other criteria derived from the REM detection algorithm described by Agarwal et al.
#'   The function returns a list containing filtered EOG signals, artifact measures, and detected REM events
#'   with their characteristics and validity based on the algorithm's criteria.
#'
#' @return A list with the following elements:
#'   - `rocf`: Filtered right outer canthus EOG signal.
#'   - `locf`: Filtered left outer canthus EOG signal.
#'   - `block_art`: Maximum absolute amplitude in the EOG channels for the detection block, used for artifact measurement.
#'   - `cpm`: Product of inverted left and right EOG signals, part of REM detection criteria.
#'   - `cn`: Conditioned signal based on `cpm`, with values below a threshold set to 0.
#'   - `crems`: Data frame of candidate REMs with indices, characteristics, and validity flag.
#'   - `rems`: Subset of `crems` containing only the valid REM events.
#'
#' @references
#' Agarwal, R., Takeuchi, T., Laroche, S., & Gotman, J. (2005). Detection of Rapid-Eye Movements in 
#' Sleep Studies. IEEE Transactions on Biomedical Engineering, 52(8), 1390–1396. 
#' https://doi.org/10.1109/TBME.2005.851512
#'
#' @import signal
#' @importFrom dplyr bind_rows
#' @export
detect_rem = function(
    roc,
    loc,
    sRate,
    l = 0.5,
    art = 500,
    nip = 120,
    cc = -0.2,
    da = 45,
    dadiff = 15){
  
  result = list()
  
  # Filter EEGs
  low_freq <- 1
  high_freq <- 5
  nyquist <- sRate / 2
  low_norm <- low_freq / nyquist
  high_norm <- high_freq / nyquist
  butter_filter <- signal::butter(5, c(low_norm, high_norm), type = "pass")
  rocf <- signal::filtfilt(butter_filter, roc)
  locf <- signal::filtfilt(butter_filter, loc)
  
  result[["rocf"]] = rocf
  result[["locf"]] = locf
  
  # Artifact Measure (ART). This is defined as the maximum absolute amplitude
  # in the two EOG channels for each detection block
  block_art = max(c(max(abs(roc)),max(abs(loc))))
  result[["block_art"]] = block_art
  
  #C prime m
  cpm = -locf*rocf
  result[["cpm"]] = cpm
  
  cn <- ifelse(cpm > 10, cpm, 0)
  result[["cn"]] = cn
  from = 1
  to = length(cn)-l*sRate*2
  by = l*sRate*2
  idxw <- seq(from, to, by = by)
  
  crems = list()
  
  phi <- function(x) {
    y = seq(length(x))
    N <- length(x)
    sum_xi_yi <- sum(x * y)
    sum_xi <- sum(x)
    sum_yi <- sum(y)
    sum_xi2 <- sum(x^2)
    m <- (N * sum_xi_yi - sum_xi * sum_yi) / (N * sum_xi2 - sum_xi^2)
    return(atan(m)*(180/pi))}
  
  for(idxs in idxw){
    
    wcn = cn[idxs:(idxs+l*sRate*2-1)]
    peakidx = which.max(wcn)
    
    if(wcn[peakidx] > 0){
      
      peakidx = peakidx+idxs-1
      locw = loc[idxs:(idxs+l*sRate*2-1)]
      rocw = roc[idxs:(idxs+l*sRate*2-1)]
      locr = loc[(peakidx-0.2*sRate):(peakidx-1)]
      locl = loc[peakidx:(peakidx+0.2*sRate-1)]
      rocr = roc[(peakidx-0.2*sRate):(peakidx-1)]
      rocl = roc[peakidx:(peakidx+0.2*sRate-1)]
      
      phill = phi(locl)
      philr = phi(locr)
      phirl = phi(rocl)
      phirr = phi(rocr)
      
      phil = abs(phill-philr)
      phir = abs(phirl-phirr)
      
      # Inclusion rules
      
      # Angle
      
      if((phil>da) & (phir>da)){
        valid = 1
      } else if((phil>(da-dadiff)) &(phir>(da+dadiff))){
        valid = 1
      } else if ((phir>(da-dadiff)) & (phil>(da+dadiff))){
        valid = 1
      } else {
        valid = 0
      }
      
      # Correlation
      localcc = stats::cor(locw,rocw)
      if(localcc > cc){
        valid = 0
      }
      
      # Power
      if(block_art > art){
        valid = 0
      }
      
      # Nip
      if(cn[peakidx] < cc){
        valid = 0
      }
      
      crems[[length(crems)+1]] = list(
        index=peakidx,
        nip=cn[peakidx],
        cc = localcc,
        phill = phill,
        philr = philr,
        phirl = phirl,
        phirr = phirr,
        phil = phil,
        phir = phir,
        valid = valid
      )
    }
  }
  result[["crems"]] = dplyr::bind_rows(crems)
  if(nrow(result[["crems"]]) > 0){
    result[["rems"]] = result[["crems"]][result[["crems"]]$valid == 1,]
  } else {
    result[["rems"]] = result[["crems"]]
  }
  
  
  return(result)
  
}