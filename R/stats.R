#' Get stages related statistics in a named vector.
#'
#' \code{stages_stats} computes stages related statistics.
#'
#' @param e Events dataframe. Dataframe must have \code{begin} (\code{POSIXt}), \code{end} (\code{POSIXt}) and \code{event} (\code{character}) columns.
#' @return stages vector
#' @examples
#' e <- data.frame(begin = as.POSIXlt(seq(from = 0, to = 30*10, by = 30),origin = "1970-01-01"))
#' e$end <- as.POSIXlt(seq(from = 30, to = 30*11, by = 30), origin = "1970-01-01")
#' e$event = c("AWA","N1","N2","N3","N3","REM","N2","REM","N2","REM","AWA")
#' stages_stats(e)
#' @export
stages_stats <- function(e){

  # Check events dataframe
  check_events(e)

  # Stages duration
  r = c("rem_duration" = sum(as.numeric(difftime(e$end[e$event == "REM"],e$begin[e$event == "REM"],units="min"))))
  r = c(r, "n1_duration" = sum(as.numeric(difftime(e$end[e$event == "N1"],e$begin[e$event == "N1"],units="min"))))
  r = c(r, "n2_duration" = sum(as.numeric(difftime(e$end[e$event == "N2"],e$begin[e$event == "N2"],units="min"))))
  r = c(r, "n3_duration" = sum(as.numeric(difftime(e$end[e$event == "N3"],e$begin[e$event == "N3"],units="min"))))
  r = c(r, "awa_duration" = sum(as.numeric(difftime(e$end[e$event == "AWA"],e$begin[e$event == "AWA"],units="min"))))

  # Time To Sleep (TTS)
  r = c(r, "tts" = sum(as.numeric(difftime(e$end[e$event %in% c("N1", "N2", "N3", "REM")],e$begin[e$event  %in% c("N1", "N2", "N3", "REM")],units="min"))))

  # Time To Sleep (TTS) by stage
  r = c(r, "rem_tts" = ifelse(r[["tts"]] == 0, 0, r[["rem_duration"]]/r[["tts"]]))
  r = c(r, "n1_tts" = ifelse(r[["tts"]] == 0, 0, r[["n1_duration"]]/r[["tts"]]))
  r = c(r, "n2_tts" = ifelse(r[["tts"]] == 0, 0, r[["n2_duration"]]/r[["tts"]]))
  r = c(r, "n3_tts" = ifelse(r[["tts"]] == 0, 0, r[["n3_duration"]]/r[["tts"]]))
  r = c(r, "awa_tts" = ifelse(r[["tts"]] == 0, 0, r[["awa_duration"]]/r[["tts"]]))

  # TSP Total Sleep Period
  r = c(r, "tsp" = as.numeric(difftime(max(e$end), min(e$begin), units="mins")))

  # Sleep efficiency
  r = c(r, "efficiency" = ifelse(r[["tsp"]] == 0, 0, r[["tts"]]/r[["tsp"]]))

  # Latencies
  sleep <- e[e$event %in% c("N1","N2","N3","REM"),]
  if(nrow(sleep) > 0){
    r = c(r, "latency" = as.numeric(difftime(min(sleep$begin),min(e$begin),units="mins")))
  } else {
    r = c(r, "latency" = NA)
  }

  # Stages latencies
  for(s in c("N1", "N2", "N3", "REM")){

    ss <- e[e$event == s,]

    if(nrow(ss) > 0){
      start <- min(ss$begin)

      r[[paste0(tolower(s),"_latency")]] <- as.numeric(difftime(start, min(e$begin), units="mins")) - r[["latency"]]
    }
  }

  # WASO
  r[["waso"]] <- r[["tsp"]] - r[["latency"]] - r[["tts"]]

  r
}

#' Compute TST90, the percentage of time during sleep with an oxygen saturation below 90.
#' @param spo2_signal The SpO2 signal vector.
#' @param sRate The SpO2 signal vector sample rate.
#' @param startTime The SpO2 signal start time.
#' @param hypnogram Events dataframe containing hypnogram.
#' @export
tst90 <- function(
    spo2_signal, 
    sRate,
    startTime,
    hypnogram){
  periods <- rsleep::periods(hypnogram)
  u90 <- 0
  for(i in c(1:nrow(periods))){
    x <- as.numeric(difftime(periods[i,]$begin, startTime, units="secs"))
    y <- as.numeric(difftime(periods[i,]$end, startTime, units="secs"))
    signal <- spo2_signal[(x*sRate):(y*sRate)]
    u90 <- u90 + (length(signal[signal<90])/sRate)
  }
  return(u90/sum(periods$duration))
}

#' Computes Cohen's Kappa for agreement in the case of 2 raters.
#' @description Cohen’s kappa coefficient value is a robust statistical measure of inter-rater agreement published in 1960 by Jacob Cohen. It has been reused by numerous studies in sleep medicine to measure the accuracy of predictions, especially for automatic sleep staging. 
#' @param observed The vector of observed values (truth).
#' @param predicted The vector of predicted values.
#' @references Cohen J. A Coefficient of Agreement for Nominal Scales. Educational and Psychological Measurement. 1960;20:37-46. 
#' @examples 
#' observed = c("AWA", "N1", "N2", "N3", "REM")
#' predicted = c("AWA", "AWA", "N2", "N3", "REM")
#' ckappa(observed, predicted)
#' @export
ckappa <- function(observed, predicted){

  # calculate the number of observations
  n <- length(observed)
  
  # calculate the number of agreements
  agreements <- sum(observed == predicted)
  
  # calculate the expected number of agreements
  tab <- table(observed, predicted)
  p_o <- prop.table(tab,1)
  p_p <- prop.table(tab,2)
  p_e <- p_o %*% t(p_p)
  p_e <- p_e*n
  
  # calculate Cohen's Kappa
  #kappa <- (agreements - sum(p_e)) / (n - sum(p_e))
  kappa <- sum(agreements - p_e) / sum(n - p_e)
  
  kappa
}