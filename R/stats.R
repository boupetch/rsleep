#' Check events dataframe.
#'
#' @param e Events dataframe. Dataframe must have \code{begin} (\code{POSIXt}), \code{end} (\code{POSIXt}) and \code{event} (\code{character}) columns.
check_events <- function(e){
  if(!("begin" %in% colnames(e))){
    stop("Events dataframe must contain a 'begin' column.")
  } else if(!("end" %in% colnames(e))){
    stop("Events dataframe must contain a 'end' column.")
  } else  if(!("event" %in% colnames(e))){
    stop("Events dataframe must contain a 'event' column.")
  } else if(!("POSIXt" %in% class(e$begin))){
    stop("'begin' column must be a datetime.")
  } else if(!("POSIXt" %in% class(e$end))){
    stop("'end' column must be a datetime.")
  } else if(!("character" %in% class(e$event))){
    stop("'events' column must be character type.")
  }
}

#' Get stages events related stats in a named vector.
#'
#' \code{stages_stats} computes stages related stats.
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

  # Time To Sleep (TTS)
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
