
#' Write a European Data Format (EDF) record file to disk using Morpheo Data Format (MDF) guidelines
#' 
#' @description Write a European Data Format (EDF) record file to disk using Morpheo Data Format (MDF) guidelines. Target directory is erased if it already exists. Signals are stored in binary file, events and metadata in JavaScript Object Notation (JSON) files.
#' @references P. Bouchequet, D. Jin, G. Solelhac, M. Chennaoui, D. Leger, "Morpheo Data Format (MDF), un nouveau format de données simple, robuste et performant pour stocker et analyser les enregistrements de sommeil", Médecine du Sommeil, vol. 15, n 1, p. 48/49, march 2018.
#' @param edfPath character. European Data Format (EDF) file path.
#' @param mdfPath character. Morpheo Data Format (MDF) directory path.
#' @param channels character. Vector of channels labels to write.
#' @param events dataframe. Events dataframe to write. Events dataframe. Dataframe must contain \code{begin} (\code{POSIXt}), \code{end} (\code{POSIXt}) and \code{event} (\code{character}) columns.
#' @param endian character. Endianess. \code{"big"} or \code{"little"}. Defaults to platform endian.
#' @export
write_mdf <- function(edfPath, mdfPath, channels = c(NA), events = c(), endian=.Platform$endian) {

  # Reset MDF directory
  if(dir.exists(mdfPath)){
    unlink(mdfPath, recursive = TRUE)
    dir.create(mdfPath)
  } else {
    dir.create(mdfPath)
  }

  for(edf in edfPath){
    # Read EDF
    headers <- edfReader::readEdfHeader(edf)
    signals <- edfReader::readEdfSignals(headers)
    # Write each channel
    edfchannels <- headers$sHeaders$label

    if (length(channels) > 0){
      if (!is.na(channels[1])){
        edfchannels <- edfchannels[edfchannels %in% channels]
      }
    } else {
      edfchannels <- c()
    }

    for(channel in edfchannels){
      write_channel(channel, signals, headers, mdfPath, endian = endian)
    }
  }

  # Write metadata
  metadata <- headers
  metadata$sHeaders <- NULL
  metadata <- lapply(metadata, function(x) x[1])
  jsonlite::write_json(metadata,
                       path = paste0(mdfPath,
                                     "/metadata.json"),
                       auto_unbox = TRUE)

  # Write events
  if(length(events > 0)){
    jsonlite::write_json(events,
                         path = paste0(mdfPath,
                                       "/events.json"))
  }
}

#' Write a timeserie to disk using Morpheo Data Format (MDF) guidelines.
#'
#' @references P. Bouchequet, D. Jin, G. Solelhac, M. Chennaoui, D. Leger, "Morpheo Data Format (MDF), un nouveau format de données simple, robuste et performant pour stocker et analyser les enregistrements de sommeil", Médecine du Sommeil, vol. 15, n 1, p. 48-49, march 2018.
#' @param channel character. Channel name.
#' @param signals list. European Data Format (EDF) signals list.
#' @param headers list. European Data Format (EDF) file headers.
#' @param mdfPath character. Morpheo Data Format (MDF) directory path.
#' @param endian character. Endianess. \code{"big"} or \code{"little"}. Defaults to platform endian.
write_channel <- function(channel, signals, headers, mdfPath, endian=.Platform$endian){

  signal <- signals[[channel]]

  if (is.null(signal)){
      # signal <- signals
  }

  if (!is.null(signal)){

    # Create channel directory
    channelPath <- paste0(mdfPath,"/",channel)
    dir.create(channelPath)

    # Write file
    writeBin(signal$signal,
             con = paste0(channelPath,"/data.bin"),
             endian = endian, size = 4)

    # Write metadata
    metadata <- headers$sHeaders[headers$sHeaders$label == channel,]
    jsonlite::write_json(as.list(metadata),
                         path = paste0(channelPath,"/metadata.json"),
                         auto_unbox = TRUE)
  } else {
    warning(
      paste0("Signal ",channel," corrupted.")
    )
  }

}

#' Write a XML file containing scored stages for Compumedics software.
#'
#' @param hypnogram A rsleep hypnogram dataframe.
#' @param filename character File name to write on disk.
#' @export
write_hypnogram_compumedics <- function(hypnogram, filename){

  header <- paste0(
    "<?xml version=\"1.0\" encoding=\"utf-8\"?>",
    "<CMPPSGSCOREDATA><AUTHOR>Compumedics</AUTHOR><CREATEDON>",
    as.character(format(Sys.time(),"%m/%d/%Y %H:%M%:%S")),
    "</CREATEDON><LASTMODIFIEDBY>Compumedics</LASTMODIFIEDBY><LASTMODIFIEDON>",
    as.character(format(Sys.time(),"%m/%d/%Y %H:%M%:%S")),
    "</LASTMODIFIEDON><MODE>1</MODE><COMMENTS>souris1</COMMENTS>",
    "<SCOREDEVENTS/><SLEEPSTAGES>")

  stages <- hypnogram$event
  stages <- ifelse(stages == "NREM", "1",stages)
  stages <- ifelse(stages == "AWA", "10",stages)
  stages <- ifelse(stages == "REM", "2",stages)
  stages <- paste0("<SLEEPSTAGE>",stages,"</SLEEPSTAGE>")
  stages <- paste0(stages,collapse = "")

  footer <- "</SLEEPSTAGES></CMPPSGSCOREDATA>"

  xml <- paste0(header, stages, footer, collapse = "")

  fileConn <- file(filename)
  writeLines(xml, fileConn)
  close(fileConn)

}

#' Read a Noxturnal events file (Unicode CSV format)
#'
#' @param dir Noxturnal events file path.
#' @return A dataframe of scored events.
#' @export
read_events_noxturnal <- function(dir){
  
  suppressWarnings({
  events <- readr::read_csv(
    dir, show_col_types = FALSE,
    col_names = c("begin","end","event","duration"),
    locale = readr::locale(encoding = "UTF-16LE"), skip = 1)
  })
  
  if (ncol(events) < 4) {
    stop(paste0(
      "Noxturnal events file must have at least 4 columns. Number of columns: ",
      ncol(events),"."))
  }

  events <- events[,1:4]
  if(events[1,1][1] == "[]"){
    events <- events[-1,]
  }

  # for (i in 1:4){
  #   if(colnames(events)[i] == "Heure.de.d.but" | colnames(events)[i] == paste0("Heure.de.d","\u00E9","but")){
  #     colnames(events)[i] <- "begin"
       events$begin <- strptime(events$begin, format = "%d/%m/%Y %H:%M:%S")
  #   } else if(colnames(events)[i] == "Heure.de.fin") {
  #     colnames(events)[i] <- "end"
       events$end <- strptime(events$end, format = "%d/%m/%Y %H:%M:%S")
  #   } else if(colnames(events)[i] == "X.v.nement" | colnames(events)[i] == paste0("\u00C9","v","\u00E9","nement")) {
  #     colnames(events)[i] <- "event"
  #     events$event <- as.character(events$event)
  #   } else if(colnames(events)[i] == "Dur.e" | colnames(events)[i] == paste0("Dur","\u00E9","e")) {
  #     colnames(events)[i] <- "duration"
       events$duration <- as.numeric(events$duration)
  #   }
  # }

  events$duration <- NULL
  events$event[events$event == "?veil"] <- paste0("\u00C9","veil")
  events$event[events$event == paste0("\u00C9","veil")] <- "AWA"
  events$event[events$event == "D?but de l'analyse"] <- paste0("D","\u00E9","but de l'analyse")
  events$event[events$event == "Micro-?veil"] <- paste0("Micro-","\u00C9","veil")
  events$event[events$event == "Hypopn?e"] <- paste0("Hypopn","\u00E9","e")
  events$event[events$event == "D?sat"] <- paste0("D","\u00E9","sat")

  if(nrow(events[events$event == paste0("D","\u00E9","but de l'analyse"),]) > 0){
    events <- events[events$begin >= min(events$begin[events$event == paste0("D","\u00E9","but de l'analyse")]),]
  }

  # Normalize events names
  events$event[events$event == paste0("Micro-","\u00C9","veil")] <- "micro-arousal"
  events$event[events$event == paste0("Micro-","\u00C9","veil")] <- "micro-arousal"
  events$event[events$event == "Dos"] <- "back"
  events$event[events$event == "Gauche"] <- "left"
  events$event[events$event == "Droite"] <- "right"
  events$event[events$event == "Ventre"] <- "stomach"

  # Normalize Cycles
  events <- rbind(events,rsleep::normalize_cycles(events))

  events$event = as.character(events$event)
  
  return(events)
}

#' Read a SleepEDFX events file EDF+
#'
#' @param dir EDF+ path
#' @param update merge N3 and N4 or not
#' @return A dataframe of scored events.
#' @export
read_events_sleepedfx <- function(dir, update = TRUE){

  h <- edfReader::readEdfHeader(dir)
  s <- edfReader::readEdfSignals(h)
  events <- s[["annotations"]]
  events$begin <- events$onset + as.numeric(s[["startTime"]])
  events$end <- events$end + as.numeric(s[["startTime"]])
  events$event[events$annotation == "Sleep stage W"] <- "AWA"
  events$event[events$annotation == "Sleep stage 1"] <- "N1"
  events$event[events$annotation == "Sleep stage 2"] <- "N2"
  events$event[events$annotation == "Sleep stage 3"] <- "N3"
  events$event[events$annotation == "Sleep stage 4"] <- "N4"
  events$event[events$annotation == "Sleep stage R"] <- "REM"
  events <- events[,c("begin","end","event")]
  events_final <- utils::head(events,0)
  events <- events[order(events$begin),]
  events$duration <- events$end - events$begin
  events$epochs <- events$duration/30

  begin <- min(events$begin)
  for(i in c(1:nrow(events))){
    for(j in c(1:events[i,]$epochs)){
      end <- begin + 30
      events_final[nrow(events_final)+1,] <- list(begin,end,events[i,]$event)
      begin <- begin + 30
    }
  }

  if(update){
    events_final$event[events_final$event == "N4"] <- "N3"
  }

  events_final$begin <- as.POSIXlt(events_final$begin,origin= "1970-01-01 00:00.00 UTC")
  events_final$end <-  as.POSIXlt(events_final$end,origin= "1970-01-01 00:00.00 UTC")

  return(stats::na.omit(events_final))
}

#' Read a stages export from Compumedics software in .txt format.
#'
#' @param txt txt file path.
#' @param startTime Character string or date object of the hypnogram start.
#' @param labels Labels and values as a named list. Defaults to c("AWA" = 0, "N1" = 1, "N2" = 2, "N3" = 3, "REM" = 5).
#' @return A dataframe of stages.
#' @export
read_events_compumedics <- function(
    txt, startTime = as.POSIXlt("2000-01-01"), labels = c("AWA" = 0, "N1" = 1, "N2" = 2, "N3" = 3, "REM" = 5)){
  hypno <- utils::read.table(txt, stringsAsFactors = FALSE, col.names = "event")
  hypno$begin <- as.POSIXlt(startTime) + ((c(1:nrow(hypno))-1)*4)
  hypno$end <- hypno$begin+4
  hypno$event <- as.character(hypno$event)
  for(label in names(labels)){
    hypno$event <- ifelse(hypno$event == as.character(labels[label]),label,hypno$event)
  }
  # hypno$event <- ifelse(hypno$event == "1","NREM",hypno$event)
  # hypno$event <- ifelse(hypno$event == "2","REM",hypno$event)
  # hypno$event <- ifelse(hypno$event == "?","AWA",hypno$event)
  hypno
}

#' Read a Morpheo Data Format (MDF) directory to a list.
#'
#' @references P. Bouchequet, D. Jin, G. Solelhac, M. Chennaoui, D. Leger, "Morpheo Data Format (MDF), un nouveau format de données simple, robuste et performant pour stocker et analyser les enregistrements de sommeil", Médecine du Sommeil, vol. 15, n 1, p. 48/49, march 2018.
#' @param mdfPath character. MDF path.
#' @param channels character. Channels to read.
#' @param metadata boolean. Read or not the metadata.
#' @return A list.
#' @export
read_mdf <- function(mdfPath, channels = c(NA), metadata = TRUE) {

  # Init list
  mdf <- list()

  # Filter channels to read
  mdfchannels <- list.dirs(mdfPath,full.names = FALSE)[-1]
  if (length(channels) > 0){
    if (!is.na(channels[1])){
      mdfchannels <- mdfchannels[mdfchannels %in% channels]
    }
  } else {
    mdfchannels <- c()
  }

  for (channel in mdfchannels){
    mdf[["channels"]][[channel]][["metadata"]] <- jsonlite::read_json(
      paste0(mdfPath,"/",channel,"/metadata.json"))
    mdf[["channels"]][[channel]][["signal"]] <- readBin(
      con = paste0(mdfPath,"/",channel,"/data.bin"),
      what = "numeric",
      endian = "little",
      n = mdf[["channels"]][[channel]][["metadata"]]$sLength,
      size = 4)
  }

  # Read metadata
  if(metadata){
    metadataPath <- paste0(mdfPath,"/metadata.json")
    if(file.exists(metadataPath)){
      mdf[["metadata"]] <- jsonlite::read_json(metadataPath)
    }
  }

  eventsPath <- paste0(mdfPath,"/events.json")
  if(file.exists(eventsPath)){
    mdf[["events"]] <- jsonlite::read_json(eventsPath,simplifyVector = TRUE)
    mdf[["events"]]$begin <- as.POSIXlt(mdf[["events"]]$begin, origin = "1970-01-01 00:00")
    mdf[["events"]]$end <- as.POSIXlt(mdf[["events"]]$end, origin = "1970-01-01 00:00")
  }

  return(mdf)
}

#' Read a annotation file from Compumedics Profusion software in XML format.
#'
#' @param xml XML file path.
#' @param startTime Character string or date object of the hypnogram start.
#' @return A dataframe of stages and events.
#' @export
read_events_profusion <- function(
    xml, 
    startTime = as.POSIXlt("1970-01-01 00:00:00")){
  
  profusion <- xml2::read_xml(xml)
  profusion <- xml2::as_list(profusion)
  events <- do.call(rbind.data.frame, profusion[["CMPStudyConfig"]][["SleepStages"]])
  colnames(events) <- "event"
  events$event[events$event == 0] <- "AWA"
  events$event[events$event == 1] <- "N1"
  events$event[events$event == 2] <- "N2"
  events$event[events$event == 3] <- "N3"
  events$event[events$event == 4] <- "REM"
  events$event[events$event == 5] <- "REM"
  row.names(events) <- NULL
  epoch_duration <- as.numeric(profusion[["CMPStudyConfig"]][["EpochLength"]][[1]])
  events$begin <- as.POSIXlt(startTime) + (epoch_duration * (c(1:nrow(events))-1))
  events$end <- events$begin + epoch_duration
  
  for(scored_event in profusion[["CMPStudyConfig"]][["ScoredEvents"]]){
    event <- as.character(scored_event["Name"][[1]])
    begin <- startTime + as.numeric(scored_event["Start"][[1]])
    end <- begin + as.numeric(scored_event["Duration"][[1]])
    row <- data.frame("event" = event, "begin" = begin, "end" = end)
    events <- rbind(events,row)
  }
  
  return(events)
}