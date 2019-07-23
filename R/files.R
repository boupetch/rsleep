#' Write a European Data Format (EDF) record file to disk using Morpheo Data Format (MDF) guidelines.
#' Target directory is erased if it already exists. Signals are stored in binary file,
#' events and metadata in JavaScript Object Notation (JSON) files.
#'
#' @references P. Bouchequet, D. Jin, G. Solelhac, M. Chennaoui, D. Leger, "Morpheo Data Format (MDF), un nouveau format de données simple, robuste et performant pour stocker et analyser les enregistrements de sommeil", Médecine du Sommeil, vol. 15, n 1, p. 48/49, march 2018.
#' @param edfPath character. European Data Format (EDF) file path.
#' @param mdfPath character. Morpheo Data Format (MDF) directory path.
#' @param channels character. Vector of channels labels to write.
#' @param events dataframe. Events dataframe to write. Events dataframe. Dataframe must contain \code{begin} (\code{POSIXt}), \code{end} (\code{POSIXt}) and \code{event} (\code{character}) columns.
#' @param endian character. Endianess. \code{"big"} or \code{"little"}. Defaults to \code{"little"}.
#' @examples
#' \donttest{
#' download.file("http://cloud.frenchkpi.com/s/65cm6DMq7SYKQ6J/download",
#' paste0(tempdir(),"/15012016HD.edf"))
#' download.file("http://cloud.frenchkpi.com/s/wreGqkitWNnWwnP/download",
#' paste0(tempdir(),"15012016HD.csv"))
#' mdfPath = paste0(tempdir(),"/15012016HD/")
#' channels = c("C3-M2","ECG")
#' events = read_events_noxturnal(paste0(tempdir(),"15012016HD.csv"))
#' write_mdf(paste0(tempdir(),"/15012016HD.edf"),
#'     mdfPath,
#'     channels,
#'     events
#' )
#' }
#' @export
write_mdf <- function(edfPath, mdfPath, channels = c(NA), events = c(), endian="little") {

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
#' @param endian character. Endianess. \code{"big"} or \code{"little"}. Defaults to \code{"little"}.
write_channel <- function(channel, signals, headers, mdfPath, endian="little"){

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

#' Read a Noxturnal events file (Unicode CSV format)
#'
#' @param dir Noxturnal events file path.
#' @return A dataframe of scored events.
#' @export
read_events_noxturnal <- function(dir){

  events <- tryCatch({
    utils::read.csv(dir,
                    fileEncoding = "UTF-8")
  },
  # nocov start
  error = function(e){
    utils::read.csv(dir, fileEncoding = "UTF-16")
  },
  # nocov end
  warning = function(e){
    utils::read.csv(dir,
                    fileEncoding = "UTF-16")
  }
  )

  events <- events[,1:4]
  if(events[1,1][1] == "[]"){
    events <- events[-1,]
  }

  for (i in 1:4){
    if(colnames(events)[i] == "Heure.de.d.but" | colnames(events)[i] == paste0("Heure.de.d","\u00E9","but")){
      colnames(events)[i] <- "begin"
      events$begin <- strptime(events$begin, format = "%d/%m/%Y %H:%M:%S")
    } else if(colnames(events)[i] == "Heure.de.fin") {
      colnames(events)[i] <- "end"
      events$end <- strptime(events$end, format = "%d/%m/%Y %H:%M:%S")
    } else if(colnames(events)[i] == "X.v.nement" | colnames(events)[i] == paste0("\u00C9","v","\u00E9","nement")) {
      colnames(events)[i] <- "event"
      events$event <- as.character(events$event)
    } else if(colnames(events)[i] == "Dur.e" | colnames(events)[i] == paste0("Dur","\u00E9","e")) {
      colnames(events)[i] <- "duration"
      events$duration <- as.numeric(events$duration)
    }
  }

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
  events <- rbind(events,normalize_cycles(events))

  return(events)
}

#' Read a Morpheo Data Format (MDF) directory to a list.
#'
#' @references P. Bouchequet, D. Jin, G. Solelhac, M. Chennaoui, D. Leger, "Morpheo Data Format (MDF), un nouveau format de données simple, robuste et performant pour stocker et analyser les enregistrements de sommeil", Médecine du Sommeil, vol. 15, n 1, p. 48/49, march 2018.
#' @param mdfPath character. MDF path.
#' @param channels character. Channels to read.
#' @param metadata boolean. Read or not the metadata.
#' @return A list.
#' @examples
#' \donttest{
#' download.file("http://cloud.frenchkpi.com/s/65cm6DMq7SYKQ6J/download",
#' paste0(tempdir(),"/15012016HD.edf"))
#' download.file("http://cloud.frenchkpi.com/s/wreGqkitWNnWwnP/download",
#' paste0(tempdir(),"15012016HD.csv"))
#' mdfPath = paste0(tempdir(),"/15012016HD/")
#' channels = c("C3-M2","ECG")
#' events = read_events_noxturnal(paste0(tempdir(),"15012016HD.csv"))
#' write_mdf(paste0(tempdir(),"/15012016HD.edf"),
#'     mdfPath,
#'     channels,
#'     events
#' )
#' mdf <- read_mdf(paste0(tempdir(),"/15012016HD.edf"))
#' }
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


