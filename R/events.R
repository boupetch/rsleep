#' Check events dataframe format compliance.
#'
#' @param events Events dataframe. Dataframe must contain \code{begin} (\code{POSIXt}), \code{end} (\code{POSIXt}) and \code{event} (\code{character}) columns.
#' @return Boolean, according to the events dataframe format compliance.
#' @examples
#' events <- data.frame(begin = as.POSIXct(c(1536967800,1536967830,1536967860), origin = "1970-01-01"))
#' events$end <- as.POSIXct(c(1536967830,1536967860,1536967890), origin = "1970-01-01")
#' events$event = c("N3","N3","REM")
#' check_events(events)
check_events <- function(events){

  if(!("begin" %in% colnames(events))){

    stop("Events dataframe must contain a 'begin' column.")

  } else if(!("end" %in% colnames(events))){

    stop("Events dataframe must contain a 'end' column.")

  } else  if(!("event" %in% colnames(events))){

    stop("Events dataframe must contain a 'event' column.")

  } else if(!("POSIXt" %in% class(events$begin))){

    stop("'begin' column must be a datetime.")

  } else if(!("POSIXt" %in% class(events$end))){

    stop("'end' column must be a datetime.")

  } else if(!("character" %in% class(events$event))){

    stop("'events' column must be character type.")

  }
}

#' Normalize sleep cycles scored on Noxturnal software from start and stop flags to unique events.
#'
#' @param events Events dataframe. Dataframe must have \code{begin} (\code{POSIXt}), \code{end} (\code{POSIXt}) and \code{event}. Cycles flags must be named \code{Activity-CLASSICstart}, \code{Activity-BNstart}, \code{Activity-BNend}, \code{Activity-REMstart}, \code{Activity-REMend}, \code{Activity-ENstart} or \code{Activity-ENend}.
#' @examples
#' cycles <- data.frame(event = c("Activity-CLASSICstart","Activity-CLASSICend"))
#' cycles$begin <- as.POSIXct(c("2016-01-16 01:13:30","2016-01-16 01:15:30"))
#' cycles$end <- as.POSIXct(c("2016-01-16 01:13:30","2016-01-16 01:15:30"))
#' normalize_cycles(cycles)
#' @export
normalize_cycles <- function(events){
  cycles_labels <- list(
    c("Activity-CLASSICstart","Activity-CLASSICend"),
    c("Activity-BNstart","Activity-BNend"),
    c("Activity-REMstart","Activity-REMend"),
    c("Activity-ENstart","Activity-ENend")
  )
  cycles_raw <- events[events$event %in% unlist(cycles_labels),]
  cycles <- events[0,]
  if(nrow(cycles_raw) >= 2){
    cycles_raw <- cycles_raw[order(cycles_raw$begin),]
    cycles_raw$used <- FALSE
    for(cycles_pairs in cycles_labels){
      for(i in c(1:nrow(cycles_raw))){
        if(cycles_raw$event[i] == cycles_pairs[1]){
          ends <- cycles_raw[cycles_raw$used == FALSE & cycles_raw$begin > cycles_raw$begin[i] & cycles_raw$event == cycles_pairs[2],]
          end <- ends[ends$begin == min(ends$begin),]
          begin <- cycles_raw[i,]
          if(nrow(begin) == 1 & nrow(end) == 1){
            event <- paste0("cycle-",gsub("Activity-","",gsub("start","",begin$event)))
            begin <- begin$begin
            end <- end$end
            cycle_df <- data.frame("begin" = begin,"end" = end,"event" = as.character(event))
            cycle_df$event <- as.character(event)
            cycles <- rbind(cycles,cycle_df)
          }
        }
      }
    }
  }
  return(cycles)
}

#' Plot a hypnogram from an events dataframe.
#'
#' @description Plot a hypnogram from an events dataframe.
#' @param events Events dataframe. Dataframe must have \code{begin} (\code{POSIXt}), \code{end} (\code{POSIXt}) and \code{event}
#' @param labels Sleep stages labels. Defaults to \code{c("N3","N2","N1","REM","AWA")}.
#' @return a ggplot object.
#' @examples
#' hypnogram <- data.frame(begin = as.POSIXlt(
#' c(1536967800,1536967830,1536967860),origin = "1970-01-01"))
#' hypnogram$end <- as.POSIXlt(c(1536967830,1536967860,1536967890), 
#' origin = "1970-01-01")
#' hypnogram$event = c("N3","N3","REM")
#' plot_hypnogram(hypnogram)
#'
#' fpath <- paste0(tempdir(),"SC4001EC-Hypnogram.edf")
#' furl <- paste0("https://www.physionet.org/files/sleep-edfx/1.0.0/",
#' "sleep-cassette/SC4001EC-Hypnogram.edf?download")
#' download.file(furl,fpath)
#' hypnogram <- read_events_sleepedfx(fpath)
#' unlink(fpath)
#' plot_hypnogram(hypnogram)
#' @export
plot_hypnogram <- function(events, labels = c("N3","N2","N1","REM","AWA")){

  stages <- hypnogram(events, labels)

  stages$begin <- as.POSIXct(stages$begin)
  stages$end <- as.POSIXct(stages$end)

  hypnogram <- ggplot2::ggplot(
    stages,
    ggplot2::aes_string(
      x="begin",
      y="event",
      group=1)) +
    ggplot2::geom_line() +
    ggplot2::theme_bw() +
    ggplot2::xlab("") +
    ggplot2::ylab("")

  rem = stages[stages$event == "REM",]

  if(nrow(rem) > 0){
    for(i in c(1:nrow(rem))){
      df <- stats::reshape(
        rem[i,],
        idvar = "event",
        varying = c("begin","end"),
        v.names = "value", direction = "long")
      hypnogram <- hypnogram+ggplot2::geom_line(
        data=df,
        mapping = ggplot2::aes_string(
          x="value",y="event",group=1),colour='red')
    }
  }

  return(hypnogram)
}

#' Filter and reorder an events dataframe to keep only sleep stages related-events.
#'
#' @description Remove non-sleep stages events and reorder dataframe rows using the \code{begin} column.
#' @param events Events dataframe. Dataframe must have \code{begin} (\code{POSIXt}), \code{end} (\code{POSIXt}) and \code{event}
#' @param labels Sleep stages labels. Defaults to \code{c("N3","N2","N1","REM","AWA")}.
#' @return hypnogram dataframe.
#' @examples
#' e <- data.frame(begin = as.POSIXlt(c(1536967800,1536967860,1536967830),origin = "1970-01-01"))
#' e$end <- as.POSIXlt(c(1536967830,1536967890,1536967860), origin = "1970-01-01")
#' e$event = c("back-position","N3","REM")
#' hypnogram(e)
#' @export
hypnogram <- function(events, labels = c("N3","N2","N1","REM","AWA")){
  check_events(events)
  stages <- events[events$event %in% labels,]
  stages$event <- factor(stages$event, levels = labels)
  stages <- stages[order(stages$begin),]
  return(stages)
}

#' Plot a hypnodensity graph using `ggplot2`.
#'
#' @description Plot a hypnodensity graph using `ggplot2` from the values returned from `score_stages_edf` function.
#' @references Stephansen, J.B., Olesen, A.N., Olsen, M., Ambati, A., Leary, E.B., Moore, H.E., Carrillo, O., Lin, L., Han, F., Yan, H. and Sun, Y.L., 2018. Neural network analysis of sleep stages enables efficient diagnosis of narcolepsy. Nature communications, 9(1), p.5229.
#' @param hypnodensity A hypnodensity dataframe as returned by the `score_stages_edf` function.
#' @param stages Vector of stages labels to plot.
#' @return A `ggplot2` hypnodensity graph.
#' @export
plot_hypnodensity <- function(hypnodensity,
                              stages = c("AWA","REM","N1","N2","N3")){

  pal <- c("#5BBCD6", "#FF0000", "#00A08A", "#F2AD00", "#F98400")

  if(length(stages) == 3){
    pal <- c("#5BBCD6","#F2AD00","#FF0000")
  }

  melt <- stats::reshape(data = hypnodensity,
                         direction = "long",
                         varying = 1:length(stages),
                         idvar='begin',
                         timevar = "stage",
                         v.names = "likelihood",
                         times = stages,
                         sep="")

  row.names(melt) <- NULL

  melt$stage <- factor(melt$stage, levels = stages)

  ggplot2::ggplot(melt, ggplot2::aes_string(x = "begin",
                                            y= "likelihood",
                                            fill = "stage")) +
    ggplot2::geom_area(position = 'stack') +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom",
                   legend.title = ggplot2::element_blank()) +
    ggplot2::xlab("") + ggplot2::ylab("Stage likelihood") +
    ggplot2::scale_fill_manual(values = pal)
}

#' Smooth hypnogram epoch, simulating human scorers behaviour.
#'
#' @description Smooth hypnograms epoch, simulating human scorers behaviour.
#' @references Liang, Sheng-Fu, Chin-En Kuo, Yu-Han Hu, Yu-Hsiang Pan, and Yung-Hung Wang. "Automatic stage scoring of single-channel sleep EEG by using multiscale entropy and autoregressive models." IEEE Transactions on Instrumentation and Measurement 61, no. 6 (2012): 1649-1657.
#' @param hypnogram A hypnogram dataframe.
#' @param event Central stage label.
#' @param neighbors Extremities stages labels.
#' @param count Number of consecutive central stages.
#' @return A hypnogram dataframe.
#' @examples
#' hypnogram <- data.frame(begin = as.POSIXlt(
#' c(1536967800,1536967830,1536967860),origin = "1970-01-01"))
#' hypnogram$end <- as.POSIXlt(c(1536967830,1536967860,1536967890), 
#' origin = "1970-01-01")
#' hypnogram$event = c("REM","N2","REM")
#' smooth_hypnogram(hypnogram, "N2","REM",1)
#' @export
smooth_hypnogram <- function(
  hypnogram,
  event = "N2",
  neighbors = "REM",
  count = 2){

  for(i in c((1+count):(nrow(hypnogram)-count))){
    if(hypnogram$event[i-1] == neighbors &&
       hypnogram$event[i+count] == neighbors &&
       all(hypnogram$event[c(i):c(i+count-1)] == event)){
      hypnogram$event[c(i):c(i+count-1)] <- neighbors
    }
  }
  hypnogram
}

#' Smooth hypnogram according to the 11 rules described by Liang & Al.
#'
#' @description Smooth hypnogram according to the 11 rules described by Liang & Al.
#' @references Liang, Sheng-Fu, Chin-En Kuo, Yu-Han Hu, and Yu-Shian Cheng. “A Rule-Based Automatic Sleep Staging Method.” Journal of Neuroscience Methods 205, no. 1 (March 2012): 169–76. https://doi.org/10.1016/j.jneumeth.2011.12.022.
#' @param hypnogram A hypnogram dataframe.
#' @return A smoothed hypnogram dataframe.
#' @export
smooth_liang2012 <- function(hypnogram){

  # Rule 1: Any REM epochs before the very first appearance of S2 are replaced
  # with S1 epochs.
  hypnogram$event[hypnogram$event == "REM" &&
                    hypnogram$begin < min(hypnogram$begin[hypnogram$event == "N2"])] <- "N1"

  # Rule 2: Wake, REM, S2 -> Wake, S1, S2
  for(i in c(1:(nrow(hypnogram)-2))){
    if(all(hypnogram$event[i:(i+2)] == c("AWA","REM","N2"))){
      hypnogram$event[i:(i+2)] <- c("AWA","N1","N2")
    }
  }

  # Rule 3: S1, REM, S2 -> S1, S1, S2
  for(i in c(1:(nrow(hypnogram)-2))){
    if(all(hypnogram$event[i:(i+2)] == c("N1","REM","N2"))){
      hypnogram$event[i:(i+2)] <- c("N1","N1","N2")
    }
  }

  # Rule 4: S2, S1, S2 ->  S2, S2, S2
  hypnogram <- smooth_hypnogram(hypnogram, "N1", "N2", 1)

  # Rule 5: S2, SWS, S2 -> S2, S2, S2
  hypnogram <- smooth_hypnogram(hypnogram, "N3", "N2", 1)

  # Rule 6: S2, REM, S2 -> S2, S2, S2
  hypnogram <- smooth_hypnogram(hypnogram, "REM", "N2", 1)

  # Rule 7: SWS, S2, SWS -> SWS, SWS, SWS
  hypnogram <- smooth_hypnogram(hypnogram, "N2", "N3", 1)

  # Rule 8: REM, Wake, REM -> REM, REM, REM
  hypnogram <- smooth_hypnogram(hypnogram, "AWA", "REM", 1)

  # Rule 9: REM, S1, REM -> REM, REM, REM
  hypnogram <- smooth_hypnogram(hypnogram, "N1", "REM", 1)

  # Rule 10: REM, S2, REM -> REM, REM, REM
  hypnogram <- smooth_hypnogram(hypnogram, "N2", "REM", 1)

  # Rule 11: Mov, REM, S2 -> Mov, S1, S2
  for(i in c(1:(nrow(hypnogram)-2))){
    if(all(hypnogram$event[i:(i+2)] == c("MOV","REM","N2"))){
      hypnogram$event[i:(i+2)] <- c("MOV","N1","N2")
    }
  }

  hypnogram
}
