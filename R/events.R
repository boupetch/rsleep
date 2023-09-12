#' Check events dataframe format compliance.
#'
#' @param events Events dataframe. Dataframe must contain \code{begin} (\code{POSIXt}), \code{end} (\code{POSIXt}) and \code{event} (\code{character}) columns.
#' @return Boolean, according to the events dataframe format compliance.
#' @examples
#' events <- data.frame(begin = as.POSIXct(c(1536967800,1536967830,1536967860), origin = "1970-01-01"))
#' events$end <- as.POSIXct(c(1536967830,1536967860,1536967890), origin = "1970-01-01")
#' events$event = c("N3","N3","REM")
#' rsleep::check_events(events)
#' @export
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
#' @export
plot_hypnogram <- function(events, labels = c("N3","N2","N1","REM","AWA")){
  
  events$event = as.character(events$event)
  
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

#' Filter and reorder an events dataframe or a hypnodensity to keep only sleep stages related-events.
#'
#' @description Remove non-sleep stages events and reorder dataframe rows using the \code{begin} column.
#' @param events Events dataframe. Dataframe must have \code{begin} (\code{POSIXt}), \code{end} (\code{POSIXt}) and \code{event}
#' @param labels Sleep stages labels. Defaults to \code{c("N3","N2","N1","REM","AWA")}.
#' @param startTime Hypnogram start time. Used when a hypnodensity dataframe is passed as events. Defaults to 946681200.
#' @param epoch_duration Epoch duration in seconds. Used when a hypnodensity dataframe is passed as events. Defaults to 30.
#' @param plot Plot the hypnogram or in not using \code{ggplot2}.
#' @return Hypnogram dataframe or plot.
#' @examples
#' fpath <- paste0(tempdir(),"SC4001EC-Hypnogram.edf")
#' 
#' furl <- paste0("https://www.physionet.org/files/sleep-edfx/1.0.0/",
#'  "sleep-cassette/SC4001EC-Hypnogram.edf?download")
#'  
#' download.file(furl,fpath)
#' 
#' events <- read_events_sleepedfx(fpath)
#' 
#' unlink(fpath)
#' 
#' hypnogram(events, plot = TRUE)
#' @export
hypnogram <- function(
    events,
    labels = c("N3", "N2", "N1", "REM", "AWA"),
    startTime = 946681200,
    epoch_duration = 30,
    plot = FALSE){
  
  if(all(colnames(events) %in% labels)){
    startTime <- as.POSIXct(946681200, origin = "1970-01-01")
    stages <- apply(events, 1, function(x) names(which.max(x)))
    events <- data.frame(
      "event" = stages,
      "begin" = startTime + (c(1:length(stages))*epoch_duration-epoch_duration))
    events$end <- events$begin+epoch_duration
  }
  
  check_events(events)
  
  stages <- events[events$event %in% labels,]
  stages$event <- factor(stages$event, levels = labels)
  stages <- stages[order(stages$begin),]
  
  if(plot == FALSE){
    
    return(stages)
    
  } else {
    
    stages$begin <- as.POSIXct(stages$begin)
    stages$end <- as.POSIXct(stages$end)
    
    hypnogram <- ggplot2::ggplot(
      stages,
      ggplot2::aes_string(
        x = "begin", 
        y = "event",
        group = 1)) + 
      ggplot2::geom_line() + 
      ggplot2::theme_bw() + 
      ggplot2::xlab("") + 
      ggplot2::ylab("")
    
    rem = stages[stages$event == "REM", ]
    
    if (nrow(rem) > 0) {
      for (i in c(1:nrow(rem))) {
        df <- stats::reshape(
          rem[i, ],
          idvar = "event",
          varying = c("begin", "end"), v.names = "value",
          direction = "long")
        hypnogram <- hypnogram + 
          ggplot2::geom_line(data = df, mapping = ggplot2::aes_string(x = "value", y = "event", 
                                                                      group = 1), colour = "red")}}
    
    return(hypnogram)
  }
}

#' Plot a hypnodensity graph.
#'
#' @description Plot a hypnodensity graph using `ggplot2`. Hypnodensity can be read from file or returned by the `score_stages_edf` function.
#' @references Stephansen, J.B., Olesen, A.N., Olsen, M., Ambati, A., Leary, E.B., Moore, H.E., Carrillo, O., Lin, L., Han, F., Yan, H. and Sun, Y.L., 2018. Neural network analysis of sleep stages enables efficient diagnosis of narcolepsy. Nature communications, 9(1), p.5229.
#' @param hypnodensity A hypnodensity dataframe as returned by the `score_stages_edf` function.
#' @param stages Vector of stages labels to plot.
#' @param colors Vector of colors to use.
#' @return A `ggplot2` hypnodensity graph.
#' @examples
#' download.file("https://rsleep.org/data/hypnodensity.csv", "hypnodensity.csv")
#' 
#' hypnodensity <- read.csv2("hypnodensity.csv")
#' 
#' unlink("hypnodensity.csv")
#' 
#' plot_hypnodensity(hypnodensity)
#' @export
plot_hypnodensity <- function(hypnodensity,
                              stages = c("AWA","REM","N1","N2","N3"),
                              colors = c("#5BBCD6", "#FF0000", "#00A08A", "#F2AD00", "#F98400")){

  pal <- colors

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

#' Get a dataframe of sleep periods from a hypnogram, continuous or by stages.
#'
#' @param hypnogram A hypnogram dataframe. Dataframe must contain \code{begin} (\code{POSIXt}), \code{end} (\code{POSIXt}) and \code{event} (\code{character}) columns.
#' @param mode Period mode. \code{"continuous"} computes periods of N1, N2, N3 or REM sleep, regardless of stage. \code{"stages"} computes periods of sleep by stage.
#' @param stages Stages to include in periods. Defaults to `c("N1", "N2", "N3", "N4", "REM")`.
#' @return A dataframe of periods with their begin and stop times, duration and stages for stage mode.
#' @export
#' @examples
#' library(ggplot2)
#' 
#' download.file(
#'  "https://rsleep.org/data/hypnodensity.csv",
#'  "hypnodensity.csv")
#'
#' hypnodensity <- read.csv2("hypnodensity.csv")
#' 
#' unlink("hypnodensity.csv")
#' 
#' events <- hypnogram(hypnodensity)
#' 
#' periods_continuous <- periods(events, mode = "continuous")
#' 
#' ggplot(periods_continuous, aes(x=duration)) + geom_histogram(bins = 30)
#' 
#' periods_stages <- periods(events, mode = "stages")
#'  
#' ggplot(periods_stages, aes(x=event,y=duration,color=event)) + geom_boxplot()
periods <- function(
    hypnogram,
    mode = "continuous",
    stages = c("N1", "N2", "N3", "N4", "REM")){
  
  # Check mode parameter
  modes <- c("continuous", "stages")
  if(!(mode %in% modes)){
    stop("Mode must be continuous or stages")}
  
  # Clean and order hypnogram
  hypnogram$event <- as.character(hypnogram$event)
  hypnogram <- hypnogram[hypnogram$event %in% stages,]
  if(mode == "continuous"){
    hypnogram$event = "SLEEP"}
  
  # Init data.frame
  periods <- hypnogram[1,]
  
  # Iterate through hypnogram
  for(i in c(2:nrow(hypnogram))){
    if((hypnogram$event[i] == hypnogram$event[i-1]) & (hypnogram$end[i-1] == hypnogram$begin[i])){
      periods$end[length(periods$end)] <- hypnogram$end[i]
    } else {
      periods <- rbind(periods, hypnogram[i,])
    }
  }
  if(mode == "continuous"){
    periods$event <- NULL}
  periods$duration = as.numeric(difftime(periods$end, periods$begin, units="secs"))
  return(periods)
}

#' Count and format stages transitions.
#'
#' @references Swihart BJ, Punjabi NM, Crainiceanu CM. Modeling sleep fragmentation in sleep hypnograms: An instance of fast, scalable discrete-state, discrete-time analyses. Comput Stat Data Anal. 2015 Sep;89:1-11. doi: 10.1016/j.csda.2015.03.001. PMID: 27182097; PMCID: PMC4865264.
#' @param hypnogram A hypnogram dataframe. Dataframe must contain \code{begin} (\code{POSIXt}), \code{end} (\code{POSIXt}) and \code{event} (\code{character}) columns.
#' @param stages Stages to include in transitions Defaults to \code{c("N1", "N2", "N3", "N4", "REM")}.
#' @param format Set the return format. 'vector', 'dataframe' or 'heatmap'.
#' @return Count of stages transitions in selected format.
#' @export
#' @examples
#' download.file("https://rsleep.org/data/hypnodensity.csv", "hypnodensity.csv")
#' 
#' hypnodensity <- read.csv2("hypnodensity.csv")
#' 
#' unlink("hypnodensity.csv")
#' 
#' events <- hypnogram(hypnodensity)
#' 
#' transitions(events)
#' 
#' transitions(events, format = "dataframe")
#' 
#' transitions(events, format = "heatmap")
#' 
#' # 3 Dimensions sleep transitions
#' levels(events$event)[levels(events$event)=="N1"] <- "NREM"
#' levels(events$event)[levels(events$event)=="N2"] <- "NREM"
#' levels(events$event)[levels(events$event)=="N3"] <- "NREM"
#' 
#' round(
#'   transitions(
#'     events, 
#'     format = "dataframe")/(
#'       sum(transitions(events)))*100,2)
#'       
transitions <- function(
    hypnogram,
    stages = c("AWA", "REM", "N1", "N2", "N3", "NREM"),
    format = "vector"){
  
  hypnogram$event <- as.character(hypnogram$event)
  
  hypnogram_clean <- hypnogram(hypnogram)
  
  hypnogram_stages <- stages[stages %in% unique(hypnogram$event)] 
  
  transitions <- data.frame(
    row.names = hypnogram_stages)
  
  for(hypnogram_stage in hypnogram_stages){
    transitions[[hypnogram_stage]] = rep(0, length(hypnogram_stages))}
  
  for(i in c(2:nrow(hypnogram))){
    if(hypnogram$end[i-1] == hypnogram$begin[i]){
      y <- which(colnames(transitions) == hypnogram$event[i])
      x <- which(rownames(transitions) == hypnogram$event[i-1])
      transitions[x,y] <- transitions[x,y]+1
    }
  }
  
  if(format == "vector"){
    
    return(named_matrix2named_vector(transitions))
    
  } else if(format == "dataframe"){
    
    return(transitions)
    
  } else if(format == "heatmap"){
    
    transitions$from <- row.names(transitions)
    transitions <- reshape2::melt(transitions, value.name = "from", id="from")
    colnames(transitions) <- c("to","from","count")
    transitions$to <- factor(transitions$to,levels = rev(stages))
    transitions$from <- factor(transitions$from,levels = stages)
    
    ggplot2::ggplot(transitions, ggplot2::aes_string("from", "to")) +
      ggplot2::geom_tile(ggplot2::aes_string(fill = "count"), show.legend = FALSE) + 
      ggplot2::xlab("") + ggplot2::ylab("") +
      ggplot2::geom_text(ggplot2::aes_string(label = "count")) +
      ggplot2::scale_fill_gradient(low = "white", high = "red") +
      ggplot2::scale_x_discrete(position = "top") +
      ggplot2::theme(panel.background = ggplot2::element_blank())
    
  } else {
    
    stop("'format' parameter must be 'vector', 'matrix' or 'heatmap'.")
    
  }
}

#' Read events from a Resmed Noxturnal .ndb file.
#'
#' @param data_file .ndb file path.
#' @return An events dataframe.
#' @export
read_events_ndb <- function(data_file){
  
  # TODO clean this function
  
  data_ndb <- paste0(data_file)
  
  nox_db <- DBI::dbConnect(RSQLite::SQLite(), data_ndb)
  
  tables <- RSQLite::dbListTables(nox_db)
  
  res <- list()
  
  res$tables <- list()
  
  for(table in tables){
    res$tables[[table]] <- RSQLite::dbGetQuery(
      conn = nox_db,
      paste0('SELECT * FROM ', table))
  }
  
  # Start time
  timestamp <- as.numeric(
    res$tables$internal_property$value[
      res$tables$internal_property$key == "RecordingStart"])
  epoch0.Csharp <- 621355968000000000
  timestamp.conv <- (timestamp - epoch0.Csharp) / 1e7
  startTime <- as.POSIXct(timestamp.conv, origin="1970-01-01")
  
  events <- data.frame(
    "begin" = startTime,
    "end" = startTime,
    "event" = "RecordingStart",
    stringsAsFactors = FALSE)
  
  # Stop time
  timestamp <- as.numeric(
    res$tables$internal_property$value[
      res$tables$internal_property$key == "RecordingStop"])
  timestamp.conv <- (timestamp - epoch0.Csharp) / 1e7
  stopTime <- as.POSIXct(timestamp.conv, origin="1970-01-01")
  
  events <- rbind(events, data.frame(
    "begin" = stopTime,
    "end" = stopTime,
    "event" = "RecordingStop",
    stringsAsFactors = FALSE))
  
  events$location <- NA
  
  # Stages
  # stages <- res$tables$scoring_marker
  # stages <- stages[,c("starts_at", "ends_at", "type")]
  # colnames(stages) <- c("begin", "end", "event")
  # stages$begin <- as.POSIXct(((stages$begin - epoch0.Csharp) / 1e7), origin="1970-01-01")
  # stages$end <- as.POSIXct(((stages$end - epoch0.Csharp) / 1e7), origin="1970-01-01")
  # stages$location <- NA
  
  if("temporary_scoring_marker" %in% tables){
    scored_events <- res$tables$temporary_scoring_marker
  } else {
    scored_events <- res$tables$scoring_marker
  }
  
  scored_events <- scored_events[,c("starts_at","ends_at", "type", "location")]
  colnames(scored_events) <- c("begin", "end", "event", "location")
  scored_events$begin <- as.POSIXct(((scored_events$begin - epoch0.Csharp) / 1e7), origin="1970-01-01")
  scored_events$end <- as.POSIXct(((scored_events$end - epoch0.Csharp) / 1e7), origin="1970-01-01")
  scored_events$location[scored_events$location == ""] <- NA
  
  scored_events$event[scored_events$event == "sleep-wake"] <- "AWA"
  scored_events$event[scored_events$event == "sleep-n1"] <- "N1"
  scored_events$event[scored_events$event == "sleep-n2"] <- "N2"
  scored_events$event[scored_events$event == "sleep-n3"] <- "N3"
  scored_events$event[scored_events$event == "sleep-rem"] <- "REM"
  scored_events$event[scored_events$event == "arousal"] <- "Arousal"
  
  events <- rbind(events, scored_events)
  
  return(scored_events)
}

#' Highlight a scored event over a signal.
#'
#' @param signal The signal vector.
#' @param sRate Sample rate of the signal.
#' @param sig_start Date-Time value of the signal start.
#' @param event_start Date-Time value of the event start.
#' @param event_end Date-Time value of the event end.
#' @param window Number of seconds of signal to plot before, and after.
#' @return A plot of the highlighted event over the signal.
#' @export
plot_event <- function(
    signal,
    sRate,
    sig_start,
    event_start,
    event_end,
    window = 10){
  idx_signal_start <- floor(as.numeric(difftime(event_start,sig_start, units = "sec"))*sRate-window*sRate)
  idx_signal_end <- floor(as.numeric(difftime(event_end,sig_start, units = "sec"))*sRate+window*sRate)
  df <- data.frame(
    "y" = signal[idx_signal_start:idx_signal_end],
    "x" = seq(event_start-window,event_end+window, length.out =length(signal[idx_signal_start:idx_signal_end])))
  ggplot2::ggplot(
    data = df)+ggplot2::aes_string(x="x",y="y")+ ggplot2::geom_line() +
    ggplot2::geom_rect(ggplot2::aes(xmin=event_start,
                                    xmax = event_end,
                                    ymin = -Inf,
                                    ymax = Inf), fill = 'pink', alpha = 0.02) +
    ggplot2::xlab("Time")+ ggplot2::ylab("Signal values")}


named_matrix2named_vector <- function(m){
  r = c()
  for(i in c(1:length(names(m)))){
    for(j in c(1:length(row.names(m)))){
      r[paste0(names(m)[i],"_",row.names(m)[j])] = m[i,j]
    }
  }
  return(r)
}
