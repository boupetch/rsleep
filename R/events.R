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

#' Draw a hypnogram with ggplot2.
#'
#' @description A hypnogram represents the stages of sleep as a function of time. \code{plot_hypnogram()} plot a hypnogram using the \code{ggplot2} library from stages sleep in an event dataframe. \code{REM} stage is higlighted in red.
#' @references Silber MH, Ancoli-Israel S, Bonnet MH, Chokroverty S, Grigg-Damberger MM, et al. (2007). "The visual scoring of sleep in adults". Journal of Clinical Sleep Medicine. 3 (2): 121–31. PMID 17557422
#' @param events Events dataframe. Dataframe must have \code{begin} (\code{POSIXt}), \code{end} (\code{POSIXt}) and \code{event}
#' @param labels Sleep stages labels. Defaults to \code{c("N3","N2","N1","REM","AWA")}.
#' @return a ggplot object.
#' @examples
#' e <- data.frame(begin = as.POSIXlt(c(1536967800,1536967830,1536967860),origin = "1970-01-01"))
#' e$end <- as.POSIXlt(c(1536967830,1536967860,1536967890), origin = "1970-01-01")
#' e$event = c("N3","N3","REM")
#' plot_hypnogram(e)
#' @export
plot_hypnogram <- function(events, labels = c("N3","N2","N1","REM","AWA")){
  stages <- hypnogram(events, labels)
  stages$begin <- as.POSIXct(stages$begin)
  stages$end <- as.POSIXct(stages$end)
  hypnogram <- ggplot2::ggplot(stages,ggplot2::aes_string(x="begin",y="event",group=1)) +
    ggplot2::geom_line() +
    ggplot2::theme_bw() +
    ggplot2::xlab("") +
    ggplot2::ylab("")
  rem = stages[stages$event == "REM",]
  if(nrow(rem) > 0){
    for(i in c(1:nrow(rem))){
      df <- stats::reshape(rem[i,], idvar = "event", varying = c("begin","end"),
                           v.names = "value", direction = "long")
      hypnogram <- hypnogram+ggplot2::geom_line(data=df,mapping = ggplot2::aes_string(x="value",y="event",group=1),colour='red')
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
