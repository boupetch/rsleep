#' normalize_cycles
#'
#' @param events events
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
