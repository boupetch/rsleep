# Polysomnography ----

#' Generates files batches from PSG data.
#'
#' @description Generates train batches from PSG data to be used by the `train_batches()` function.
#' @references Chambon, S., Galtier, M., Arnal, P., Wainrib, G. and Gramfort, A. (2018) A Deep Learning Architecture for Temporal Sleep Stage Classification Using Multivariate and Multimodal Time Series. IEEE Trans. on Neural Systems and Rehabilitation Engineering 26:(758-769).
#' @param records Character vector of EDF files paths to be included in the train batches.
#' @param events List of events dataframes containing hypnograms corresponding to EDF records in `records` parameter.
#' @param batches_path Character. Path where batches files will be saved.
#' @param channels Character vector. Channels labels to include in the dataset.
#' @param resample Integer. Sample rate to resample selected signals.
#' @param padding Epochs added before and after each epoch.
#' @param batches_size Number of epochs in each batch file.
#' @param verbose Boolean, display status messages or not.
#' @export
write_batches_psg <- function(
  records,
  events,
  batches_path = tempdir(),
  channels = c("C3-M2", "C4-M1", "O1-M2", "E1-M2", "E2-M1", "1-2"),
  resample = 70,
  padding = 1,
  batches_size = 1024,
  verbose = TRUE){

  if(length(records) != length(events)){
    stop("Parameters record & events dot not have the same length.")
  }

  batch_count <- 0

  residual_x <- NA
  residual_y <- NA

  for(i in c(1:length(records))){

    if(verbose) message(
      paste0("Processing record ",basename(records[i])))

    edf <- edfReader::readEdfHeader(fileName = records[i])
    edf <- edfReader::readEdfSignals(edf, signals = channels)

    events[[i]] <- rsleep::hypnogram(events[[i]])
    events[[i]]$event <- as.character(events[[i]]$event)

    epochs <- rsleep::epochs(
      signals = lapply(channels, function(x) edf[[x]]$signal),
      sRates = lapply(channels,function(x){edf[[x]]$sRate}),
      resample = resample,
      epoch = rsleep::hypnogram(events[[i]]),
      startTime = as.numeric(as.POSIXct(edf[[1]]$startTime)),
      padding = padding)

    if(verbose) message(
      paste0("Found ",length(epochs)," epochs, normalizing..."))

    # Normalization

    epochs_normalized <- lapply(epochs, function(x){
      #x <- x[,channels]
      x <- t(x)
      t(apply(x,1,function(y){
        y <- y-mean(y)
        y <- y/stats::sd(y)
        y
      }))
    })

    x <- abind::abind(epochs_normalized,along=-1)

    # Y processing
    y <- events[[i]]$event
    y[y == "AWA"] <- 0
    y[y == "REM"] <- 1
    y[y == "N1"] <- 2
    y[y == "N2"] <- 3
    y[y == "N3"] <- 4
    y <- keras::to_categorical(y,num_classes=5)

    while(dim(x)[1] >= batches_size){

      batch_x <- x[1:batches_size,,]
      batch_y <- y[1:batches_size,]

      batch_count <- batch_count + 1
      if(verbose) message(paste0("Batch ",batch_count))
      saveRDS(
        object = list(keras::array_reshape(
          batch_x, c(
            dim(batch_x)[1],
            dim(batch_x)[2],
            dim(batch_x)[3],1)),
          batch_y),
        file = paste0(batches_path,"/batch_",batch_count,".rds"))

      if(dim(x)[1] > batches_size){
        x <- x[(batches_size+1):dim(x)[1],,]
        y <- y[(batches_size+1):dim(y)[1],]
      } else if(dim(x)[1] == batches_size){
        x <- x[0,,]
        y <- y[0,]
      }


    }

    if(dim(x)[1] > 0){

      if(is.na(residual_x)){

        residual_x <- x
        residual_y <- y

      } else {

        residual_x <- abind::abind(residual_x,x,along = 1)
        residual_y <- abind::abind(residual_y,y,along = 1)

        while(dim(residual_x)[1] >= batches_size){

          batch_x <- residual_x[1:batches_size,,]
          batch_y <- residual_y[1:batches_size,]

          batch_count <- batch_count + 1
          if(verbose) message(paste0("Residual batch ", batch_count))
          saveRDS(object = list(
            keras::array_reshape(
              batch_x,
              c(dim(batch_x)[1], dim(batch_x)[2], dim(batch_x)[3],1)), batch_y),
            file = paste0(batches_path,"/batch_",batch_count,".rds"))

          if(dim(residual_x)[1] > batches_size){
            residual_x <- residual_x[(batches_size+1):dim(residual_x)[1],,]
            residual_y <- residual_y[(batches_size+1):dim(residual_y)[1],]

          } else if(dim(residual_x)[1] == batches_size){
            residual_x <- residual_x[0,,]
            residual_x <- residual_x[0,]
          }
        }
      }
    }
  }
}

#' Score 30 seconds epochs from European Data Format (EDF) files.
#'
#'
#' @description Score 30 seconds epochs from European Data Format (EDF) files.
#' @references Chambon, S., Galtier, M., Arnal, P., Wainrib, G. and Gramfort, A. (2018) A Deep Learning Architecture for Temporal Sleep Stage Classification Using Multivariate and Multimodal Time Series. IEEE Trans. on Neural Systems and Rehabilitation Engineering 26:(758-769).
#' @references Kemp, B., Värri, A., Rosa, A.C., Nielsen, K.D. and Gade, J., 1992. A simple format for exchange of digitized polygraphic recordings. Electroencephalography and clinical neurophysiology, 82(5), pp.391-393.
#' @param edf Character. European Data Format (EDF) file path.
#' @param channels A vector containing the channels names if names differ from `c("C3-M2","C4-M1","O1-M2","E1-M2","E2-M1","1-2")`.
#' @param model The Keras model.
#' @param verbose Boolean. Display or not status messages.
#' @return A dataframe containing predicted hypnodensity values of the record.
#' @export
score_psg <- function(
  edf,
  channels = c("C3-M2","C4-M1","O1-M2","E1-M2","E2-M1","1-2"),
  model = chambon2018(6,3*30*70),
  verbose = TRUE){

  if(!("keras" %in%  utils::installed.packages()[,1])){
    stop("Keras packagae required. Please install the Keras R package to continue: https://keras.rstudio.com/")
  }

  if(verbose){
    message("Reading EDF file...")
  }

  h <- edfReader::readEdfHeader(edf)
  s <- edfReader::readEdfSignals(h, signals = channels)

  signals = lapply(channels,function(x){s[[x]]$signal})

  sRates = lapply(channels,function(x){s[[x]]$sRate})

  if(verbose) message("Epoching signals...")

  epochs <- epochs(signals = signals,
                   sRates = sRates,
                   resample = 70,
                   epoch = 30,
                   padding = 1)
  
  while(length(epochs[[length(epochs)]]) != length(epochs[[1]])){
    epochs[[length(epochs)]] <- NULL
  }

  if(verbose) message("Normalizing signals...")

  epochs <- lapply(epochs, function(x){
    #x <- x[,c("C3-M2","C4-M1","O1-M2","E1-M2","E2-M1","1-2")]
    x <- t(x)
    t(apply(x,1,function(y){
      y <- y-mean(y)
      y <- y/stats::sd(y)
      y
    }))
  })

  x <- abind::abind(epochs,along=-1)

  x <- keras::array_reshape(x,dim = c(dim(x)[1],dim(x)[2],dim(x)[3],1))

  if(verbose) message("Performing prediction...")

  res <- stats::predict(model, x)

  hypnodensity <- as.data.frame(res)

  colnames(hypnodensity) <- c("AWA","REM","N1","N2","N3")

  hypnodensity$begin <- as.POSIXct(h$startTime) +
    (c(0:(nrow(hypnodensity)-1))*30)

  hypnodensity$end <- hypnodensity$begin+30

  hypnodensity$event <- apply(hypnodensity[,1:5], 1, function(x){
    names(which.max(x))
  })

  hypnodensity
}

# Mice -----

#' Write batches from mice records
#'
#' @description Write batches from mice records
#' @param records Character. Records paths
#' @param events List of events
#' @param batches_path Path to write batches
#' @param batch_size size of each batch
#' @param classes_nb number of classes
#' @param padding consecutive epochs to add
#' @param resample resample rate
#' @param verbose Boolean. Display or not status messages.
#' @export
write_batches_mice <- function(
  records,
  events,
  batches_path = "./",
  batch_size = 128,
  classes_nb = 3,
  padding = 2,
  resample = 400,
  verbose = TRUE){

  channels <- c("EEG", "EMG")

  buffer_x <- list()
  buffer_y <- list()
  batch_count <- 1

  for(i in c(1:length(records))){

    if(verbose) message(paste0("Processing record ", basename(records[i])))

    h <- edfReader::readEdfHeader(records[i])

    labels <- unlist(lapply(channels, function(x){
      h$sHeaders$label[x == substr(h$sHeaders$label,1,3)][1]
    }))

    s <- edfReader::readEdfSignals(h, signals = unlist(labels))

    events <- rsleep::read_events_compumedics(
      txt = events[1], startTime = h$startTime)

    events <- events[(1+padding):(nrow(events)-padding-1),]

    te <- table(events$event)

    if(length(te) != classes_nb){
      warning("Num class diff")
      next
    }

    idx <- lapply(names(te),function(x){
      sample(which(events$event == x))[1:min(te)]
    })

    while(length(idx[[1]]) != 0){

      for(j in c(1:length(idx))){

        epoch_idx <- idx[[j]][1]

        epoch <- lapply(labels, function(x){

          sig <- s[[x]]$signal[
            (((as.numeric(events[epoch_idx,]$begin)-
                 as.numeric(as.POSIXlt(h$startTime)))*
                s[[x]]$sRate)-(padding*s[[x]]$sRate*4)+1):
              (((as.numeric(events[epoch_idx,]$end)-
                   as.numeric(as.POSIXlt(h$startTime)))*
                  s[[x]]$sRate)+(padding*s[[x]]$sRate*4))]

          sig <- signal::resample(sig,resample,s[[x]]$sRate)
          sig <- sig - mean(sig)
          sig <- sig/stats::sd(sig)
          sig

        })

        epoch <- abind::abind(epoch,along = -1)

        buffer_x[[length(buffer_x)+1]] <- epoch
        buffer_y[[length(buffer_x)+1]] <- events[epoch_idx,]$event

        if(length(buffer_x) == (batch_size*length(te))){

          if(verbose) message(paste0("Writing batch", batch_count))

          saveRDS(object = list(
            abind::abind(buffer_x, along = -1),
            abind::abind(buffer_y, along = -1)
          ), file = paste0(batches_path,"batch_",batch_count,".rds"))

          buffer_x <- list()
          buffer_y <- list()
          batch_count <- batch_count + 1

        }

        idx[[j]] <- idx[[j]][-1]
        print(paste0(length(buffer_x),"/",(batch_size*length(te))))
      }
    }
  }
}

#' Score mice sleep from European Data Format (EDF) files.
#'
#'
#' @description Score mice sleep from European Data Format (EDF) files.
#' @param edf Character. European Data Format (EDF) file path.
#' @param model model
#' @param verbose Boolean. Display or not status messages.
#' @return A dataframe containing predicted hypnodensity values of the record.
#' @export
score_mice <- function(
  edf,
  model,
  verbose = TRUE){

  padding <- 2
  epoch_dur <- 4
  channels <- c("EEG","EMG")

  h <- edfReader::readEdfHeader(edf)

  channels <- unlist(lapply(channels, function(x){
    h$sHeaders$label[x == substr(h$sHeaders$label,1,3)][1]
  }))

  s <- edfReader::readEdfSignals(h, signals = channels)

  epochs <- trunc(length(s[[channels[1]]]$signal)/s[[channels[1]]]$sRate/4)

  if(verbose){
    pb <- utils::txtProgressBar(
      min = 1, max = epochs,
      style = 3)#length(epochs), )
  }

  #results <- lapply(c(3:(epochs-3)),function(i){
  results <- lapply(c(1:epochs), function(i){

    if(verbose){
      utils::setTxtProgressBar(pb, i)
    }

    epoch <- lapply(channels,function(x){

      padding_samples <- s[[x]]$sRate*8
      
      if(i <= padding){

        cs <- s[[x]]$signal[
          ((1+((i-1)*s[[x]]$sRate*4))):
            ((((i-1)*s[[x]]$sRate*4)+(s[[x]]$sRate*4))+padding_samples)]
        cs <- c(rep(0,padding_samples),cs)

      } else if (i >= (length(epochs) - padding)){

        cs <- s[[x]]$signal[
          ((1+((i-1)*s[[x]]$sRate*4))-padding_samples):
            ((((i-1)*s[[x]]$sRate*4)+(s[[x]]$sRate*4)))]
        cs <- c(cs,rep(0,padding_samples))

      } else {

        cs <- s[[x]]$signal[
          ((1+((i-1)*s[[x]]$sRate*4))-padding_samples):
            ((((i-1)*s[[x]]$sRate*4)+(s[[x]]$sRate*4))+padding_samples)]

      }

      cs <- signal::resample(cs,400,s[[x]]$sRate)
      cs <- cs-mean(cs)
      cs <- cs/stats::sd(cs)

    })

    epoch <- abind::abind(epoch,along=-1)
    epoch <- keras::array_reshape(epoch,c(1,2,8000,1))

    stats::predict(model,epoch)
  })

  hypnodensity <- data.frame(matrix(unlist(results), nrow=length(results), byrow=T))

  colnames(hypnodensity) <- c("AWA","NREM","REM")

  hypnodensity$begin <- as.POSIXct(h$startTime)
  hypnodensity$begin <- hypnodensity$begin + 4*(c(1:nrow(hypnodensity))-1)
  hypnodensity$end <- hypnodensity$begin +4

  hypnodensity
}

# Model training ----

#' Trains a model from files batches.
#'
#' @description Trains a model from files batches.
#' @param model Keras model.
#' @param batches Character vector of batches files.
#' @param epochs Integer. Number of epochs to train the model.
#' @return A trained and serialized Keras model.
#' @export
train_batches <- function(model, batches, epochs = 10){

  batch_generator <- function(batch_files) {

    next_file <- 0

    function() {

      next_file <<- next_file + 1

      if (next_file > length(batch_files)) next_file <<- 1

      file <- batch_files[[next_file]]

      rds <- readRDS(file)

      list(rds[[1]],rds[[2]])
    }
  }

  keras::fit_generator(
    object = model,
    generator = batch_generator(batches),
    epochs = epochs,
    steps_per_epoch = length(batches))

  keras::serialize_model(model)

}

