#' Score 30 seconds epochs directly from European Data Format (EDF) files.
#'
#'
#' @description Convenient wrapper for `score_stage` to score 30 seconds epochs directly from European Data Format (EDF) files.
#' @references Chambon, S., Galtier, M., Arnal, P., Wainrib, G. and Gramfort, A. (2018) A Deep Learning Architecture for Temporal Sleep Stage Classification Using Multivariate and Multimodal Time Series. IEEE Trans. on Neural Systems and Rehabilitation Engineering 26:(758-769).
#' @references Kemp, B., Värri, A., Rosa, A.C., Nielsen, K.D. and Gade, J., 1992. A simple format for exchange of digitized polygraphic recordings. Electroencephalography and clinical neurophysiology, 82(5), pp.391-393.
#' @param edf The EDF file path.
#' @param channels A vector containing the channels names if names differ from `c("C3-M2","C4-M1","O1-M2","E1-M2","E2-M1","1-2")`.
#' @param model_path The path of the model file. Model will be downloaded if a directory is passed or if the file passed is different from the latest available model.
#' @param verbose Boolean. Display or not status messages.
#' @return A dataframe containing predicted hypnodensity values of the record.
#' @export
score_stages_edf <- function(
  edf,
  channels = c("C3-M2","C4-M1","O1-M2","E1-M2","E2-M1","1-2"),
  model = chambon2018(6,3*30*70,TRUE,TRUE),
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

  hypnodensity$begin <- as.POSIXct(h$startTime)+(c(0:(nrow(hypnodensity)-1))*30)

  hypnodensity$end <- hypnodensity$begin+30

  hypnodensity$event <- apply(hypnodensity[,1:5], 1, function(x){
    names(which.max(x))
  })

  hypnodensity
}

#' Generates train batches to be used by the `train_batches()` function.
#'
#' @description Generates train batches to be used by the `train_batches()`
#' function.
#' @references Chambon, S., Galtier, M., Arnal, P., Wainrib, G. and Gramfort,
#'A. (2018) A Deep Learning Architecture for Temporal Sleep Stage Classification
#'Using Multivariate and Multimodal Time Series. IEEE Trans. on Neural Systems
#'and Rehabilitation Engineering 26:(758-769).
#' @param records Character vector of MDF records to be included in the train batches.
#' @param events List of events dataframes containing hypnograms corresponding
#' to records in the records character vector
#' @param batches_path Character. Path where batches files will be saved.
#' @param channels Character vector. Channels labels to include in the dataset.
#' @param resample Integer. Sample rate to resample signals.
#' @param padding Epochs added before and after each epoch.
#' @param batches_size Number of epoch by batch.
#' @param verbose Boolean, display messages or not.
#' @export
generate_batches <- function(
  records, events, batches_path = tempdir(),
  channels = c("C3-M2", "C4-M1", "O1-M2", "E1-M2", "E2-M1", "1-2"),
  resample = 70, padding = 1, batches_size = 1024, verbose = TRUE){

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
            keras::array_reshape(batch_x, c(dim(batch_x)[1], dim(batch_x)[2], dim(batch_x)[3],1)), batch_y),
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



