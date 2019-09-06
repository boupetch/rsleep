#' Score stages from polysomnography signals. Provided for research purposes. Do not use in production.
#'
#' @description Score stages from polysomnography signals using a pre-trained convolutional neural network (See references for model architecture). Model have been pre-trained on data from the Hotel-Dieu, Paris, recorded with Resmed Nox polysomnography device. The model may not be suitable for data recorded with other kind of device. It is provided for research purposes. Do not use in production. Model needs 6 channels to predict sleep stage over 30 seconds epochs: Electroencephalogram:`C3-M2`,`C4-M1`,`O1-M2`; Electrooculogram:`E1-M2`,`E2-M1`, Electromyogram:`1-2`.
#' @references Chambon, S., Galtier, M., Arnal, P., Wainrib, G. and Gramfort, A. (2018) A Deep Learning Architecture for Temporal Sleep Stage Classification Using Multivariate and Multimodal Time Series. IEEE Trans. on Neural Systems and Rehabilitation Engineering 26:(758-769).
#' @param signals A list of the 7 signals vectors. Must be ordered in the following order: `c("C3-M2","C4-M1","O1-M2","E1-M2","E2-M1","1-2")`.
#' @param sRates A vector of the sample rates of the 7 signals passed in the `signals` parameter.
#' @param model_path The path of the model file. Model will be downloaded if a directory is passed or if the file passed is different from the latest available model.
#' @param verbose Boolean. Display or not status messages.
#' @return A matrix containing hypnodensity values for each epoch and each of the 5 stages: `AWA`, `REM`, `N1`, `N2`, `N3`.
#' @export
score_stages <- function(signals,
                         sRates,
                         model_path = tempdir(),
                         verbose = TRUE){

  if(!("keras" %in%  utils::installed.packages()[,1])){
    stop("Keras packagae required. Please install the Keras R package to continue: https://keras.rstudio.com/")
  }

  model_fname <- "hd_conv_v3.h5"
  model_f_md5 <- "5a757f2258c0675010ef617eb3e6f563"
  model_url <- "https://osf.io/axcvf/download"

  if((!utils::file_test("-f", model_path) && dir.exists(model_path)) |
     (file.exists(paste0(model_path,"/",model_fname)) && digest::digest(object = paste0(model_path,"/",model_fname), algo = "md5") != model_f_md5) ){

    model_fullpath <- paste0(model_path,"/",model_fname)

    if(verbose) message(paste0("Model missing or outdated. Downloading to ",model_fullpath))

    if(.Platform$OS.type == "unix") {
      utils::download.file(model_url, model_fullpath, "wget", T)
    } else {
      utils::download.file(model_url, model_fullpath, method = "wininet", T)
    }

  } else if (!utils::file_test("-f", model_path) && !dir.exists(model_path)){

    stop("Model directory does not exist.")

  } else {

    if(verbose) message(paste0("Model found."))
    model_fullpath <- model_path

  }

  if(verbose) message("Reading model...")
  model <- keras::load_model_hdf5(model_fullpath)

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
  stats::predict(model, x)
}

#' Convenient wrapper for `score_stage` to score 30 seconds epochs directly from European Data Format (EDF) files.
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
score_stages_edf <- function(edf,
                             channels = c("C3-M2","C4-M1","O1-M2","E1-M2","E2-M1","1-2"),
                             model_path = tempdir(), verbose = TRUE){

  if(verbose){
    message("Reading EDF file...")
  }

  h <- edfReader::readEdfHeader(edf)
  s <- edfReader::readEdfSignals(h, signals = channels)

  signals = lapply(channels,function(x){s[[x]]$signal})

  sRates = lapply(channels,function(x){s[[x]]$sRate})

  res <- score_stages(signals = signals,
                      sRates = sRates,
                      model_path = model_path,
                      verbose = verbose)

  hypnodensity <- as.data.frame(res)

  colnames(hypnodensity) <- c("AWA","REM","N1","N2","N3")

  hypnodensity$begin <- as.POSIXct(h$startTime)+(c(0:(nrow(hypnodensity)-1))*30)

  hypnodensity$end <- hypnodensity$begin+30

  hypnodensity$event <- apply(hypnodensity[,1:5], 1, function(x){
    names(which.max(x))
  })

  hypnodensity
}

#' Plot a hypnodensity graph using `ggplot2`.
#'
#' @description Plot a hypnodensity graph using `ggplot2` from the values returned from `score_stages_edf` function.
#' @references Stephansen, J.B., Olesen, A.N., Olsen, M., Ambati, A., Leary, E.B., Moore, H.E., Carrillo, O., Lin, L., Han, F., Yan, H. and Sun, Y.L., 2018. Neural network analysis of sleep stages enables efficient diagnosis of narcolepsy. Nature communications, 9(1), p.5229.
#' @param hypnodensity A hypnodensity dataframe as returned by the `score_stages_edf` function.
#' @return A `ggplot2` hypnodensity graph.
#' @export
plot_hypnodensity <- function(hypnodensity){

  pal <- c("#5BBCD6", "#FF0000", "#00A08A", "#F2AD00", "#F98400")
  stages <- c("AWA","REM","N1","N2","N3")

  melt <- stats::reshape(data = hypnodensity,
                  direction = "long",
                  varying = 1:5,
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

#' Generates train batches for machine learning
#'
#' @description Generates train batches for machine learning
#' @references Stephansen, J.B., Olesen, A.N., Olsen, M., Ambati, A., Leary, E.B., Moore, H.E., Carrillo, O., Lin, L., Han, F., Yan, H. and Sun, Y.L., 2018. Neural network analysis of sleep stages enables efficient diagnosis of narcolepsy. Nature communications, 9(1), p.5229.
#' @param records
#' @param batches_path
#' @param channels
#' @param resample
#' @param padding
#' @param batches_size
#' @param verbose Boolean, display messages or not.
#' @export
generate_batches <- function(records,
                             batches_path = tempdir(),
                             channels = c("C3-M2","C4-M1","O1-M2","E1-M2","E2-M1","1-2"),
                             resample = 70,
                             padding = 1,
                             batches_size = 1024,
                             verbose = TRUE){

  batch_count <- 0

  residual_x <- NA
  residual_y <- NA

  for(record in records){

    if(verbose) message(paste0("Processing record ",basename(record)))

    # Read MDF
    mdf <- rsleep::read_mdf(mdfPath = record,
                            channels = channels)
    mdf$events <- rsleep::hypnogram(mdf$events)
    mdf$events <- mdf$events[-nrow(mdf$events),]
    mdf$events$event <- as.character(mdf$events$event)

    # Epoching
    epochs <- rsleep::epochs(signals = lapply(mdf$channels,function(x){x$signal}),
                             sRates = lapply(mdf$channels,function(x){x$metadata$sRate}),
                             resample = resample,
                             epoch = rsleep::hypnogram(mdf$events),
                             startTime = as.numeric(as.POSIXct(mdf$metadata$startTime)),
                             padding = padding)

    if(verbose) message(paste0("Found ",length(epochs)," epochs, normalizing..."))

    # Normalization

    epochs_normalized <- lapply(epochs, function(x){
      x <- x[,channels]
      x <- t(x)
      t(apply(x,1,function(y){
        y <- y-mean(y)
        y <- y/sd(y)
        y
      }))
    })

    x <- abind <- abind(epochs_normalized,along=-1)

    # Y processing
    y <- mdf$events$event
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
      saveRDS(object = list(keras::array_reshape(batch_x, c(dim(batch_x)[1], dim(batch_x)[2], dim(batch_x)[3],1)),
                            batch_y),
              file = paste0(batches_path,"/",batch_count,".rds"))

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

        residual_x <- abind(residual_x,x,along = 1)
        residual_y <- abind(residual_y,y,along = 1)

        while(dim(residual_x)[1] >= batches_size){

          batch_x <- residual_x[1:batches_size,,]
          batch_y <- residual_y[1:batches_size,]

          batch_count <- batch_count + 1
          if(verbose) message(paste0("Residual batch ", batch_count))
          saveRDS(object = list(keras::array_reshape(batch_x, c(dim(batch_x)[1], dim(batch_x)[2], dim(batch_x)[3],1)),
                                batch_y),
                  file = paste0(batches_path,"/",batch_count,".rds"))

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
