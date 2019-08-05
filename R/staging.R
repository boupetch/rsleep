score_stages <- function(signals,
                         sRates,
                         signals_names = c("C3-M2","C4-M1","O1-M2","E1-M2","E2-M1","1-2"),
                         model_path = tempdir(),
                         verbose = TRUE){

  if(!("keras" %in%  installed.packages()[,1])){
    stop("Keras packagae required. Please install the Keras R package to continue: https://keras.rstudio.com/")
  } else {
    library(keras)
  }

  model_fname <- "hd_conv_v3.h5"
  model_f_md5 <- "5a757f2258c0675010ef617eb3e6f563"
  model_url <- "http://cloud.frenchkpi.com/s/fjWTZZ2omGgGa6R/download"

  if((!file_test("-f", model_path) && dir.exists(model_path)) |
     (file.exists(paste0(model_path,"/",model_fname)) && digest::digest(object = paste0(model_path,"/",model_fname), algo = "md5") != model_f_md5) ){

    model_fullpath <- paste0(model_path,"/",model_fname)

    if(verbose) message(paste0("Model missing or outdated. Downloading to ",model_fullpath))

    if(.Platform$OS.type == "unix") {
      download.file(model_url, model_fullpath, "wget", T)
    } else {
      download.file(model_url, model_fullpath, method = "wininet", T)
    }

  } else if (!file_test("-f", model_path) && !dir.exists(model_path)){

    error("Model directory does not exist.")

  } else {

    if(verbose) message(paste0("Model found."))
    model_fullpath <- model_path

  }

  if(verbose) message("Reading model...")
  model <- load_model_hdf5(model_fullpath)

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
        y <- y/sd(y)
        y
      }))
    })

  x <- abind::abind(epochs,along=-1)

  x <- array_reshape(x,dim = c(dim(x)[1],dim(x)[2],dim(x)[3],1))

  if(verbose) message("Performing prediction...")
  model %>% predict(x)
}

hypnodensity <- function(m,
                         labels = c("AWA","REM","N1","N2","N3"),
                         startTime = as.POSIXct("1970/01/01 00:00:00"),
                         epoch_duration = 30,
                         plot = TRUE){

  pal <- c("#5BBCD6", "#FF0000", "#00A08A", "#F2AD00", "#F98400")

  df <- as.data.frame(m)

  colnames(df) <- labels

  df$epoch <- startTime + ((c(1:nrow(df))-1)*epoch_duration)

  if(plot){
    df2 <- cbind(df[ncol(df)], stack(df[1:length(labels)]))

    p <- ggplot2::ggplot(df2, ggplot2::aes(x = epoch,
                                           y= values,
                                           fill = ind)) +
      ggplot2::geom_area(position = 'stack') +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "bottom",
                     legend.title = ggplot2::element_blank()) +
      ggplot2::xlab("") + ggplot2::ylab("Stage likelihood")

    if(length(labels) == 5){
      p <- p + ggplot2::scale_fill_manual(values = pal)
    }
    p
  } else {
    df
  }
}

train_stages <- function(){
  "TODO"
}

build_batches <- function(){
  "TODO"
}

# h <- edfReader::readEdfHeader("15012016HD.edf")
# s <- edfReader::readEdfSignals(h)
#
# res <- score_stages(
#   signals = lapply(c("C3-M2","C4-M1","O1-M2","E1-M2","E2-M1","1-2"),function(x){
#     s[[x]]$signal}),
#   sRates = lapply(c("C3-M2","C4-M1","O1-M2","E1-M2","E2-M1","1-2"),function(x){
#     s[[x]]$sRate}))
#
# hypnodensity(res,startTime = as.POSIXct(s[[1]]$startTime))
#
#
# events<- apply(res, 1, function(x){
#   s <- which(x[1:5] == max(x[1:5]))
#   s[s == 1] <- "AWA"
#   s[s == 2] <- "REM"
#   s[s == 3] <- "N1"
#   s[s == 4] <- "N2"
#   s[s == 5] <- "N3"
#   s
# })
# hypnogram <- data.frame(event=events,
#                         begin = as.POSIXct(c((0:(length(events)-1))*30)),
#                         end = as.POSIXct(c((0:(length(events)-1))*30+30)))
#
# plot_hypnogram(r)

