score_stages <- function(signals,
                         sRates,
                         signals_names = c("C3-M2","C4-M1","O1-M2","E1-M2","E2-M1","1-2"),
                         model_path = tempdir()){

  if(!("keras" %in%  installed.packages()[,1])){
    stop("Keras packagae required. Please install the Keras R package to continue: https://keras.rstudio.com/")
  }

  #http://cloud.frenchkpi.com/s/fjWTZZ2omGgGa6R/download




}
