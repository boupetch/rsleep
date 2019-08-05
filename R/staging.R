score_stages <- function(signals,
                         sRates,
                         signals_names = c("C3-M2","C4-M1","O1-M2","E1-M2","E2-M1","1-2"),
                         model_path = tempdir()){

  if(!("keras" %in%  installed.packages()[,1])){
    stop("Keras packagae required. Please install the Keras R package to continue: https://keras.rstudio.com/")
  }

  model_fname <- "hd_conv_v3.h5"
  model_f_md5 <- "5a757f2258c0675010ef617eb3e6f563"
  model_url <- "http://cloud.frenchkpi.com/s/fjWTZZ2omGgGa6R/download"

  if((!file_test("-f", model_path) && dir.exists(model_path)) |
     (file.exists(model_path) && digest::digest(object = model_path, algo = "md5") == model_f_md5) ){

    model_fullpath <- paste0(model_path,"/",model_fname)

    message(paste0("Model missing or outdated. Downloading to ",model_fullpath))

    if(.Platform$OS.type == "unix") {
      if(!file.exists(model_fullpath)){
        download.file(model_url, model_fullpath, "wget", T)
      }
    } else {
      if(!file.exists(model_fullpath)){
        download.file(model_url, model_fullpath, method = "wininet", T)
      }
    }

  } else if (!file_test("-f", model_path) && !dir.exists(model_path)){

    error("Model directory does not exist.")

  } else {

    model_fullpath <- model_path

  }

  print("Model OK")
  print(model_fullpath)
}

train_stages <- function(){
  "TODO"
}

build_batches <- function(){
  "TODO"
}


score_stages(c(1:2),1,model_path = "./")
