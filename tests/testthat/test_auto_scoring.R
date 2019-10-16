skip_if_no_keras <- function(version = NULL) {
  if (!keras::is_keras_available(version))
    # nocov start
    skip("Required keras version not available for testing")
    # nocov end
}

test_that("Automatic scoring", {

  skip_if_no_keras()

  # Download test files
  edf_path <- file.path(tempdir(),"15012016HD.edf")
  csv_path <- file.path(tempdir(),"15012016HD.csv")

  download.file("https://osf.io/57j2u/download", edf_path, quiet = TRUE)
  download.file("https://osf.io/h4ysj/download", csv_path, quiet = TRUE)

  # # Write MDF
  # mdf_path <- paste0(tempdir(),"/15012016HD")
  # write_mdf(edfPath = edf_path,
  #           mdfPath = paste0(tempdir(),"/15012016HD"),
  #           events = read_events_noxturnal(csv_path))

  # Stages clasification
  hypnodensity <- score_stages_edf(edf_path)

  expect_equal(nrow(hypnodensity), 1472)
  expect_equal(ncol(hypnodensity), 8)
  expect_equal(sum(hypnodensity[1,1:5]), 1)

  # Plot hypnodensity
  p <- plot_hypnodensity(hypnodensity)

  expect_equal(class(p)[2], "ggplot")

  # Train model
  # generate_batches(
  #   records = edf_path,
  #   events = list(read_events_noxturnal(csv_path)),
  #   batches_path = tempdir(),
  #   resample = 100,
  #   padding = 3,
  #   batches_size = 128,
  #   verbose = FALSE)
  #
  # batches <- list.files(tempdir(),pattern = "batch*", recursive = FALSE,full.names = TRUE)
  #
  # trained_model <- train_chambon2018(batches[1:2], 2)
  #
  # expect_true(("keras.engine.training.Model" %in% class(trained_model)))
  #
  # test <- readRDS(batches[10])
  #
  # val_res <- predict(trained_model,test[[1]])
  #
  # ypred <- apply(val_res,1,function(x){
  #   which(x == max(x))
  # })
  #
  # ytrue <- apply(test[[2]],1,function(x){
  #   which(x == max(x))
  # })
  #
  # ck <- psy::ckappa(cbind(ypred, ytrue))
  #
  # expect_equal(length(ck), 2)

})

test_that("Generate batches", {

  skip_if_no_keras()

  # Download test files
  edf_path <- file.path(tempdir(),"15012016HD.edf")
  csv_path <- file.path(tempdir(),"15012016HD.csv")

  download.file("https://osf.io/57j2u/download", edf_path, quiet = TRUE)
  download.file("https://osf.io/h4ysj/download", csv_path, quiet = TRUE)

  generate_batches(
    records = edf_path,
    events = list(read_events_noxturnal(csv_path)),
    batches_path = tempdir(),
    resample = 10,
    padding = 1,
    batches_size = 128,
    verbose = FALSE)

})


