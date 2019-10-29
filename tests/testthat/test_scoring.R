skip_if_no_keras <- function(version = NULL) {
  if (!keras::is_keras_available(version))
    # nocov start
    skip("Required keras version not available for testing")
    # nocov end
}

test_that("Polysomnography scoring", {

  skip_if_no_keras()

  # Download test files
  edf_path <- file.path(tempdir(),"15012016HD.edf")
  csv_path <- file.path(tempdir(),"15012016HD.csv")

  download.file("https://osf.io/57j2u/download", edf_path, quiet = TRUE)
  download.file("https://osf.io/h4ysj/download", csv_path, quiet = TRUE)

  # Model
  model <- chambon2018(6,6000)

  # Stages clasification
  hypnodensity <- score_psg(edf_path)

  expect_equal(nrow(hypnodensity), 1472)
  expect_equal(ncol(hypnodensity), 8)
  expect_equal(sum(hypnodensity[1,1:5]), 1)

  # Plot hypnodensity
  p <- plot_hypnodensity(hypnodensity)

  expect_equal(class(p)[2], "ggplot")

})

test_that("Polysomnography batches", {

  skip_if_no_keras()

  edf_path <- file.path(tempdir(),"15012016HD.edf")
  csv_path <- file.path(tempdir(),"15012016HD.csv")

  download.file("https://osf.io/57j2u/download", edf_path, quiet = TRUE)
  download.file("https://osf.io/h4ysj/download", csv_path, quiet = TRUE)

  expect_error(
    write_batches_psg(
      records = c(edf_path,edf_path),
      events = list(read_events_noxturnal(csv_path))))

  write_batches_psg(
    records = edf_path,
    events = list(read_events_noxturnal(csv_path)),
    batches_path = tempdir(),
    resample = 70,
    padding = 1,
    batches_size = 128,
    verbose = FALSE)

})

test_that("Mice sleep scoring", {

  skip_if_no_keras()

  model <- schwabedal2018()

  expect_equal(class(model)[1], "keras.engine.sequential.Sequential")

  model <- schwabedal2018(weights = TRUE)

  expect_equal(class(model)[1], "keras.engine.sequential.Sequential")
})



