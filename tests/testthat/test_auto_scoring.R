skip_if_no_keras <- function(version = NULL) {
  if (!keras::is_keras_available(version))
    # nocov start
    skip("Required keras version not available for testing")
    # nocov end
}

test_that("Test automatic scoring", {
  skip_if_no_keras()

  edf_path <- file.path(tempdir(),"15012016HD.edf")

  download.file("https://osf.io/57j2u/download", edf_path, quiet = TRUE)

  hypnodensity <- score_stages_edf(edf_path)

  expect_equal(nrow(hypnodensity), 1472)
  expect_equal(ncol(hypnodensity), 8)
  expect_equal(sum(hypnodensity[1,1:5]), 1)

  p <- plot_hypnodensity(hypnodensity)

  expect_equal(class(p)[2], "ggplot")


})
