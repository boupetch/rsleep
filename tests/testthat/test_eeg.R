context("EEG signal functions testing")

test_that("Spectrogram", {

  library(signal)

  sig <- chirp(seq(-2, 15, by = 0.001), 400, 10, 100, 'quadratic')

  # spectrogram(
  #   signal = sig,
  #   sRate = 200,
  #   n=2048,
  #   window = 2048,
  #   plot=TRUE)
  
  # if(file.exists("Rplots.pdf")){
  #   if(file.access("Rplots.pdf") == 0){
  #     file.remove("Rplots.pdf")
  #   }
  # }

  spec <- spectrogram(signal = sig,
                      sRate = 200,
                      n=2048,
                      window = 2048,
                      plot=FALSE)

  expect_equal(class(spec),"specgram")
})

test_that("Spectral power computing", {

  # Generating dummy signal
  library(signal)
  sig <- chirp(seq(-2, 15, by = 0.001), 400, 10, 100, 'quadratic')

  # No normalization
  bands <- bands_psd(bands = list(c(0.3,4),c(1,2)),signal = sig,sRate = 200, normalize = FALSE)
  expect_equal(length(bands), 2)
  expect_equal(is.null(bands[[1]]),FALSE)

  bands <- bands_psd(bands = list(c(0.3,4),c(1,2)),signal = sig,sRate = 200, normalize = FALSE, method="pwelch")
  expect_equal(length(bands), 2)
  expect_equal(is.null(bands[[1]]),FALSE)

  bands <- bands_psd(bands = list(c(0.3,4),c(1,2)),signal = sig,sRate = 200, normalize = FALSE, method="psm")
  expect_equal(length(bands), 2 )
  expect_equal(is.null(bands[[1]]), FALSE)

  # Normalization
  bands <- bands_psd(bands = list(c(0.3,4),c(1,2)),signal = sig,sRate = 200, normalize = c(0.3,40))
  expect_equal(length(bands),2)
  expect_equal(is.null(bands[[1]]),FALSE)

  # PSM
  p <- psm(sin(c(1:10000)), 200, 100)
  expect_equal(nrow(p),100)

})
