context("EEG signal functions testing")

test_that("Spectrogram", {
  library(signal)
  spec <- spectrogram(chirp(seq(-2, 15, by = 0.001), 400, 10, 100, 'quadratic'),20,n=1024,plot=T)
  suppressWarnings(spec <- spectrogram(chirp(seq(-2, 15, by = 0.001), 400, 10, 100, 'quadratic'),20,n=1024,plot=F))
  expect_equal(class(spec),"specgram")
})
