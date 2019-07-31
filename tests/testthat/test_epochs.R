context("Epoching test")

test_that("Test", {

  library(signal)

  sig <- chirp(seq(-2, 15, by = 0.001), 400, 10, 100, 'quadratic')

  epochs <- rsleep::epochs(signals = list(sig,sig),
                           sRates = c(100,100),
                           resample = 200,
                           epoch = 1,
                           startTime = 0)

  epochs <- rsleep::epochs(signals = list(sig,sig),
                           sRates = c(100,100),
                           resample = 100,
                           epoch = data.frame(begin=c(0,1),end=c(1,2)),
                           startTime = 0)

  epochs <- rsleep::epochs(signals = list(sig,sig),
                           sRates = c(100,100),
                           resample = 100,
                           epoch = 1,
                           startTime = 0,
                           padding = 2)

  # Single vector
  epochs <- rsleep::epochs(signals = sig,
                           sRates = 100,
                           epoch = 1,
                           startTime = 0)

  expect_error(rsleep::epochs(signals = list(sig,sig),
                              sRates = c(100,100),
                              resample = 100,
                              epoch = "OK",
                              startTime = 0))
  expect_equal(length(epochs),171)
})
