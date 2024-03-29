context("ECG signal functions testing")

test_that("Peak detection", {
  
  path <- paste0(tempdir(),"rec_1.dat")
  download.file("https://rsleep.org/data/rec_1.dat",path)
  ecg <- readBin(path,integer(),500*30)
  unlink(path)
  peaks <- detect_rpeaks(ecg, sRate = 500)
  expect_true(length(peaks) > 0)
})
