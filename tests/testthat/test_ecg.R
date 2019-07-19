context("ECG signal functions testing")

test_that("Peak detection", {
  peaks <- detect_rpeaks(example_ecg_200hz, 200)
  expect_equal(length(peaks), 9)
})
