context("Respiratory & apnea functions testing")

test_that("Apnea Detection", {
  
  spo2_sample <- c(98, 97, 96, 95, 94, 93, 92, 91, 90, 89, 88)
  sample_rate <- 1  # Assuming 1 Hz sampling rate
  detected_apneas <- detect_apneic_events(spo2_sample, sample_rate)

})
