context("Statistics related tests")

test_that("Stages statistics", {

  e <- read_events_noxturnal("data/noxturnal_events_example_unicode.csv")

  r <- stages_stats(e)

  expect_equal(r[["rem_duration"]],  124.5)
  expect_equal(r[["n1_duration"]],  33)
  expect_equal(r[["n2_duration"]],  233)
  expect_equal(r[["n3_duration"]],  182)
  expect_equal(r[["awa_duration"]],  106.5)
  expect_equal(r[["tts"]],  572.5)
  expect_equal(r[["rem_tts"]],  0.21746725)
  expect_equal(r[["n1_tts"]], 0.05764192)
  expect_equal(r[["n2_tts"]], 0.40698690)
  expect_equal(r[["n3_tts"]], 0.31790393)
  expect_equal(r[["awa_tts"]], 0.18602620)
  expect_equal(r[["tsp"]], 748.5)
  expect_equal(r[["efficiency"]], 0.76486306)
  expect_equal(r[["latency"]], 54.5)
  expect_equal(r[["n1_latency"]], 0)
  expect_equal(r[["n2_latency"]], 4)
  expect_equal(r[["n3_latency"]], 18)
  expect_equal(r[["rem_latency"]], 107)
  expect_equal(r[["waso"]], 121.5)

  # AWA only events
  e <- e[e$event == "AWA",]

  r <- stages_stats(e)

  expect_equal(r[["rem_duration"]],  0)
  expect_equal(r[["n1_duration"]],  0)
  expect_equal(r[["n2_duration"]],  0)
  expect_equal(r[["n3_duration"]],  0)
  expect_equal(r[["awa_duration"]],  106.5)
  expect_equal(r[["tts"]],  0)
  expect_equal(r[["rem_tts"]],  0)
  expect_equal(r[["n1_tts"]], 0)
  expect_equal(r[["n2_tts"]], 0)
  expect_equal(r[["n3_tts"]], 0)
  expect_equal(r[["awa_tts"]], 0.)
  expect_equal(r[["tsp"]], 748.5)
  expect_equal(r[["efficiency"]], 0)
  expect_true(is.na(r[["latency"]]))
  expect_true(is.na(r[["waso"]]))
})
