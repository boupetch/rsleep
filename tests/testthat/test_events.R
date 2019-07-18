context("Events related tests")

context("Creating hypnograms")

test_that("Plotting a hypnogram", {
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode.csv")
  hypnogram <- hypnogram(events)
  hypnogram <- plot_hypnogram(events)
  expect_equal(class(hypnogram)[1], "gg")
  expect_equal(class(hypnogram)[2], "ggplot")
})

test_that("Events file check", {

  # begin column
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode.csv")
  events$begin <- NULL
  expect_error(hypnogram(events))

  # end column
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode.csv")
  events$end <- NULL
  expect_error(hypnogram(events))

  # event column
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode.csv")
  events$event <- NULL
  expect_error(hypnogram(events))

  # begin type
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode.csv")
  events$begin <- as.character(events$begin)
  expect_error(hypnogram(events))

  # end type
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode.csv")
  events$end <- as.character(events$end)
  expect_error(hypnogram(events))

  # event type
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode.csv")
  events$event <- as.factor(events$event)
  expect_error(hypnogram(events))

})
