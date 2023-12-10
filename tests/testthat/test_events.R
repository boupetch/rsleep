context("Events related tests")

context("Creating hypnograms")

test_that("Plotting a hypnogram", {
  
  hypnogram <- data.frame(begin = as.POSIXlt(c(1536967800,1536967830,1536967860),origin = "1970-01-01"))
  hypnogram$end <- as.POSIXlt(c(1536967830,1536967860,1536967890),origin = "1970-01-01")
  hypnogram$event = c("N3","N3","REM")
  hypnogram <-plot_hypnogram(hypnogram)
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

test_that("Hypnogram smoothing",{
  
  hypnogram <- data.frame(
    begin = as.POSIXlt(c(1536967800,1536967830,1536967860),
                       origin = "1970-01-01"))
  hypnogram$end <- as.POSIXlt(c(1536967830,1536967860,1536967890),
                              origin = "1970-01-01")
  hypnogram$event = c("REM","N2","REM")
  
  # smooth_liang2012(hypnogram)
  
  hypnogram <- smooth_hypnogram(hypnogram, "N2","REM", 1)
  
  expect_true(all(hypnogram$event == "REM"))
  
  # smooth_liang2012(
  #   read_events_noxturnal("data/noxturnal_events_example_unicode.csv"))
  
})

test_that("Sleep periods",{
  hypnogram <- data.frame(
    begin = as.POSIXlt(c(1536967800,1536967830,1536967860),
                       origin = "1970-01-01"))
  hypnogram$end <- as.POSIXlt(c(1536967830,1536967860,1536967890),
                              origin = "1970-01-01")
  hypnogram$event = c("REM","N2","REM")
  periods <- periods(hypnogram = hypnogram,mode = "continuous")
  expect_true(nrow(periods) == 1)
  periods <- periods(hypnogram = hypnogram,mode = "stages")
  expect_true(nrow(periods) == 3)
  expect_true(ncol(periods) == 4)
})

test_that("Transitions",{
  events <- data.frame(event = c(
    "AWA","N1","N2","N2", "N3","N3",
    "REM","N2","REM","REM", "N2","REM","AWA"))
  
  events$begin <- as.POSIXlt(seq(from = 0, to = 30*(nrow(events)-1), by = 30),origin = "1970-01-01")
  events$end <- as.POSIXlt(seq(from = 30, to = 30*nrow(events), by = 30), origin = "1970-01-01")
  expect_true(ncol(transitions(events, format = "dataframe")) == 5)
  expect_true(length(transitions(events)) == 25)
  
  events <- data.frame(event = c(
    "N1","N2","N2", "N3","N3",
    "REM","N2","REM","REM", "N2","REM"))
  
  events$begin <- as.POSIXlt(seq(from = 0, to = 30*(nrow(events)-1), by = 30),origin = "1970-01-01")
  events$end <- as.POSIXlt(seq(from = 30, to = 30*nrow(events), by = 30), origin = "1970-01-01")
  expect_true(ncol(transitions(events, format = "dataframe")) == 4)
  expect_true(length(transitions(events)) == 16)
  
})



