context("Writing records")

test_that("Writing all channels and overwriting with events", {

  # First write
  write_mdf(edfPath = "data/subject1.edf",
            mdfPath = "data/sample")
  expect_equal(length(list.dirs("data/sample")), 91)

  # Overwrite
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode.csv")
  write_mdf(edfPath = "data/subject1.edf",
            mdfPath = "data/sample",
            channels = c("Activity","Airflow"),
            events = read_events_noxturnal("data/noxturnal_events_example_unicode.csv"))
  expect_equal(length(list.dirs("data/sample")), 3)

  events.write <- jsonlite::read_json("data/sample/events.json",simplifyVector = TRUE)
  expect_equal(length(events.write), length(events))
  expect_equal(nrow(events.write), nrow(events))
})

test_that("Do not write channels", {
  write_mdf(edfPath = "data/subject1.edf",
            mdfPath = "data/sample",
            channels = c())
  expect_equal(length(list.dirs("data/sample")), 1)
  unlink("data/sample", recursive = TRUE)
})

test_that("Corrupted", {
  expect_warning({
    write_channel(channel = "F4",
                  signals = NULL,
                  headers = edfReader::readSignal("data/subject1.edf"),
                  mdfPath = "data/sample",
                  endian = "little")
  })
})

test_that("Missing mdfPath argument", {
  expect_error({
    write_channel(channel = "F4",
                  signals = list("F4"=list("signal"=c(1:100))),
                  headers = edfReader::readEdfHeader("data/subject1.edf"),
                  endian = "little")
  })
})

test_that("Read file", {
  write_mdf(edfPath = "data/subject1.edf",
            mdfPath = "data/sample",
            channels = c(),
            events = read_events_noxturnal("data/noxturnal_events_example_unicode.csv"))
  r <- read_mdf("data/sample",channels = c())
  expect_equal(nrow(r$events), 1411)
  unlink("data/sample",recursive = TRUE)
})

test_that("Read selected channels from MDF", {
  write_mdf(edfPath = "data/subject1.edf",
            mdfPath = "data/sample",
            channels = c("Airflow"),
            events = read_events_noxturnal("data/noxturnal_events_example_unicode.csv"))
  r <- read_mdf("data/sample",channels = c("Airflow"))
  expect_equal(nrow(r$events), 1411)
  unlink("data/sample",recursive = TRUE)
})

test_that("Read events Noxturnal 2", {
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode_2.csv")
  expect_equal(nrow(events), 1412)
})

test_that("Read events Noxturnal 3", {
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode_3.csv")
  expect_equal(nrow(events), 2073)
})


