## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
# Do not run chunks if files are not present.
knitr::opts_chunk$set(eval = all(file.exists("15012016HD.edf","15012016HD.csv")))

## ----download, eval=FALSE------------------------------------------------
#  download.file("https://osf.io/57j2u/download", "15012016HD.edf")

## ----read_edf------------------------------------------------------------
library(edfReader)

h <- readEdfHeader("15012016HD.edf")

s <- readEdfSignals(h, signals = c("C3-M2", "ECG"))

## ------------------------------------------------------------------------
c3m2 <- s$`C3-M2`$signal

c3m2sr <- s$`C3-M2`$sRate

## ----fig.width = 7-------------------------------------------------------
plot(c3m2[(c3m2sr*30):(c3m2sr*30*2)],type = "l")

## ----download_events, eval=FALSE-----------------------------------------
#  download.file("https://osf.io/h4ysj/download", "15012016HD.csv")

## ----read_events---------------------------------------------------------
library(rsleep)

events <- read_events_noxturnal("15012016HD.csv")

summary(events)

## ----unique_events-------------------------------------------------------
unique(events$event)

## ----hypnogram, fig.width = 7--------------------------------------------
plot_hypnogram(events)

## ----write_mdf-----------------------------------------------------------
write_mdf(edfPath = "15012016HD.edf",
          mdfPath = "15012016HD",
          channels = c("C3-M2","ECG"),
          events = events)

## ----read_mdf------------------------------------------------------------
mdf <- read_mdf("15012016HD")

## ----ecg_example, fig.width = 7------------------------------------------
ecgSrate <- mdf$channels$ECG$metadata$sRate

ecg10 <- mdf$channels$`ECG`$signal[(50*ecgSrate):(60*ecgSrate)]

plot(ecg10, type="l")

