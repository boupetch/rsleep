## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

# Do not run chunks if files are not present.
knitr::opts_chunk$set(eval = all(file.exists("15012016HD.edf","15012016HD.csv")))

## ----setup---------------------------------------------------------------
library(rsleep)

## ------------------------------------------------------------------------
hypnodensity <- score_stages_edf("15012016HD.edf")

## ------------------------------------------------------------------------
plot_hypnodensity(hypnodensity)

## ------------------------------------------------------------------------
plot_hypnogram(hypnodensity)

## ------------------------------------------------------------------------
plot_hypnogram(
  read_events_noxturnal("15012016HD.csv"))

