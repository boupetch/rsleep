## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

# Do not run chunks if files are not present.
knitr::opts_chunk$set(eval = all(file.exists("15012016HD.edf","15012016HD.csv")))

## ----setup---------------------------------------------------------------
library(rsleep)

## ----download_edf, eval=FALSE--------------------------------------------
#  download.file("https://osf.io/57j2u/download", "15012016HD.edf")

## ------------------------------------------------------------------------
hypnodensity <- score_stages_edf("15012016HD.edf")

## ---- fig.width=7--------------------------------------------------------
plot_hypnodensity(hypnodensity)

## ---- fig.width=7--------------------------------------------------------
plot_hypnogram(hypnodensity)

## ---- fig.width=7--------------------------------------------------------
download.file("https://osf.io/h4ysj/download", "15012016HD.csv")

plot_hypnogram(
  read_events_noxturnal("15012016HD.csv"))

