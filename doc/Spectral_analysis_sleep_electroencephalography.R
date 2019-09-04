## ----env, include = FALSE------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
# Do not run chunks if files are not present.
knitr::opts_chunk$set(eval = all(file.exists("15012016HD.edf","15012016HD.csv")))

## ----download, eval=FALSE------------------------------------------------
#  download.file("https://osf.io/57j2u/download", "15012016HD.edf")
#  download.file("https://osf.io/h4ysj/download", "15012016HD.csv")

## ----edf-----------------------------------------------------------------
library(edfReader)

h <- readEdfHeader("15012016HD.edf")

s <- readEdfSignals(h, signals = "C3-M2")

## ----hypnogram, fig.width = 7--------------------------------------------
library(rsleep)

events <- read_events_noxturnal("15012016HD.csv")

plot_hypnogram(events)

## ----epoching------------------------------------------------------------
hypnogram <- head(hypnogram(events),-2)

startTime <- as.numeric(as.POSIXct(s$startTime))

epochs <- rsleep::epochs(s$signal,s$sRate,
                         epoch = hypnogram,
                         startTime = startTime)

## ----pwelch, fig.width=7, message=FALSE, error=FALSE---------------------
p <- pwelch(epochs[[200]], sRate = s$sRate)

summary(p)

## ----avg_pdg_compute-----------------------------------------------------
periodograms <- mapply(x = epochs, y = hypnogram$event, FUN = function(x,y){
  p <- pwelch(x, sRate = s$sRate, show = FALSE)
  p <- as.data.frame(p[p$hz <= 30,])
  p$stage <- y
  p
}, SIMPLIFY = F)

## ----pdg_rbind-----------------------------------------------------------
periodograms_df <- do.call("rbind", periodograms)

## ----pdg_aggregate-------------------------------------------------------
avg_periodograms <- aggregate(psd ~ hz+stage, periodograms_df, mean)

## ----periodogram_plot, fig.width=7, message=FALSE, error=FALSE-----------
library(ggplot2)

palette <- c("#F98400","#F2AD00","#00A08A","#FF0000","#5BBCD6")

ggplot(avg_periodograms, aes(x=hz,y=psd,color=stage)) +
  geom_line() + theme_bw() +
  theme(legend.title = element_blank()) + 
  scale_colour_manual(name = "stage",
                      values = palette) +
  xlab("Frequency (Hertz)") + ylab("PSD")

## ----bands_compute-------------------------------------------------------
bands <- lapply(epochs,function(x){
    bands_psd(bands = list(c(0.5,3.5), # Delta
                             c(3.5,7.5), # Theta
                             c(7.5,13), # Alpha
                             c(13,30)), # Beta
                signal = x, sRate = s$sRate,
                normalize = c(0.5,30))
})

## ----bands_reshape-------------------------------------------------------
bands_df <- data.frame(matrix(unlist(bands), nrow=length(bands), byrow=TRUE))

colnames(bands_df) <- c("Delta","Theta","Alpha","Beta")

## ----bands_stages--------------------------------------------------------
bands_df$stage <- hypnogram$event

## ----bands_plot, fig.width=7, fig.height=10, message=FALSE, error=FALSE----
bands_df_long <- reshape2::melt(bands_df, "stage")

palette <-c("#F98400", "#F2AD00", "#00A08A", "#FF0000", "#5BBCD6")

ggplot(bands_df_long,
       aes(x=stage,y=value,color=stage)) +
  geom_boxplot() +
  facet_grid(rows = vars(variable),scales = "free") +
  scale_colour_manual(name = "stage",
                      values = palette) +
  theme_bw() + xlab("") + ylab("PSD") + 
  theme(legend.position = "none")

## ----ml_psd--------------------------------------------------------------
features <- mapply(x = epochs, y = hypnogram$event, 
             FUN = function(x,y){
  periodogram <- psm(x, sRate = s$sRate, 200)
  periodogram <- periodogram[periodogram$hz <= 30,]
  periodogram <- setNames(t(periodogram)[1,], t(periodogram)[2,])
  periodogram <- c(periodogram, setNames(y,"stage"))
}, SIMPLIFY = F)

features <- as.data.frame(do.call("rbind", features))

features$stage <- as.factor(features$stage)

## ----split---------------------------------------------------------------
set.seed(12345)

library(caret)

inTrain <- createDataPartition(features$stage, p=0.70, list=F)

training <- features[inTrain,]
validation <- features[-inTrain,]

## ----fit-----------------------------------------------------------------
control <- caret::trainControl(method="cv", 3, allowParallel = TRUE)

fit <- train(stage ~ ., training, method= 'rf', trControl = control)

## ----predict-------------------------------------------------------------
results <- predict(fit, validation)

## ----ck------------------------------------------------------------------
library(psy)

ckappa(cbind(results,validation$stage))

