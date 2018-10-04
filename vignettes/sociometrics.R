## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  echo = TRUE, 
  warning = FALSE,
  message = FALSE, 
  fig.width=7
)

library(tidyverse)
library(sociometrics)

## ------------------------------------------------------------------------

df1 <- read_interaction(file="../data/datalab-full.xlsx", type="IR_BT")

class(df1)

## ------------------------------------------------------------------------
df1

## ------------------------------------------------------------------------
unique_ids(df1)

## ------------------------------------------------------------------------
unique_dates(df1)

## ------------------------------------------------------------------------

df2 <- read_audio(file="../data/datalab-full.xlsx", type="VOL_F", replv=T)
class(df2)

## ------------------------------------------------------------------------
df2[which(df2$Badge.ID==2),]

## ------------------------------------------------------------------------
unique_ids(df2, cols="Badge.ID")

## ------------------------------------------------------------------------
df3 <- read_interaction(file="../data/datalab-full.xlsx", type="BT", undirect=T)
df3

## ---- eval=F-------------------------------------------------------------
#  #filter(df1, ids=c("3119", "3180"), RSSI > -75)

## ---- eval=F-------------------------------------------------------------
#  unique_dates(df2)
#  
#  #filter(df2, days=c("2016-01-18"))

## ------------------------------------------------------------------------
qexpl(df1)

## ------------------------------------------------------------------------
qexpl(df3, colour = "Pairs")

## ---- eval=F-------------------------------------------------------------
#  #df2 %>%
#  #  filter(ids=c(1:8), Timestamp > "2016-01-18 10:10:00" & Timestamp < "2016-01-18 11:30:00" & !is.na(Volume)) %>%
#  #  qexpl(., size=.5, title="Volume for badges", leg.pos="right")

## ------------------------------------------------------------------------
df4 <- read_interaction(file="../data/datalab-full.xlsx", type="IR", undirect=T)
df4.1 <- cluster_ts(df4, twl=120) 
df4.2 <- cluster_ts(df4, twl=520)

## ------------------------------------------------------------------------
qexpl(df4.1, colour="clusterTS", leg.pos="right", title="Cluster of 120 seconds")

## ------------------------------------------------------------------------
qexpl(df4.2, colour="clusterTS", leg.pos="right", title="Cluster of 520 seconds")

## ------------------------------------------------------------------------
df4.3 <- cluster_ids(df4) 
qexpl(df4.3, colour="clusterIDSeq")

