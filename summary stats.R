#--------------------------------------------------------------
#Ben Neely
#12/04/2024
#Summary statistics
#--------------------------------------------------------------

## Clear R
cat("\014")  
rm(list=ls())

## Install and load packages
## Checks if package is installed, installs if not, activates for current session
if("FSA" %in% rownames(installed.packages()) == FALSE) {install.packages("FSA")}
library(FSA)

if("rio" %in% rownames(installed.packages()) == FALSE) {install.packages("rio")}
library(rio)

if("lubridate" %in% rownames(installed.packages()) == FALSE) {install.packages("lubridate")}
library(lubridate)

if("patchwork" %in% rownames(installed.packages()) == FALSE) {install.packages("patchwork")}
library(patchwork)

if("tidyverse" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyverse")}
library(tidyverse)

## Set ggplot theme
pubtheme=theme_classic()+
  theme(panel.grid=element_blank(), 
        panel.background=element_blank(),
        plot.background=element_blank(),
        panel.border=element_rect(fill="transparent"),
        axis.title=element_text(size=22,color="black",face="bold"),
        axis.text=element_text(size=18,color="black"),
        legend.position=c(0.02,0.98),
        legend.justification=c("left","top"),
        legend.title=element_blank(),
        legend.text=element_text(size=8))

## Set working directory
setwd("C:/Users/Ben.Neely/OneDrive - State of Kansas, OITS/Documents/Active manuscripts/FCF project/FCF TKAS submission/")

## Read in data with import
samp=import("samp.csv")
fish=import("fish.csv")

#########################################################
## Summarize sampling effort
## Total effort
sum(samp$effort)

## Total grids sampled
samp%>%
  group_by(impd,grid)%>%
  summarize(n=n())

## Number of grids sampled at each impoundment
samp%>%
  group_by(impd)%>%
  summarize(n=n()/9)

#########################################################
## Summarize fish catch
## Total fish sampled
nrow(fish)

## Number of fish recaptured at least once
nrow(subset(fish,recap==1))

## Number of fish captured per impoundment
fish1=fish%>%
  group_by(impd)%>%
  summarize(catch=n(),
            recap=sum(recap,na.rm=T),
            recap_prop=recap/catch)

## Min, max, mean, and SE of catch per impoundment
min(fish1$catch)
max(fish1$catch)
mean(fish1$catch)
sd(fish1$catch)/sqrt(nrow(fish1))

## Min, max, mean, and SE of recapture proportion per impoundment
min(fish1$recap_prop)
max(fish1$recap_prop)
mean(fish1$recap_prop)
sd(fish1$recap_prop)/sqrt(nrow(fish1))