#--------------------------------------------------------------
#Ben Neely
#12/04/2024
#Flathead catfish size structure
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

if("ggridges" %in% rownames(installed.packages()) == FALSE) {install.packages("ggridges")}
library(ggridges)

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
        legend.position="top",
        legend.key.width=unit(4,"line"),
        legend.justification="left")
options(scipen=999)

## Set working directory
setwd("C:/Users/Ben.Neely/OneDrive - State of Kansas, OITS/Documents/Active manuscripts/FCF project/FCF TKAS submission/")

## Read in data with import
fish=import("fish.csv")

## Convert impd code to lake name
fish1=fish%>%
  mutate(impd=factor(impd))
levels(fish1$impd)=list(`Afton`="SGCA",
                        `Crawford`="CRSL",
                        `Geary`="GESL",
                        `Leavenworth`="LVSL",
                        `Middle Creek`="MCLA",
                        `Montgomery`="MGSL",
                        `Neosho`="NOSL",
                        `Wilson`="WLSL")

######################################################################################
## Ridgeline plot
######################################################################################

## Organize data frame
lfdat=fish1%>%
  replace_na(list(recap=0))%>%
  filter(recap==0)%>%
  group_by(impd)%>%
  mutate(`Number of fish`=n())%>%
  ungroup()

## Ridgeline plot with points
ggplot(lfdat,aes(x=tl,y=impd))+
  geom_density_ridges(jittered_points=T,
                      quantile_lines=T,
                      quantiles=2,
                      alpha=0.5)+
  scale_y_discrete(limits=rev(levels(factor(lfdat$impd))))+
  scale_x_continuous(limits=c(0,1400),
                     breaks=seq(0,14200,200))+
  labs(y="",x="Total length (mm)")+
  pubtheme
ggsave(plot=last_plot(),"size structure and body condition/lf.png",width=10,height=7,units="in",bg="white")

######################################################################################
## Body condition
######################################################################################
## Add relative weight and PSD values to each fish
condat=lfdat%>%
  select(impd,spp,tl,w,sex)%>%
  mutate(wr=wrAdd(w~tl+spp),
         psd=psdAdd(tl~spp),
         psd_inc=case_when(psd=="substock"~"SS",
                           psd=="stock"~"S-Q",
                           psd=="quality"~"Q-P",
                           psd=="preferred"~"P-M",
                           psd=="memorable"~"M-T",
                           psd=="trophy"~"T",
                           TRUE~"other"))
condatout=condat%>%
  group_by(impd,psd_inc)%>%
  summarize(p05_wr=quantile(wr,0.05,na.rm=T),
            p25_wr=quantile(wr,0.25,na.rm=T),
            p50_wr=quantile(wr,0.50,na.rm=T),
            p75_wr=quantile(wr,0.75,na.rm=T),
            p95_wr=quantile(wr,0.95,na.rm=T))%>%
  ungroup()
#export(condatout,"size structure and body condition/wr_dat.csv")           

## Make boxplots
## Define the boxplot summary function
f=function(x) {
  r=quantile(x,probs=c(0.05,0.25,0.5,0.75,0.95))
  names(r)=c("ymin","lower","middle","upper","ymax")
  r
}

## Level PSD increments as factors appropriately
psdcats=c("SS","S-Q","Q-P","P-M","M-T","T")
condat=condat%>%
  mutate(psd_inc=factor(psd_inc,levels=psdcats))

## Create boxplots
ggplot(condat,aes(x=psd_inc,y=wr))+
  stat_summary(fun.data=f,
               geom="boxplot",
               position=position_dodge(0.75),
               fill="gray75")+
  geom_point(position=position_jitter(width=0.1),alpha=0.4)+
  scale_y_continuous(limits=c(60,160.5),
                     breaks=seq(60,160,20),
                     expand=c(0,0),
                     name="Relative weight")+
  scale_x_discrete(name="PSD category")+
  geom_hline(aes(yintercept=100),linetype="dashed")+
  facet_wrap(~impd)+
  pubtheme+
  theme(strip.text.x = element_text(size=16))
ggsave(plot=last_plot(),"size structure and body condition/wr.png",width=12.5,height=8.25,units="in",bg="white")