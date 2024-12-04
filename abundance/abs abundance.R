#--------------------------------------------------------------
#Ben Neely
#12/04/2024
#Population estimates using Schnabel estimator
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
fish=import("fish.csv")%>%
  mutate(impd=recode(impd,
                     SGCA="Afton",
                     CRSL="Crawford",
                     GESL="Geary",
                     LVSL="Leavenworth",
                     MCLA="Middle Creek",
                     MGSL="Montgomery",
                     NOSL="Neosho",
                     WLSL="Wilson"))

samp=import("samp.csv")%>%
  mutate(impd=recode(impd,
                     SGCA="Afton",
                     CRSL="Crawford",
                     GESL="Geary",
                     LVSL="Leavenworth",
                     MCLA="Middle Creek",
                     MGSL="Montgomery",
                     NOSL="Neosho",
                     WLSL="Wilson"))

## Summarize total number of marked and unmarked fish from each day
## Note that we're ignoring site and grouping by impoundment/date
fish1=fish%>%
  group_by(impd,date)%>%
  summarize(n=n(),
            m=sum(recap,na.rm=T),
            M=n-m)%>%
  ungroup()

## Summarize sample data
## We only need to keep one sample per impoundment/date
## Grid is irrevalent since our study unit is impoundment
samp1=samp%>%
  group_by(impd,date)%>%
  slice(1)%>%
  ungroup()

## Combine sample and fish data
dat=samp1%>%
  left_join(fish1,by=c("impd","date"))%>%
  arrange(impd,date)%>%
  complete(impd,
           fill=list(n=0,m=0,M=0))

dat$Mcum=ave(dat$M,dat$impd,FUN=cumsum)

###################################################################
## Look at Schnabel population estimates for each impoundment
###################################################################

## SGCA
sgca=filter(dat,impd=="Afton")
sgca1=mrClosed(M=sgca$Mcum,n=sgca$n,m=sgca$m,method="Schnabel")
(sgca_out=cbind("Afton",summary(sgca1),confint(sgca1,verbose=T)))

## CRSL
crsl=filter(dat,impd=="Crawford")
crsl1=mrClosed(M=crsl$Mcum,n=crsl$n,m=crsl$m,method="Schnabel")
(crsl_out=cbind("Crawford",summary(crsl1),confint(crsl1,verbose=T)))

## GESL
gesl=filter(dat,impd=="Geary")
gesl1=mrClosed(M=gesl$Mcum,n=gesl$n,m=gesl$m,method="Schnabel")
(gesl_out=cbind("Geary",summary(gesl1),confint(gesl1,verbose=T)))

## LVSL
lvsl=filter(dat,impd=="Leavenworth")
lvsl1=mrClosed(M=lvsl$Mcum,n=lvsl$n,m=lvsl$m,method="Schnabel")
(lvsl_out=cbind("Leavenworth",summary(lvsl1),confint(lvsl1,verbose=T)))

## MCLA
mcla=filter(dat,impd=="Middle Creek")
mcla1=mrClosed(M=mcla$Mcum,n=mcla$n,m=mcla$m,method="Schnabel")
(mcla_out=cbind("Middle Creek",summary(mcla1),confint(mcla1,verbose=T)))

## MGSL
mgsl=filter(dat,impd=="Montgomery")
mgsl1=mrClosed(M=mgsl$Mcum,n=mgsl$n,m=mgsl$m,method="Schnabel")
(mgsl_out=cbind("Montgomery",summary(mgsl1),confint(mgsl1,verbose=T)))

## NOSL
nosl=filter(dat,impd=="Neosho")
nosl1=mrClosed(M=nosl$Mcum,n=nosl$n,m=nosl$m,method="Schnabel")
(nosl_out=cbind("Neosho",summary(nosl1),confint(nosl1,verbose=T)))

## WLSL
wlsl=filter(dat,impd=="Wilson")
wlsl1=mrClosed(M=wlsl$Mcum,n=wlsl$n,m=wlsl$m,method="Schnabel")
(wlsl_out=cbind("Wilson",summary(wlsl1),confint(wlsl1,verbose=T)))

###################################################################
###################################################################

## Combine estimates
ests=as_tibble(rbind(crsl_out,gesl_out,lvsl_out,mcla_out,
                         mgsl_out,nosl_out,sgca_out,wlsl_out))%>%
  rename(impd=V1,n_est=N,lci95=`95% LCI`,uci95=`95% UCI`)%>%
  mutate(n_est=as.numeric(n_est),lci95=as.numeric(lci95),uci95=as.numeric(uci95))%>%
  select(impd,n_est,lci95,uci95)%>%
  arrange(desc(n_est))

## Add surface acres and hectares
size=tibble(impd=c("Afton","Wilson","Crawford","Geary","Leavenworth","Neosho","Middle Creek","Montgomery"),
           ac=c(248,110,150,97,160,92,280,105),
           ha=ac*0.404686)

comb_dat=inner_join(ests,size,"impd")%>%
  mutate(n_est_ac=n_est/ac,
         lci95_ac=lci95/ac,
         uci95_ac=uci95/ac,
         n_est_ha=n_est/ha,
         lci95_ha=lci95/ha,
         uci95_ha=uci95/ha)

###################################################################
## Plot number per hectare
###################################################################

ha=ggplot(comb_dat)+
  geom_pointrange(aes(y=factor(impd),x=n_est_ha,xmin=lci95_ha,xmax=uci95_ha))+
  labs(x="Individuals/ha",y="")+
  scale_x_continuous(limits=c(0,14.8),
                     breaks=seq(0,14,1),
                     expand=c(0,0))+
  scale_y_discrete(limits=rev)+
  pubtheme
ggsave(plot=ha,"abundance/abs_abun.png",height=5,width=8,bg="white")