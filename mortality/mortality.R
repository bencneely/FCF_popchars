#--------------------------------------------------------------
#Ben Neely
#12/04/2024
#Flathead mortality
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
        legend.position="none")
options(scipen=999)

## Set working directory
setwd("C:/Users/Ben.Neely/OneDrive - State of Kansas, OITS/Documents/Active manuscripts/FCF project/FCF TKAS submission")

## Read in data with import
dat=import("fish.csv")

## Filter out so we just have impd and age
all=dat%>%
  select(impd,age)

## Summarize number of fish per age group
allsum=all%>%
  group_by(impd,age)%>%
  summarize(n=n())%>%
  ungroup()

allsum%>%
  group_by(impd)%>%
  summarize(max(age))

## Add rows showing zero fish of a given age captured and add 1 to each count
allsum1=allsum%>%
  complete(impd,age,fill=list(n=0))%>%
  mutate(n=n+1)

## CRSL
CRSL_cc=catchCurve(n~age,data=subset(allsum1,impd=="CRSL"),ages2use=4:13,weight=T)
plot(CRSL_cc)
CRSL_A=summary(CRSL_cc)[2,1]
CRSL_A_lci=confint(CRSL_cc)[2,1]
CRSL_A_uci=confint(CRSL_cc)[2,2]
CRSL_out=tibble(bind_cols("CRSL",CRSL_A_lci,CRSL_A,CRSL_A_uci))

## GESL
GESL_cc=catchCurve(n~age,data=subset(allsum1,impd=="GESL"),ages2use=4:15,weight=T)
plot(GESL_cc)
GESL_A=summary(GESL_cc)[2,1]
GESL_A_lci=confint(GESL_cc)[2,1]
GESL_A_uci=confint(GESL_cc)[2,2]
GESL_out=tibble(bind_cols("GESL",GESL_A_lci,GESL_A,GESL_A_uci))

## LVSL
LVSL_cc=catchCurve(n~age,data=subset(allsum1,impd=="LVSL"),ages2use=5:17,weight=T)
plot(LVSL_cc)
LVSL_A=summary(LVSL_cc)[2,1]
LVSL_A_lci=confint(LVSL_cc)[2,1]
LVSL_A_uci=confint(LVSL_cc)[2,2]
LVSL_out=tibble(bind_cols("LVSL",LVSL_A_lci,LVSL_A,LVSL_A_uci))

## MCLA
MCLA_cc=catchCurve(n~age,data=subset(allsum1,impd=="MCLA"),ages2use=6:15,weight=T)
plot(MCLA_cc)
MCLA_A=summary(MCLA_cc)[2,1]
MCLA_A_lci=confint(MCLA_cc)[2,1]
MCLA_A_uci=confint(MCLA_cc)[2,2]
MCLA_out=tibble(bind_cols("MCLA",MCLA_A_lci,MCLA_A,MCLA_A_uci))

## MGSL
MGSL_cc=catchCurve(n~age,data=subset(allsum1,impd=="MGSL"),ages2use=4:13,weight=T)
plot(MGSL_cc)
MGSL_A=summary(MGSL_cc)[2,1]
MGSL_A_lci=confint(MGSL_cc)[2,1]
MGSL_A_uci=confint(MGSL_cc)[2,2]
MGSL_out=tibble(bind_cols("MGSL",MGSL_A_lci,MGSL_A,MGSL_A_uci))

## NOSL
NOSL_cc=catchCurve(n~age,data=subset(allsum1,impd=="NOSL"),ages2use=4:12,weight=T)
plot(NOSL_cc)
NOSL_A=summary(NOSL_cc)[2,1]
NOSL_A_lci=confint(NOSL_cc)[2,1]
NOSL_A_uci=confint(NOSL_cc)[2,2]
NOSL_out=tibble(bind_cols("NOSL",NOSL_A_lci,NOSL_A,NOSL_A_uci))

## SGCA
SGCA_cc=catchCurve(n~age,data=subset(allsum1,impd=="SGCA"),ages2use=8:21,weight=T)
plot(SGCA_cc)
SGCA_A=summary(SGCA_cc)[2,1]
SGCA_A_lci=confint(SGCA_cc)[2,1]
SGCA_A_uci=confint(SGCA_cc)[2,2]
SGCA_out=tibble(bind_cols("SGCA",SGCA_A_lci,SGCA_A,SGCA_A_uci))

## WLSL
WLSL_cc=catchCurve(n~age,data=subset(allsum1,impd=="WLSL"),ages2use=5:21,weight=T)
plot(WLSL_cc)
WLSL_A=summary(WLSL_cc)[2,1]
WLSL_A_lci=confint(WLSL_cc)[2,1]
WLSL_A_uci=confint(WLSL_cc)[2,2]
WLSL_out=tibble(bind_cols("WLSL",WLSL_A_lci,WLSL_A,WLSL_A_uci))

## Combine all estimates
plotdat=tibble(bind_rows(CRSL_out,GESL_out,LVSL_out,MCLA_out,
                         MGSL_out,NOSL_out,SGCA_out,WLSL_out))
colnames(plotdat)=c("impd","A_lci","A","A_uci")
#export(plotdat,"mortality/annual mortality.csv")

## Summary stats for annual mortality
min(plotdat$A)
max(plotdat$A)
mean(plotdat$A)
sd(plotdat$A)/sqrt(nrow(plotdat))

## Plot estimates
plotdat=plotdat%>%
  mutate(impd=factor(impd,levels=c("WLSL","NOSL","MGSL","MCLA","GESL","LVSL","CRSL","SGCA")),
         impd=recode(impd,
                     SGCA="Afton",
                     CRSL="Crawford",
                     GESL="Geary",
                     LVSL="Leavenworth",
                     MCLA="Middle Creek",
                     MGSL="Montgomery",
                     NOSL="Neosho",
                     WLSL="Wilson"))
         
plotdat1=plotdat%>%
  mutate(A_lci=case_when(impd=="Montgomery" ~ A,
                         TRUE ~ A_lci),
         A_uci=case_when(impd=="Montgomery" ~ A,
                         TRUE ~ A_uci))
mn_A=mean(plotdat1$A)

ggplot(plotdat1)+
  geom_pointrange(aes(y=impd,x=A,xmin=A_lci,xmax=A_uci),linewidth=1.5,fatten=10)+
  scale_x_continuous(breaks=seq(0,50,5),
                     name="Annual mortality (%)",
                     expand=c(0,0))+
  scale_y_discrete(name="")+
  coord_cartesian(xlim=c(0,52))+
  geom_vline(xintercept=mn_A,linewidth=1.5,linetype="dashed")+
  pubtheme
ggsave(plot=last_plot(),"mortality/ann mort.png",width=10,height=7,units="in",bg="white")
