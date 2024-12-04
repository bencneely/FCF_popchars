#--------------------------------------------------------------
#Ben Neely
#12/04/2024
#Examine growth of flathead catfish
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

if("nlstools" %in% rownames(installed.packages()) == FALSE) {install.packages("nlstools")}
library(nlstools)

if("car" %in% rownames(installed.packages()) == FALSE) {install.packages("car")}
library(car)

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
setwd("C:/Users/Ben.Neely/OneDrive - State of Kansas, OITS/Documents/Active manuscripts/FCF project/FCF TKAS submission/")

## Read in data with import
dat=import("fish.csv")%>%
  filter(age>=0)

## Examine growth with no models
ggplot(dat)+
  geom_point(aes(x=age,y=tl))+
  facet_wrap(~impd,nrow=2)+
  labs(x="Estimated age",y="Total length (mm)")+
  pubtheme

#######################################################################
## Estimate Gompertz growth models for each population

## Set up function to estimate length at lots of ages
gompred=function(x) predict(x,data.frame(age=ages))
ages=seq(0,22,by=0.05)

## Define growth model functions
gom=GompertzFuns()

#######################################################################

####################
## SGCA
sgca=filter(dat,impd=="SGCA")

## Gompertz growth model
## Manually adjust starting values until things look good
plot(tl~age,data=sgca)
sgca_gom_sv=list(Linf=1200,gi=0.2,ti=0.5)
curve(gom(x,unlist(sgca_gom_sv)),from=1,to=22,add=T)

## Fit the model and calculate confidence intervals
sgca_gom_fit=nls(tl~gom(age,Linf,gi,ti),data=sgca,start=sgca_gom_sv)
sgca_gom_boot=nlsBoot(sgca_gom_fit,niter=400)
(sgca_gom_parms=cbind(ests=coef(sgca_gom_fit),confint(sgca_gom_boot,conf.level=0.80)))

## Use model to predict length at age for plotting
sgca_predboot=Boot(sgca_gom_fit,f=gompred)
sgca_preds=data.frame("SGCA",
                      ages,
                      gompred(sgca_gom_fit),
                      confint(sgca_predboot))%>%
  filter(ages<=max(sgca$age))
names(sgca_preds)=c("impd","age","tl","lci80","uci80")
####################
## CRSL
crsl=filter(dat,impd=="CRSL")

## Gompertz growth model
## Manually adjust starting values until things look good
plot(tl~age,data=crsl)
crsl_gom_sv=list(Linf=1200,gi=0.1,ti=4)
curve(gom(x,unlist(crsl_gom_sv)),from=1,to=22,add=T)

## Fit the model and calculate confidence intervals
crsl_gom_fit=nls(tl~gom(age,Linf,gi,ti),data=crsl,start=crsl_gom_sv)
crsl_gom_boot=nlsBoot(crsl_gom_fit,niter=400)
(crsl_gom_parms=cbind(ests=coef(crsl_gom_fit),confint(crsl_gom_boot,conf.level=0.80)))

## Use model to predict length at age for plotting
crsl_predboot=Boot(crsl_gom_fit,f=gompred)
crsl_preds=data.frame("CRSL",
                      ages,
                      gompred(crsl_gom_fit),
                      confint(crsl_predboot))%>%
  filter(ages<=max(crsl$age))
names(crsl_preds)=c("impd","age","tl","lci80","uci80")

####################
## GESL
gesl=filter(dat,impd=="GESL")

## Gompertz growth model
## Manually adjust starting values until things look good
plot(tl~age,data=gesl)
gesl_gom_sv=list(Linf=1200,gi=0.1,ti=4)
curve(gom(x,unlist(gesl_gom_sv)),from=1,to=22,add=T)

## Fit the model and calculate confidence intervals
gesl_gom_fit=nls(tl~gom(age,Linf,gi,ti),data=gesl,start=gesl_gom_sv)
gesl_gom_boot=nlsBoot(gesl_gom_fit,niter=400)
(gesl_gom_parms=cbind(ests=coef(gesl_gom_fit),confint(gesl_gom_boot,conf.level=0.80)))

## Use model to predict length at age for plotting
gesl_predboot=Boot(gesl_gom_fit,f=gompred)
gesl_preds=data.frame("GESL",
                      ages,
                      gompred(gesl_gom_fit),
                      confint(gesl_predboot))%>%
  filter(ages<=max(gesl$age))
names(gesl_preds)=c("impd","age","tl","lci80","uci80")

####################
## LVSL
lvsl=filter(dat,impd=="LVSL")

## Gompertz growth model
## Manually adjust starting values until things look good
plot(tl~age,data=lvsl)
lvsl_gom_sv=list(Linf=1200,gi=0.1,ti=4)
curve(gom(x,unlist(lvsl_gom_sv)),from=1,to=22,add=T)

## Fit the model and calculate confidence intervals
lvsl_gom_fit=nls(tl~gom(age,Linf,gi,ti),data=lvsl,start=lvsl_gom_sv)
lvsl_gom_boot=nlsBoot(lvsl_gom_fit,niter=400)
(lvsl_gom_parms=cbind(ests=coef(lvsl_gom_fit),confint(lvsl_gom_boot,conf.level=0.80)))

## Use model to predict length at age for plotting
lvsl_predboot=Boot(lvsl_gom_fit,f=gompred)
lvsl_preds=data.frame("LVSL",
                      ages,
                      gompred(lvsl_gom_fit),
                      confint(lvsl_predboot))%>%
  filter(ages<=max(lvsl$age))
names(lvsl_preds)=c("impd","age","tl","lci80","uci80")

####################
## MCLA
mcla=filter(dat,impd=="MCLA")

## Gompertz growth model
## Manually adjust starting values until things look good
plot(tl~age,data=mcla)
mcla_gom_sv=list(Linf=1200,gi=0.1,ti=4)
curve(gom(x,unlist(mcla_gom_sv)),from=1,to=22,add=T)

## Fit the model and calculate confidence intervals
mcla_gom_fit=nls(tl~gom(age,Linf,gi,ti),data=mcla,start=mcla_gom_sv)
mcla_gom_boot=nlsBoot(mcla_gom_fit,niter=400)
(mcla_gom_parms=cbind(ests=coef(mcla_gom_fit),confint(mcla_gom_boot,conf.level=0.80)))

## Use model to predict length at age for plotting
mcla_predboot=Boot(mcla_gom_fit,f=gompred)
mcla_preds=data.frame("MCLA",
                      ages,
                      gompred(mcla_gom_fit),
                      confint(mcla_predboot))%>%
  filter(ages<=max(mcla$age))
names(mcla_preds)=c("impd","age","tl","lci80","uci80")

####################
## MGSL
mgsl=filter(dat,impd=="MGSL")

## Gompertz growth model
## Manually adjust starting values until things look good
plot(tl~age,data=mgsl)
mgsl_gom_sv=list(Linf=1200,gi=0.1,ti=4)
curve(gom(x,unlist(mgsl_gom_sv)),from=1,to=22,add=T)

## Fit the model and calculate confidence intervals
mgsl_gom_fit=nls(tl~gom(age,Linf,gi,ti),data=mgsl,start=mgsl_gom_sv)
mgsl_gom_boot=nlsBoot(mgsl_gom_fit,niter=400)
(mgsl_gom_parms=cbind(ests=coef(mgsl_gom_fit),confint(mgsl_gom_boot,conf.level=0.80)))

## Use model to predict length at age for plotting
mgsl_predboot=Boot(mgsl_gom_fit,f=gompred)
mgsl_preds=data.frame("MGSL",
                      ages,
                      gompred(mgsl_gom_fit),
                      confint(mgsl_predboot))%>%
  filter(ages<=max(mgsl$age))
names(mgsl_preds)=c("impd","age","tl","lci80","uci80")

####################
## NOSL
nosl=filter(dat,impd=="NOSL")

## Gompertz growth model
## Manually adjust starting values until things look good
plot(tl~age,data=nosl)
nosl_gom_sv=list(Linf=1200,gi=0.1,ti=4)
curve(gom(x,unlist(nosl_gom_sv)),from=1,to=22,add=T)

## Fit the model and calculate confidence intervals
#nosl_gom_fit=nls(tl~gom(age,Linf,gi,ti),data=nosl,start=nosl_gom_sv)
#nosl_gom_boot=nlsBoot(nosl_gom_fit,niter=400)
#nosl_gom_parms=cbind(ests=coef(nosl_gom_fit),confint(nosl_gom_boot,conf.level=0.80))

## Use model to predict length at age for plotting
#nosl_predboot=Boot(nosl_gom_fit,f=gompred)
#nosl_preds=data.frame("NOSL",
#                      ages,
#                      gompred(nosl_gom_fit),
#                      confint(nosl_predboot))%>%
#           filter(ages<=max(nosl$age))
#names(nosl_preds)=c("impd","age","tl","lci80","uci80")

####################
## WLSL
wlsl=filter(dat,impd=="WLSL")

## Gompertz growth model
## Manually adjust starting values until things look good
plot(tl~age,data=wlsl)
wlsl_gom_sv=list(Linf=1200,gi=0.1,ti=4)
curve(gom(x,unlist(wlsl_gom_sv)),from=1,to=22,add=T)

## Fit the model and calculate confidence intervals
wlsl_gom_fit=nls(tl~gom(age,Linf,gi,ti),data=wlsl,start=wlsl_gom_sv)
wlsl_gom_boot=nlsBoot(wlsl_gom_fit,niter=400)
(wlsl_gom_parms=cbind(ests=coef(wlsl_gom_fit),confint(wlsl_gom_boot,conf.level=0.80)))

asdf=confint(wlsl_gom_boot,conf.level=0.80)

## Use model to predict length at age for plotting
wlsl_predboot=Boot(wlsl_gom_fit,f=gompred)
wlsl_preds=data.frame("WLSL",
                      ages,
                      gompred(wlsl_gom_fit),
                      confint(wlsl_predboot))%>%
  filter(ages<=max(wlsl$age))
names(wlsl_preds)=c("impd","age","tl","lci80","uci80")

#######################################################################
## Output Gompertz model parameters for each structure
growth_ests=as.data.frame(rbind(crsl_gom_parms,gesl_gom_parms,
                                lvsl_gom_parms,mcla_gom_parms,
                                mgsl_gom_parms,sgca_gom_parms,wlsl_gom_parms))%>%
  mutate(impd=c(rep("CRSL",3),
                rep("GESL",3),
                rep("LVSL",3),
                rep("MCLA",3),
                rep("MGSL",3),
                rep("SGCA",3),
                rep("WLSL",3)),
         parm=c("Linf","gi","ti","Linf","gi","ti",
                "Linf","gi","ti","Linf","gi","ti",
                "Linf","gi","ti","Linf","gi","ti","Linf","gi","ti"))%>%
  select(impd,parm,est=ests,lci80=`80% LCI`,uci80=`80% UCI`)

## Export as spreadsheet
#export(growth_ests,"growth/Gompertz growth estimates.xlsx")

#######################################################################
## Plot Gompertz growth models

## SGCA
sgca_plot=ggplot()+
  geom_ribbon(sgca_preds,mapping=aes(x=age,ymin=lci80,ymax=uci80),fill="gray70")+
  geom_point(sgca,mapping=aes(x=age,y=tl),alpha=0.8)+
  scale_x_continuous(limits=c(0,22.5),
                     breaks=seq(0,22,2),
                     labels=seq(0,22,2),
                     expand=c(0,0),
                     name="")+
  scale_y_continuous(limits=c(0,1620),
                     breaks=seq(0,1600,200),
                     labels=seq(0,1600,200),
                     expand=c(0,0),
                     name="Total length (mm)")+
  annotate("text",x=0.2,y=1200,label="Afton",hjust=0,vjust=1,size=8)+
  coord_cartesian(ylim=c(0,1220),xlim=c(0,22.5))+
  pubtheme

## CRSL
crsl_plot=ggplot()+
  geom_ribbon(crsl_preds,mapping=aes(x=age,ymin=lci80,ymax=uci80),fill="gray70")+
  geom_point(crsl,mapping=aes(x=age,y=tl),alpha=0.8)+
  scale_x_continuous(limits=c(0,22.5),
                     breaks=seq(0,22,2),
                     labels=seq(0,22,2),
                     expand=c(0,0),
                     name="")+
  scale_y_continuous(limits=c(0,1620),
                     breaks=seq(0,1600,200),
                     labels=seq(0,1600,200),
                     expand=c(0,0),
                     name="")+
  annotate("text",x=0.2,y=1200,label="Crawford",hjust=0,vjust=1,size=8)+
  coord_cartesian(ylim=c(0,1220),xlim=c(0,22.5))+
  pubtheme

## GESL
gesl_plot=ggplot()+
  geom_ribbon(gesl_preds,mapping=aes(x=age,ymin=lci80,ymax=uci80),fill="gray70")+
  geom_point(gesl,mapping=aes(x=age,y=tl),alpha=0.8)+
  scale_x_continuous(limits=c(0,22.5),
                     breaks=seq(0,22,2),
                     labels=seq(0,22,2),
                     expand=c(0,0),
                     name="")+
  scale_y_continuous(limits=c(0,1620),
                     breaks=seq(0,1600,200),
                     labels=seq(0,1600,200),
                     expand=c(0,0),
                     name="")+
  annotate("text",x=0.2,y=1200,label="Geary",hjust=0,vjust=1,size=8)+
  coord_cartesian(ylim=c(0,1220),xlim=c(0,22.5))+
  pubtheme

## LVSL
lvsl_plot=ggplot()+
  geom_ribbon(lvsl_preds,mapping=aes(x=age,ymin=lci80,ymax=uci80),fill="gray70")+
  geom_point(lvsl,mapping=aes(x=age,y=tl),alpha=0.8)+
  scale_x_continuous(limits=c(0,22.5),
                     breaks=seq(0,22,2),
                     labels=seq(0,22,2),
                     expand=c(0,0),
                     name="")+
  scale_y_continuous(limits=c(0,1620),
                     breaks=seq(0,1600,200),
                     labels=seq(0,1600,200),
                     expand=c(0,0),
                     name="")+
  annotate("text",x=0.2,y=1200,label="Leavenworth",hjust=0,vjust=1,size=8)+
  coord_cartesian(ylim=c(0,1220),xlim=c(0,22.5))+
  pubtheme

## MCLA
mcla_plot=ggplot()+
  geom_ribbon(mcla_preds,mapping=aes(x=age,ymin=lci80,ymax=uci80),fill="gray70")+
  geom_point(mcla,mapping=aes(x=age,y=tl),alpha=0.8)+
  scale_x_continuous(limits=c(0,22.5),
                     breaks=seq(0,22,2),
                     labels=seq(0,22,2),
                     expand=c(0,0),
                     name="Estimated age")+
  scale_y_continuous(limits=c(0,1620),
                     breaks=seq(0,1600,200),
                     labels=seq(0,1600,200),
                     expand=c(0,0),
                     name="Total length (mm)")+
  annotate("text",x=0.2,y=1200,label="Middle Creek",hjust=0,vjust=1,size=8)+
  coord_cartesian(ylim=c(0,1220),xlim=c(0,22.5))+
  pubtheme

## MGSL
mgsl_plot=ggplot()+
  geom_ribbon(mgsl_preds,mapping=aes(x=age,ymin=lci80,ymax=uci80),fill="gray70")+
  geom_point(mgsl,mapping=aes(x=age,y=tl),alpha=0.8)+
  scale_x_continuous(limits=c(0,22.5),
                     breaks=seq(0,22,2),
                     labels=seq(0,22,2),
                     expand=c(0,0),
                     name="Estimated age")+
  scale_y_continuous(limits=c(0,1620),
                     breaks=seq(0,1600,200),
                     labels=seq(0,1600,200),
                     expand=c(0,0),
                     name="")+
  annotate("text",x=0.2,y=1200,label="Montgomery",hjust=0,vjust=1,size=8)+
  coord_cartesian(ylim=c(0,1220),xlim=c(0,22.5))+
  pubtheme

## NOSL
nosl_plot=ggplot()+
  #geom_ribbon(crsl_preds,mapping=aes(x=age,ymin=lci80,ymax=uci80),fill="gray70")+
  geom_point(nosl,mapping=aes(x=age,y=tl),alpha=0.8)+
  scale_x_continuous(limits=c(0,22.5),
                     breaks=seq(0,22,2),
                     labels=seq(0,22,2),
                     expand=c(0,0),
                     name="Estimated age")+
  scale_y_continuous(limits=c(0,1620),
                     breaks=seq(0,1600,200),
                     labels=seq(0,1600,200),
                     expand=c(0,0),
                     name="")+
  annotate("text",x=0.2,y=1200,label="Neosho",hjust=0,vjust=1,size=8)+
  coord_cartesian(ylim=c(0,1220),xlim=c(0,22.5))+
  pubtheme

## WLSL
wlsl_plot=ggplot()+
  geom_ribbon(wlsl_preds,mapping=aes(x=age,ymin=lci80,ymax=uci80),fill="gray70")+
  geom_point(wlsl,mapping=aes(x=age,y=tl),alpha=0.8)+
  scale_x_continuous(limits=c(0,22.5),
                     breaks=seq(0,22,2),
                     labels=seq(0,22,2),
                     expand=c(0,0),
                     name="Estimated age")+
  scale_y_continuous(limits=c(0,1620),
                     breaks=seq(0,1600,200),
                     labels=seq(0,1600,200),
                     expand=c(0,0),
                     name="")+
  annotate("text",x=0.2,y=1200,label="Wilson",hjust=0,vjust=1,size=8)+
  coord_cartesian(ylim=c(0,1220),xlim=c(0,22.5))+
  pubtheme

gom_out=(sgca_plot|crsl_plot|gesl_plot|lvsl_plot)/(mcla_plot|mgsl_plot|nosl_plot|wlsl_plot)

ggsave(plot=gom_out,"growth/growth_mods.png",width=20,height=7,bg="white")


################################################################################
################################################################################
## Length at age 5
(filter(sgca_preds,age==5))
(filter(crsl_preds,age==5))
(filter(gesl_preds,age==5))
(filter(lvsl_preds,age==5))
(filter(mcla_preds,age==5))
(filter(mgsl_preds,age==5))
#(filter(nosl_preds,age==5))
(filter(wlsl_preds,age==5))
