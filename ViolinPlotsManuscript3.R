library(ggplot2)
library(dplyr)
library(naniar)
library(extrafont)
#library(EcoHydrology)
library(imputeTS)
library(readr)
library(zoo)
library(hydroGOF)
library(operators)
library(topmodel)
library(DEoptim)
library(parallel)
library(baseflow)
library(XML)
library(EcoHydRology)
library("extrafont")
#install.packages("ggthemes") # Install 
library(ggthemes) 

windowsFonts(TNR=windowsFont("Arial"))


#############VIOLIN FIGURES##########
###FROSTDAY###

####NORTHBAY####

NB <- read.csv("C:/Ph.D/Thesis/SNF.Data/SNF Data Package/R codes/TempManuscript3/VIOLIN PLOTS/NORTHBAY-FROST DAYS.csv")

ggplot(NB, aes(x=PERIODS, y=MEAN)) + 
  geom_violin(trim=FALSE, fill='tan1', color="black")+
  stat_summary(fun=mean, geom="point", shape=5, size=2, stroke = 2, color= "black")+
  theme(text=element_text(family="TNR", size=14)) +
  xlab("Time Period") +
  ylab(bquote('Numer of Frost Days')) 

####ICE DAY########


NB <- read.csv("C:/Ph.D/Thesis/SNF.Data/SNF Data Package/R codes/TempManuscript3/VIOLIN PLOTS/NORTHBAY-ICE DAYS.csv")

ggplot(NB, aes(x=PERIODS, y=MEAN)) + 
  geom_violin(trim=FALSE, fill='tan1', color="black")+
  stat_summary(fun=mean, geom="point", shape=5, size=2, stroke = 2, color= "black")+
  theme(text=element_text(family="TNR", size=14)) +
  xlab("Time Period") +
  ylab(bquote('Numer of Ice Days')) 


###TMIN-GENERAL###

NB <- read.csv("C:/Ph.D/Thesis/SNF.Data/SNF Data Package/R codes/TempManuscript3/VIOLIN PLOTS/NORTHBAY-YEARS MIN TEMP.csv")

ggplot(NB, aes(x=PERIODS, y=MEAN)) + 
  geom_violin(trim=FALSE, fill='tan1', color="black")+
  stat_summary(fun=mean, geom="point", shape=5, size=2, stroke = 2, color= "black")+
  theme(text=element_text(family="TNR", size=14)) +
  xlab("Time Period") +
  ylab(bquote("Tmin (°C)")) 

###TMAX-GENERAL###
NB <- read.csv("C:/Ph.D/Thesis/SNF.Data/SNF Data Package/R codes/TempManuscript3/VIOLIN PLOTS/NORTHBAY-YEARS MAX TEMP.csv")

ggplot(NB, aes(x=PERIODS, y=MEAN)) + 
  geom_violin(trim=FALSE, fill='tan1', color="black")+
  stat_summary(fun=mean, geom="point", shape=5, size=2, stroke = 2, color= "black")+
  theme(text=element_text(family="TNR", size=14)) +
  xlab("Time Period") +
  ylab(bquote("Tmax (°C)")) 




###################MONTHLY-TMIN###########
############NORTHBAY##############

NB <- read.csv("C:/Ph.D/Thesis/SNF.Data/SNF Data Package/R codes/TempManuscript3/NORTHBAY-TMIN-MONTHLY.csv")


ggplot(NB)+
  #scale_x_continuous(expand = c(0, 0),  limits = c(0, NA)) + 
  #scale_y_continuous(expand = c(0, 0),  limits = c(0, NA))+
  
  
  
  geom_line(mapping = aes(x= MONTH, y=BASEMEAN, col="Baseline"),  size=1.5, linetype="longdash")+
  geom_ribbon(mapping= aes(x= MONTH, ymin=BASEMIN, ymax=BASEMAX), fill="gray", alpha= 2/10)+
  
  geom_line(mapping = aes(x= MONTH, y=P1MEAN, col="2020-2040"), size=1.5)+ 
  geom_ribbon(mapping= aes(x= MONTH, ymin=P1MIN, ymax=P1MAX), fill="limegreen", alpha= 1/10)+
  
  geom_line(mapping = aes(x= MONTH, y=P2MEAN, col="2041-2061"), size=1.5)+ 
  geom_ribbon(mapping= aes(x= MONTH, ymin=P2MIN, ymax=P2MAX), fill="blue", alpha= 1/10)+
  
  geom_line(mapping = aes(x= MONTH, y=P3MEAN, col="2062-2082"), size=1.5)+ 
  geom_ribbon(mapping= aes(x= MONTH, ymin=P3MIN, ymax=P3MAX), fill="blueviolet", alpha= 1/10)+
  
  
  scale_color_manual(values=c("green4", "deepskyblue4", "blueviolet", "black")) +
  scale_x_time(breaks=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"), 
               labels=c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) +
  xlab("Month") +
  ylab(bquote("Tmin (°C)")) +
  #theme_bw() +
  #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  #theme(text = element_text(size = 18)) +
  theme(text=element_text(family="TNR", size=18)) +
  theme(legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6), legend.title = element_blank()
  )


###################MONTHLY-TMAXN###########
############NORTHBAY##############

NB <- read.csv("C:/Ph.D/Thesis/SNF.Data/SNF Data Package/R codes/TempManuscript3/NORTHBAY-TMAX-MONTHLY.csv")


ggplot(NB)+
  #scale_x_continuous(expand = c(0, 0),  limits = c(0, NA)) + 
  #scale_y_continuous(expand = c(0, 0),  limits = c(0, NA))+
  
  
  
  geom_line(mapping = aes(x= MONTH, y=BASEMEAN, col="Baseline"),  size=1.5, linetype="longdash")+
  geom_ribbon(mapping= aes(x= MONTH, ymin=BASEMIN, ymax=BASEMAX), fill="gray", alpha= 4/10)+
  
  geom_line(mapping = aes(x= MONTH, y=P1MEAN, col="2020-2040"), size=1.5)+ 
  geom_ribbon(mapping= aes(x= MONTH, ymin=P1MIN, ymax=P1MAX), fill="limegreen", alpha= 1/10)+
  
  geom_line(mapping = aes(x= MONTH, y=P2MEAN, col="2041-2061"), size=1.5)+ 
  geom_ribbon(mapping= aes(x= MONTH, ymin=P2MIN, ymax=P2MAX), fill="blue", alpha= 1/10)+
  
  geom_line(mapping = aes(x= MONTH, y=P3MEAN, col="2062-2082"), size=1.5)+ 
  geom_ribbon(mapping= aes(x= MONTH, ymin=P3MIN, ymax=P3MAX), fill="blueviolet", alpha= 1/10)+
  
  
  scale_color_manual(values=c("green4", "deepskyblue4", "blueviolet", "black")) +
  scale_x_time(breaks=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"), 
               labels=c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) +
  xlab("Month") +
  ylab(bquote("Tmax (°C)")) +
  #theme_bw() +
  #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  #theme(text = element_text(size = 18)) +
  theme(text=element_text(family="TNR", size=18)) +
  theme(legend.position = c(.95, -2),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6), legend.title = element_blank()
  )