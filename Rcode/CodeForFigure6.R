##ANALYSIS FOR FIGURE SIX
rm(list=ls(all=TRUE))
#Load libraries
library(ggplot2)
require(plyr) # for ddply
library(dplyr) # for summarise
require(lme4)#
require(Hmisc)
library(lme4)
require(ggpmisc)
library(cowplot)# for multi-plots


#FORMULA USED TO GENERATE TIMING CSV BELOW (Timing.csv)
#For Pcrit of 30
V1=.30#Pcrit
V0=.70#Ptarget
t=log(V0/V1)/(-remove_rate);t
log(V1/V0)/remove_rate-growth_rate
#For Pcrit of 20
V1=.20#Pcrit
V0=.70#Ptarget
t=log(V0/V1)/(-remove_rate);t
log(V1/V0)/remove_rate-growth_rate


dat <- read.csv("data/Timing.csv",na.strings="NA") # 
str(dat)


geom_segment(aes(x, y, xend, yend))
ggplot(dat, aes(y = Coverage)) + 
  geom_line(aes(x = P1), colour="blue") + 
  geom_line(aes(x = P), colour = "red") + 
  labs(x = "Interval between vaccination campaigns (in months)")+
  labs(y = "Coverage (%)")+
  annotate(geom="text", x=22.8, y=80, label="20% Pcrit",
           color="red")+
  annotate(geom="text", x=0, y=46, label="44%",
          color="red")+
  annotate(geom="text", x=19.5, y=96, label="30% Pcrit",
          color="blue")+
  annotate(geom="text", x=0, y=68, label="66%",
           color="blue")+
  #annotate(geom="text", x=12, y=12.3, label="12 months",
      #   color="black")+
  geom_segment(linetype="dashed", color = "blue"
    ,aes(x = 12, y = 0, xend =12, yend =66))+
  geom_segment(linetype="dashed", color = "red"
               ,aes(x = 12, y = 0, xend = 12, yend =44))+
geom_segment(linetype="dashed", color = "blue"
             ,aes(x = 0, y = 66, xend = 12, yend =66))+
geom_segment(linetype="dashed", color = "red"
             ,aes(x = 0, y = 44, xend = 12, yend =44))
#ggsave("figs/timing.png", width=8, height=8.5); dev.off()

