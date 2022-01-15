# DATA ANALYSIS FOR VACCINATIONS -
rm(list=ls(all=TRUE))
#Load libraries
library(plyr) # for ddply
library(ggplot2)
library(grid) # no longer loaded automatically by ggplot
library(lubridate) # for date manipulations
library(sp)
library(maptools)
library(lattice)
library(reshape)
library(RColorBrewer) #library(gridExtra)
library(cowplot)# for multi-plots
library(dplyr)
library(tidyr)
library(grid)
source("R/sortdateday.R")
options(stringsAsFactors=FALSE)
if(!exists('theme_rabies_web', mode='function'))
{source("R/theme_rabies_web.R")} # Settings for ggplot

#Import data
vac= read.csv("data/AllDogVaccinatedGA.csv") # 
cov= read.csv("data/MeanCov.csv") 

str(vac)
str(cov)
# A few basic expressions to explore the data
dim(vac)
names(vac)
table(names(vac))
vac$district <- as.character(vac$Correct_District)
#vac$district <- gsub("liwale ", "liwale", vac$district)
# Date formatting
vac$campaign_date <- as.Date(vac$dateVaccination, "%d/%m/%Y") #
vac$dogs <- vac$Dogs
vac$cats <- vac$Cats

#Format start and end date of vaccination
start.date <- as.Date("2010-01-01")
end.date <- as.Date(Sys.Date()) # System time = "now" by default

vac$m <- format(vac$campaign_date, "%m/%Y") # Summarise by month
vac$y <- format(vac$campaign_date, "%Y") # Summarise by yr
vac$campaign_mth <- floor_date(vac$campaign_date, "month") # Summarise by month
vac$mth <- (as.POSIXlt(vac$campaign_date)$year-110)*12 + (as.POSIXlt(vac$campaign_date)$mon+1)



drop.regions <- c("Kaskazini Pemba", "Kusini Pemba", "Mara", "Pemba")

unique(vac$Correct_Region)
length(unique(vac$Correct_Region))
vac<- vac[!vac$Correct_Region %in% drop.regions,] 
length(unique(vac$Correct_Region))
unique(vac$Correct_Region)
sort(unique(vac$Correct_District))

vac$Correct_District<- gsub("Masasi Township Authority", "Masasi Urban", as.character(vac$Correct_District))
sort(unique(vac$Correct_District))

# 1. MONTHLY CAMPAIGNS - DOGS AND CATS VACCINATED
months = 1:85 # From Jan 2013 to Jan 2017
DVM = vill = rep(0, length(months)) # Initialise vectors for dogs vaccinated/ month & villages with campaigns

for (i in 1:max(months)){ 
  index <- which(vac$mth == i)
  DVM[i] <- sum(vac$Dogs [index])
  vill[i] <- length(index)
}
################################################
districts = unique(vac$Correct_District)
mths = 1:max(vac$mth)
vtable = matrix(NA, ncol=max(vac$mth), nrow=length(districts))

# make table of vacc dogs by month by district
for(i in 1:length(districts)){
  d = vac[which(vac$district==districts[i]),]
  for(j in mths){
    index = which(d$mth==j)
    vtable[i,j] <- ifelse(length(index)==0, 0, sum(d$dogs[index]))
  }
}

vtable = as.data.frame(vtable)
names(vtable) = 1:85
vtable$district = districts
vtab = melt(vtable)

str(vtab)
vdata <- setNames(vtab, c("district","vaccmonth","vaxdog"))
vdata$vaccmonth<- as.numeric(vdata$vaccmonth)
str(vdata)
#write.csv(vtable, "outputs/vtable.csv", row.names=FALSE)
#################################################################
# MONTHLY CAMPAIGNS - DOGS AND CATS VACCINATED
vacc.summary <- ddply(vac, .(campaign_mth), summarise,
                      dogs = sum(dogs),
                      cats = sum(cats),
                      villages = length(unique(Correct_Village))) # Vaccinated



vac %>%
  group_by(campaign_mth, .drop=T) %>%
  summarise(dgs=sum(dogs))


# by district
vacc.mth <- ddply(vac, .(y, Correct_District), summarise,
                  Vdogs = sum(dogs),
                  Vcats = sum(cats),
                  Vvillages = length(unique(Correct_Village))) # Vaccinated



# colour ramp for the heatmap
pal = colorRampPalette(c("red", "brown", "white"))
colours = pal(19)


fil <- paste(c("figs/dogs_vac_heatmap1", format(start.date), "--", format(end.date), ".png"), collapse='')
plotA = ggplot(vdata, aes(y=district, x=vaccmonth, fill=sqrt(vaxdog))) +
  geom_tile(colour="white", width=.95, height=.95) + theme_minimal() +  # works
  scale_fill_gradientn(colours=rev(colours),
                       limits=c(0, 130),
                       breaks=seq(0,130,10),
                      # labels=paste(c(0:13), rep("k",13), sep=""),
                       labels=c("0k","1k","2k","3k","4k","5k","6k","7k",
                                "8k","9k","10k","11k","12k","13k"),
                       guide=guide_colourbar(ticks=T, nbin=50, barheight=12, label=T, barwidth=.7)) +
  scale_x_continuous(limits=c(0, 86), breaks=seq(1,86,12), labels=2010:2017) +

  labs(x="", y="", fill="")+
  geom_segment(x=1, xend=1, y=0, yend=27.5, size=.5, colour="black", linetype="dashed") +
  geom_segment(x=18.5, xend=18.5, y=0, yend=27.5, size=.5, colour="black", linetype="dotted") +
  #geom_segment(x=0, xend=0, y=0, yend=27.5, size=.5, colour="black", linetype="dashed") +
  # annotate("text", x = -9.5, y = 26.2, size=5,label = "1st round ")+
  # annotate("text", x = 9.5, y = 25.9, size=5,label = "1st round ")+
  geom_segment(x=34.5, xend=34.5, y=0, yend=27.5, size=.5, colour="black", linetype="dotted") +
  #  annotate("text", x = 24.5, y = 25.9,size=5, label = "2nd round")+
  geom_segment(x=59.8, xend=59.8, y=0, yend=27.5, size=.5, colour="black", linetype="dotted") +
  #  annotate("text", x = 41.5, y = 25.9,size=5, label = "3rd round ")+
  geom_segment(x=72.8, xend=72.8, y=0, yend=27.5, size=.5, colour="black", linetype="dotted") +
  # annotate("text", x = 65, y = 25.9,size=5, label = "4th round")+
  geom_segment(x=88.5, xend=88.5, y=0, yend=27.5, size=.5, colour="black", linetype="dotted") +
  # annotate("text", x = 78, y = 25.9,size=5, label = "5th round")+
  
  theme(legend.position="right",
       # legend.justification="center",
        legend.direction="vertical",
        legend.text=element_text(colour="grey20", size = 14),
        axis.text.y=element_text(size=13, family="Helvetica", hjust=1),
        axis.text.x=element_text(size=15, vjust = 1),
        axis.ticks.y=element_blank(),
        panel.grid=element_blank(),
        title=element_text(hjust=-.04, face="bold", vjust=1, family="Helvetica"),
        text=element_text(family="Helvetica"))


plotB=ggplot(cov, aes(Month, VacDogs)) +
   geom_vline(aes(xintercept=1,linetype ="dashed"), show.legend = F)+# linetype = threshold), show_guide = TRUE
    geom_vline(aes(xintercept=-2.001,linetype = "dashed"), show.legend = F)+# linetype = threshold), show_guide = TRUE
    geom_vline(aes(xintercept=18,linetype = "dotted"), show.legend = F)+# linetype = threshold), show_guide = TRUE
   # geom_vline(aes(xintercept=10), show.legend = F)+# linetype = threshold), show_guide = TRUE
    geom_vline(aes(xintercept = 34, linetype = "dotted"), show.legend = F)+#, size=4, angle=90, vjust=-0.4, hjust=0
   geom_vline(aes(xintercept = 57, linetype = "dotted"), show.legend = F)+
   geom_vline(aes(xintercept = 71,linetype = "dotted"), show.legend = F)+
   geom_vline(aes(xintercept = 85.5, linetype = "dotted"), show.legend = F)+
    annotate("text", x=10, y=59000, label= "1st round" , colour = I('black'), size = 6) + 
    annotate("text", x = 24, y=59000, label = "2nd round", colour = I('black'), size = 6)+
    annotate("text", x=40.2, y=59000, label= "3rd round", colour = I('black'), size = 6) + 
    annotate("text", x = 63.8, y=59000, label = "4th round", colour = I('black'), size = 6)+
    annotate("text", x=78.4, y=59000, label= "5th round", colour = I('black'), size = 6) + 
    geom_col() +
 # theme_gray()+
  #theme(panel.border = element_rect(linetype = "dashed"))+
  geom_line(aes(Month, MeanCov*60000/100), color = "blue")+
  scale_y_continuous("Number of dogs vaccinated", sec.axis = sec_axis(~ .*100/60000, name = "Coverage (%)")) +
  scale_x_continuous(limits=c(0, 86), breaks=seq(1,86,12), labels=2010:2017) +
    theme(axis.line.y.right = element_line(color = "blue"),
        axis.ticks.y.right = element_line(color = "blue"),
        axis.text.y.right = element_text(color = "blue", size=12),
        axis.text.y=element_text(size=14),
        axis.text.x=element_text(size=14),
        axis.title.y.right = element_text(color = "blue", size=14),
        axis.title.y.left = element_text(size=14),
        #panel.grid=element_blank(),
        axis.title.x = element_blank())


windowsFonts(RobotoBold=windowsFont("Palatino Linotype-Bold"))
plots=plot_grid(plotB, plotA, labels = c("(a)", "(b)"), ncol = 1, align = 'v', axis = 'l');plots
#ggsave("figure_211.png", plot = plots, width = 30, height = 28, units = "cm", dpi = 300)

