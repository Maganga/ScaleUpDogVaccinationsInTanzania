# DATA ANALYSIS FOR VACCINATIONS -
#######################
rm(list=ls(all=TRUE))
#Load libraries
library(ggplot2)
library(grid) # 
library(lubridate) # for date manipulations
library(sp)
library(maptools)
library(lattice)
library(reshape)
library(RColorBrewer) #library(gridExtra)
library(cowplot)# for multi-plots
library(dplyr)
library(tidyr)
require(lme4)#
require(Hmisc)
library(MASS)
library(grid)
library(plyr)
library(reshape2)
require(ggpmisc)
library(glmmTMB)
options(stringsAsFactors=FALSE)
source("R/sortdateday.R")
options(stringsAsFactors=FALSE)
if(!exists('theme_rabies_web', mode='function'))
{source("R/theme_rabies_web.R")} # Settings for ggplot

#Import data
vac= read.csv("data/AllDogVaccinatedGA.csv") # 
pvt <- read.csv("data/transectsall_corrected_locs.csv") # POST-VACCINATION TRANSECTS
str(vac)
str(pvt)
dim(vac)
dim(pvt)
vac[5:13]
pvt[5:13]

# A few basic expressions to explore the data
pvt$District <- as.character(tolower(pvt$Correct_District))
vac$District <- tolower(as.character(vac$Correct_District))

vac$District <- gsub("Morogoro rural", "Morogoro", vac$District)

#####################################################################
# Date formatting
start.date <- as.Date("2010-01-01")
end.date <- as.Date(Sys.Date()) # System time = "now" by default


pvt$date = as.Date(pvt$Date, "%d/%m/%Y")
start.date <- as.Date("2010-01-01")
end.date <- as.Date(Sys.Date()) # System time = "now" by default
vac$campaign_date <- as.Date(vac$dateVaccination, "%d/%m/%Y") #

vac$m <- format(vac$date, "%m/%Y") # Summarise by month
vac$y <- format(vac$date, "%Y") # Summarise by yr
vac$campaign_mth <- floor_date(vac$date, "month") # Summarise by month
#vac$mth <- (as.POSIXlt(vac$date)$year-110)*12 + (as.POSIXlt(vac$date)$mon+1)

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

pvt<- pvt[!pvt$Correct_Region %in% drop.regions,] 
length(unique(pvt$Correct_Region))
unique(pvt$Correct_Region)
sort(unique(pvt$Correct_District))


#vac$Correct_District<- gsub("Masasi Township Authority", "Masasi Urban", as.character(vac$Correct_District))
sort(unique(vac$Correct_District))


## Subset transect data, calculate vaccination coverage 
transect1 <- subset(pvt, date>="2013-10-01"& date<="2014-02-28")# round 3 Coast Region and Pwani 
transect2 <- subset(pvt, date>="2014-03-01"& date<="2015-11-30")# round 4 all regions 
transect3<- subset(pvt, date>="2016-01-01"& date<="2018-11-01")# round 5 all regions 

###################################################################
## Subset transect data, calculate vaccination coverage 
vaccination1<- subset(vac, campaign_date >="2013-10-01"& campaign_date <="2014-02-28")# round 3 Coast Region and Pwani 
vaccination2 <- subset(vac, campaign_date >="2014-03-01"& campaign_date <="2015-11-30")# round 4 all regions 
vaccination3<- subset(vac, campaign_date >="2016-01-01"& campaign_date <="2018-11-01")# round 5 all regions 

pvt.summary1 <- ddply(transect1, .(District), summarise,
                      vacc = sum(Dogs.with.collars, na.rm = TRUE), # No. of vaccinated dogs
                      unvacc = sum(Dogs.without.collars, na.rm = TRUE), # No. of unvaccinated dogs
                      tdogs = sum(vacc, unvacc, na.rm = TRUE), # total dogs
                      cov = vacc/tdogs) #vaccination coverage
vaccination1$dogs <- vaccination1$Dogs # number of vaccinated dogs
vacc.dogs1<- ddply(vaccination1, .( District), summarise,
                   Vdogs1 = sum(dogs)) # Vaccinated dogs (per month per district)

campaign1=merge(vacc.dogs1, pvt.summary1, by=c("District"), all = TRUE);campaign1 #merge vaccinated dogs with transect data


pvt_cov <- transect1 %>%
  group_by(Village, District) %>%
  summarise(
    vacc = sum(Dogs.with.collars, na.rm = TRUE), # No. of vaccinated dogs
    unvacc = sum(Dogs.without.collars, na.rm = TRUE), # No. of unvaccinated dogs
    tdogs = sum(vacc, unvacc, na.rm = TRUE), # total dogs
    cov = vacc/tdogs) #vaccination coverage

##### PVT
table(!is.na(pvt_cov$cov))

pvt_cov2 <- droplevels(pvt_cov[!is.na(pvt_cov$cov), ])  
#length(unique(pvt_cov$District))
length(unique(pvt_cov2$District))
pvt_cov2$District <- as.character(pvt_cov2$District)

names_districts<-unique(pvt_cov2$District)
out_final_pvt <-
  sapply(1:length(names_districts),
         function(i){
           # i <- 9
           dist.name<-names_districts[i]
           print(paste(i, dist.name))
           
           pvt.d <-
             droplevels(
               pvt_cov2[pvt_cov2$District == dist.name,
                        c("vacc", "unvacc",  "Village")])
           
           fit=glmer(cbind(vacc, unvacc) ~ 1 + (1 | Village), family = "binomial", 
                     data = pvt.d)
           Beta <- fixef(fit)
           Beta.CI <- confint(fit, 2)
           Var <- sum(unlist(VarCorr(fit)))
           cov.est <- plogis(Beta - 0.5 * Var * tanh(Beta * (1 + 2 * exp(-0.5 * Var))/6))
           cov.ci <- plogis(Beta.CI - 0.5 * Var * tanh(Beta.CI * (1 + 2 * exp(-0.5 * Var))/6))
           c(cov.est, cov.ci)
           
         })


colnames(out_final_pvt) <- campaign1$District
t(out_final_pvt)
campaign1[, c("cov.est", "cov.ci.lo", "cov.ci.hi")] <- t(out_final_pvt)[campaign1$District, ]


campaign1<- 
  campaign1[order(-campaign1$cov.est), 
            c("District", 
              "Vdogs1",
              "cov.est",
              "cov.ci.lo",
              "cov.ci.hi")]

rownames(campaign1) <-campaign1$cov.est
campaign1$District <- 
  factor(campaign1$District, campaign1$District[order(campaign1$cov.est)])# remove - order(-merge$cov.est)]
###########################################################################################################
#### 2nd VACCINATIONS
pvt.summary2 <- ddply(transect2, .(District), summarise,
                      vacc2 = sum(Dogs.with.collars, na.rm = TRUE), # No. of vaccinated dogs
                      unvacc2 = sum(Dogs.without.collars, na.rm = TRUE), # No. of unvaccinated dogs
                      tdogs2 = sum(vacc2, unvacc2, na.rm = TRUE), # total dogs
                      cov2 = vacc2/tdogs2) #vaccination coverage
vaccination2$dogs2 <- vaccination2$Dogs # number of vaccinated dogs

vacc.dogs2<- ddply(vaccination2, .( District), summarise,
                   Vdogs2 = sum(dogs2), # Vaccinated dogs (per month per district)
                   vaccinator = length(unique(team_leader)), 
                   Vvillages = length(unique(Correct_Village ))) # Vaccinated villages (per month per district)

campaign2=merge(vacc.dogs2, pvt.summary2, by=c("District"), all = F);campaign2 #merge vaccinated dogs with transect data


pvt_cov2 <- transect2 %>%
  group_by(Village, District) %>%
  summarise(
    vacc2 = sum(Dogs.with.collars, na.rm = TRUE), # No. of vaccinated dogs
    unvacc2 = sum(Dogs.without.collars, na.rm = TRUE), # No. of unvaccinated dogs
    tdogs2 = sum(vacc2, unvacc2, na.rm = TRUE), # total dogs
    cov2 = vacc2/tdogs2) #vaccination coverage

##### PVT
table(!is.na(pvt_cov2$cov2))

pvt_cov22 <- droplevels(pvt_cov2[!is.na(pvt_cov2$cov2), ])  
#length(unique(pvt.summary$District))
length(unique(pvt_cov22$District))
pvt_cov22$District <- as.character(pvt_cov22$District)

names_districts<-unique(pvt_cov22$District)
out_final_pvt2 <-
  sapply(1:length(names_districts),
         function(i){
           # i <- 9
           dist.name<-names_districts[i]
           print(paste(i, dist.name))
           
           pvt.d <-
             droplevels(
               pvt_cov22[pvt_cov22$District == dist.name,
                         c("vacc2", "unvacc2",  "Village")])
           
           fit=glmer(cbind(vacc2, unvacc2) ~ 1 + (1 | Village), family = "binomial", 
                     data = pvt.d)
           Beta <- fixef(fit)
           Beta.CI <- confint(fit, 2)
           Var <- sum(unlist(VarCorr(fit)))
           cov.est2 <- plogis(Beta - 0.5 * Var * tanh(Beta * (1 + 2 * exp(-0.5 * Var))/6))
           cov.ci2 <- plogis(Beta.CI - 0.5 * Var * tanh(Beta.CI * (1 + 2 * exp(-0.5 * Var))/6))
           c(cov.est2, cov.ci2)
           
         })


colnames(out_final_pvt2) <- campaign2$District
t(out_final_pvt2)
campaign2[, c("cov.est2", "cov.ci.lo2", "cov.ci.hi2")] <- t(out_final_pvt2)[campaign2$District, ]


#### 3rd VACCINATIONS
pvt.summary3 <- ddply(transect3, .(District), summarise,
                      vacc3 = sum(Dogs.with.collars, na.rm = TRUE), # No. of vaccinated dogs
                      unvacc3 = sum(Dogs.without.collars, na.rm = TRUE), # No. of unvaccinated dogs
                      tdogs3 = sum(vacc3, unvacc3, na.rm = TRUE), # total dogs
                      cov3 = vacc3/tdogs3) #vaccination coverage
vaccination3$dogs3 <- vaccination3$Dogs # number of vaccinated dogs
vacc.dogs3<- ddply(vaccination3, .( District), summarise,
                   Vdogs3 = sum(dogs3), # Vaccinated dogs (per month per district)
                   vaccinator = length(unique(team_leader)), 
                   Vvillages = length(unique(Correct_Village ))) # Vaccinated villages (per month per district)

campaign3=merge(vacc.dogs3, pvt.summary3, by=c("District"), all = F);campaign3 #merge vaccinated dogs with transect data

pvt_cov3 <- transect3 %>%
  group_by(Village, District) %>%
  summarise(
    vacc3 = sum(Dogs.with.collars, na.rm = TRUE), # No. of vaccinated dogs
    unvacc3 = sum(Dogs.without.collars, na.rm = TRUE), # No. of unvaccinated dogs
    tdogs3 = sum(vacc3, unvacc3, na.rm = TRUE), # total dogs
    cov3 = vacc3/tdogs3) #vaccination coverage

##### PVT
table(!is.na(pvt_cov3$cov3))

pvt_cov23 <- droplevels(pvt_cov3[!is.na(pvt_cov3$cov3), ])  
#length(unique(pvt.summary$District))
length(unique(pvt_cov23$District))
pvt_cov23$District <- as.character(pvt_cov23$District)

names_districts<-unique(pvt_cov23$District)
out_final_pvt3 <-
  sapply(1:length(names_districts),
         function(i){
           # i <- 9
           dist.name<-names_districts[i]
           print(paste(i, dist.name))
           
           pvt.d <-
             droplevels(
               pvt_cov23[pvt_cov23$District == dist.name,
                         c("vacc3", "unvacc3",  "Village")])
           
           fit=glmer(cbind(vacc3, unvacc3) ~ 1 + (1 | Village), family = "binomial", 
                     data = pvt.d)
           Beta <- fixef(fit)
           Beta.CI <- confint(fit, 2)
           Var <- sum(unlist(VarCorr(fit)))
           cov.est3 <- plogis(Beta - 0.5 * Var * tanh(Beta * (1 + 2 * exp(-0.5 * Var))/6))
           cov.ci3 <- plogis(Beta.CI - 0.5 * Var * tanh(Beta.CI * (1 + 2 * exp(-0.5 * Var))/6))
           c(cov.est3, cov.ci3)
           
         })


colnames(out_final_pvt3) <- campaign3$District
t(out_final_pvt3)
campaign3[, c("cov.est3", "cov.ci.lo3", "cov.ci.hi3")] <- t(out_final_pvt3)[campaign3$District, ]

camp12=merge(campaign1, campaign2, by=c("District"), all = T) #merge vaccinated 
camp13=merge(campaign3, camp12, by=c("District"), all = T);camp13 #merge vaccinated 
camp=merge(camp13, data, by=c("District"), all = T);camp #merge 

camp<- 
  camp[order(-camp$cov.est3), 
       c("District", 
         "Vdogs3",
         "cov.est3",
         "cov.ci.lo3",
         "cov.ci.hi3",
         "Vdogs1",
         "cov.est",
         "cov.ci.lo",
         "cov.ci.hi",
         "Vdogs2",
         "cov.est2",
         "cov.ci.lo2",
         "cov.ci.hi2")]


###########################################################################################################
camp$mean.cov <- apply(camp[, c("cov.est3", "cov.est2", "cov.est")], 1, mean, na.rm = TRUE) 
camp <- camp[order(camp$mean.cov), ]

camp$District <- factor(camp$District, camp$District)
asdf

camp$Transects <- factor(rep_len(c(2013:2015), length.out = nrow(camp)))
camp$DummyCov <- -1000000

#png("figs/coverage_district_level.png")
ggplot(camp, aes(x=District))+ylim(0.15, 1)+
  geom_point(aes(x=District, y=DummyCov, color = Transects)) +
  geom_line(aes(x=District, y=DummyCov, color = Transects)) +
  geom_point(aes(y=cov.est), colour="black", position=position_nudge(x = 0.1),  
             shape = 16, size = 1.5) +
  geom_point(aes(y=cov.est2), colour="blue", position=position_nudge(x = 0),  
             shape = 16, size = 1.5) +
  geom_point(aes(y=cov.est3), colour="red", position=position_nudge(x = -0.1),  
             shape = 16, size = 1.5) +
  geom_errorbar(data=camp, aes(ymin=cov.ci.lo, ymax =cov.ci.hi), position=position_nudge(x = 0.1),width = 0.5, colour="black")+
  geom_errorbar(data=camp, aes(ymin=cov.ci.lo2, ymax =cov.ci.hi2), position=position_nudge(x = 0),width = 0.5, colour="blue")+
  geom_errorbar(data=camp, aes(ymin=cov.ci.lo3, ymax =cov.ci.hi3), position=position_nudge(x = -0.1),width = 0.5, colour="red")+
  coord_flip() +
  theme_bw() +
  labs(y = "Estimated coverage")+
  #theme(legend.position=c(0.25, 5)) +
  theme(legend.position="top") +
  scale_color_manual(labels = c("3rd round", "4th round","5th round"), values = c("black", "blue", "red")) 


#ggsave("figs/coverage_district_level.png", width=7.5, height=9.5); dev.off()
##################################################################################################
A= ggplot(camp, aes(y=cov.est3,x=District))+ylim(0, 1)+
  geom_point(position=position_dodge(width=0.65)) +
  geom_errorbar(data=camp, aes(ymin=cov.ci.lo3, ymax =cov.ci.hi3), position=position_dodge(width=0.7), colour="red")+
  #geom_errorbar(data=camp, aes(ymin=cov.ci.lo, ymax =cov.ci.hi), position=position_dodge(width=0.65), colour="black")+
  #geom_errorbar(data=camp, aes(ymin=cov.ci.lo2, ymax =cov.ci.hi2),position=position_dodge(width=0.6), colour="blue")+
  coord_flip() + 
  theme_bw() +
  labs(y = "Estimated coverage")+
  scale_colour_discrete(name="Coverage in 5th campaign)")+
  theme(legend.position="left") +# legend at top
  theme(axis.text.x=element_text(angle = 45, hjust = 1))

B= ggplot(camp, aes(y=cov.est,x=District))+ylim(0, 1)+
  geom_point(position=position_dodge(width=0.65)) +
  #geom_errorbar(data=camp, aes(ymin=cov.ci.lo3, ymax =cov.ci.hi3), position=position_dodge(width=0.7), colour="red")+
  geom_errorbar(data=camp, aes(ymin=cov.ci.lo, ymax =cov.ci.hi), position=position_dodge(width=0.7), colour="black")+
  #geom_errorbar(data=camp, aes(ymin=cov.ci.lo2, ymax =cov.ci.hi2),position=position_dodge(width=0.7), colour="blue")+
  coord_flip() + 
  theme_bw() +
  labs(y = "Estimated coverage")+
  theme(axis.text.x=element_text(angle = 45, hjust = 1))

C= ggplot(camp, aes(y=cov.est2,x=District))+ylim(0, 1)+
  geom_point(position=position_dodge(width=0.65)) +
  #geom_errorbar(data=camp, aes(ymin=cov.ci.lo3, ymax =cov.ci.hi3), position=position_dodge(width=0.7), colour="red")+
  #geom_errorbar(data=camp, aes(ymin=cov.ci.lo, ymax =cov.ci.hi), position=position_dodge(width=0.7), colour="black")+
  geom_errorbar(data=camp, aes(ymin=cov.ci.lo2, ymax =cov.ci.hi2),position=position_dodge(width=0.7), colour="blue")+
  coord_flip() + 
  theme_bw() +
  labs(y = "Estimated coverage")+
  theme(axis.text.x=element_text(angle = 45, hjust = 1))

plot_grid(A, B, C, labels=c("A", "B", "C"), ncol = 3, nrow = 1) # PLOT Grid



