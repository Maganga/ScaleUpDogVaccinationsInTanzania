# ANALYSIS FOR FIGURE FIVE
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
library(dplyr)
library(scales)
library(rgdal)
library(GISTools)
library(gplots)
library(plyr) # library for adding the repeats within the data frame
library(AER) # library for overdispersion test
library(MASS) # library for negative binomial
library(Hmisc)
library(cowplot)
library(Rmisc) # library for standard error
library(gridExtra) # 

source("R/sortdateday.R")
options(stringsAsFactors=FALSE)

#Import mobile surveillance data
dogs_est <- read.csv("data/dogs_vacc_pvt.csv")#,
pvt <- read.csv("data/transectsall_corrected_locs.csv", stringsAsFactors = FALSE)#,
vax = read.csv("data/AllDogVaccinatedGA.csv") # 

#hb <- read.csv("data/Bites.csv")

# gdata::trim("  Ruagwa    ")

drop.regions <- c("Kaskazini Pemba", "Kusini Pemba", "Mara", "Pemba")

unique(pvt$Correct_Region)
length(unique(pvt$Correct_Region))
pvt<- pvt[!pvt$Correct_Region %in% drop.regions,] 
length(unique(pvt$Correct_Region))
unique(pvt$Correct_Region)
sort(unique(pvt$Correct_District))

unique(vax$Correct_Region)
length(unique(vax$Correct_Region))
vax <- vax[!vax$Correct_Region %in% drop.regions,] 
length(unique(vax$Correct_Region))
unique(vax$Correct_Region)
sort(unique(vax$Correct_District))

unique(dogs_est$Region)
length(unique(dogs_est$Region))
dogs_est <- dogs_est[!dogs_est$Region %in% drop.regions,] 
length(unique(dogs_est$Region))
unique(dogs_est$Region)
sort(unique(dogs_est$District))



str(vax)
dim(vax)
str(dogs_est)

table(dogs_est$District)
table(pvt$Correct_District)
table(vax$Correct_District)


# Date formatting
start.date <- as.Date("2010-01-01")
end.date <- as.Date(Sys.Date()) # System time = "now" by default

vax$date = as.Date(vax$dateVaccination, "%d/%m/%Y")
vax$y = format(vax$date, "%Y")

# by district
#vacc.mth <- ddply(vax, .(y, district), summarise,


vacc.y <- ddply(vax, .(y), summarise,             
                Vdogs = sum(Dogs),
                Vvillages = length(unique(Correct_Village))) # Vaccinated
#Format vax
vax$Correct_Village=as.character(vax$Correct_Village)
vax$Correct_Ward=as.character(vax$Correct_Ward)
vax$district=as.character(vax$Correct_District)
table(vax$district)
vax$Correct_Village <- paste(vax$Correct_Ward, vax$Correct_Village, sep = "-")
##############################################################################
vax$m <- format(vax$date, "%m/%Y") # Summarise by month
vax$y <- format(vax$date, "%Y") # Summarise by yr
vax$campaign_mth <- floor_date(vax$date, "month") # Summarise by month
vax$mth <- (as.POSIXlt(vax$date)$year-110)*12 + (as.POSIXlt(vax$date)$mon+1)
####Date formatting
# Create a for loop to sum the dogs vaccinated every month
months = 1:85 # From Jan 2013 to April 2017
DVM = vill = rep(0, length(months)) # Initialise vectors for dogs vaccinated/ month & villages with campaigns

for (i in 1:max(months)){ 
  index <- which(vax$mth == i)
  DVM[i] <- sum(vax$Dogs [index])
  vill[i] <- length(index)
}

## Check no villages vaccinated twice in same month
#check <- rep(NA,length(unique(vax$Village)))
#for(i in 1:length(unique(vax$Village))){
# if(length(vax$month[which(vax$Village==unique(vax$Village)[i])])!=length(unique(vax$month[which(vax$Village==unique(vax$Village)[i])]))){
#  check[i] <- T
#}else{check[i] <- F}
#}
#check #all good!

districts = unique(vax$district)
mths = 1:max(vax$mth)
vtable = matrix(NA, ncol=max(vax$mth), nrow=length(districts))

# make table of vacc dogs by month by district
for(i in 1:length(districts)){
  d = vax[which(vax$district==districts[i]),]
  for(j in mths){
    index = which(d$mth==j)
    vtable[i,j] <- ifelse(length(index)==0, 0, sum(d$Dogs[index]))
  }
}

vtable = as.data.frame(vtable)
names(vtable) = 1:85
vtable$district = districts
vtab = melt(vtable)
#vtab$mth=as.numeric(levels(vtab$variable))[vtab$variable]

str(vtab)
vdata <- setNames(vtab, c("district","vaccmonth","vaxdog"))
for (k in 1:nrow(vdata)){
  vdata$Region[k] <-  vax$Correct_Region[match(vdata$district[k], vax$district)]
}

######################################################################################
#######################################################################################




## estimated dog data 2014  (assume aug 2014 for now)

initial_pop <- function(nt,t,r){
  N0 = nt/exp(r*t)
}


# given a population number, nt.est, in month t.est, predict the 
# population in month t, given the growth rate r 

pop.predict <- function(nt.est, t.est, t, r) nt.est * exp(r * (t - t.est))
pop.predict(nt.est = 1000, t.est = 68, t = 1:85, r = 0.0259/12)




continuous_dens <- function(N0,r,t){
  N0*exp(r*t)  
}

months = 12
immunity = 3 
lifespan =2.2222

growth_rate = 0.0205/months
remove_rate = -(1/lifespan + 1/immunity)/months #1/3 wave immunity for 3 years

for (j in 1:length(districts)){
  dogs_est2014 <- (dogs_est
                   %>% mutate(estimated_pop_year = c(rep(c(64:70),each=7),68), 
                              estimated_pop_2010 = initial_pop(nt=est_dogs,
                                                               t=estimated_pop_year,r=growth_rate)
                   )
                   %>% filter(District == districts[j])
                   %>% dplyr:::select(District, est_pop=estimated_pop_2010)
  )
  
  sample_dat <- data.frame(time = 1:85, District= dogs_est2014$district, start_pop=dogs_est2014$est_pop
  )
  sample_dat2 <- (sample_dat
                  %>% mutate(est_dogs = continuous_dens(N0=start_pop,r=growth_rate, t=time)
                             
                  ))
                                                                                                                
  sample_vdat <- (vdata 
                  %>% filter(district == districts[j])
                  %>% transmute (Region = Region
                                 , District = district
                                 , time = as.numeric(vaccmonth)
                                 , vac = vaxdog
                  )
  )
  str(sample_dat2)
  str(sample_vdat)
  
  sample_dat3 <- (sample_dat2
                  %>% left_join(sample_vdat)        
                  %>% mutate(vac_time = ifelse(vac>0,1,0))
  )
  
  for(i in 2:nrow(sample_dat3)){
    if(sample_dat3$vac_time[i-1] > 0){
      sample_dat3$vac_time[i] <- sample_dat3$vac_time[i-1] + 1
      if(sample_dat3$vac[i] > 0){
        sample_dat3$vac_time[i] <- 1
      }
    }
  }
  
  for(i in 2:nrow(sample_dat3)){
    if((sample_dat3$vac[i-1]>sample_dat3$vac[i]) & (sample_dat3$vac[i]==0)){
      sample_dat3$vac[i] <- sample_dat3$vac[i-1]  
    }
  }
  
  sample_dat4 <- (sample_dat3
                  %>% rowwise()
                  %>% mutate(num_protected = continuous_dens(vac,remove_rate,vac_time)
                             , percent = num_protected/est_dogs
                             , date = as.Date('1-1-2010','%m-%d-%Y')+(time-1)*30 + 10
                  )
  )
  
  if(j == 1){
    fulldata = sample_dat4
  }else{
    fulldata = rbind(fulldata, sample_dat4)
  }
}

fulldata.reg <- split(fulldata, fulldata$Region)

#png("figs/interval.png")
plot.list <- 
  lapply(fulldata.reg,
         function(x) {
           ggplot(x, aes(x=date))+ facet_wrap(~Region, nrow=2)+ylim(0, 1)+
             geom_hline(yintercept=c(0.2,0.6), colour = "black", linetype="dashed")+
             annotate("text",parse = TRUE,x = as.Date('1-15-2010','%m-%d-%Y'), y = .25, label =as.character(expression(P[crit])))+
             annotate("text",parse = TRUE,x = as.Date('1-15-2010','%m-%d-%Y'), y = .65, label =as.character(expression(P[target])))+
             #annotate("text", x = as.Date('8-15-2010','%m-%d-%Y'), y = .63, label = "Ptarget")+
             geom_line(aes(y=percent, group = District, color = District))+
             ylab("Coverage")+
             xlab("Years") + 
             theme(legend.position="top", 
                   legend.title = element_blank(),
                   legend.key.width = unit(1, "line"),
                   legend.text=element_text(size = 9))+
             guides(col = guide_legend(nrow=1))
         })


do.call("grid.arrange", plot.list)

#ggsave("figs/plot.list.png", width=20, height=14); dev.off()

#V1=.20#Pcrit
#V0=.70#Ptarget
#t=log(V0/V1)/(-remove_rate);t
#log(V1/V0)/remove_rate-growth_rate

###OLD FORMULA
#pcrit=.20
#ptarget=.70
#log(pcrit*1/ptarget)/remove_rate-growth_rate


#################################################################################################