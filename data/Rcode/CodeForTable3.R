# Completeness
rm(list=ls(all=TRUE))
require(ggplot2)
dat=read.csv("data/CompletenessVillage1.csv")
dim(dat)
str(dat)
table(dat$Percentage)
table(dat$Phases)
n=length(unique(dat$District))

dat$District2 = paste(dat$District,' (',dat$Vill,')',sep='')


ggplot() + 
  aes(x=District2,order=Phases ) + 
  geom_bar(data=dat, aes(y=Percentage), position="stack", stat="identity") + 
  facet_grid(. ~Phases, space="free") + 
 # geom_hline(yintercept=100,linetype="dashed")+
  geom_hline(yintercept=90,linetype="dotted")+
  coord_flip()+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  xlab("Districts (Total number of vaccination units)")+
  ylab("Percentages of vaccinating unit that had vaccination campaign (%)")
  #ggsave("figs/Completeness25Feb.png", width = 24, height = 10,dpi=300, 
       #  units = "cm"); dev.off()

#####################
###Round 1
round190 <- dat[which(dat$Phases == "1st round"& dat$Percentage > 90), ];round190
4/n*100
round1100 <- dat[which(dat$Phases == "1st round"& dat$Percentage >= 100), ];round1100
1/n*100

round1 <- dat[which(dat$Phases == "1st round"), ]
medianrd1=median(round1$Percentage);medianrd1
rangerd1=range(round1$Percentage);rangerd1
#####Round 2
round290 <- dat[which(dat$Phases == "2nd round"& dat$Percentage > 90), ];round290
5/n*100
round2100 <- dat[which(dat$Phases == "2nd round"& dat$Percentage >= 100), ];round2100
1/n*100

round2 <- dat[which(dat$Phases == "2nd round"), ]
medianrd2=median(round2$Percentage);medianrd2
rangerd2=range(round2$Percentage);rangerd2

#####Round 3
round390 <- dat[which(dat$Phases == "3rd round"& dat$Percentage > 90), ];round390
10/n*100
round3100 <- dat[which(dat$Phases == "3rd round"& dat$Percentage >= 100), ];round3100
2/n*100

round3 <- dat[which(dat$Phases == "3rd round"), ]
medianrd3=median(round3$Percentage);medianrd3
rangerd3=range(round3$Percentage);rangerd3

#####Round 4
round490 <- dat[which(dat$Phases == "4th round"& dat$Percentage > 90), ];round490
15/n*100
round4100 <- dat[which(dat$Phases == "4th round"& dat$Percentage >= 100), ];round4100
5/n*100

round4 <- dat[which(dat$Phases == "4th round"), ]
medianrd4=median(round4$Percentage);medianrd4
rangerd4=range(round4$Percentage);rangerd4


#####Round 5
round590 <- dat[which(dat$Phases == "5th round"& dat$Percentage > 90), ];round590
17/n*100
round4100 <- dat[which(dat$Phases == "5th round"& dat$Percentage >= 100), ];round4100
6/n*100

round5 <- dat[which(dat$Phases == "5th round"), ]
medianrd5=median(round5$Percentage);medianrd5
rangerd5=range(round5$Percentage);rangerd5


####ALTENATIVE
dat %>% 
  filter(Phases == "5th round" & Percentage >= 100)# 1
6/28*100

dat %>% 
  filter(Phases == "5th round" & Percentage  > 90)# 4
17/28*100
