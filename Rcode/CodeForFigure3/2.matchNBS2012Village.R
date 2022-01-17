
rm(list=ls())

library(maptools)
library(rgeos)
library(rgdal)
library(stringr)

options(stringsAsFactors=F) 


cell_size <- 2 #km



## Village shapefile
STzVill <- readOGR(paste("data/GIS/STzVill_NBS2012",sep=""), paste("STzVill_NBS2012",sep=""))

## NBS 2012 human population census
NBSpop <- read.csv("data/NBS2012_STzVillagePop.csv")




## make sure districts match
matchDist <- match(unique(STzVill$District_N),unique(NBSpop$matchDistrict))
unique(STzVill$District_N)[which(is.na(matchDist))]

## remove districts from NBS data that don't occur in study area
removeDist <- unique(NBSpop$District)[which(is.na(match(unique(NBSpop$matchDistrict),unique(STzVill$District_N))))]
NBSpop <- NBSpop[which(!is.element(NBSpop$matchDistrict,removeDist)),]

## make sure wards match
NBSpop$matchDW <- gsub("'|\"", "",paste(NBSpop$matchDistrict,tolower(NBSpop$matchWard)))
STzVill$matchWard <- gsub("'|\"", "",paste(STzVill$District_N,tolower(STzVill$Ward_Name)))
matchWards <- match(unique(STzVill$matchWard),unique(NBSpop$matchDW))
unique(STzVill$matchWard)[which(is.na(matchWards))]
which(is.na(match(unique(NBSpop$matchDW),unique(STzVill$matchWard)))) 

## Match villages exactly (or minus most of the weird characters anyway!)
STzVill$matchDWV <- gsub("'|\"|`|*|/|,", "",paste(STzVill$District_N,tolower(STzVill$Ward_Name),gsub(" ","",tolower(STzVill$Vil_Mtaa_N))))
NBSpop$matchDWV <- gsub("'|\"|`|*|/|,", "",paste(NBSpop$matchDistrict,tolower(NBSpop$matchWard),gsub(" ","",tolower(NBSpop$Village))))
matchVill <- match(STzVill$matchDWV,NBSpop$matchDWV) # 174 unmatched
STzVill$matchDWV[which(is.na(matchVill))] # 218 unmatched
NBSpop$matchDWV[which(is.na(match(NBSpop$matchDWV,STzVill$matchDWV))) ]

## Find approximate matches
partialMatch<-rep(NA,length(which(is.na(matchVill))))
for(i in 1:length(which(is.na(matchVill)))){
  pmatch<-agrep(STzVill$matchDWV[which(is.na(matchVill))[i]],NBSpop$matchDWV[which(is.na(match(NBSpop$matchDWV,STzVill$matchDWV)))],max=1)
  if(length(pmatch)==1){partialMatch[i]<-agrep(STzVill$matchDWV[which(is.na(matchVill))[i]],NBSpop$matchDWV[which(is.na(match(NBSpop$matchDWV,STzVill$matchDWV)))],max=1)}
}
cbind( STzVill$matchDWV[which(is.na(matchVill))[which(!is.na(partialMatch))]],NBSpop$matchDWV[which(is.na(match(NBSpop$matchDWV,STzVill$matchDWV)))[partialMatch[which(!is.na(partialMatch))]]])
# all matches found look ok

## Add correct village names to the NBS data
NBSpop$matchVillage <- NBSpop$Village
NBSpop$matchVillage[which(is.na(match(NBSpop$matchDWV,STzVill$matchDWV)))[partialMatch[which(!is.na(partialMatch))]]]<-STzVill$Vil_Mtaa_N[which(is.na(matchVill))[which(!is.na(partialMatch))]]

## Match villages again
STzVill$matchDWV <- gsub("'|\"|`|*|/|,", "",paste(STzVill$District_N,tolower(STzVill$Ward_Name),gsub(" ","",tolower(STzVill$Vil_Mtaa_N))))
NBSpop$matchDWV <- gsub("'|\"|`|*|/|,", "",paste(NBSpop$matchDistrict,tolower(NBSpop$matchWard),gsub(" ","",tolower(NBSpop$matchVillage))))
matchVill <- match(STzVill$matchDWV,NBSpop$matchDWV)
STzVill$matchDWV[which(is.na(matchVill))] # 81 shapefile villages still unmatched
NBSpop$matchDWV[which(is.na(match(NBSpop$matchDWV,STzVill$matchDWV))) ] # 112 census villages still unmatched
length(matchVill[which(!is.na(matchVill))])
length(unique(matchVill[which(!is.na(matchVill))])) # all matches unique so far


## Approximate matches with more than one potential match
partialMatch<-vector('list',length(which(is.na(matchVill))))
for(i in 1:length(which(is.na(matchVill)))){
  pmatch<-agrep(STzVill$matchDWV[which(is.na(matchVill))[i]],NBSpop$matchDWV[which(is.na(match(NBSpop$matchDWV,STzVill$matchDWV)))],max=1)
  if(length(pmatch)>0){partialMatch[[i]]<-c(STzVill$matchDWV[which(is.na(matchVill))[i]],agrep(STzVill$matchDWV[which(is.na(matchVill))[i]],NBSpop$matchDWV[which(is.na(match(NBSpop$matchDWV,STzVill$matchDWV)))],max=1,value=T))}
}
partialMatch[vapply(partialMatch, Negate(is.null), NA)]

## Some more corrections
## Seem to be a few cases where the census data has split villages up further than the shapefile - just match them all to the same shapefile village
NBSpop$matchVillage[match(unlist(partialMatch[vapply(partialMatch, Negate(is.null), NA)][1])[-1],NBSpop$matchDWV)] <- "Mkupama"
NBSpop$matchVillage[match(unlist(partialMatch[vapply(partialMatch, Negate(is.null), NA)][2])[-1],NBSpop$matchDWV)] <- "Kambaya"
NBSpop$matchVillage[which(NBSpop$matchDWV=="Kilwa nanjirinji nanjirinjia")] <- "Nanji A"
NBSpop$matchVillage[which(NBSpop$matchDWV=="Kilwa nanjirinji nanjirinjib")] <- "Nanjiri B"
NBSpop$matchVillage[match(unlist(partialMatch[vapply(partialMatch, Negate(is.null), NA)][4])[-1],NBSpop$matchDWV)] <- "Mbuyuni"
NBSpop$matchVillage[match(unlist(partialMatch[vapply(partialMatch, Negate(is.null), NA)][5])[-1],NBSpop$matchDWV)] <- "Tandangongoro"
NBSpop$matchVillage[which(NBSpop$matchDWV=="Ulanga lupiro igumbirokati")] <- "Igumbiro"
NBSpop$matchVillage[match(unlist(partialMatch[vapply(partialMatch, Negate(is.null), NA)][8])[-1],NBSpop$matchDWV)] <- "Likongo"
NBSpop$matchVillage[match(unlist(partialMatch[vapply(partialMatch, Negate(is.null), NA)][9])[-1],NBSpop$matchDWV)] <- "Narunyu"
NBSpop$matchVillage[match(unlist(partialMatch[vapply(partialMatch, Negate(is.null), NA)][10])[-1],NBSpop$matchDWV)] <- "Nyengedi"


## Match villages again
STzVill$matchDWV <- gsub("'|\"|`|*|/|,", "",paste(STzVill$District_N,tolower(STzVill$Ward_Name),gsub(" ","",tolower(STzVill$Vil_Mtaa_N))))
NBSpop$matchDWV <- gsub("'|\"|`|*|/|,", "",paste(NBSpop$matchDistrict,tolower(NBSpop$matchWard),gsub(" ","",tolower(NBSpop$matchVillage))))
matchVill <- match(STzVill$matchDWV,NBSpop$matchDWV)
STzVill$matchDWV[which(is.na(matchVill))] # 71 shapefile villages still unmatched
NBSpop$matchDWV[which(is.na(match(NBSpop$matchDWV,STzVill$matchDWV))) ] # 79 census villages still unmatched
length(matchVill[which(!is.na(matchVill))])
length(unique(matchVill[which(!is.na(matchVill))])) # all matches to census data unique so far


## Find approximate matches (with slightly less accuracy than before)
partialMatch<-rep(NA,length(which(is.na(matchVill))))
for(i in 1:length(which(is.na(matchVill)))){
  pmatch<-agrep(STzVill$matchDWV[which(is.na(matchVill))[i]],NBSpop$matchDWV[which(is.na(match(NBSpop$matchDWV,STzVill$matchDWV)))],max=2)
  if(length(pmatch)==1){partialMatch[i]<-agrep(STzVill$matchDWV[which(is.na(matchVill))[i]],NBSpop$matchDWV[which(is.na(match(NBSpop$matchDWV,STzVill$matchDWV)))],max=2)}
}
cbind( STzVill$matchDWV[which(is.na(matchVill))[which(!is.na(partialMatch))]],NBSpop$matchDWV[which(is.na(match(NBSpop$matchDWV,STzVill$matchDWV)))[partialMatch[which(!is.na(partialMatch))]]])
# all matches found look ok

## Add correct village names to the NBS data
NBSpop$matchVillage[which(is.na(match(NBSpop$matchDWV,STzVill$matchDWV)))[partialMatch[which(!is.na(partialMatch))]]]<-STzVill$Vil_Mtaa_N[which(is.na(matchVill))[which(!is.na(partialMatch))]]


## Match villages again
STzVill$matchDWV <- gsub("'|\"|`|*|/|,", "",paste(STzVill$District_N,tolower(STzVill$Ward_Name),gsub(" ","",tolower(STzVill$Vil_Mtaa_N))))
NBSpop$matchDWV <- gsub("'|\"|`|*|/|,", "",paste(NBSpop$matchDistrict,tolower(NBSpop$matchWard),gsub(" ","",tolower(NBSpop$matchVillage))))
matchVill <- match(STzVill$matchDWV,NBSpop$matchDWV)
STzVill$matchDWV[which(is.na(matchVill))] # 48 shapefile villages still unmatched
NBSpop$matchDWV[which(is.na(match(NBSpop$matchDWV,STzVill$matchDWV))) ] # 56 census villages still unmatched
length(matchVill[which(!is.na(matchVill))])
length(unique(matchVill[which(!is.na(matchVill))])) # all matches unique so far

## Approximate matches with more than one potential match (with slightly less accracy than before)
partialMatch<-vector('list',length(which(is.na(matchVill))))
for(i in 1:length(which(is.na(matchVill)))){
  pmatch<-agrep(STzVill$matchDWV[which(is.na(matchVill))[i]],NBSpop$matchDWV[which(is.na(match(NBSpop$matchDWV,STzVill$matchDWV)))],max=2)
  if(length(pmatch)>0){partialMatch[[i]]<-c(STzVill$matchDWV[which(is.na(matchVill))[i]],agrep(STzVill$matchDWV[which(is.na(matchVill))[i]],NBSpop$matchDWV[which(is.na(match(NBSpop$matchDWV,STzVill$matchDWV)))],max=2,value=T))}
}
partialMatch[vapply(partialMatch, Negate(is.null), NA)]


## Find approximate matches (with even less accuracy than before)
partialMatch<-rep(NA,length(which(is.na(matchVill))))
for(i in 1:length(which(is.na(matchVill)))){
  pmatch<-agrep(STzVill$matchDWV[which(is.na(matchVill))[i]],NBSpop$matchDWV[which(is.na(match(NBSpop$matchDWV,STzVill$matchDWV)))],max=3)
  if(length(pmatch)==1){partialMatch[i]<-agrep(STzVill$matchDWV[which(is.na(matchVill))[i]],NBSpop$matchDWV[which(is.na(match(NBSpop$matchDWV,STzVill$matchDWV)))],max=3)}
}
cbind( STzVill$matchDWV[which(is.na(matchVill))[which(!is.na(partialMatch))]],NBSpop$matchDWV[which(is.na(match(NBSpop$matchDWV,STzVill$matchDWV)))[partialMatch[which(!is.na(partialMatch))]]])
# all seem likely

## Add correct village names to the NBS data
NBSpop$matchVillage[which(is.na(match(NBSpop$matchDWV,STzVill$matchDWV)))[partialMatch[which(!is.na(partialMatch))]]]<-STzVill$Vil_Mtaa_N[which(is.na(matchVill))[which(!is.na(partialMatch))]]

## Match villages again
STzVill$matchDWV <- gsub("'|\"|`|*|/|,", "",paste(STzVill$District_N,tolower(STzVill$Ward_Name),gsub(" ","",tolower(STzVill$Vil_Mtaa_N))))
NBSpop$matchDWV <- gsub("'|\"|`|*|/|,", "",paste(NBSpop$matchDistrict,tolower(NBSpop$matchWard),gsub(" ","",tolower(NBSpop$matchVillage))))
matchVill <- match(STzVill$matchDWV,NBSpop$matchDWV)
STzVill$matchDWV[which(is.na(matchVill))] # 41 shapefile villages still unmatched
NBSpop$matchDWV[which(is.na(match(NBSpop$matchDWV,STzVill$matchDWV))) ] # 49 census villages still unmatched
length(matchVill[which(!is.na(matchVill))])
length(unique(matchVill[which(!is.na(matchVill))])) # all matches unique so far

## Approximate matches with more than one potential match (with even less accuracy than before)
partialMatch<-vector('list',length(which(is.na(matchVill))))
for(i in 1:length(which(is.na(matchVill)))){
  pmatch<-agrep(STzVill$matchDWV[which(is.na(matchVill))[i]],NBSpop$matchDWV[which(is.na(match(NBSpop$matchDWV,STzVill$matchDWV)))],max=3)
  if(length(pmatch)>0){partialMatch[[i]]<-c(STzVill$matchDWV[which(is.na(matchVill))[i]],agrep(STzVill$matchDWV[which(is.na(matchVill))[i]],NBSpop$matchDWV[which(is.na(match(NBSpop$matchDWV,STzVill$matchDWV)))],max=3,value=T))}
}
partialMatch[vapply(partialMatch, Negate(is.null), NA)]

## Find approximate matches (with even less accuracy than before)
partialMatch<-rep(NA,length(which(is.na(matchVill))))
for(i in 1:length(which(is.na(matchVill)))){
  pmatch<-agrep(STzVill$matchDWV[which(is.na(matchVill))[i]],NBSpop$matchDWV[which(is.na(match(NBSpop$matchDWV,STzVill$matchDWV)))],max=4)
  if(length(pmatch)==1){partialMatch[i]<-agrep(STzVill$matchDWV[which(is.na(matchVill))[i]],NBSpop$matchDWV[which(is.na(match(NBSpop$matchDWV,STzVill$matchDWV)))],max=4)}
}
cbind( STzVill$matchDWV[which(is.na(matchVill))[which(!is.na(partialMatch))]],NBSpop$matchDWV[which(is.na(match(NBSpop$matchDWV,STzVill$matchDWV)))[partialMatch[which(!is.na(partialMatch))]]])
# all probably right (one involves change of ward rather than village)

## Add correct village names to the NBS data
NBSpop$matchVillage[which(is.na(match(NBSpop$matchDWV,STzVill$matchDWV)))[partialMatch[which(!is.na(partialMatch))[c(1:3)]]]]<-STzVill$Vil_Mtaa_N[which(is.na(matchVill))[which(!is.na(partialMatch))[c(1:3)]]]
NBSpop$matchWard[which(NBSpop$matchDWV=="Masasi mkundi mbugo")]<-"Mkululu"

## Match villages again
STzVill$matchDWV <- gsub("'|\"|`|*|/|,", "",paste(STzVill$District_N,tolower(STzVill$Ward_Name),gsub(" ","",tolower(STzVill$Vil_Mtaa_N))))
NBSpop$matchDWV <- gsub("'|\"|`|*|/|,", "",paste(NBSpop$matchDistrict,tolower(NBSpop$matchWard),gsub(" ","",tolower(NBSpop$matchVillage))))
matchVill <- match(STzVill$matchDWV,NBSpop$matchDWV)
STzVill$matchDWV[which(is.na(matchVill))] # 37 shapefile villages still unmatched
NBSpop$matchDWV[which(is.na(match(NBSpop$matchDWV,STzVill$matchDWV))) ] # 45 census villages still unmatched
length(matchVill[which(!is.na(matchVill))])
length(unique(matchVill[which(!is.na(matchVill))])) # all matches unique so far

## Approximate matches with more than one potential match (with even less accuracy than before)
partialMatch<-vector('list',length(which(is.na(matchVill))))
for(i in 1:length(which(is.na(matchVill)))){
  pmatch<-agrep(STzVill$matchDWV[which(is.na(matchVill))[i]],NBSpop$matchDWV[which(is.na(match(NBSpop$matchDWV,STzVill$matchDWV)))],max=4)
  if(length(pmatch)>0){partialMatch[[i]]<-c(STzVill$matchDWV[which(is.na(matchVill))[i]],agrep(STzVill$matchDWV[which(is.na(matchVill))[i]],NBSpop$matchDWV[which(is.na(match(NBSpop$matchDWV,STzVill$matchDWV)))],max=4,value=T))}
}
partialMatch[vapply(partialMatch, Negate(is.null), NA)]
NBSpop$matchVillage[match(unlist(partialMatch[vapply(partialMatch, Negate(is.null), NA)][1])[-1],NBSpop$matchDWV)] <- "Ng'apang'apa"


## Match villages again
STzVill$matchDWV <- gsub("'|\"|`|*|/|,", "",paste(STzVill$District_N,tolower(STzVill$Ward_Name),gsub(" ","",tolower(STzVill$Vil_Mtaa_N))))
NBSpop$matchDWV <- gsub("'|\"|`|*|/|,", "",paste(NBSpop$matchDistrict,tolower(NBSpop$matchWard),gsub(" ","",tolower(NBSpop$matchVillage))))
matchVill <- match(STzVill$matchDWV,NBSpop$matchDWV)
STzVill$matchDWV[which(is.na(matchVill))] # 24 shapefile villages still unmatched
NBSpop$matchDWV[which(is.na(match(NBSpop$matchDWV,STzVill$matchDWV))) ] # 39 census villages still unmatched
length(matchVill[which(!is.na(matchVill))])
length(unique(matchVill[which(!is.na(matchVill))])) # all matches unique so far

## Find approximate matches (with even less accuracy than before)
partialMatch<-rep(NA,length(which(is.na(matchVill))))
for(i in 1:length(which(is.na(matchVill)))){
  pmatch<-agrep(STzVill$matchDWV[which(is.na(matchVill))[i]],NBSpop$matchDWV[which(is.na(match(NBSpop$matchDWV,STzVill$matchDWV)))],max=5)
  if(length(pmatch)==1){partialMatch[i]<-agrep(STzVill$matchDWV[which(is.na(matchVill))[i]],NBSpop$matchDWV[which(is.na(match(NBSpop$matchDWV,STzVill$matchDWV)))],max=5)}
}
cbind( STzVill$matchDWV[which(is.na(matchVill))[which(!is.na(partialMatch))]],NBSpop$matchDWV[which(is.na(match(NBSpop$matchDWV,STzVill$matchDWV)))[partialMatch[which(!is.na(partialMatch))]]])
NBSpop$matchVillage[which(is.na(match(NBSpop$matchDWV,STzVill$matchDWV)))[partialMatch[which(!is.na(partialMatch))]]]<-STzVill$Vil_Mtaa_N[which(is.na(matchVill))[which(!is.na(partialMatch))]]


## Match villages again
STzVill$matchDWV <- gsub("'|\"|`|*|/|,", "",paste(STzVill$District_N,tolower(STzVill$Ward_Name),gsub(" ","",tolower(STzVill$Vil_Mtaa_N))))
NBSpop$matchDWV <- gsub("'|\"|`|*|/|,", "",paste(NBSpop$matchDistrict,tolower(NBSpop$matchWard),gsub(" ","",tolower(NBSpop$matchVillage))))
matchVill <- match(STzVill$matchDWV,NBSpop$matchDWV)
STzVill$matchDWV[which(is.na(matchVill))] # 33 shapefile villages still unmatched
NBSpop$matchDWV[which(is.na(match(NBSpop$matchDWV,STzVill$matchDWV))) ] # 36 census villages still unmatched
length(matchVill[which(!is.na(matchVill))])
length(unique(matchVill[which(!is.na(matchVill))])) # all matches unique so far


## Find approximate matches (with even less accuracy than before)
partialMatch<-rep(NA,length(which(is.na(matchVill))))
for(i in 1:length(which(is.na(matchVill)))){
  pmatch<-agrep(STzVill$matchDWV[which(is.na(matchVill))[i]],NBSpop$matchDWV[which(is.na(match(NBSpop$matchDWV,STzVill$matchDWV)))],max=6)
  if(length(pmatch)==1){partialMatch[i]<-agrep(STzVill$matchDWV[which(is.na(matchVill))[i]],NBSpop$matchDWV[which(is.na(match(NBSpop$matchDWV,STzVill$matchDWV)))],max=6)}
}
cbind( STzVill$matchDWV[which(is.na(matchVill))[which(!is.na(partialMatch))]],NBSpop$matchDWV[which(is.na(match(NBSpop$matchDWV,STzVill$matchDWV)))[partialMatch[which(!is.na(partialMatch))]]])
NBSpop$matchVillage[which(is.na(match(NBSpop$matchDWV,STzVill$matchDWV)))[partialMatch[which(!is.na(partialMatch))]]]<-STzVill$Vil_Mtaa_N[which(is.na(matchVill))[which(!is.na(partialMatch))]]


## Match villages again
STzVill$matchDWV <- gsub("'|\"|`|*|/|,", "",paste(STzVill$District_N,tolower(STzVill$Ward_Name),gsub(" ","",tolower(STzVill$Vil_Mtaa_N))))
NBSpop$matchDWV <- gsub("'|\"|`|*|/|,", "",paste(NBSpop$matchDistrict,tolower(NBSpop$matchWard),gsub(" ","",tolower(NBSpop$matchVillage))))
matchVill <- match(STzVill$matchDWV,NBSpop$matchDWV)
STzVill$matchDWV[which(is.na(matchVill))] # 15 shapefile villages still unmatched
NBSpop$matchDWV[which(is.na(match(NBSpop$matchDWV,STzVill$matchDWV))) ] # 18 census villages still unmatched
length(matchVill[which(!is.na(matchVill))])
length(unique(matchVill[which(!is.na(matchVill))])) # all matches unique so far


## Find approximate matches (with even less accuracy than before)
partialMatch<-rep(NA,length(which(is.na(matchVill))))
for(i in 1:length(which(is.na(matchVill)))){
  pmatch<-agrep(STzVill$matchDWV[which(is.na(matchVill))[i]],NBSpop$matchDWV[which(is.na(match(NBSpop$matchDWV,STzVill$matchDWV)))],max=7)
  if(length(pmatch)==1){partialMatch[i]<-agrep(STzVill$matchDWV[which(is.na(matchVill))[i]],NBSpop$matchDWV[which(is.na(match(NBSpop$matchDWV,STzVill$matchDWV)))],max=7)}
}
cbind( STzVill$matchDWV[which(is.na(matchVill))[which(!is.na(partialMatch))]],NBSpop$matchDWV[which(is.na(match(NBSpop$matchDWV,STzVill$matchDWV)))[partialMatch[which(!is.na(partialMatch))]]])
NBSpop$matchVillage[which(is.na(match(NBSpop$matchDWV,STzVill$matchDWV)))[partialMatch[which(!is.na(partialMatch))[c(1,3:4)]]]]<-STzVill$Vil_Mtaa_N[which(is.na(matchVill))[which(!is.na(partialMatch))[c(1,3:4)]]]
#one match I don't trust


## Match villages again
STzVill$matchDWV <- gsub("'|\"|`|*|/|,", "",paste(STzVill$District_N,tolower(STzVill$Ward_Name),gsub(" ","",tolower(STzVill$Vil_Mtaa_N))))
NBSpop$matchDWV <- gsub("'|\"|`|*|/|,", "",paste(NBSpop$matchDistrict,tolower(NBSpop$matchWard),gsub(" ","",tolower(NBSpop$matchVillage))))
matchVill <- match(STzVill$matchDWV,NBSpop$matchDWV)
STzVill$matchDWV[which(is.na(matchVill))] # 12 shapefile villages still unmatched
NBSpop$matchDWV[which(is.na(match(NBSpop$matchDWV,STzVill$matchDWV))) ] # 15 census villages still unmatched
length(matchVill[which(!is.na(matchVill))])
length(unique(matchVill[which(!is.na(matchVill))])) # all matches unique so far


## Do some by eye
NBSpop$matchVillage[which(NBSpop$Ward=="Mahenge")]<- "Mahenge"
NBSpop$matchVillage[which(NBSpop$matchDWV=="Morogoro Urban kilakala migombani(ttc)")]<- "Migombani"
NBSpop$matchVillage[which(NBSpop$matchDWV=="Lindi Urban tandangongoro nandambi(kilolambwani)")]<- "Nandambi"
NBSpop$matchVillage[which(NBSpop$matchDWV=="Lindi Urban tandangongoro nandambi(umoja)")]<- "Nandambi"


## Match villages again
STzVill$matchDWV <- gsub("'|\"|`|*|/|,", "",paste(STzVill$District_N,tolower(STzVill$Ward_Name),gsub(" ","",tolower(STzVill$Vil_Mtaa_N))))
NBSpop$matchDWV <- gsub("'|\"|`|*|/|,", "",paste(NBSpop$matchDistrict,tolower(NBSpop$matchWard),gsub(" ","",tolower(NBSpop$matchVillage))))
matchVill <- match(STzVill$matchDWV,NBSpop$matchDWV)
STzVill$matchDWV[which(is.na(matchVill))] # 12 shapefile villages still unmatched
NBSpop$matchDWV[which(is.na(match(NBSpop$matchDWV,STzVill$matchDWV))) ] # 9 census villages still unmatched
length(matchVill[which(!is.na(matchVill))])
length(unique(matchVill[which(!is.na(matchVill))])) # all matches unique so far

## Identified by Maganga
NBSpop$matchVillage[which(is.na(match(NBSpop$matchDWV,STzVill$matchDWV)))] <- c("Kidunda","Nyachiro","Ngongolo","Tambuu","Bigwa barabarani","Lukuyu","Mitole","Ngea","Mbanja")
NBSpop$matchWard[which(NBSpop$matchDWV=="Kilwa njinjo mitole")] <- "Mitole"
NBSpop$matchWard[which(NBSpop$matchDWV=="Kilwa njinjo ngea")] <- "Mitole"

## Match villages again
STzVill$matchDWV <- gsub("'|\"|`|*|/|,", "",paste(STzVill$District_N,tolower(STzVill$Ward_Name),gsub(" ","",tolower(STzVill$Vil_Mtaa_N))))
NBSpop$matchDWV <- gsub("'|\"|`|*|/|,", "",paste(NBSpop$matchDistrict,tolower(NBSpop$matchWard),gsub(" ","",tolower(NBSpop$matchVillage))))
matchVill <- match(STzVill$matchDWV,NBSpop$matchDWV)
STzVill$matchDWV[which(is.na(matchVill))] # 12 unmatched
NBSpop$matchDWV[which(is.na(match(NBSpop$matchDWV,STzVill$matchDWV)))] # all matched!
length(matchVill[which(!is.na(matchVill))])
length(unique(matchVill[which(!is.na(matchVill))])) # all matches unique so far


## Save matched Census data
write.csv(NBSpop[,1:8],file="Output/NBS2012_STzVillagePop_matched.csv",row.names = F)

