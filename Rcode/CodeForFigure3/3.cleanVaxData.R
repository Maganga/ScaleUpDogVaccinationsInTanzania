#
## Fix vaccination data so that it matches to the village shapefile
#__________________________________


options(stringsAsFactors=F) 

rm(list=ls())

library(lubridate)
library(rgdal)
library(stringr)




## Read in data
#------------------

## Vaccination data
vax <- read.csv(paste("data/AllDogVaccinatedGA.csv",sep=""))
vax <- vax[which(vax$Correct_Region!="Pemba"),]

## Village shapefile
STzVill <- readOGR(paste("data/GIS/STzVill_NBS2012",sep=""), paste("STzVill_NBS2012",sep=""))

## Some checks 
length(unique(vax$Correct_Ward))
length(unique(paste(vax$Correct_District,vax$Correct_Ward))) #Ward names not unique
length(unique(vax$Correct_District))
length(unique(paste(vax$Correct_Region,vax$Correct_District))) #District names unique 


## Get correct amalgamated district_ward_village name
vax$Correct_Village <-
  apply(vax[, c("Correct_District", "Correct_Ward", "Correct_Village")], 1,
        function(x) paste(tolower(gsub("[^[:alpha:]]", "", x)), collapse = "_"))


## Match villages
STzVill$matchWard<-tolower(gsub("[^[:alpha:]]", "", STzVill$Ward_Name))
STzVill$matchVill<-paste(
  tolower(gsub("[^[:alpha:]]", "", STzVill$District_N)),
  tolower(gsub("[^[:alpha:]]", "", STzVill$Ward_Name)),
  tolower(gsub("[^[:alpha:]]", "", STzVill$Vil_Mtaa_N)),
  sep = "_")
studyVill <- match(unique(vax$Correct_Village),unique(STzVill$matchVill))
sort(unique(vax$Correct_Village)[which(is.na(studyVill))]) ## 196 not matching


## Make corrections to district, ward, and village names in vax to allow match with shapefile
vax$Correct_Village<- gsub("kilombero_katurukila_mkula","kilombero_mkula_katurukila", vax$Correct_Village)
vax$Correct_Village<- gsub("lindirural_nyangamara_litupu", "lindirural_nyangamara_liputi", vax$Correct_Village)
vax$Correct_Village<- gsub("lindirural_kilolambwani_kilolombwani", "lindirural_kilolambwani_kilolambwani", vax$Correct_Village)
vax$Correct_Village<- gsub("lindirural_namangwale_chiwelele","lindirural_namangale_chiwelele", vax$Correct_Village)
vax$Correct_Village<- gsub("lindirural_namangwale_namangale", "lindirural_namangale_namangale", vax$Correct_Village)
vax$Correct_Village<- gsub("lindirural_namangwale_namangaleb", "lindirural_namangale_namangaleb", vax$Correct_Village)
vax$Correct_Village<- gsub("lindirural_namangwale_mawilo", "lindirural_namangale_namangaleb", vax$Correct_Village)
vax$Correct_Village<- gsub("lindirural_nyengedi_nyegedi", "lindirural_nyengedi_nyengedi", vax$Correct_Village)
vax$Correct_Village<- gsub("lindirural_nyengedi_nyegedib", "lindirural_nyengedi_nyengedib", vax$Correct_Village)
vax$Correct_Village<- gsub("lindirural_sudi_tipuli", "lindirural_sudi_sudi", vax$Correct_Village)
vax$Correct_Village<- gsub("masasi_lipumburu_lupaso", "masasi_lipumburu_mtojolupaso", vax$Correct_Village)
vax$Correct_Village<- gsub("masasi_mkundi_mbugo", "masasi_mkululu_mkundi", vax$Correct_Village)
vax$Correct_Village<- gsub("masasi_mkululu_mkundiamani", "masasi_mkululu_mkundi", vax$Correct_Village)
vax$Correct_Village<- gsub("masasi_lulindi_mnopwe", "masasi_lulindi_mropwe", vax$Correct_Village)
vax$Correct_Village<- gsub("masasi_mchauru_nagomwa", "masasi_mchauru_nangomwa", vax$Correct_Village)
vax$Correct_Village<- gsub("masasi_lipumburu_namtona", "masasi_lipumburu_nantona", vax$Correct_Village)
vax$Correct_Village <-gsub("morogoro_chanyumbu_kidunda", "morogoro_mkulazi_kidunda", vax$Correct_Village)
vax$Correct_Village<- gsub("morogoro_tomondo_kungwa", "morogoro_kiroka_kungwe", vax$Correct_Village)
vax$Correct_Village <- gsub("kilwa_chumo_kinywanyi", "kilwa_chumo_kinywanyubugo",vax$Correct_Village)
vax$Correct_Village <- gsub("kilwa_kiranjeranje_kiswale", "kilwa_kiranjeranje_kiswele",vax$Correct_Village)
vax$Correct_Village <- gsub("kilwa_lihimalyao_lihimalyaokusini", "kilwa_lihimalyao_lihmalyokusini",vax$Correct_Village)
vax$Correct_Village <- gsub("kilwa_masoko_mnazimmoja", "kilwa_masoko_mnazimmo",vax$Correct_Village)
vax$Correct_Village <- gsub("kilwa_pande_mtandula", "kilwa_pandemikoma_malalani",vax$Correct_Village)
vax$Correct_Village <- gsub("kilwa_kinjumbi_mtyalambuko", "kilwa_kinjumbi_mtyalambu",vax$Correct_Village)
vax$Correct_Village <- gsub("kilwa_mingumbi_nangambinaipuli", "kilwa_mingumbi_nangambi",vax$Correct_Village)
vax$Correct_Village <- gsub("kilwa_nakindu_nakindu", "kilwa_kibata_hanga",vax$Correct_Village)
vax$Correct_Village <- gsub("kilwa_lihimalyao_namakongoro", "kilwa_lihimalyao_kisongo",vax$Correct_Village)
vax$Correct_Village <- gsub("kilwa_nanjirinji_nanjirinjia", "kilwa_nanjirinji_nanjia",vax$Correct_Village)
vax$Correct_Village <- gsub("kilwa_nanjirinji_nanjirinjib", "kilwa_nanjirinji_nanjirib",vax$Correct_Village)
vax$Correct_Village <- gsub("kilwa_njinjo_ngea", "kilwa_mitole_ngea",vax$Correct_Village)
vax$Correct_Village<- gsub("lindiurban_msanjihili_benki", "lindiurban_msinjahili_benki", vax$Correct_Village)
vax$Correct_Village<- gsub("lindiurban_matopeni_kawawaroad", "lindiurban_matopeni_kawawa", vax$Correct_Village)
vax$Correct_Village<- gsub("lindiurban_mingoyo_mawasilino", "lindiurban_mingoyo_mawasiliano", vax$Correct_Village)
vax$Correct_Village<- gsub("lindiurban_mbanja_mitonga", "lindiurban_mbanja_mbanja", vax$Correct_Village)
vax$Correct_Village<- gsub("lindiurban_rasbura_mitwerostand", "lindiurban_rasbura_mitwero", vax$Correct_Village)
vax$Correct_Village<- gsub("lindiurban_tandangongoro_mkangai", "lindiurban_tandangongoro_mkanga", vax$Correct_Village)
vax$Correct_Village<- gsub("lindiurban_mtanda_mmukule", "lindiurban_mtanda_mukule", vax$Correct_Village)
vax$Correct_Village<- gsub("lindiurban_msanjihili_msonobarichini", "lindiurban_msinjahili_msonobarichini", vax$Correct_Village)
vax$Correct_Village<- gsub("lindiurban_msanjihili_msonobarijuu", "lindiurban_msinjahili_msonobarijuu", vax$Correct_Village)
vax$Correct_Village<- gsub("lindiurban_msanjihili_nachingwea", "lindiurban_msinjahili_nachingwea", vax$Correct_Village)
vax$Correct_Village<- gsub("lindiurban_nachingwea_nachingweakusini" ,"lindiurban_nachingwea_nachingwea", vax$Correct_Village)
vax$Correct_Village<- gsub("lindiurban_nachingwea_nachingweakaskazini", "lindiurban_nachingwea_nachingwea", vax$Correct_Village)
vax$Correct_Village<- gsub("lindiurban_kitumbikwela_mnali", "lindiurban_nachingwea_nachingwea", vax$Correct_Village)
vax$Correct_Village<- gsub("liwale_mangirirkiti_kimbemba", "liwale_mangirikiti_kimbemba", vax$Correct_Village)
vax$Correct_Village<- gsub("liwale_mangirirkiti_kipule", "liwale_mangirikiti_kipule", vax$Correct_Village)
vax$Correct_Village<- gsub("liwale_liwaleb_lekezamwendo", "liwale_liwaleb_legezamwendo", vax$Correct_Village)
vax$Correct_Village<- gsub("liwale_kiangara_litou", "liwale_kiangara_litoo", vax$Correct_Village)
vax$Correct_Village<- gsub("liwale_mangirirkiti_makinda", "liwale_mangirikiti_makinda", vax$Correct_Village)
vax$Correct_Village<- gsub("liwale_mangirirkiti_mangirikiti", "liwale_mangirikiti_mangirikiti", vax$Correct_Village)
vax$Correct_Village<- gsub("liwale_mangirirkiti_mkonganage", "liwale_mangirikiti_mkonganage", vax$Correct_Village)
vax$Correct_Village<- gsub("liwale_mlembwe_nambinda", "liwale_mlembwe_ndambinda", vax$Correct_Village)
vax$Correct_Village<- gsub("liwale_mangirirkiti_ngorongopa", "liwale_mangirikiti_ngorongopa", vax$Correct_Village)
vax$Correct_Village<- gsub("mtwararural_nanyamba_chikwaya", "mtwararural_nanyamba_chikaya", vax$Correct_Village)
vax$Correct_Village<- gsub("mtwararural_kiyanga_kiyanga", "mtwararural_kiyanga_mkahara", vax$Correct_Village)
vax$Correct_Village<- gsub("mtwararural_nanguruwe_namayakatabarabarani", "mtwararural_nanguruwe_namanyakatabarabara", vax$Correct_Village)
vax$Correct_Village<- gsub("mtwaraurban_magengeni_bomani", "mtwaraurban_magengeni_boma", vax$Correct_Village)
vax$Correct_Village<- gsub("mtwaraurban_mtawanya_chihiko", "mtwaraurban_likombe_mtawanya", vax$Correct_Village)
vax$Correct_Village<- gsub("mtwaraurban_chuno_chuno", "mtwaraurban_chuno_majengo", vax$Correct_Village)
vax$Correct_Village<- gsub("mtwaraurban_rahaleo_ffu", "mtwaraurban_rahaleo_nandope", vax$Correct_Village)
vax$Correct_Village<- gsub("mtwaraurban_shangani_indianquarters", "mtwaraurban_shangani_indiankota", vax$Correct_Village)
vax$Correct_Village<- gsub("mtwaraurban_shangani_kiangua", "mtwaraurban_shangani_kiyangua", vax$Correct_Village)
vax$Correct_Village<- gsub("mtwaraurban_shangani_kiangub", "mtwaraurban_shangani_kiyangub", vax$Correct_Village)
vax$Correct_Village<- gsub("mtwaraurban_chuno_lilungu", "mtwaraurban_likombe_lilungu", vax$Correct_Village)
vax$Correct_Village<- gsub("mtwaraurban_likombe_madukamakubwa", "mtwaraurban_shangani_madukamakubwa", vax$Correct_Village)
vax$Correct_Village<- gsub("mtwaraurban_shangani_magengeni", "mtwaraurban_magengeni_magengeni", vax$Correct_Village)
vax$Correct_Village<- gsub("mtwaraurban_magengeni_magomenia", "mtwaraurban_ufukoni_magomenia", vax$Correct_Village)
vax$Correct_Village<- gsub("mtwaraurban_ufukoni_majengo", "mtwaraurban_ufukoni_ufukonistendi", vax$Correct_Village)
vax$Correct_Village<- gsub("mtwaraurban_chuno_mangamba", "mtwaraurban_likombe_mangamba", vax$Correct_Village)
vax$Correct_Village<- gsub("mtwaraurban_chuno_matopeni", "mtwaraurban_ufukoni_matopeni", vax$Correct_Village)
vax$Correct_Village<- gsub("mtwaraurban_mangamba_mbae", "mtwaraurban_ufukoni_mbae", vax$Correct_Village)
vax$Correct_Village<- gsub("mtwaraurban_mangamba_mbawalachini", "mtwaraurban_naliendele_mkangala", vax$Correct_Village)
vax$Correct_Village<- gsub("mtwaraurban_mangamba_mbelenje", "mtwaraurban_shangani_ligulambelenje", vax$Correct_Village)
vax$Correct_Village<- gsub("mtwaraurban_ufukoni_mdenga", "mtwaraurban_likombe_mdenga", vax$Correct_Village)
vax$Correct_Village<- gsub("mtwaraurban_ufukoni_mikunguni", "mtwaraurban_ufukoni_ufukonistendi", vax$Correct_Village)
vax$Correct_Village<- gsub("mtwaraurban_mkangala_miseti", "mtwaraurban_chuno_miseti", vax$Correct_Village)
vax$Correct_Village<- gsub("mtwaraurban_shangani_mkangala", "mtwaraurban_naliendele_mkangala", vax$Correct_Village)
vax$Correct_Village<- gsub("mtwaraurban_likombe_mkundi", "mtwaraurban_magengeni_mkundi", vax$Correct_Village)
vax$Correct_Village<- gsub("mtwaraurban_mangamba_mlimani", "mtwaraurban_likombe_mlimani", vax$Correct_Village)
vax$Correct_Village<- gsub("mtwaraurban_chuno_mnaida", "mtwaraurban_chuno_miseti", vax$Correct_Village)
vax$Correct_Village<- gsub("mtwaraurban_mkangala_mnazimmoja", "mtwaraurban_chuno_miseti", vax$Correct_Village)
vax$Correct_Village<- gsub("mtwaraurban_magengeni_msikitini", "mtwaraurban_magengeni_magengeni", vax$Correct_Village)
vax$Correct_Village<- gsub("mtwaraurban_likombe_mtangashari", "mtwaraurban_jangwani_mtangashari", vax$Correct_Village)
vax$Correct_Village<- gsub("mtwaraurban_mitengo_mtawike", "mtwaraurban_chikongola_mtawike", vax$Correct_Village)
vax$Correct_Village<- gsub("mtwaraurban_mitengo_mtepwezi", "mtwaraurban_likombe_mtepwezi", vax$Correct_Village)
vax$Correct_Village<- gsub("mtwaraurban_mangamba_mtonyaa", "mtwaraurban_mtonya_mtonyaa", vax$Correct_Village)
vax$Correct_Village<- gsub("mtwaraurban_jangwani_naleindele", "mtwaraurban_naliendele_naliendele", vax$Correct_Village)
vax$Correct_Village<- gsub("mtwaraurban_chikongola_namayanga", "mtwaraurban_likombe_mtawanya", vax$Correct_Village)
vax$Correct_Village<- gsub("mtwaraurban_likombe_nantibwili", "mtwaraurban_likombe_mdenga", vax$Correct_Village)
vax$Correct_Village<- gsub("mtwaraurban_mtonya_ndolo", "mtwaraurban_magengeni_magengeni", vax$Correct_Village)
vax$Correct_Village<- gsub("mtwaraurban_naliendele_pentekoste", "mtwaraurban_rahaleo_pentecoste", vax$Correct_Village)
vax$Correct_Village<- gsub("mtwaraurban_namayanga_shanganieast", "mtwaraurban_shangani_shanganieast", vax$Correct_Village)
vax$Correct_Village<- gsub("mtwaraurban_chuno_shanganiwest", "mtwaraurban_shangani_shanganiwest", vax$Correct_Village)
vax$Correct_Village<- gsub("mtwaraurban_magengeni_sinani", "mtwaraurban_vigaeni_sinani", vax$Correct_Village)
vax$Correct_Village<- gsub("mtwaraurban_shangani_tesligula", "mtwaraurban_shangani_ligulates", vax$Correct_Village)
vax$Correct_Village<- gsub("mtwaraurban_shangani_ttcligula", "mtwaraurban_chuno_ligulac", vax$Correct_Village)
vax$Correct_Village<- gsub("mtwaraurban_shangani_tulivu", "mtwaraurban_ufukoni_magomenitulivu", vax$Correct_Village)
vax$Correct_Village<- gsub("mtwaraurban_mtonya_ufukoni", "mtwaraurban_mtonya_mtonyaa", vax$Correct_Village)
vax$Correct_Village<- gsub("mtwaraurban_mtawanya_ufukonistand", "mtwaraurban_ufukoni_ufukonistendi", vax$Correct_Village)
vax$Correct_Village<- gsub("mtwaraurban_shangani_vigaeni", "mtwaraurban_vigaeni_sokokuu", vax$Correct_Village)
vax$Correct_Village<- gsub("mtwaraurban_shangani_zahanati", "mtwaraurban_shangani_shanganieast", vax$Correct_Village)
vax$Correct_Village<- gsub("mtwaraurban_rahaleo_sokoni", "mtwaraurban_rahaleo_shapriya", vax$Correct_Village)
vax$Correct_Village<- gsub("nachingwea_nambambo_chekereni", "nachingwea_stesheni_stesheni", vax$Correct_Village)
vax$Correct_Village<- gsub("nachingwea_nambambo_farm", "nachingwea_mneromiembeni_farm", vax$Correct_Village)
vax$Correct_Village<- gsub("nachingwea_nambambo_joshoni", "nachingwea_naipanga_naipanga", vax$Correct_Village)
vax$Correct_Village<- gsub("nachingwea_naipingo_kampanga", "nachingwea_naipingo_kwakamkamba", vax$Correct_Village)
vax$Correct_Village<- gsub("nachingwea_kilimahewa_kilimanihewa", "nachingwea_kilimanihewa_kilimanihewa", vax$Correct_Village)
vax$Correct_Village<- gsub("nachingwea_nambambo_libundu", "nachingwea_mkoka_rweje", vax$Correct_Village)
vax$Correct_Village<- gsub("nachingwea_nambambo_mailisita", "nachingwea_mkotokuyana_mtokokuyana", vax$Correct_Village)
vax$Correct_Village<- gsub("nachingwea_nambambo_majengo", "nachingwea_nditi_namanja", vax$Correct_Village)
vax$Correct_Village<- gsub("nachingwea_nambambo_manoari", "nachingwea_mtua_kiparamtua", vax$Correct_Village)
vax$Correct_Village<- gsub("nachingwea_nambambo_mapochelo", "nachingwea_naipingo_naipingo", vax$Correct_Village)
vax$Correct_Village<- gsub("nachingwea_nambambo_matemanga", "nachingwea_mkotokuyana_mandai", vax$Correct_Village)
vax$Correct_Village<- gsub("nachingwea_kilimahewa_mazoezi", "nachingwea_kilimanihewa_mazoezi", vax$Correct_Village)
vax$Correct_Village<- gsub("nachingwea_nambambo_miumbuti", "nachingwea_nangowe_mitumbati", vax$Correct_Village)
vax$Correct_Village<- gsub("nachingwea_mkotokuyana_mkotokuyana", "nachingwea_mkotokuyana_mtokokuyana", vax$Correct_Village)
vax$Correct_Village<- gsub("nachingwea_naipingo_namatumbisi", "nachingwea_naipingo_namatumbusi", vax$Correct_Village)
vax$Correct_Village<- gsub("nachingwea_nambambo_nambambo", "nachingwea_nambambo_boma", vax$Correct_Village)
vax$Correct_Village<- gsub("nachingwea_mtua_nmmanga", "nachingwea_mtua_nammanga", vax$Correct_Village)
vax$Correct_Village<- gsub("nachingwea_kilimahewa_nampemba", "nachingwea_kilimanihewa_nampemba", vax$Correct_Village)
vax$Correct_Village<- gsub("nachingwea_nambambo_naulingo", "nachingwea_lionja_lionjab", vax$Correct_Village)
vax$Correct_Village<- gsub("nachingwea_nambambo_ndangalimbo", "nachingwea_nditi_ngangambo", vax$Correct_Village)
vax$Correct_Village<- gsub("nachingwea_nambambo_mtepeche", "nachingwea_kilimanihewa_mtepeche", vax$Correct_Village)
vax$Correct_Village<- gsub("nachingwea_nambambo_tunduruyaleo", "nachingwea_nambambo_nachingwea", vax$Correct_Village)
vax$Correct_Village<- gsub("nanyumbu_sengenya_kisimatuli", "nanyumbu_sengenya_kisimatu", vax$Correct_Village)
vax$Correct_Village<- gsub("nanyumbu_mkonona_mbangalambuyuni", "nanyumbu_mkonona_mbagalambuyuni", vax$Correct_Village)
vax$Correct_Village<- gsub("nanyumbu_nanyumbu_mtawatawa", "nanyumbu_nanyumbu_chitawatawa", vax$Correct_Village)
vax$Correct_Village<- gsub("newala_mnyambe_butiama", "newala_luchindu_luchingu", vax$Correct_Village)
vax$Correct_Village<- gsub("newala_makote_chikwedu", "newala_mcholii_chiunjira", vax$Correct_Village)
vax$Correct_Village<- gsub("newala_chilangalanga_chilangala", "newala_chilangala_chilangala", vax$Correct_Village)
vax$Correct_Village<- gsub("newala_luchingu_chitandi", "newala_luchindu_chitandi", vax$Correct_Village)
vax$Correct_Village<- gsub("newala_mcholii_chiunjila", "newala_mcholii_chiunjira", vax$Correct_Village)
vax$Correct_Village<- gsub("newala_kitangari_kitangalihospitali", "newala_kitangari_kitangarihospitali", vax$Correct_Village)
vax$Correct_Village<- gsub("newala_kitangari_kitangalisokoni", "newala_kitangari_kitangarisokoni", vax$Correct_Village)
vax$Correct_Village<- gsub("newala_mkunya_legeza", "newala_mkunya_rahaleo", vax$Correct_Village)
vax$Correct_Village<- gsub("newala_mcholiii_lidumbeshuleni", "newala_mcholiii_lidumbeshule", vax$Correct_Village)
vax$Correct_Village<- gsub("newala_mkomaii_likwaya", "newala_maputi_mtongwele", vax$Correct_Village)
vax$Correct_Village<- gsub("newala_luchingu_lingana", "newala_luchindu_lingana", vax$Correct_Village)
vax$Correct_Village<- gsub("newala_namiyonga_lubido", "newala_nandwahi_chikundalubido", vax$Correct_Village)
vax$Correct_Village<- gsub("newala_nanguruwe_magunchila", "newala_nanguruwe_magumuchila", vax$Correct_Village)
vax$Correct_Village<- gsub("newala_kitangari_mandumba", "newala_namiyonga_namiyonga", vax$Correct_Village)
vax$Correct_Village<- gsub("newala_chilangalanga_mkulungulu", "newala_namiyonga_nakachela", vax$Correct_Village)
vax$Correct_Village<- gsub("newala_maputi_matale", "newala_mkwedu_mnyambachi", vax$Correct_Village)
vax$Correct_Village<- gsub("newala_mkunya_mbebede", "newala_chiwonga_pachanne", vax$Correct_Village)
vax$Correct_Village<- gsub("newala_mcholii_mdenga", "newala_chilangala_mikumbi", vax$Correct_Village)
vax$Correct_Village<- gsub("newala_chilangalanga_mikumbi", "newala_chilangala_mikumbi", vax$Correct_Village)
vax$Correct_Village<- gsub("newala_kitangari_mitumbati", "newala_mcholiii_mitumbati", vax$Correct_Village)
vax$Correct_Village<- gsub("newala_chilangalanga_miyuyu", "newala_chilangala_miyuyu", vax$Correct_Village)
vax$Correct_Village<- gsub("newala_chilangalanga_mkongi", "newala_chilangala_mkongi", vax$Correct_Village)
vax$Correct_Village<- gsub("newala_chilangalanga_mkudumba", "newala_chilangala_mkudumba", vax$Correct_Village)
vax$Correct_Village<- gsub("newala_mcholiii_mnalale", "newala_mcholii_chiunjira", vax$Correct_Village)
vax$Correct_Village<- gsub("newala_nanguruwe_mnaudya", "newala_makukwe_mnachi", vax$Correct_Village)
vax$Correct_Village<- gsub("newala_nanguruwe_mnauke", "newala_mcholiii_mnaida", vax$Correct_Village)
vax$Correct_Village<- gsub("newala_maputi_mnayope", "newala_kitangari_mmovo", vax$Correct_Village)
vax$Correct_Village<- gsub("newala_mnyambe_mnyengachi", "newala_chilangala_namdimba", vax$Correct_Village)
vax$Correct_Village<- gsub("newala_nandwahi_mpilani", "newala_nandwahi_mpirani", vax$Correct_Village)
vax$Correct_Village<- gsub("newala_chilangalanga_namangudu", "newala_chilangala_namangadu", vax$Correct_Village)
vax$Correct_Village<- gsub("newala_chilangalanga_namdimba", "newala_chilangala_namdimba", vax$Correct_Village)
vax$Correct_Village<- gsub("newala_luchingu_nangwala", "newala_luchindu_nangwala", vax$Correct_Village)
vax$Correct_Village<- gsub("newala_luchingu_nankonda", "newala_kitangari_namkonda", vax$Correct_Village)
vax$Correct_Village<- gsub("newala_chilangalanga_navanga", "newala_chilangala_navanga", vax$Correct_Village)
vax$Correct_Village<- gsub("newala_mtunguru_niamoja", "newala_kitangari_kitangarisokoni", vax$Correct_Village)
vax$Correct_Village<- gsub("newala_mtopwa_tulieni", "newala_mtopwa_mbeya", vax$Correct_Village)
vax$Correct_Village<- gsub("newala_luchingu_tupendane", "newala_luchindu_tupendane", vax$Correct_Village)
vax$Correct_Village<- gsub("newala_nandwahi_viokoli", "newala_nandwahi_vihokoli", vax$Correct_Village)
vax$Correct_Village<- gsub("ruangwa_chibula_amienje", "ruangwa_chibula_namihenje", vax$Correct_Village)
vax$Correct_Village<- gsub("ruangwa_chienjele_chienjele", "ruangwa_chienjele_njiechele", vax$Correct_Village)
vax$Correct_Village<- gsub("ruangwa_nachingwea_kilimanihewa", "ruangwa_nachingwea_kilimahewa", vax$Correct_Village)
vax$Correct_Village<- gsub("ruangwa_matambarale_matambaralekusini", "ruangwa_matambarale_matambarale", vax$Correct_Village)
vax$Correct_Village<- gsub("ruangwa_makanjiro_mbagara", "ruangwa_makanjiro_mbangara", vax$Correct_Village)
vax$Correct_Village<- gsub("ruangwa_chienjele_mibwe", "ruangwa_chienjele_mibure", vax$Correct_Village)
vax$Correct_Village<- gsub("ruangwa_malolo_michengaa", "ruangwa_malolo_mchengaa", vax$Correct_Village)
vax$Correct_Village<- gsub("ruangwa_malolo_michengab", "ruangwa_malolo_mchengab", vax$Correct_Village)
vax$Correct_Village<- gsub("ruangwa_nandagara_mmawa", "ruangwa_nandagala_mmawa", vax$Correct_Village)
vax$Correct_Village<- gsub("ruangwa_nandagara_namahemaa", "ruangwa_nandagala_namahemaa", vax$Correct_Village)
vax$Correct_Village<- gsub("ruangwa_nandagara_namahemab", "ruangwa_nandagala_namahemab", vax$Correct_Village)
vax$Correct_Village<- gsub("ruangwa_matambarale_namilema", "ruangwa_mbekenyera_namilema", vax$Correct_Village)
vax$Correct_Village<- gsub("ruangwa_nandagara_nandagalaa", "ruangwa_nandagala_nandagalaa", vax$Correct_Village)
vax$Correct_Village<- gsub("ruangwa_matambarale_nandandala", "ruangwa_matambarale_nandandara", vax$Correct_Village)
vax$Correct_Village <- gsub("tandahimba_mihenjele_bandari", "tandahimba_michenjele_bandari",vax$Correct_Village)
vax$Correct_Village<- gsub("tandahimba_mkundi_chitoholia", "tandahimba_mkundi_chitoholijuu", vax$Correct_Village)
vax$Correct_Village<- gsub("tandahimba_mkundi_chitoholib", "tandahimba_mkundi_chitoholijuu", vax$Correct_Village)
vax$Correct_Village<- gsub("tandahimba_mahuta_lidumbemtoni", "tandahimba_mahuta_lidumbwemtoni", vax$Correct_Village)
vax$Correct_Village<- gsub("tandahimba_mkundi_lipalwea", "tandahimba_mkundi_lipalwei", vax$Correct_Village)
vax$Correct_Village<- gsub("tandahimba_mkundi_lipalweb", "tandahimba_mkundi_lipalweii", vax$Correct_Village)
vax$Correct_Village<- gsub("tandahimba_lyenje_lyenje", "tandahimba_lyenje_mahona", vax$Correct_Village)
vax$Correct_Village<- gsub("tandahimba_chikongola_mahutamjini", "tandahimba_chikongola_mahuta", vax$Correct_Village)
vax$Correct_Village<- gsub("tandahimba_mchichira_mchichirashangani", "tandahimba_mchichira_shangani", vax$Correct_Village)
vax$Correct_Village<- gsub("tandahimba_mkundi_mikundashuleni", "tandahimba_mkundi_mikundasokoni", vax$Correct_Village)
vax$Correct_Village<- gsub("tandahimba_milingodi_milongodi", "tandahimba_milingodi_milingodi", vax$Correct_Village)
vax$Correct_Village<- gsub("tandahimba_mihuta_miuta", "tandahimba_miuta_miuta", vax$Correct_Village)
vax$Correct_Village<- gsub("tandahimba_mihuta_ngongolo", "tandahimba_miuta_ngongolo", vax$Correct_Village)
vax$Correct_Village<- gsub("tandahimba_mdumbwe_mdumbwe", "tandahimba_mdumbwe_mndumbwe", vax$Correct_Village)
vax$Correct_Village<- gsub("tandahimba_mihuta_namedi", "tandahimba_miuta_namedi", vax$Correct_Village)
vax$Correct_Village<- gsub("tandahimba_milingodi_namkongo", "tandahimba_milingodi_namkomolela", vax$Correct_Village)
vax$Correct_Village<- gsub("tandahimba_nanhyanga_nanhyangab", "tandahimba_nanhyanga_nanyangab", vax$Correct_Village)
vax$Correct_Village<- gsub("tandahimba_nanhyanga_nanhyangaa", "tandahimba_nanhyanga_nanyangaa", vax$Correct_Village)

studyVill <- match(unique(vax$Correct_Village),unique(STzVill$matchVill))
unique(vax$Correct_Village)[which(is.na(studyVill))] ## all matching


## Remove records where no dogs were vaccinated
vax<-vax[which(vax$Dogs>0),]

## Find and delete duplicated vaccination records
duplicates<-which(duplicated(vax[,c("dateVaccination","Dogs","Cats","Correct_Village")]))
if(length(duplicates)>0){vax <-vax[-duplicates,]}





## Group vaccinations that occurred on the same date
#------------------

vax$dateVaccination <- as.Date(vax$dateVaccination, format = "%d/%m/%Y")
vax_agg <- aggregate(Dogs~Correct_Village+Correct_Ward+Correct_District+Correct_Region+dateVaccination,vax,sum)



## Save cleaned vaccination data
#-------------------
  
if(!dir.exists("Output/VaxData")){dir.create("Output/VaxData")}
write.csv(vax_agg,paste("Output/VaxData/AllDogVaccinatedGA_cleaned.csv",sep=""),row.names = F)

