
setwd(paste0(Sys.getenv("CS_HOME"),'/UrbanDynamics/Models/ABMCitiesFirms'))

library(sp)
library(sf)
library(dplyr)
# library(cartography)
library(ggplot2)

source('analysis/functions.R')


##########
# 1) FUA / links overlay
#


fuas <- st_read(paste0(Sys.getenv('CS_HOME'),'/Data/JRC_EC/GHS/GHS_FUA_UCDB2015_GLOBE_R2019A_54009_1K_V1_0/GHS_FUA_UCDB2015_GLOBE_R2019A_54009_1K_V1_0.gpkg'))

ucdb <- st_read(paste0(Sys.getenv('CS_HOME'),'/Data/JRC_EC/GHS/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_0/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_0.shp'))
ucdb <- st_transform(ucdb,st_crs(fuas))

firms <- read.csv('Data/firms/amadeus_nodes.csv',sep=";",quote = "")
firmlinks <- read.csv('Data/firms/amadeus_links.csv',sep=";",quote = "")

firmswithcoords = firms[!is.na(firms$lon)&!is.na(firms$lat),]

rawfirmpoints = SpatialPointsDataFrame(coords = firmswithcoords[,c("lon","lat")],data = firmswithcoords,proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
firmpoints <- st_transform(st_as_sf(rawfirmpoints),st_crs(fuas))

# overlay with fuas
firmfuas = st_join(firmpoints,fuas,join=st_within)

# overlay ucdb with fuas for pop/gdp properties
ucdbfuas = st_join(ucdb,fuas,join=st_within)

# Filter 
firms_withfuas <- firmfuas %>% filter(!is.na(eFUA_ID))

eucountries_iso = c(
    "GBR","JEY", # must add Jersey - keep UK
    "DEU","FRA","NLD", # Allemagne France Pays-Bas
    "ITA","LUX","ESP", #Italie  Luxembourg  Espagne
    "AUT","BEL","FIN", # Autriche Belgique Finlande
    "POL","DNK","PRT", # Pologne Danemark Portugal
    "HUN","ROU","CZE", # Hongrie Roumanie Tchéquie
    "LVA","LTU","SVK", # Lettonie Lituanie Slovaquie
    "GRC","IRL","BGR", # Grèce Irlande Bulgarie
    "CYP","EST","SVN", # Chypre Estonie Slovénie
    "HRV","MLT","SWE") #Croatie Malte Suède
# + North Cyprus - not taken into account

firms_withfuas_eu <- firms_withfuas %>% filter(Cntry_ISO%in%eucountries_iso)


# firms in fua with are connected to a link
linkids = unique(c(as.character(firmlinks$from),as.character(firmlinks$to)))


# join fuaids to firmlinks -> ! ~ 1000 companies are repeated - should already filter by year?
#  ! should be a semi_join ? - or filter firms table before - which info to keep? the closest in time to the link ?
#  -> nested join and summarize
linkfuas = left_join(firmlinks,firms_withfuas_eu[,c('id','eFUA_ID','turnover','latestinfo','Cntry_ISO')],by=c('from'='id'))
names(linkfuas)[6:9]<-c("from_fua","from_turnover","from_year","from_country")
linkfuas = left_join(linkfuas,firms_withfuas_eu[,c('id','eFUA_ID','turnover','latestinfo','Cntry_ISO')],by=c('to'='id'))
names(linkfuas)[11:14] <- c('to_fua',"to_turnover","to_year","to_country")

# year of link observation
linkfuas$year = substring(linkfuas$date,4,7)

# proportion
linkfuas$proportion = ifelse(!is.na(linkfuas$total_ownership),linkfuas$total_ownership,linkfuas$direct_ownership)

linkfuas$from_fua=as.character(linkfuas$from_fua)
linkfuas$to_fua=as.character(linkfuas$to_fua)

####
# save for further analysis
#save(firmpoints,firmfuas,firms_withfuas,linkfuas,file='Data/firms/amadeus_aggregFUA.RData')
save(firmpoints,firmfuas,firms_withfuas,firms_withfuas_eu,linkfuas,file='Data/firms/amadeus_aggregGHSFUA.RData')
#load(file='Data/firms/amadeus_aggregGHSFUA.RData')




# filter links between fuas # -> remains 555196
links = linkfuas[!is.na(linkfuas$from_fua)&!is.na(linkfuas$to_fua),]
# with turnover at destination and proportion of ownership # remains 161303
links = links[!is.na(links$to_turnover)&!is.na(links$proportion),]




# for all links: country is obtained through fua -> need an other join
linkall = left_join(firmlinks,firms[,c('id','country')],by=c('from'='id'))
linkall = left_join(linkall,firms[,c('id','country')],by=c('to'='id'))

# exports for UK
names(linkall)[6:7]=c('from_country','to_country')
# from uk
write.table(linkall[which(linkall$from_country=='GB'),],file='Data/firms/extracts/fromUK_links.csv',sep = ";",quote = F,col.names = T,row.names = F)
linkids=unique(c(linkall$from[which(linkall$from_country=='GB')],linkall$to[which(linkall$from_country=='GB')]))
write.table(firms[firms$id%in%linkids,],file='Data/firms/extracts/fromUK_nodes.csv',sep = ";",quote = F,col.names = T,row.names = F)

# to uk
write.table(linkall[which(linkall$to_country=='GB'),],file='Data/firms/extracts/toUK_links.csv',sep = ";",quote = F,col.names = T,row.names = F)
linkids=unique(c(linkall$from[which(linkall$to_country=='GB')],linkall$to[which(linkall$to_country=='GB')]))
write.table(firms[firms$id%in%linkids,],file='Data/firms/extracts/toUK_nodes.csv',sep = ";",quote = F,col.names = T,row.names = F)

# within uk
write.table(linkall[which(linkall$from_country=='GB'&linkall$to_country=='GB'),],file='Data/firms/extracts/withinUK_links.csv',sep = ";",quote = F,col.names = T,row.names = F)
linkids=unique(c(linkall$from[which(linkall$from_country=='GB'&linkall$to_country=='GB')],linkall$to[which(linkall$from_country=='GB'&linkall$to_country=='GB')]))
write.table(firms[firms$id%in%linkids,],file='Data/firms/extracts/withinUK_nodes.csv',sep = ";",quote = F,col.names = T,row.names = F)

# from UK fuas
uklinks = linkfuas[which(linkfuas$from_country=='GBR'|linkfuas$to_country=='GBR'),]
uklinks[["geometry.x"]]=NULL;uklinks[["geometry.y"]]=NULL;uklinks[["proportion"]]=NULL;
uklinks[["from_turnover"]]=NULL;uklinks[["to_turnover"]]=NULL;uklinks[["from_year"]]=NULL;uklinks[["to_year"]]=NULL;

write.table(uklinks[which(uklinks$from_country=='GBR'),],file='Data/firms/extracts/fromUK_FUA_links.csv',sep = ";",quote = F,col.names = T,row.names = F)
linkids=unique(c(uklinks$from[which(uklinks$from_country=='GBR')],uklinks$to[which(uklinks$from_country=='GBR')]))
write.table(firms[firms$id%in%linkids,],file='Data/firms/extracts/fromUK_FUA_nodes.csv',sep = ";",quote = F,col.names = T,row.names = F)

write.table(uklinks[which(uklinks$to_country=='GBR'),],file='Data/firms/extracts/toUK_FUA_links.csv',sep = ";",quote = F,col.names = T,row.names = F)
linkids=unique(c(uklinks$from[which(uklinks$to_country=='GBR')],uklinks$to[which(uklinks$to_country=='GBR')]))
write.table(firms[firms$id%in%linkids,],file='Data/firms/extracts/toUK_FUA_nodes.csv',sep = ";",quote = F,col.names = T,row.names = F)

write.table(uklinks[which(uklinks$to_country=='GBR'&uklinks$from_country=='GBR'),],file='Data/firms/extracts/withinUK_FUA_links.csv',sep = ";",quote = F,col.names = T,row.names = F)
linkids=unique(c(uklinks$from[which(uklinks$to_country=='GBR'&uklinks$to_country=='GBR')],uklinks$to[which(uklinks$to_country=='GBR'&uklinks$to_country=='GBR')]))
write.table(firms[firms$id%in%linkids,],file='Data/firms/extracts/withinUK_FUA_nodes.csv',sep = ";",quote = F,col.names = T,row.names = F)



# Aggregate links
aggrlinks <- links %>% filter(to_turnover>0) %>% group_by(from_fua,to_fua) %>% summarize(weight = sum(proportion*to_turnover))


####
# uk
fuas$eFUA_ID=as.character(fuas$eFUA_ID)
aggrlinkscountries = left_join(aggrlinks,fuas[,c('eFUA_ID','Cntry_ISO')],by=c('from_fua'='eFUA_ID'))
aggrlinkscountries = left_join(aggrlinkscountries,fuas[,c('eFUA_ID','Cntry_ISO')],by=c('to_fua'='eFUA_ID'))




######
# network construction



# unique(floor(as.numeric(as.character(firms_withfuas_eu$nacecode))/1000))
# ! NO first digit does not mean anything: either first level (letter), or two first digits
#firms_withfuas_eu$nace_firstdigit = floor(as.numeric(as.character(firms_withfuas_eu$nacecode))/1000)
getNaceFirstLevel <- function(code){
  digs = floor(as.numeric(as.character(code))/100)
  if(!is.numeric(digs)){show(as.character(code));return(NA)}
  if(is.na(digs)){show(as.character(code));return(NA)}
  if(digs>1&digs<=3){return('A')}
  if(digs>3&digs<=9){return('B')}
  if(digs>9&digs<=33){return('C')}
  if(digs==35){return('D')}
  if(digs>35&digs<=39){return('E')}
  if(digs>39&digs<=43){return('F')}
  if(digs>43&digs<=47){return('G')}
  if(digs>47&digs<=53){return('H')}
  if(digs>53&digs<=56){return('I')}
  if(digs>56&digs<=63){return('J')}
  if(digs>63&digs<=66){return('K')}
  if(digs==68){return('L')}
  if(digs>68&digs<=75){return('M')}
  if(digs>75&digs<=82){return('N')}
  if(digs==84){return('O')}
  if(digs==85){return('P')}
  if(digs>85&digs<=88){return('Q')}
  if(digs>88&digs<=93){return('R')}
  if(digs>93&digs<=96){return('S')}
  if(digs>96&digs<=98){return('T')}
  if(digs==99){return('U')}
  return(NA)
}
firms_withfuas_eu$nace_firstdigit = sapply(firms_withfuas_eu$nacecode,getNaceFirstLevel)
firms_withfuas_eu$nace_firstdigit = unlist(firms_withfuas_eu$nace_firstdigit)

# ! must have indus compo by aggregating industrial sectors -> first nace digit (10)

# one city has negative sector composition?
#exportnodes[exportnodes$name=='Tulcea',] # -> negative turnover
# firms_withfuas_eu[firms_withfuas_eu$turnover<0&!is.na(firms_withfuas_eu$turnover),] # -> 2281 : filter negative turnovers

aggrnodes = firms_withfuas_eu %>% filter(!is.na(turnover)&!is.na(nace_firstdigit)&turnover>0) %>% group_by(eFUA_ID) %>% summarize(
  turnover=sum(turnover)#,
  #sector0 = sum(turnover[floor(nacecode/1000)==0])/sum(turnover)
)
for(k in unique(firms_withfuas_eu$nace_firstdigit[!is.na(firms_withfuas_eu$nace_firstdigit)])){
  show(k)
  saggr = firms_withfuas_eu %>% filter(!is.na(turnover)&!is.na(nace_firstdigit)&turnover>0) %>%
    group_by(eFUA_ID) %>% summarize(sector = sum(turnover[nace_firstdigit==k])/sum(turnover))
  aggrnodes[[paste0('sector',k)]]=saggr$sector
}
for(k in unique(firms_withfuas_eu$nace_firstdigit[!is.na(firms_withfuas_eu$nace_firstdigit)])){aggrnodes[is.na(aggrnodes[,paste0('sector',k)]),paste0('sector',k)]=0}
aggrnodes$geometry=NULL
# export to wgs84 for gis ext in netlogo
coords = as.tbl(data.frame(fuaid = fuas$eFUA_ID,fuacountry = fuas$Cntry_name,fuaname=fuas$eFUA_name,st_coordinates(st_transform(st_centroid(fuas),CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")))))
aggrnodes = left_join(aggrnodes,coords,by=c('eFUA_ID'='fuaid'))
names(aggrnodes)[1]='fua'
aggrnodes$fua = as.character(aggrnodes$fua)


sectornames = paste0('sector',unique(firms_withfuas_eu$nace_firstdigit[!is.na(firms_withfuas_eu$nace_firstdigit)]))


####
# export aggrlinks and aggrnodes for model calibration

# extent for real world size
bb=st_bbox(fuas[fuas$eFUA_ID%in%aggrnodes$eFUA_ID,])
(bb$xmax - bb$xmin)/1000 # -> 4541km
(bb$ymax - bb$ymin)/1000 # -> 3977km

# netlogo format is
#   id ; name ; time (=> set to t0 in "static network", i.e. start from no network and try to reproduce final network, configuration)
#   x ; y ; country ; GDP ( == aggregated turnover) ;
#   sectors (any number, as proportions)

aggrnodes$fuacountry=as.character(aggrnodes$fuacountry)
exportnodes = as.data.frame(aggrnodes)
exportnodes$time = rep(0,nrow(exportnodes))
exportnodes = exportnodes[,c("fua","fuaname","time","X","Y","fuacountry","turnover",sectornames)]
names(exportnodes) <- c("id","name","time","x","y","country","turnover",sectornames)
# country number needed! - order by alphabetical name (do same for estimated dmat)
#exportnodes$country = as.numeric(as.factor(as.character(exportnodes$country)))


exportlinks = as.data.frame(aggrlinks)
exportlinks$time = rep(0,nrow(exportlinks))
exportlinks = exportlinks[,c("from_fua","to_fua","time","weight")]
# filter self-links
exportlinks=exportlinks[exportlinks$from_fua!=exportlinks$to_fua,]

# export for NetLogo
write.table(exportlinks,file='model_nl6/setup/fualinks.csv',row.names = F,sep=";",quote = F)
write.table(exportnodes,file='model_nl6/setup/fuacities.csv',row.names = F,sep=";",quote=F)

# save aggreg
save(aggrnodes, aggrlinks, exportnodes, exportlinks, file='Data/firms/amadeus_aggregnw.RData')
#load('Data/firms/amadeus_aggregnw.RData')

# UK export



######
## Correlation betweem turnover and UCDB properties

# ! if scaling law, no pearson corr -> use rank corr !

sucdb = ucdbfuas %>% filter(!is.na(eFUA_ID)) %>% group_by(eFUA_ID) %>% summarize(pop=sum(P15),gdp=sum(GDP15_SM))
sucdb$eFUA_ID=as.character(sucdb$eFUA_ID)
saggrnodes = left_join(aggrnodes,sucdb,by=c('fua'='eFUA_ID'))

# 's' for summarized
save(sucdb,saggrnodes, file='Data/firms/amadeus_saggregnw.RData')
#load('Data/firms/amadeus_saggregnw.RData')


# -> summary stats in summaryStats.R
# -> Network analysis in networkAnalysis.R




###
# Statistical analysis
#  -> prepare data for statistical models

# construct flow data
stataggrlinks = left_join(aggrlinks,aggrnodes[,c("fua","turnover","fuacountry")],by=c('from_fua'='fua'));names(stataggrlinks)[4:5]<-c("from_turnover","from_country")
stataggrlinks = left_join(stataggrlinks,aggrnodes[,c("fua","turnover","fuacountry")],by=c('to_fua'='fua'));names(stataggrlinks)[6:7]<-c("to_turnover","to_country")
sectornames = names(aggrnodes)[3:21]

# ! still link with zero weight ?

# add geo distance
dists = spDists(as.matrix(aggrnodes[,c("X","Y")]))
rownames(dists)<- aggrnodes$fua;colnames(dists)<- aggrnodes$fua
distsdf = melt(dists)
distsdf$Var1=as.character(distsdf$Var1);distsdf$Var2=as.character(distsdf$Var2)
stataggrlinks=left_join(stataggrlinks,distsdf,by=c("from_fua"="Var1","to_fua"="Var2"));names(stataggrlinks)[8]<-"distance"

# cosine similarity: matrix product
proximities = as.matrix(aggrnodes[,sectornames])%*%t(as.matrix(aggrnodes[,sectornames]))
rownames(proximities)<-aggrnodes$fua;colnames(proximities)<-aggrnodes$fua
mprox = melt(proximities);mprox$Var1=as.character(mprox$Var1);mprox$Var2 = as.character(mprox$Var2)
stataggrlinks = left_join(stataggrlinks,mprox,by=c('from_fua'='Var1','to_fua'='Var2'))
names(stataggrlinks)[9]<-c("sim")

# 2021/06/15: added distsdf and mprox in saved data to extend analysis to zeroinfl in statistics
save(stataggrlinks,distsdf,mprox,file='Data/firms/amadeus_stataggrlinks.RData')

# -> stat analysis in statistic.R



#####

# validation of Companies_house_data/SOURCE/
# by sectors ?

# Todo sector composition of FUAs

# NACE nomenclature https://ec.europa.eu/eurostat/ramon/nomenclatures/index.cfm?TargetUrl=DSP_NOM_DTL_VIEW&StrNom=NACE_REV2&StrLanguageCode=FR&IntPcKey=18493784&IntKey=18493934&StrLayoutCode=HIERARCHIC&IntCurrentPage=1


# aggregate firm stats at FUAs and by year (cumulated year)
# firmfuas$year <- firmfuas$latestinfo
# # check if duplicates
# summary(firmpoints %>% group_by(id) %>% summarise(count = n()))
# 
# years = unique(c(linkfuas$year,firmfuas$year))
#   
# aggrnodes = data.frame()
# for(year in years){
#   currentaggrnodes <- firmfuas[firmfuas$year<=year] %>% group_by(FUA_CODE,id) %>% mutate(
#     # select latest observation only
#     latest_turnover = turnover[year==max(year)]
#   ) %>% group_by(FUA_CODE,sector) %>% summarize(
#     turnover = sum(latest_turnover)
#   )
#   aggrnodes = rbind(aggrnodes,currentaggrnodes)
# }




