
setwd(paste0(Sys.getenv("CS_HOME"),'/UrbanDynamics/Models/ABMCitiesFirms'))

library(sp)
library(sf)
library(dplyr)
library(cartography)
library(ggplot2)

source('analysis/functions.R')


##########
# 1) FUA / links overlay
#

#fuas <- st_read('Data/UI-boundaries-FUA/','FUA_Boundaries')
fuas <- st_read(paste0(Sys.getenv('CS_HOME'),'/Data/JRC_EC/GHS/GHS_FUA_UCDB2015_GLOBE_R2019A_54009_1K_V1_0/GHS_FUA_UCDB2015_GLOBE_R2019A_54009_1K_V1_0.gpkg'))
#fuas <- st_transform(fuas,CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")) # NO - transform the points to fuas

ucdb <- st_read(paste0(Sys.getenv('CS_HOME'),'/Data/JRC_EC/GHS/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_0/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_0.shp'))
ucdb <- st_transform(ucdb,st_crs(fuas))

countries <- st_read('Data/','countries')
#countries <- st_read(paste0(Sys.getenv('CS_HOME'),'/Data/Countries/'),'countries') # same

#countries <- st_transform(countries,st_crs(fuas)) #makes no sense at this scale

firms <- read.csv('Data/firms/amadeus_nodes.csv',sep=";",quote = "")
firmlinks <- read.csv('Data/firms/amadeus_links.csv',sep=";",quote = "")

dim(firms)
# 3,053,540
dim(firmlinks)
# 1,866,936

firmswithcoords = firms[!is.na(firms$lon)&!is.na(firms$lat),]
dim(firmswithcoords)
# 2,715,188

rawfirmpoints = SpatialPointsDataFrame(coords = firmswithcoords[,c("lon","lat")],data = firmswithcoords,proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
firmpoints <- st_transform(st_as_sf(rawfirmpoints),st_crs(fuas))

# overlay with fuas
firmfuas = st_join(firmpoints,fuas,join=st_within)

# overlay ucdb with fuas for pop/gdp properties
ucdbfuas = st_join(ucdb,fuas,join=st_within)



# Filter 
#firms_withfuas <- firmfuas %>% filter(!is.na(FUA_CODE))
firms_withfuas <- firmfuas %>% filter(!is.na(eFUA_ID))
# 2,036,272 with GHS FUA


# overlay with Europe countries
#countriesEurope = st_transform(countries,st_crs(fuas))
#firmcountries = st_join(firmpoints,countriesEurope,join=st_within)
# proportion of firms within Europe
#100*length(which(is.na(firmcountries$CNTR_ID)))/nrow(firmcountries)
#length(which(!is.na(firmcountries$CNTR_ID)))
# remove Switzerland and Norway and other non EU countries
#100*length(which(is.na(firmcountries$CNTR_ID)|as.character(firmcountries$CNTR_ID)%in%c("NO")))/nrow(firmcountries)
#eucountries = c(
#  "UK","DE","FR","NL","IT","LU","ES","AT","BE","FI",
#  "PL","DK","PT","HU","RO","CZ","LV","LT","SK","EL",
#  "IE","BG","SE","EE","SI","HR")
#100*length(which(as.character(firmcountries$CNTR_ID)%in%eucountries))/nrow(firmcountries)
# 97.71 %
#firmseu = firmswithcoords[as.character(firmcountries$CNTR_ID)%in%c("UK","DE","FR","NL","IT","LU","ES","AT","BE","FI","PL","DK","PT","HU","RO","CZ","LV","LT","SK","EL","IE","BG","SE","EE","SI","HR"),]
#firmseuid = unique(as.character(firmseu$id))
#100*length(which(firmlinks$from%in%firmseuid&firmlinks$to%in%firmseuid))/nrow(firmlinks)
# only 40.69% of within EU links

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

# firms within EU
nrow(firmfuas %>% filter(Cntry_ISO%in%eucountries_iso))
# 2,033,799

firms_withfuas_eu <- firms_withfuas %>% filter(Cntry_ISO%in%eucountries_iso)
nrow(firms_withfuas_eu)
# 2,033,799


# firms in fua with are connected to a link
linkids = unique(c(as.character(firmlinks$from),as.character(firmlinks$to)))
length(which(firms_withfuas_eu$id%in%linkids))

# firms in fua with turnover and a link
length(which(firms_withfuas$id%in%linkids&!is.na(firms_withfuas$turnover)))

# join fuaids to firmlinks -> ! ~ 1000 companies are repeated - should already filter by year?
# FIXME should be a semi_join ? - or filter firms table before - which info to keep? the closest in time to the link ?
#  -> nested join and summarize
linkfuas = left_join(firmlinks,firms_withfuas_eu[,c('id','eFUA_ID','turnover','latestinfo','Cntry_ISO')],by=c('from'='id'))
names(linkfuas)[6:9]<-c("from_fua","from_turnover","from_year","from_country")
length(which(!is.na(linkfuas$from_fua))) # 652258
length(which(!is.na(linkfuas$from_fua)))/nrow(linkfuas) # 32.99 % = 652258 between fuas
# join for to_fuas
linkfuas = left_join(linkfuas,firms_withfuas_eu[,c('id','eFUA_ID','turnover','latestinfo','Cntry_ISO')],by=c('to'='id'))
names(linkfuas)[11:14] <- c('to_fua',"to_turnover","to_year","to_country")
length(which(!is.na(linkfuas$to_fua)))

length(which(!is.na(linkfuas$to_fua)&!is.na(linkfuas$from_fua)))

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

# where do links go ?
length(which(linkfuas$from_country%in%eucountries_iso))
length(which(linkfuas$to_country%in%eucountries_iso))
length(which(linkfuas$from_country%in%eucountries_iso&linkfuas$to_country%in%eucountries_iso))
# - country ownership matrix (cumulated in time)
# - visualize inter-country flows: all world and EU

# - TODO
#length(which(!is.na(linkcountries$from_country)))
#length(which(!is.na(linkcountries$to_country)))
#length(which(!is.na(linkcountries$from_country)&!is.na(linkcountries$to_country)))



####
# filter links between fuas # -> remains 555196
length(which(!is.na(linkfuas$from_fua)))
length(which(!is.na(linkfuas$to_fua)))

links = linkfuas[!is.na(linkfuas$from_fua)&!is.na(linkfuas$to_fua),]
# with turnover at destination and proportion of ownership # remains 161303
length(which(!is.na(links$to_turnover)&!is.na(links$proportion)))
links = links[!is.na(links$to_turnover)&!is.na(links$proportion),]


####
# ! not needed, as year link info is crap
# links can be aggregated by o-d and year
#aggrlinksyear <- linkfuas %>% group_by(from_fua,to_fua,year,.drop=T) %>% summarize(weight = sum(proportion*to_turnover))
#summary(aggrlinks[aggrlinks$year=="2018",])
#aggrlinksyear = aggrlinksyear[!is.na(aggrlinksyear$from_fua)&!is.na(aggrlinksyear$to_fua)&!is.na(aggrlinksyear$year)&!is.na(aggrlinksyear$weight),]
#aggrlinksyear$dummy=rep("link",nrow(aggrlinksyear))


####
# UK Statistics
length(which(firms$country=='GB')) # 463731
length(which(firmfuas$Cntry_ISO=='GBR'&!is.na(firmfuas$eFUA_ID))) # 413740
length(which(firmfuas$Cntry_ISO=='GBR'&firmfuas$id%in%linkids)) # 336804
length(which(firmfuas$Cntry_ISO=='GBR'&!is.na(firmfuas$turnover))) #78273
length(which(firmfuas$Cntry_ISO=='GBR'&!is.na(firmfuas$turnover)&firmfuas$id%in%linkids)) # 68273

length(which(linkfuas$from_country=='GBR')) # 128066
length(which(linkfuas$to_country=='GBR')) # 293820
length(which(linkfuas$from_country=='GBR'&linkfuas$to_country=='GBR')) # 116661
length(which(linkfuas$from_country=='GBR'&linkfuas$to_country=='GBR'&!is.na(linkfuas$from_fua)&!is.na(linkfuas$to_fua))) # 116661
length(which(linkfuas$from_country=='GBR'&linkfuas$to_country=='GBR'&!is.na(linkfuas$to_turnover)&!is.na(linkfuas$proportion))) # 12480
# same, as country is obtained through fua -> need an other join
linkall = left_join(firmlinks,firms[,c('id','country')],by=c('from'='id'))
linkall = left_join(linkall,firms[,c('id','country')],by=c('to'='id'))
length(which(linkall$country.x=='GB')) # 142975
length(which(linkall$country.y=='GB')) # 323618
length(which(linkall$country.x=='GB'&linkall$country.y=='GB')) #131819

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
length(which(aggrlinkscountries$Cntry_ISO.x=="GBR")) # 1524
length(which(aggrlinkscountries$Cntry_ISO.y=="GBR")) # 1541
length(which(aggrlinkscountries$Cntry_ISO.y=="GBR"&aggrlinkscountries$Cntry_ISO.x=="GBR"))

# year repartition is shitty
#table(links$year)
#length(which(duplicated(links[,c("from","to")]))) # too few replicated links to have double observations
# -> dynamical info can not really be used ?
# -> fit the model from empty network ('stationary state') ?



######
# network analysis
#  -> modularities of countries, of NUTS
#  -> Louvain communities
#timeaggrlinks = aggrlinks

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

# bug in aggreg? ! all fuas should have sectors summing to 1 as all companies have nace digits!
#aggrnodes[rowSums(aggrnodes[,3:12])==0,]
#faggr = firms_withfuas_eu[firms_withfuas_eu$eFUA_ID==42,]
#firms_withfuas_eu[firms_withfuas_eu$eFUA_ID==42&firms_withfuas_eu$nace_firstdigit==4,]

sectornames = paste0('sector',unique(firms_withfuas_eu$nace_firstdigit[!is.na(firms_withfuas_eu$nace_firstdigit)]))
##
# histograms (clustered) of sector profiles
km = kmeans(aggrnodes[,sectornames],centers = 5,nstart = 100)
aggrnodes$cluster = km$cluster
clustsizes = aggrnodes%>% group_by(cluster) %>% summarize(turnover=mean(turnover))
lcentroids = data.frame()
for(k in sectornames){lcentroids=rbind(lcentroids,data.frame(prop=km$centers[,k],sector=rep(k,nrow(km$centers)),centre=1:nrow(km$centers),size=clustsizes$turnover))}
g=ggplot(lcentroids,aes(x=as.character(sector),y=prop,color=as.character(centre),group=as.character(centre)))
g+geom_line()#+stat_smooth(method = 'loess',span = 0.5)
ggsave(file='Results/EmpiricalNetwork/sectordistrib_clusters5.png',width=20,height=18,units='cm')

g=ggplot(lcentroids,aes(x=as.character(sector),y=prop,color=log(size),group=as.character(centre)))
g+geom_line()#+stat_smooth(method = 'loess',span = 0.5)
ggsave(file='Results/EmpiricalNetwork/sectordistrib_clusters5_colorMeanTurnover.png',width=20,height=18,units='cm')
# ! -> largest city have the sctor 9 profile - slightly smallest flat to the right, smaller peaks
# -> heuristic for synthetic sectors ~ relevant !


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

cor.test(saggrnodes$turnover,saggrnodes$pop,method='pearson')
cor.test(saggrnodes$turnover,saggrnodes$pop,method='spearman')

cor.test(saggrnodes$turnover,saggrnodes$gdp,method='pearson')
cor.test(saggrnodes$turnover,saggrnodes$gdp,method='spearman')


######
##  try basic flow map
currentlinks = aggrlinks[aggrlinks$weight>quantile(aggrlinks$weight,c(0.97)),]
linkLayer <- getLinkLayer(x=fuas,xid='FUA_CODE',df=currentlinks,dfid = c("from_fua","to_fua"))
#osm <- getTiles(x = countries, type = "osm", zoom = 11, crop = TRUE)
#tilesLayer(x = osm)
#plot(fuas)
png(filename = 'Results/EmpiricalNetwork/fua_flows.png',width = 20, height = 18, units = "cm",res=300)

plot(st_centroid(st_geometry(fuas)))#, col = NA, border = "grey", add=TRUE)
gradLinkTypoLayer(
  x = linkLayer,
  xid=c("from_fua","to_fua"),
  df = currentlinks,
  dfid=c("from_fua","to_fua"),
  var = "weight",
  breaks = c(min(currentlinks$weight),quantile(currentlinks$weight,c(0.25)), median(currentlinks$weight), quantile(currentlinks$weight,c(0.995)), quantile(currentlinks$weight,c(0.9995))),
  #lwd = c(1,2,5,10)/2#,
  var2 = "dummy"
)
#layoutLayer(title = paste0("Ownership links"),
#            frame = FALSE, col = "grey25", coltitle = "white",
#            tabtitle = TRUE)
dev.off()





#######
## Network Analysis

library(igraph)
library(Matrix)
library(reshape2)
library(poweRlaw)

g = graph_from_data_frame(aggrlinks,directed = T,vertices = saggrnodes[,1:30])
V(g)$lon = (V(g)$X - min(V(g)$X)) / (max(V(g)$X) - min(V(g)$X))
V(g)$lat = (V(g)$Y - min(V(g)$Y)) / (max(V(g)$Y) - min(V(g)$Y))
write_graph(g,file='Results/EmpiricalNetwork/fua_graph.gml',format='gml')


# Centralities 
mean(degree(g))
mean(strength(g))
plot(log(1:length(V(g))),sort(log(strength(g)),decreasing = T))
plot(log(1:length(V(g))),sort(log(strength(g,mode='in')),decreasing = T))
plot(log(1:length(V(g))),sort(log(strength(g,mode='out')),decreasing = T))
# roughly the same! balance in/out ?
summary(strength(g,mode='in')/strength(g,mode='out'))

# degree distribution (remove 2 zeros: fuas with no links)
fitdeg = fitDistrPowerLaw(strength(g)[strength(g)>0],'Weighted degree',file='Results/EmpiricalNetwork/degreeDistr.png')
#get_distance_statistic(fitdeg$powerlaw)
#get_distance_statistic(fitdeg$ln)
fitDistrPowerLaw(strength(g,mode='in')[strength(g,mode='in')>0],'Weighted in-degree',file='Results/EmpiricalNetwork/in-degreeDistr.png')
fitDistrPowerLaw(strength(g,mode='out')[strength(g,mode='out')>0],'Weighted out-degree',file='Results/EmpiricalNetwork/out-degreeDistr.png')

# edge weight distribution
fitdegweights = fitDistrPowerLaw(E(g)$weight[E(g)$weight>0],'Edge weight',file='Results/EmpiricalNetwork/edgeweight.png')

# centrality distrib: bw? # ! makes not much sense in practice



# Communities
A = get.adjacency(g,sparse = T)
g_undir = graph_from_adjacency_matrix(adjmatrix = (A+t(A))/2,weighted = T,mode = "undirected")

set.seed(42)
communities_clauset = cluster_fast_greedy(g_undir)
set.seed(42)
communities_louvain = cluster_louvain(g_undir) 
# 0.35 modularity in both cases

# directed mod
directedmodularity(communities_clauset$membership,A) # 0.3521929
directedmodularity(communities_louvain$membership,A) # 0.361524

# countries mod
directedmodularity(V(g)$fuacountry,A) # 0.3200182

# null model with 30 random communities
bnum=1000; mods = c()
for(b in 1:bnum){
  if(b%%100==0){show(b)}
  mods=append(mods,directedmodularity(sample.int(30,size=length(V(g)),replace = T),A))
}
mean(mods);sd(mods)

# max modularity?
maxmodularity(communities_louvain$membership,A)

# overlap between countries / communities


###
# Community map

saggrnodes$Community = as.character(communities_louvain$membership)
#saggrnodes$Population = saggrnodes$pop # issue with ghs - fua or pop - anyway plot with turnover
saggrnodes$Turnover = saggrnodes$turnover
comsizes = as.data.frame(table(communities_louvain$membership))
keptcoms = as.character(comsizes[comsizes[,2]>5,1])
saggrnodes$Community[!saggrnodes$Community%in%keptcoms]=NA

map(data = saggrnodes,var = "Community", sizevar = "Turnover",
    xlim=c(-10,30),ylim=c(35,62),
    filename = 'Results/EmpiricalNetwork/map_communities_louvain.png',discrete = T,
    width=22,height=18
    )



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




