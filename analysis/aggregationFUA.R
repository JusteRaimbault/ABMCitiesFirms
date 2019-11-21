
setwd(paste0(Sys.getenv("CS_HOME"),'/UrbanDynamics/Models/ABMCitiesFirms'))

library(sp)
library(sf)
library(dplyr)
library(cartography)

fuas <- st_read('Data/UI-boundaries-FUA/','FUA_Boundaries')

countries <- st_read('model_nl6/setup/gis/','europe_wgs84')
countries <- st_transform(countries,st_crs(fuas))

firms <- read.csv('Data/firms/amadeus_nodes.csv',sep=";",quote = "")
firmlinks <- read.csv('Data/firms/amadeus_links.csv',sep=";",quote = "")
firmswithcoords = firms[!is.na(firms$lon)&!is.na(firms$lat),]

rawfirmpoints = SpatialPointsDataFrame(coords = firmswithcoords[,c("lon","lat")],data = firmswithcoords,proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
firmpoints <- st_transform(st_as_sf(rawfirmpoints),st_crs(fuas))

# overlay with fuas
firmfuas = st_join(firmpoints,fuas,join=st_within)
firms_withfuas <- firmfuas %>% filter(!is.na(FUA_CODE))

# overlay with Europe countries
firmcountries = st_join(firmpoints,countries,join=st_within)
# proportion of firms within Europe
100*length(which(is.na(firmcountries$CNTR_ID)))/nrow(firmcountries)
# remove Switzerland and Norway and other non EU countries
#100*length(which(is.na(firmcountries$CNTR_ID)|as.character(firmcountries$CNTR_ID)%in%c("NO")))/nrow(firmcountries)
100*length(which(as.character(firmcountries$CNTR_ID)%in%c(
  "UK","DE","FR","NL","IT","LU","ES","AT","BE","FI",
  "PL","DK","PT","HU","RO","CZ","LV","LT","SK","EL",
  "IE","BG","SE","EE","SI","HR")))/nrow(firmcountries)
# 97.71 %
firmseu = firmswithcoords[as.character(firmcountries$CNTR_ID)%in%c("UK","DE","FR","NL","IT","LU","ES","AT","BE","FI","PL","DK","PT","HU","RO","CZ","LV","LT","SK","EL","IE","BG","SE","EE","SI","HR"),]
firmseuid = unique(as.character(firmseu$id))
100*length(which(firmlinks$from%in%firmseuid&firmlinks$to%in%firmseuid))/nrow(firmlinks)
# only 40.69% of within EU links

# join fuaids to firmlinks -> ! ~ 1000 companies are repeated - should already filter by year?
linkfuas = left_join(firmlinks,firms_withfuas[,c('id','FUA_CODE','turnover','latestinfo')],by=c('from'='id'))
names(linkfuas)[6:8]<-c("from_fua","from_turnover","from_year")
length(which(!is.na(linkfuas$from_fua)))/nrow(linkfuas) # 32.99 % = 616031 between fuas
# join for to_fuas
linkfuas = left_join(linkfuas,firms_withfuas[,c('id','FUA_CODE','turnover','latestinfo')],by=c('to'='id'))
names(linkfuas)[10:12] <- c('to_fua',"to_turnover","to_year")

# year of link observation
linkfuas$year = substring(linkfuas$date,4,7)

# proportion
linkfuas$proportion = ifelse(!is.na(linkfuas$total_ownership),linkfuas$total_ownership,linkfuas$direct_ownership)

linkfuas$from_fua=as.character(linkfuas$from_fua)
linkfuas$to_fua=as.character(linkfuas$to_fua)

# filter links between fuas # -> remains 555196
links = linkfuas[!is.na(linkfuas$from_fua)&!is.na(linkfuas$to_fua),]
# with turnover at destination and proportion of ownership # remains 161303
links = links[!is.na(links$to_turnover)&!is.na(links$proportion),]


# links can be aggregated by o-d and year
aggrlinks <- linkfuas %>% group_by(from_fua,to_fua,year,.drop=T) %>% summarize(weight = sum(proportion*to_turnover))
#summary(aggrlinks[aggrlinks$year=="2018",])
aggrlinks = aggrlinks[!is.na(aggrlinks$from_fua)&!is.na(aggrlinks$to_fua)&!is.na(aggrlinks$year)&!is.na(aggrlinks$weight),]
aggrlinks$dummy=rep("link",nrow(aggrlinks))

# year repartition is shitty
table(links$year)
length(which(duplicated(links[,c("from","to")]))) # too few replicated links to have double observations
# -> dynamical info can not really be used ?
# -> fit the model from empty network ('stationary state') ?

##
# try basic flow map
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
  breaks = c(min(currentlinks$weight),  quantile(currentlinks$weight,c(0.25)), median(currentlinks$weight), quantile(currentlinks$weight,c(0.995)), quantile(currentlinks$weight,c(0.9995))),
  #lwd = c(1,2,5,10)/2#,
  var2 = "dummy"
)
#layoutLayer(title = paste0("Ownership links"),
#            frame = FALSE, col = "grey25", coltitle = "white",
#            tabtitle = TRUE)
dev.off()

# network analysis
#  -> modularities of countries, of NUTS
#  -> Louvain communities


# aggregate firm stats at FUAs and by year (cumulated year)
firmfuas$year <- firmfuas$latestinfo
# check if duplicates
summary(firmpoints %>% group_by(id) %>% summarise(count = n()))

years = unique(c(linkfuas$year,firmfuas$year))
  
aggrnodes = data.frame()
for(year in years){
  currentaggrnodes <- firmfuas[firmfuas$year<=year] %>% group_by(FUA_CODE,id) %>% mutate(
    # select latest observation only
    latest_turnover = turnover[year==max(year)]
  ) %>% group_by(FUA_CODE,sector) %>% summarize(
    turnover = sum(latest_turnover)
  )
  aggrnodes = rbind(aggrnodes,currentaggrnodes)
}




