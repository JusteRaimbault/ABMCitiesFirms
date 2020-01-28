
setwd(paste0(Sys.getenv("CS_HOME"),'/UrbanDynamics/Models/ABMCitiesFirms'))

library(sp)
library(sf)
library(dplyr)
library(cartography)

fuas <- st_read('Data/UI-boundaries-FUA/','FUA_Boundaries')

countries <- st_read('Data/','countries')
#countries <- st_transform(countries,st_crs(fuas)) #makes no sense at this scale

firms <- read.csv('Data/firms/amadeus_nodes.csv',sep=";",quote = "")
firmlinks <- read.csv('Data/firms/amadeus_links.csv',sep=";",quote = "")
firmswithcoords = firms[!is.na(firms$lon)&!is.na(firms$lat),]

rawfirmpoints = SpatialPointsDataFrame(coords = firmswithcoords[,c("lon","lat")],data = firmswithcoords,proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
firmpoints <- st_transform(st_as_sf(rawfirmpoints),st_crs(fuas))

# overlay with fuas
firmfuas = st_join(firmpoints,fuas,join=st_within)
firms_withfuas <- firmfuas %>% filter(!is.na(FUA_CODE))

# overlay with Europe countries
rawfirmpointswgs84 = st_as_sf(rawfirmpoints)
countrieswgs84 = st_transform(countries,st_crs(rawfirmpointswgs84))
firmcountries = st_join(rawfirmpointswgs84,countrieswgs84,join=st_within)
# proportion of firms within Europe
100*length(which(is.na(firmcountries$CNTR_ID)))/nrow(firmcountries)
length(which(!is.na(firmcountries$CNTR_ID)))
# remove Switzerland and Norway and other non EU countries
#100*length(which(is.na(firmcountries$CNTR_ID)|as.character(firmcountries$CNTR_ID)%in%c("NO")))/nrow(firmcountries)
eucountries = c(
  "UK","DE","FR","NL","IT","LU","ES","AT","BE","FI",
  "PL","DK","PT","HU","RO","CZ","LV","LT","SK","EL",
  "IE","BG","SE","EE","SI","HR")
100*length(which(as.character(firmcountries$CNTR_ID)%in%eucountries))/nrow(firmcountries)
# 97.71 %
firmseu = firmswithcoords[as.character(firmcountries$CNTR_ID)%in%c("UK","DE","FR","NL","IT","LU","ES","AT","BE","FI","PL","DK","PT","HU","RO","CZ","LV","LT","SK","EL","IE","BG","SE","EE","SI","HR"),]
firmseuid = unique(as.character(firmseu$id))
100*length(which(firmlinks$from%in%firmseuid&firmlinks$to%in%firmseuid))/nrow(firmlinks)
# only 40.69% of within EU links

# firms in fua with are connected to a link
linkids = unique(c(as.character(firmlinks$from),as.character(firmlinks$to)))
length(which(firms_withfuas$id%in%linkids))

# firms in fua with turnover and a link
length(which(firms_withfuas$id%in%linkids&!is.na(firms_withfuas$turnover)))

# join fuaids to firmlinks -> ! ~ 1000 companies are repeated - should already filter by year?
# FIXME should be a semi_join ? - or filter firms table before - which info to keep? the closest in time to the link ?
#  -> nested join and summarize
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

####
# save for further analysis
save(firmpoints,firmfuas,firms_withfuas,linkfuas,file='Data/firms/amadeus_aggregFUA.RData')


# where do links go ?
linkcountries = left_join(firmlinks,firmcountries[!duplicated(firmcountries$id),c('id','CNTR_ID')],by=c('from'='id'));names(linkcountries)[6]="from_country"
linkcountries = left_join(linkcountries,firmcountries[!duplicated(firmcountries$id),c('id','CNTR_ID')],by=c('to'='id'));names(linkcountries)[8]="to_country"
# - country ownership matrix (cumulated in time)
# - visualize inter-country flows: all world and EU

# - TODO

length(which(!is.na(linkcountries$from_country)))
length(which(!is.na(linkcountries$to_country)))
length(which(!is.na(linkcountries$from_country)&!is.na(linkcountries$to_country)))

####
# filter links between fuas # -> remains 555196
length(which(!is.na(linkfuas$from_fua)))
length(which(!is.na(linkfuas$to_fua)))
links = linkfuas[!is.na(linkfuas$from_fua)&!is.na(linkfuas$to_fua),]
# with turnover at destination and proportion of ownership # remains 161303
links = links[!is.na(links$to_turnover)&!is.na(links$proportion),]


# links can be aggregated by o-d and year
aggrlinksyear <- linkfuas %>% group_by(from_fua,to_fua,year,.drop=T) %>% summarize(weight = sum(proportion*to_turnover))
#summary(aggrlinks[aggrlinks$year=="2018",])
aggrlinksyear = aggrlinksyear[!is.na(aggrlinksyear$from_fua)&!is.na(aggrlinksyear$to_fua)&!is.na(aggrlinksyear$year)&!is.na(aggrlinksyear$weight),]
aggrlinksyear$dummy=rep("link",nrow(aggrlinksyear))


aggrlinks <- linkfuas %>% group_by(from_fua,to_fua) %>% summarize(weight = sum(proportion*to_turnover))
aggrlinks = aggrlinks[!is.na(aggrlinks$from_fua)&!is.na(aggrlinks$to_fua)&!is.na(aggrlinks$weight),]



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
  breaks = c(min(currentlinks$weight),quantile(currentlinks$weight,c(0.25)), median(currentlinks$weight), quantile(currentlinks$weight,c(0.995)), quantile(currentlinks$weight,c(0.9995))),
  #lwd = c(1,2,5,10)/2#,
  var2 = "dummy"
)
#layoutLayer(title = paste0("Ownership links"),
#            frame = FALSE, col = "grey25", coltitle = "white",
#            tabtitle = TRUE)
dev.off()


######
# network analysis
#  -> modularities of countries, of NUTS
#  -> Louvain communities
#timeaggrlinks = aggrlinks

aggrnodes = firms_withfuas %>% filter(!is.na(turnover)) %>% group_by(FUA_CODE) %>% summarize(turnover=sum(turnover))
aggrnodes$geometry=NULL
coords = as.tbl(data.frame(fuaid = fuas$FUA_CODE,fuacountry = fuas$COUNTRY_CO,st_coordinates(st_centroid(fuas))))
aggrnodes = left_join(aggrnodes,coords,by=c('FUA_CODE'='fuaid'))

library(igraph)
library(Matrix)
library(reshape2)

g = graph_from_data_frame(aggrlinks,directed = T,vertices = aggrnodes)
V(g)$lon = (V(g)$X - min(V(g)$X)) / (max(V(g)$X) - min(V(g)$X))
V(g)$lat = (V(g)$Y - min(V(g)$Y)) / (max(V(g)$Y) - min(V(g)$Y))
write_graph(g,file='Results/EmpiricalNetwork/fua_graph.gml',format='gml')

A = get.adjacency(g,sparse = T)
g_undir = graph_from_adjacency_matrix(adjmatrix = (A+t(A))/2,weighted = T,mode = "undirected")
communities_clauset = cluster_fast_greedy(g_undir)
communities_louvain = cluster_louvain(g_undir) 
# 0.35 modularity in both cases


###
# tests for statistical analysis
aggrlinks = left_join(aggrlinks,aggrnodes[,c("FUA_CODE","turnover","fuacountry")],by=c('from_fua'='FUA_CODE'));names(aggrlinks)[4:5]<-c("from_turnover","from_country")
aggrlinks = left_join(aggrlinks,aggrnodes[,c("FUA_CODE","turnover","fuacountry")],by=c('to_fua'='FUA_CODE'));names(aggrlinks)[6:7]<-c("to_turnover","to_country")

# ! still link with zero weight ?

# add geo distance
dists = spDists(as.matrix(aggrnodes[,c("X","Y")]))
rownames(dists)<- aggrnodes$FUA_CODE;colnames(dists)<- aggrnodes$FUA_CODE
distsdf = melt(dists)
aggrlinks=left_join(aggrlinks,distsdf,by=c("from_fua"="Var1","to_fua"="Var2"));names(aggrlinks)[8]<-"distance"

d = aggrlinks[aggrlinks$distance>0&aggrlinks$weight>0&aggrlinks$from_turnover>0&aggrlinks$to_turnover>0,]

summary(lm(data=d,log(weight)~log(distance)))
ols <- lm(data=d,log(weight)~log(distance)+log(from_turnover)+log(to_turnover))
summary(ols)

### consider
# - poisson models?
# - fixed effects similar to constrained spatial interaction models! (check practical 2-3 spInt)

# country fixed effect by hand
summary(lm(data=d,log(weight)~log(distance)+log(from_turnover)+log(to_turnover)+from_country+to_country))
# pair fixed effect
summary(lm(data=d,log(weight)~log(distance)+log(from_turnover)+log(to_turnover)+paste0(from_country,to_country)))

library(nlme)
lmList(data=d,(log(weight)~log(distance)+log(from_turnover)+log(to_turnover) | from_country))

# fixed effect
library(plm) # ! for panel data - we have no time here - country as individual, fua as realizations
# see https://rstudio-pubs-static.s3.amazonaws.com/372492_3e05f38dd3f248e89cdedd317d603b9a.html#1_preliminaries
plinks = plm::pdata.frame(d,index = c("from_country"))
fixed <- plm(log(weight)~log(distance)+log(from_turnover)+log(to_turnover), data=plinks, model="within")
summary(fixed)
sfixef(fixed)
pFtest(fixed, ols)

random <- plm(log(weight)~log(distance)+log(from_turnover)+log(to_turnover), data=plinks, model="random")
summary(random)
ranef(random)
phtest(fixed, random)


# -> links by sector ? NACE_origin, NACE_destination - aggregate at the level of broad sectors


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




