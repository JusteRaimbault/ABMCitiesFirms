
setwd(paste0(Sys.getenv("CS_HOME"),'/UrbanDynamics/Models/ABMCitiesFirms'))

library(sp)
library(sf)
library(dplyr)
library(cartography)
library(ggplot2)


##########
# 1) FUA / links overlay
#

#fuas <- st_read('Data/UI-boundaries-FUA/','FUA_Boundaries')
fuas <- st_read(paste0(Sys.getenv('CS_HOME'),'/Data/JRC_EC/GHS/GHS_FUA_UCDB2015_GLOBE_R2019A_54009_1K_V1_0/GHS_FUA_UCDB2015_GLOBE_R2019A_54009_1K_V1_0.gpkg'))
#fuas <- st_transform(fuas,CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")) # NO - transform the points to fuas

countries <- st_read('Data/','countries')
#countries <- st_transform(countries,st_crs(fuas)) #makes no sense at this scale

firms <- read.csv('Data/firms/amadeus_nodes.csv',sep=";",quote = "")
firmlinks <- read.csv('Data/firms/amadeus_links.csv',sep=";",quote = "")
firmswithcoords = firms[!is.na(firms$lon)&!is.na(firms$lat),]

rawfirmpoints = SpatialPointsDataFrame(coords = firmswithcoords[,c("lon","lat")],data = firmswithcoords,proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
firmpoints <- st_transform(st_as_sf(rawfirmpoints),st_crs(fuas))

# overlay with fuas
firmfuas = st_join(firmpoints,fuas,join=st_within)

# Filter 
#firms_withfuas <- firmfuas %>% filter(!is.na(FUA_CODE))
firms_withfuas <- firmfuas %>% filter(!is.na(eFUA_ID))
# 2,036,272 with GHS FUA


# overlay with Europe countries
#rawfirmpointswgs84 = st_as_sf(rawfirmpoints)
#countrieswgs84 = st_transform(countries,st_crs(rawfirmpointswgs84))
#firmcountries = st_join(rawfirmpointswgs84,countrieswgs84,join=st_within)
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

firms_withfuas_eu <- firms_withfuas %>% filter(Cntry_ISO%in%eucountries_iso)
# 2,033,799


# firms in fua with are connected to a link
linkids = unique(c(as.character(firmlinks$from),as.character(firmlinks$to)))
length(which(firms_withfuas_eu$id%in%linkids))

# firms in fua with turnover and a link
length(which(firms_withfuas$id%in%linkids&!is.na(firms_withfuas$turnover)))

# join fuaids to firmlinks -> ! ~ 1000 companies are repeated - should already filter by year?
# FIXME should be a semi_join ? - or filter firms table before - which info to keep? the closest in time to the link ?
#  -> nested join and summarize
linkfuas = left_join(firmlinks,firms_withfuas_eu[,c('id','eFUA_ID','turnover','latestinfo')],by=c('from'='id'))
names(linkfuas)[6:8]<-c("from_fua","from_turnover","from_year")
length(which(!is.na(linkfuas$from_fua)))/nrow(linkfuas) # 32.99 % = 616031 between fuas
# join for to_fuas
linkfuas = left_join(linkfuas,firms_withfuas_eu[,c('id','eFUA_ID','turnover','latestinfo')],by=c('to'='id'))
names(linkfuas)[10:12] <- c('to_fua',"to_turnover","to_year")

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


# where do links go ?
#linkcountries = left_join(firmlinks,firmcountries[!duplicated(firmcountries$id),c('id','CNTR_ID')],by=c('from'='id'));names(linkcountries)[6]="from_country"
#linkcountries = left_join(linkcountries,firmcountries[!duplicated(firmcountries$id),c('id','CNTR_ID')],by=c('to'='id'));names(linkcountries)[8]="to_country"
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
links = links[!is.na(links$to_turnover)&!is.na(links$proportion),]


# links can be aggregated by o-d and year
aggrlinksyear <- linkfuas %>% group_by(from_fua,to_fua,year,.drop=T) %>% summarize(weight = sum(proportion*to_turnover))
#summary(aggrlinks[aggrlinks$year=="2018",])
aggrlinksyear = aggrlinksyear[!is.na(aggrlinksyear$from_fua)&!is.na(aggrlinksyear$to_fua)&!is.na(aggrlinksyear$year)&!is.na(aggrlinksyear$weight),]
aggrlinksyear$dummy=rep("link",nrow(aggrlinksyear))


aggrlinks <- links %>% group_by(from_fua,to_fua) %>% summarize(weight = sum(proportion*to_turnover))



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

# unique(floor(as.numeric(as.character(firms_withfuas_eu$nacecode))/1000))
firms_withfuas_eu$nace_firstdigit = floor(as.numeric(as.character(firms_withfuas_eu$nacecode))/1000)

# ! must have indus compo by aggregating industrial sectors -> first nace digit (10)
aggrnodes = firms_withfuas_eu %>% filter(!is.na(turnover)&!is.na(nace_firstdigit)) %>% group_by(eFUA_ID) %>% summarize(
  turnover=sum(turnover)#,
  #sector0 = sum(turnover[floor(nacecode/1000)==0])/sum(turnover)
)
for(k in 0:9){
  show(k)
  saggr = firms_withfuas_eu %>% filter(!is.na(turnover)&!is.na(nace_firstdigit)) %>%
    group_by(eFUA_ID) %>% summarize(sector = sum(turnover[nace_firstdigit==k])/sum(turnover))
  aggrnodes[[paste0('sector',k)]]=saggr$sector
}
for(k in 0:9){aggrnodes[is.na(aggrnodes[,paste0('sector',k)]),paste0('sector',k)]=0}
aggrnodes$geometry=NULL
# export to wgs84 for gis ext in netlogo
coords = as.tbl(data.frame(fuaid = fuas$eFUA_ID,fuacountry = fuas$Cntry_name,st_coordinates(st_transform(st_centroid(fuas),CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")))))
aggrnodes = left_join(aggrnodes,coords,by=c('eFUA_ID'='fuaid'))


# bug in aggreg? ! all fuas should have sectors summing to 1 as all companies have nace digits!
#aggrnodes[rowSums(aggrnodes[,3:12])==0,]
#faggr = firms_withfuas_eu[firms_withfuas_eu$eFUA_ID==42,]
#firms_withfuas_eu[firms_withfuas_eu$eFUA_ID==42&firms_withfuas_eu$nace_firstdigit==4,]

##
# histograms (clustered) of sector profiles
km = kmeans(aggrnodes[,3:12],centers = 5,nstart = 100)
aggrnodes$cluster = km$cluster
clustsizes = aggrnodes%>% group_by(cluster) %>% summarize(turnover=mean(turnover))
lcentroids = data.frame();for(k in 1:10){lcentroids=rbind(lcentroids,data.frame(prop=km$centers[,k],sector=rep(k,nrow(km$centers)),centre=1:nrow(km$centers),size=clustsizes$turnover))}
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

# netlogo format is
#   id ; name ; time (=> set to t0 in "static network", i.e. start from no network and try to reproduce final network, configuration)
#   x ; y ; country ; GDP ( == aggregated turnover) ;
#   sectors (any number, as proportions)

exportlinks = as.data.frame(aggrlinks)
exportlinks$time = rep(0,nrow(exportlinks))
exportnodes = as.data.frame(aggrnodes)

write.csv(exportlinks[],file='model_nl6/setup/fualinks.csv',row.names = F)
write.csv(as.data.frame(),file='model_nl6/setup/fuacities.csv',row.names = F)




#######

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

poisson <- glm(data=d,log(weight)~log(distance)+log(from_turnover)+log(to_turnover),family = 'poisson')
summary(poisson)

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




