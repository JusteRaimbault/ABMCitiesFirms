
setwd(paste0(Sys.getenv("CS_HOME"),'/UrbanDynamics/Models/ABMCitiesFirms'))

library(dplyr)

load(file='Data/firms/amadeus_aggregGHSFUA.RData')

firms <- read.csv('Data/firms/amadeus_nodes.csv',sep=";",quote = "")
firmlinks <- read.csv('Data/firms/amadeus_links.csv',sep=";",quote = "")
firmswithcoords = firms[!is.na(firms$lon)&!is.na(firms$lat),]

dim(firms)
# 3,053,540
dim(firmlinks)
# 1,866,936

dim(firmswithcoords)
# 2,715,188

dim(firms_withfuas)
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


nrow(firms_withfuas_eu)
# 2,033,799


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


linkids = unique(c(as.character(firmlinks$from),as.character(firmlinks$to)))
length(which(firms_withfuas_eu$id%in%linkids))

# firms in fua with turnover and a link
length(which(firms_withfuas$id%in%linkids&!is.na(firms_withfuas$turnover)))



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


# filter links

links = linkfuas[!is.na(linkfuas$from_fua)&!is.na(linkfuas$to_fua),]
length(which(!is.na(linkfuas$from_fua)))
length(which(!is.na(linkfuas$to_fua)))

length(which(!is.na(links$to_turnover)&!is.na(links$proportion)))



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



linkall = left_join(firmlinks,firms[,c('id','country')],by=c('from'='id'))
linkall = left_join(linkall,firms[,c('id','country')],by=c('to'='id'))
length(which(linkall$country.x=='GB')) # 142975
length(which(linkall$country.y=='GB')) # 323618
length(which(linkall$country.x=='GB'&linkall$country.y=='GB')) #131819


# Aggregate links
aggrlinks <- links %>% filter(to_turnover>0) %>% group_by(from_fua,to_fua) %>% summarize(weight = sum(proportion*to_turnover))

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




# network construction

# bug in aggreg? ! all fuas should have sectors summing to 1 as all companies have nace digits!
#aggrnodes[rowSums(aggrnodes[,3:12])==0,]
#faggr = firms_withfuas_eu[firms_withfuas_eu$eFUA_ID==42,]
#firms_withfuas_eu[firms_withfuas_eu$eFUA_ID==42&firms_withfuas_eu$nace_firstdigit==4,]

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



#####
## Correlation betweem turnover and UCDB properties

load('Data/firms/amadeus_saggregnw.RData')


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


