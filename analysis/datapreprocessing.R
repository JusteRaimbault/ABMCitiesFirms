setwd(paste0(Sys.getenv("CS_HOME"),'/UrbanDynamics/Models/ABMCitiesFirms'))

#library(rgdal)
library(sf)
library(dplyr)

# short version of the dataset does not have gdps
#ucdb <- readOGR(paste0(Sys.getenv('CS_HOME'),'/Data/JRC_EC/GHS/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_0'),'GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_0_short_pnt',stringsAsFactors = F)
#ucdb <- readOGR(paste0(Sys.getenv('CS_HOME'),'/Data/JRC_EC/GHS/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_0'),'GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_0',stringsAsFactors = F)
ucdb <- st_read(paste0(Sys.getenv('CS_HOME'),'/Data/JRC_EC/GHS/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_0'),'GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_0')

#envelope <- st_read('model_nl6/setup/gis','envelope_wgs84')
#countries <- st_read('model_nl6/setup/gis','europe_wgs84')
fuas <- st_read('Data/UI-boundaries-FUA/','FUA_Boundaries')
fuas <- st_transform(fuas,st_crs(ucdb))

# network
nw2010 <- as.tbl(read.csv('Data/firms/ORBIS_2010_links_agreg.csv',stringsAsFactors = F))
nw2013 <- as.tbl(read.csv('Data/firms/ORBIS_2013_links_agreg.csv',stringsAsFactors = F))
nw2016 <- as.tbl(read.csv('Data/firms/ORBIS_2016_links_agreg.csv',stringsAsFactors = F))

links2010 <- as.tbl(read.csv('Data/firms/GB_links_2010_compiled2.csv',stringsAsFactors = F))
links2013 <- as.tbl(read.csv('Data/firms/GB_links_2013_compiled2.csv',stringsAsFactors = F))
links2016 <- as.tbl(read.csv('Data/firms/GB_links_2016_compiled2.csv',stringsAsFactors = F))

nwnodescodes = unique(c(nw2010$s_LUR,nw2010$o_LUR,nw2013$s_LUR,nw2013$o_LUR,nw2016$s_LUR,nw2016$o_LUR))

# code sectors
length(unique(c(links2010$s_NACE,links2010$o_NACE,links2013$s_NACE,links2013$o_NACE,links2016$s_NACE,links2016$o_NACE)))
# 4 digits: ~600 sectors
length(unique(floor(links2010$s_NACE/100))) #-> 2 digits: ~100 sectors

iata <- as.tbl(read.csv('Data/firms/IATA_codes.csv',stringsAsFactors = F))
nwnodes = iata[iata$IATA%in%nwnodescodes,c("IATA","CITY","COUNTRY","LAT","LONG")]
nwnodessp = SpatialPointsDataFrame(coords = nwnodes[,c("LONG","LAT")] ,data = nwnodes,proj4string = CRS(st_crs(countries)$proj4string))
writeOGR(nwnodessp,'Data/firms','nwnodes.shp',driver = 'ESRI Shapefile')

# fucking mess, coordinates are for airports, generally outside GHS polygons (these are built-up areas !)
#  -> either get polygons for "LUR", or aggregate everything at FUA level
# (will loose many points though ?) but these have no GDP anyway

#cities = ucdb[countries,op=st_within]
#cities = cities[envelope,op=st_within]
cities=ucdb[fuas,op=st_within]

# summarize gdp by fua
citiesaggr = st_join(cities,fuas,op=st_within) %>% group_by(FUA_CODE) %>% summarize(gdp = sum(GDP15_SM))

# check consistence of aggreg
#sum(cities$GDP15_SM)==sum(citiesaggr$gdp) # OK

# same with o/d of links
# ! pb when aggreg airports -> need to recompute the links
#dim(st_as_sf(nwnodessp)[fuas,op=st_within])
# -> no two nodes in the same FUA -> can directly join
nodesaggr = st_join(st_as_sf(nwnodessp),fuas,op=st_within) %>% group_by(FUA_CODE) %>% summarize(id = IATA[1])



# select cities only within FUAs 
#cities = ucdb[countries,op=st_within]


years= c('00','15')
# TODO note: issue with correspondance between dates across datasets?

citiesdf = data.frame()
for(i in 1:nrow(cities)){
  for(year in years){
    citiesdf=rbind(citiesdf,c(i,
                              as.numeric(paste0('20',year)),
                              cities$GCPNT_LON[i],
                              cities$GCPNT_LAT[i],
                              cities$CTR_MN_ISO[i],
                              cities[[paste0('GDP',year,'_SM')]][i],
                              rep(1/10,10)
                              ))
  }
}

names(citiesdf)<-c("id","year","xcor","ycor","country","gdp",paste0("sector",1:10))

write.table(citiesdf,file = 'model_nl6/setup/cities.csv',sep = ";",row.names = F,col.names = T,quote = F)



