setwd(paste0(Sys.getenv("CS_HOME"),'/UrbanDynamics/Models/ABMCitiesFirms'))

#library(rgdal)
library(sf)
library(dplyr)
library(reshape2)

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
years=c('2010','2013','2016')
alllinks = list('2010'=links2010,'2013'=links2013,'2016'=links2016)
allnws = list('2010'=nw2010,'2013'=nw2013,'2016'=nw2016)

nwnodescodes = unique(c(nw2010$s_LUR,nw2010$o_LUR,nw2013$s_LUR,nw2013$o_LUR,nw2016$s_LUR,nw2016$o_LUR))

# code sectors
#length(unique(c(links2010$s_NACE,links2010$o_NACE,links2013$s_NACE,links2013$o_NACE,links2016$s_NACE,links2016$o_NACE)))
# 4 digits: ~600 sectors
#length(unique(floor(links2010$s_NACE/100))) #-> 2 digits: ~100 sectors
compos = data.frame()
for(year in years){
  currentlinks=alllinks[[year]]
  #currentlinks$o_code = ifelse(currentlinks$o_NACE>=1000,floor(currentlinks$o_NACE/100),currentlinks$o_NACE);currentlinks$s_code = ifelse(currentlinks$s_NACE>=1000,floor(currentlinks$s_NACE/100),currentlinks$s_NACE)
  currentlinks$o_code = floor(currentlinks$o_NACE/100);currentlinks$s_code = floor(currentlinks$s_NACE/100)
  companies = data.frame(lur=c(currentlinks$o_LUR,currentlinks$s_LUR),
                         companyid = c(currentlinks$o_estab_id,currentlinks$s_estab_id),
                         turnover = c(currentlinks$o_turnover,currentlinks$s_turnover),
                         code = c(currentlinks$o_code,currentlinks$s_code)
                         )
  companies = companies[!duplicated(companies),]
  sector_compo = as.tbl(companies) %>% group_by(lur,code) %>% summarize(turnover=sum(as.numeric(turnover)))
  # no need to normalize here, can be done within netlogo
  sector_compo=cbind(sector_compo,year = rep(year,nrow(sector_compo)))
  if(nrow(compos)==0){compos=sector_compo}else{compos=rbind(compos,sector_compo)}
}


####

iata <- as.tbl(read.csv('Data/firms/IATA_codes.csv',stringsAsFactors = F))
nwnodes = iata[iata$IATA%in%nwnodescodes,c("IATA","CITY","COUNTRY","LAT","LONG")]
nwnodessp = SpatialPointsDataFrame(coords = nwnodes[,c("LONG","LAT")] ,data = nwnodes,proj4string = CRS(st_crs(countries)$proj4string))
writeOGR(nwnodessp,'Data/firms','nwnodes.shp',driver = 'ESRI Shapefile')

# --------
# EDIT : move LGW into London FUA for UK links consistency
nwnodessp <- readOGR('Data/firms','nwnodes.shp')
# --------


# fucking mess, coordinates are for airports, generally outside GHS polygons (these are built-up areas !)
#  -> either get polygons for "LUR", or aggregate everything at FUA level
# (will loose many points though ?) but these have no GDP anyway

#cities = ucdb[countries,op=st_within]
#cities = cities[envelope,op=st_within]
cities=ucdb[fuas,op=st_within]


####
# summarize gdp by fua
# TODO -- the spatial join must be crap - cf London GDP

#citiesaggr = st_join(cities,fuas,op=st_within) %>% group_by(FUA_CODE) %>% summarize(
citiesaggr = st_join(cities,fuas,op=st_intersects) %>% group_by(FUA_CODE) %>% summarize(
  #gdp15 = sum(GDP15_SM),
  #gdp00 = sum(GDP00_SM)
  # extrapolate 2010, 2013, 2016
  gdp2010 = sum(GDP00_SM) + (sum(GDP15_SM) - sum(GDP00_SM))*10/15,
  gdp2013 = sum(GDP00_SM) + (sum(GDP15_SM) - sum(GDP00_SM))*13/15,
  gdp2016 = sum(GDP00_SM) + (sum(GDP15_SM) - sum(GDP00_SM))*16/15
)

# check consistence of aggreg
#sum(cities$GDP15_SM)==sum(citiesaggr$gdp) # OK

# same with o/d of links
# ! pb when aggreg airports -> need to recompute the links
#dim(st_as_sf(nwnodessp)[fuas,op=st_within])
# -> no two nodes in the same FUA -> can directly join
nodesaggr = st_join(st_as_sf(nwnodessp),fuas,op=st_within) %>% filter(!is.na(FUA_CODE)) %>% 
  group_by(FUA_CODE) %>% summarize(id = IATA[1],length=length(IATA))
# also keep a table FUA -> IATA 
alliatas = st_join(st_as_sf(nwnodessp),fuas,op=st_within) %>% filter(!is.na(FUA_CODE))

consolidated = left_join(as.data.frame(nodesaggr[,c("FUA_CODE","id")]),as.data.frame(fuas[,c("FUA_CODE","FUA_NAME","COUNTRY_CO")]),by=c('FUA_CODE'='FUA_CODE'))
consolidated = left_join(consolidated,as.data.frame(citiesaggr[,c("gdp2010","gdp2013","gdp2016","FUA_CODE")]),by=c('FUA_CODE'='FUA_CODE'))

fuascoords = cbind(FUA_CODE=fuas$FUA_CODE,as.data.frame(st_coordinates(st_centroid(fuas))))

consolidated = left_join(consolidated,fuascoords,by=c('FUA_CODE'='FUA_CODE'))

# remove NAs gdp
consolidated = consolidated[!is.na(consolidated$gdp2010)&!is.na(consolidated$gdp2013)&!is.na(consolidated$gdp2016),]

# note: issue with correspondance between dates across datasets?
#  -> dirty linear extrapolation

#gives the sector order
allsectors = unique(compos$code)

citiesdf = data.frame(stringsAsFactors = F)
for(i in 1:nrow(consolidated)){
  for(year in years){
    currentcompos = compos[compos$lur==consolidated[i,"id"]&compos$year==year,]
    volumes = currentcompos$turnover;volumes=c(volumes,rep(0,length(allsectors)-length(volumes)))
    names(volumes)<-c(currentcompos$code,setdiff(allsectors,currentcompos$code))
    currentsectors = volumes[as.character(allsectors)];
    names(currentsectors)<-paste0("sector",allsectors)
    currentdata = c(id = (i-1),
                    name = as.character(consolidated$FUA_NAME)[i],
                    year = year,
                    xcor = consolidated$X[i],
                    ycor = consolidated$Y[i],
                    country = consolidated$COUNTRY_CO[i],
                    gdp = consolidated[[paste0('gdp',year)]][i],
                    currentsectors
    )
    if(nrow(citiesdf)==0){citiesdf=data.frame(matrix(currentdata,nrow=1),stringsAsFactors = F)}
    else{citiesdf=rbind(citiesdf,currentdata)}
  }
}
names(citiesdf)<-c("id","name","year","xcor","ycor","country","gdp",paste0("sector",allsectors))

write.table(citiesdf,file = 'model_nl6/setup/cities.csv',sep = ";",row.names = F,col.names = T,quote = F)

###
# construct network file
fuaids=((1:nrow(consolidated)) - 1);names(fuaids)<-consolidated$FUA_CODE
lur = as.character(alliatas$FUA_CODE);names(lur) <- alliatas$IATA

# id_origin, id_destination, year, weight
linksdf = data.frame()
for(year in years){
  show(year)
  currentnw = allnws[[year]]
  
  # for(i in 1:nrow(currentnw)){
  #   if(i%%1000==0){show(i)}
  #   currentdata = c(
  #     id_origin = fuaids[lur[currentnw$s_LUR[i]]],
  #     id_destination = fuaids[lur[currentnw$o_LUR[i]]],
  #     year = year,
  #     weight = currentnw$Sum_turnover
  #   )
  #   if(nrow(linksdf)==0){linksdf = data.frame(matrix(currentdata,nrow=1),stringsAsFactors =F)}
  #   else{linksdf = rbind(linksdf,currentdata)}
  # }
  currentdata = data.frame(id_origin = fuaids[lur[currentnw$s_LUR]],
                           id_destination = fuaids[lur[currentnw$o_LUR]],
                           year = rep(year,nrow(currentnw)),
                           weight = currentnw$Sum_turnover
                           )
  
  if(nrow(linksdf)==0){linksdf = currentdata}
  else{linksdf = rbind(linksdf,currentdata)}
}
names(linksdf)<-c("id_origin","id_destination","year","weight")

# filter NAs # 2 links only ? - issue ! : factor in lur passage table !
linksdf = linksdf[!is.na(linksdf$id_origin)&!is.na(linksdf$id_destination),]
linksdf = linksdf[linksdf$id_origin!=linksdf$id_destination,]

# filter links with weight = 0
linksdf = linksdf[linksdf$weight>0,]

write.table(linksdf,file = 'model_nl6/setup/links.csv',sep = ";",row.names = F,col.names = T,quote = F)






