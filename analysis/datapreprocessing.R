setwd(paste0(Sys.getenv("CS_HOME"),'/UrbanDynamics/Models/ABMCitiesFirms'))

#library(rgdal)
library(sf)

# short version of the dataset does not have gdps
#ucdb <- readOGR(paste0(Sys.getenv('CS_HOME'),'/Data/JRC_EC/GHS/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_0'),'GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_0_short_pnt',stringsAsFactors = F)
#ucdb <- readOGR(paste0(Sys.getenv('CS_HOME'),'/Data/JRC_EC/GHS/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_0'),'GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_0',stringsAsFactors = F)
ucdb <- st_read(paste0(Sys.getenv('CS_HOME'),'/Data/JRC_EC/GHS/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_0'),'GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_0')

envelope <- st_read('model_nl6/setup/gis','envelope_wgs84')
countries <- st_read('model_nl6/setup/gis','europe_wgs84')

cities = ucdb[countries,op=st_within]
cities = cities[envelope,op=st_within]
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



