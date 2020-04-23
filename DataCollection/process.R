
setwd(paste0(Sys.getenv('CS_HOME'),'/UrbanDynamics/Models/ABMCitiesFirms/DataCollection'))

#purpose = "Ownership_links"
purpose = "All_firms"


# filter datafiles and gather in one file valid links

filter<-function(f){
  currentdata = read.table(file=paste0('data/',f),sep='\t',quote = "\"",stringsAsFactors = F,fileEncoding = 'UTF-16',header=T)
  if(purpose=="Ownership_links"){
    lines = nchar(currentdata$Zip.code)>2 # at least origin zip code
    #lines = lines&nchar(currentdata$GUO...Name)>0
    lines = lines&nchar(currentdata$GUO...BvD.ID.number)>0 # idem than name: with a GUO
    lines = lines&!grepl('*',currentdata$GUO...BvD.ID.number,fixed=T) # which is not an individual
    lines = lines&nchar(currentdata$GUO...City)>0 # with a city
    lines = lines&nchar(currentdata$GUO...Country.ISO.code)>0 # and a country -> same?
    # keep lines without information dates, can be useful for 'stationary' network
  }
  return(currentdata[lines,])
}

#table(currentdata$Country.ISO.code)
#table(currentdata$City)
#table(currentdata$Zip.code)
#length(which(nchar(currentdata$Zip.code)==0))
#length(which(nchar(currentdata$Zip.code)<3))
# length(which(grepl('*',currentdata$BvD.ID.number,fixed=T)))

files = list.files('data')

alldata = filter(files[1])

for(i in 2:length(files)){
  show(files[i])
  alldata=rbind(alldata,filter(files[i]))
}

# add geoloc? can be done later
# https://ec.europa.eu/eurostat/web/nuts/local-administrative-units


names(alldata) <- gsub('.','',names(alldata),fixed=T)

if(purpose=="Ownership_links"){
  write.csv(alldata,'data/links_Amadeus_Europe.csv',quote=T,row.names = F)
}else {
  write.csv(alldata[,2:12],'data/firms_Amadeus_EU.csv',quote=T,row.names = F)
}




