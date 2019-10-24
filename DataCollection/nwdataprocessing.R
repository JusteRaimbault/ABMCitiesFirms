
setwd(paste0(Sys.getenv("CS_HOME"),'/UrbanDynamics/Models/ABMCitiesFirms'))

library(igraph)
#library(geonames)
library(stringi)

firmlinks <- read.csv('Data/firms/links_Amadeus_Europe.csv',quote = "\"",stringsAsFactors = F)

# dim(firmlinks)
# 3091037      23

############
# Geocoding
#options(geonamesUsername = as.character(read.csv('.geonamesUsername',header = F)[1,1]))
#GNpostalCodeLookup(postalcode="SE1 7NA",country="GB")

# unique cities to be searched for (use postal codes for destination)
#length(unique(paste0(firmlinks$CountryISOcode,"-",firmlinks$Zipcode)))
# -> 332776 - fucking too much for geonames API
#length(unique(setdiff(paste0(firmlinks$GUOCountryISOcode,"-",firmlinks$GUOCity),paste0(firmlinks$CountryISOcode,"-",firmlinks$City))))
# -> only 31045

# countries - should be EU 28
#unique(c(firmlinks$CountryISOcode,firmlinks$GUOCountryISOcode))

# -> painful: load raw geonames database (webservice api limited at 20000 per day)

# filter
firmlinks = firmlinks[!is.na(firmlinks$CountryISOcode)&!is.na(firmlinks$GUOCountryISOcode)&nchar(firmlinks$CountryISOcode)==2&nchar(firmlinks$GUOCountryISOcode)==2,]
# nrow(firmlinks) = 3056273

countries = unique(c(firmlinks$CountryISOcode,firmlinks$GUOCountryISOcode))

#table(c(firmlinks$CountryISOcode,firmlinks$GUOCountryISOcode))

# changes delimiter and add quotes
systemQuoteCSV<-function(filename){
  # fucking quote in quotes nightmare
  #command = paste0('cat ',filename,' | awk -F"\t" \'{print "\""$1"\";\""$2"\";\""$3"\";\""$4"\";\""$5"\";\""$6"\";\""$7"\";\""$8"\";\""$9"\";\""$10"\";\""$11"\";\""$12"\""}\' > ',filename,'.tmp')
  command = paste0('./quoteCSV.sh ',filename)
  system(command,intern = F)
  file.remove(filename);file.rename(paste0(filename,'.tmp'),filename)
}

dir.create('Data/geonames')
for(country in countries){
  show(country)
  try({
  download.file(paste0('https://download.geonames.org/export/zip/',country,'.zip'),destfile = paste0('Data/geonames/zip',country,'.zip'))
  unzip(paste0('Data/geonames/zip',country,'.zip'),exdir = 'Data/geonames')
  file.rename(paste0('Data/geonames/',country,'.txt'),paste0('Data/geonames/zip',country,'.txt'))
  file.remove(paste0('Data/geonames/zip',country,'.zip'))
  systemQuoteCSV(paste0('Data/geonames/zip',country,'.txt'))
  })
  try({
  download.file(paste0('https://download.geonames.org/export/dump/',country,'.zip'),destfile = paste0('Data/geonames/',country,'.zip'))
  unzip(paste0('Data/geonames/',country,'.zip'),exdir = 'Data/geonames')
  file.remove(paste0('Data/geonames/',country,'.zip'))
  systemQuoteCSV(paste0('Data/geonames/',country,'.txt'))
  })
}
# FR.txt => remove quote line 136500

# cat Data/geonames/zipFR.txt | tr "\t" ";" > Data/geonames/zipFR_.txt
# cat Data/geonames/zipFR_.txt | awk -F";" '{if(NF!=12) print $0}'
# -> ???
# R fails reading the csv; quote it manually - super dirty
# cat Data/geonames/zipFR.txt | awk -F"\t" '{print "\""$1"\";\""$2"\";\""$3"\";\""$4"\";\""$5"\";\""$6"\";\""$7"\";\""$8"\";\""$9"\";\""$10"\";\""$11"\";\""$12"\""}' > Data/geonames/zipFR_.txt

##
# build lookup tables

# zip lookup
zipcodes = list()
for(country in countries){
  show(country)
  if(file.exists(paste0('Data/geonames/zip',country,'.txt'))){
    currentzips = read.table(file=paste0('Data/geonames/zip',country,'.txt'),sep = ';',quote="\"",stringsAsFactors = F,fileEncoding = 'UTF-8',header=F)
    show(nrow(currentzips))
    #replace missing strings with NAs
    for(j in 4:9){if(is.character(currentzips[,j])){chars<-nchar(currentzips[,j]);chars[is.na(chars)]=0;currentzips[chars==0,j]<-NA}};#dirty
    # zips as character to index list
    currentzips[,2]<-as.character(currentzips[,2])
    # rownames should allow performant lookup?
    # must keep line with deeper level only
    currentlookup=list()
    for(currentzip in unique(currentzips$V2)){
      currentdata=currentzips[currentzips$V2==currentzip,]
      indslevel3 = !is.na(currentdata[,8]);indslevel2 = !is.na(currentdata[,6]);indslevel1 = !is.na(currentdata[,4])
      if(length(which(indslevel3))>0){
        currentrow = currentdata[which(indslevel3)[1],]
      }else{if(length(which(indslevel2))>0){currentrow=currentdata[which(indslevel2)[1],]}else{currentdata[which(indslevel1)[1],]}}
      currentlookup[[currentzip]]=currentrow
    }
    zipcodes[[country]]=currentlookup
  }
}


# save to gain some time
save(zipcodes,file='Data/geonames/zipcodesLookup.RData')

# note: zip is always more precise than the city? -> should be fine for coordinates
# but different granularities accross cities

# test coverage on dataset
firmzips = unique(paste0(firmlinks$CountryISOcode,firmlinks$Zipcode))
ziphead <- function(s){strsplit(substring(s,3),' ')[[1]][1]}
getCountry <- function(s){substring(s,1,2)}
lookup<-function(s){
  country = getCountry(s)
  if(country=='SE'){return(zipcodes[[country]][[substring(s,3)]])}
  if(country=='ES'||country=='DE'||country=='IT'||country=='FI'||country=='LT'||country=='RO'){return(zipcodes[[country]][[trimws(ziphead(s),'l','0')]])}
  if(country=='CZ'||country=='SK'){return(zipcodes[[country]][[paste0(substring(s,3,5),' ',substring(s,6,7))]])}
  if(country=='LU'){return(zipcodes[[country]][[paste0("L-",substring(s,3))]])}
  if(country=='LV'){return(zipcodes[[country]][[paste0("LV-",substring(s,3))]])}
  if(country=='MT'){return(zipcodes[[country]][[substring(s,3,5)]])}
  return(zipcodes[[country]][[ziphead(s)]])
  #return(zipcodes[[country]][[trimws(ziphead(s),'l','0')]]) # actually worse, some countries include the heading 0
}
zipcoverage = sapply(firmzips,lookup)
length(which(sapply(zipcoverage,length)==0&sapply(firmzips,getCountry)%in%names(zipcodes)))/length(firmzips)
# -> with this, only 1.64% of zipcodes missing

#sapply(firmzips[which(sapply(zipcoverage,length)==0)],ziphead)
#sapply(firmzips[which(sapply(zipcoverage,length)==0)],getCountry)

#sapply(firmzips[sapply(zipcoverage,length)==0&sapply(firmzips,getCountry)%in%names(zipcodes)&
#                  !sapply(firmzips,getCountry)%in%c("SE","ES","DE","IT","FI","LT","RO","CZ","SK","LU","LV","MT",
#                                                    "GB","BG","PL","FR","HU","EE","PT",'DK','AT','IE','BE','NL',
#                                                    'HR') # "verified" missing
#                ],getCountry)

# summary(sapply(zipcodes[['CZ']],function(s){length(strsplit(s[2][1,1]," ")[[1]])})) -> CZ must be split

guocountrycity = paste0(firmlinks$GUOCountryISOcode,"-",firmlinks$GUOCity)
countrycity = paste0(firmlinks$CountryISOcode,"-",firmlinks$City)

coveredzips=names(zipcoverage)[which(sapply(zipcoverage,length)==0)]
missingzips = paste0(firmlinks$CountryISOcode,firmlinks$Zipcode)%in%coveredzips

missingcities = unique(c(countrycity[missingzips],setdiff(guocountrycity,countrycity[!missingzips])))

placecountries = unique(sapply(missingcities,function(s){substring(s,1,2)}))

# places lookup
places=list()
for(country in placecountries){
  show(country)
  if(file.exists(paste0('Data/geonames/',country,'.txt'))){
    currentplaces = read.table(file=paste0('Data/geonames/',country,'.txt'),sep = ';',quote="\"",stringsAsFactors = F,fileEncoding = 'UTF-8',header=F)
    show(nrow(currentplaces))
    # currentplaces[currentplaces$V2=='Lyon',] # currentplaces[currentplaces$V2=='Montreuil',]
    # => lookup must be hierarchial - keep populated places only, should coincide with adminintrative
    currentplaces = currentplaces[currentplaces$V8%in%c("PPLC","PPL","PPLA","PPLA2","PPLA3","PPLA4","PPLA5"),]
    
    currentlookup = list()
    uniqueplaces = unique(currentplaces$V2)
    for(currentplacei in 1:length(uniqueplaces)){
      if(currentplacei%%5000==0){show(currentplacei)}
      currentplace = uniqueplaces[currentplacei]
      # unique place name? NO
      # rq : could build a hierarchical structure to access a given level only/explore spatial hierarchies etc. ?
      # or for now keep cities only in places lookup
      currentlookup[[stringi::stri_trans_toupper(currentplace)]] = currentplaces[currentplaces$V2==currentplace,]
    }
    places[[country]]==currentlookup
  }
}

# Correspondance cities/zipcodes? not totally relevant as can be more precise -> use zipcodes when relevant


########
# geocoding
  
  
#'
#' get origin and destination coordinates ?
#'  ! should do by BVDid to avoid redundancy
getCoordinatesOD <- function(r){
  
}
  
allcoords = apply(firmlinks,1,getCoordinatesOD)


#######
# construct simplified network representation -> node table and link table




