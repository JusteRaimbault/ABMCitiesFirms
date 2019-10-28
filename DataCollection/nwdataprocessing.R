
setwd(paste0(Sys.getenv("CS_HOME"),'/UrbanDynamics/Models/ABMCitiesFirms'))

library(igraph)
#library(geonames)
library(stringi)
library(dplyr)

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
# BE.txt => remove quote line 18684 and following
# RU.txt => remove line 354366 (Kalilingrad Nuclear power plant) ; remove ~2000 lines with quotes cat Data/geonames/RU_orig.txt | awk -F"\"" '{if(NF==25) {print $0}}' > Data/geonames/RU.txt 
# (should filter quotes in quoteCSV script)
# AU; IR; JM; KZ; TM; PG

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
# load('Data/geonames/zipcodesLookup.RData')

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

# placecountries=c("CY")

# places lookup
places=list()
# placecountries=setdiff(placecountries,names(places))
for(country in placecountries){
  show(country)
  if(file.exists(paste0('Data/geonames/',country,'.txt'))){
    currentplaces = read.table(file=paste0('Data/geonames/',country,'.txt'),sep = ';',quote="\"",stringsAsFactors = F,fileEncoding = 'UTF-8',header=F)
    # currentplaces[currentplaces$V2=='Lyon',] # currentplaces[currentplaces$V2=='Montreuil',] # currentplaces[currentplaces$V2=='Montreuil',]
    # => lookup must be hierarchial - keep populated places only, should coincide with adminintrative
    currentplaces = currentplaces[currentplaces$V8%in%c("PPLC","PPL","PPLA","PPLA2","PPLA3","PPLA4","PPLA5"),]
    show(nrow(currentplaces))
    
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
    places[[country]]=currentlookup
  }
}
# save lookup table
save(places,file='Data/geonames/placesLookup.RData')
# load('Data/geonames/placesLookup.RData')

#Correspondance cities/zipcodes? not totally relevant as can be more precise -> use zipcodes when relevant

# may have problems with accents. characters OK (zh in pinying e.g.)
# not: 4th field has alternative names and names in different languages -> could be refined
cityLookup <- function(countrycityname){
  country = getCountry(countrycityname);cityname=substring(countrycityname,4)
  if(!country%in%names(places)){return(NULL)}
  currentlookup = places[[country]]
  if(!cityname%in%names(currentlookup)){return(NULL)}
  placematches = currentlookup[[cityname]]
  #if(nrow(placematches)==1){return(placematches)}
  # "PPLC","PPL","PPLA","PPLA2","PPLA3","PPLA4","PPLA5" # Q: PPL < PPLAN ? Yes.
  # fucking ifelse kills the return format - bloody R damn it
  if("PPLC"%in%placematches$V8) #dirty
    {return(placematches[which(placematches$V8=="PPLC")[1],5:6])} 
    else{if("PPLA"%in%placematches$V8){return(placematches[which(placematches$V8=="PPLA")[1],5:6])}
      else{if("PPLA2"%in%placematches$V8){return(placematches[which(placematches$V8=="PPLA2")[1],5:6])}
        else{if("PPLA3"%in%placematches$V8){return(placematches[which(placematches$V8=="PPLA3")[1],5:6])}
           else{if("PPLA4"%in%placematches$V8){return(placematches[which(placematches$V8=="PPLA4")[1],5:6])}
              else{if("PPLA5"%in%placematches$V8){return(placematches[which(placematches$V8=="PPLA5")[1],5:6])}
                else{if("PPL"%in%placematches$V8){# in case of concurrent place names at lower hierarchy level, NA (rq: hope no big city classified as PPL and with homonym)
                  if(length(which(placematches$V8=="PPL"))==1){return(placematches[which(placematches$V8=="PPL")[1],5:6])}else{return(rep(NA,2))}
                  }else{return(rep(NA,2))}
                }
              }
           }
        }
      }
    }
}

citiescoverage = sapply(missingcities,cityLookup)

length(which(is.na(citiescoverage)))/length(citiescoverage)
# missing 20% when remove lower levels

# ! error - some "main names" are not the good ones
length(which(firmlinks$City=="ROMA"))
length(which(firmlinks$City=="CHARLEROI"))

# -> find missing cities with large number of links ?
citiescoverage[is.na(citiescoverage)]
# -> ex Charleroi missing, classified as PPL
# -> should keep all levels, replace alternative names if found?
length(which(unlist(sapply(places[["BE"]],function(d){sapply(d[,4],function(s){length(strsplit(s,',',fixed=T)[[1]])})}))>5))/length(places[["BE"]])
length(which(unlist(sapply(places[["IT"]],function(d){sapply(d[,4],function(s){length(strsplit(s,',',fixed=T)[[1]])})}))>5))/length(places[["IT"]])
length(which(unlist(sapply(places[["US"]],function(d){sapply(d[,4],function(s){length(strsplit(s,',',fixed=T)[[1]])})}))>5))/length(places[["US"]])

# duplicate entries for multi-name places (seen as important)
for(country in names(places)){
  show(country)
  inds = names(places[[country]])[which(sapply(places[[country]],function(d){max(sapply(d[,4],function(s){length(strsplit(s,',',fixed=T)[[1]])}))})>5)]
  show(length(inds))
  for(ind in inds){
    currentrec = places[[country]][[ind]]
    for(i in 1:nrow(currentrec)){
      altnames = stringi::stri_trans_toupper(strsplit(currentrec[i,4],',',fixed=T)[[1]])
      #show(altnames)
      for(altname in altnames){
        #if(altname %in% names(places[[country]])){places[[country]][[altname]] = rbind(places[[country]][[altname]],currentrec[i,])}else{places[[country]][[altname]] =currentrec[i,]}
        # no need to test, rbind works with null
        places[[country]][[altname]] = rbind(places[[country]][[altname]],currentrec[i,])
      }
    }
  }
}

save(places,file='Data/geonames/placesLookup_multinames.RData')

# places[["IT"]][["ROMA"]]
citiescoverage = sapply(missingcities,cityLookup)

# check proportion of NAs

save(citiescoverage,zipcoverage,file='Data/geonames/coverages.RData')
# load('Data/geonames/coverages.RData')

length(which(sapply(citiescoverage,function(r){length(which(is.na(r)))>0})))/length(citiescoverage)*100
# 9% of cities failing ; may be special characters / multi-names missing in geonames
# better remove than using erroneous locations (e.g. when multiple PPL records)

########
# geocoding
  
# -> combine zipcoverage and citycoverage  

#
# get origin and destination coordinates ?
#  ! should do by BVDid to avoid redundancy? ~ ok

bvdids = as.tbl(data.frame(id = unique(as.character(firmlinks$BvDIDnumber),as.character(firmlinks$GUOBvDIDnumber))))
# join to have df of unique ids
firmlinks$countrycity = paste0(firmlinks$CountryISOcode,"-",firmlinks$City)
firmlinks$countryzip = paste0(firmlinks$CountryISOcode,firmlinks$Zipcode) # no dash in zip names
firmlinks$guocountrycity = paste0(firmlinks$GUOCountryISOcode,"-",firmlinks$GUOCity)

# here assume that zip/city record is the same - may not be true if company headquarter changed in time?
length(which(!duplicated(firmlinks$BvDIDnumber)))
length(which(!duplicated(paste0(firmlinks$BvDIDnumber,firmlinks$City))))
# -> only five companies at origin changed their city -> ok consider as fixed
length(which(!duplicated(firmlinks$GUOBvDIDnumber)))
length(which(!duplicated(paste0(firmlinks$GUOBvDIDnumber,firmlinks$GUOCity))))
# -> 97 for GUO - also neglect

firms = left_join(bvdids,as.tbl(firmlinks[!duplicated(firmlinks$BvDIDnumber),c("BvDIDnumber","countryzip","countrycity")]),by=c("id" = "BvDIDnumber"))
firms = left_join(firms,as.tbl(firmlinks[!duplicated(firmlinks$GUOBvDIDnumber),c("GUOBvDIDnumber","guocountrycity")]),by=c("id"="GUOBvDIDnumber"))

zipcoords <- sapply(zipcoverage,function(r){r[1,10:11]})
summary(sapply(zipcoords,length))
length(zipcoords) - length(which(sapply(zipcoords,length)==0))

#getCoordinatesOD <- function(r){
#  if(r[2]%in%names(zipcoords)){if(!is.null(zipcoords[[r[2]]])){return(zipcoords[[r[2]]])}}
#  if(r[3]%in%names(citiescoverage)){if(length(which(is.na(citiescoverage[[r[3]]])))==0){return(citiescoverage[[r[3]]])}}
#  if(r[4]%in%names(citiescoverage)){return(citiescoverage[[r[4]]])} # no pb to be NA at this point
#  return(c(NA,NA))
#}

#allcoords = apply(firms,1,getCoordinatesOD)
# damn inefficient
uzipcoords = unlist(zipcoords) # nulls are removed when unlisting
ziplat = uzipcoords[seq(1,length(uzipcoords),2)]
ziplon = uzipcoords[seq(2,length(uzipcoords),2)]

firms$ziplat = ziplat[paste0(firms$countryzip,".V10")]
firms$ziplon = ziplon[paste0(firms$countryzip,".V11")]

ucities = unlist(citiescoverage)
citieslat = as.numeric(ucities[seq(1,length(ucities),2)]);names(citieslat)<-names(ucities)[seq(1,length(ucities),2)]
citieslon = as.numeric(ucities[seq(2,length(ucities),2)]);names(citieslon)<-names(ucities)[seq(2,length(ucities),2)]

firms$citylat = citieslat[paste0(firms$countrycity,".V5")]
firms$citylon = citieslon[paste0(firms$countrycity,".V6")]

firms$guocitylat = citieslat[paste0(firms$guocountrycity,".V5")]
firms$guocitylon = citieslon[paste0(firms$guocountrycity,".V6")]

firms$lat = ifelse(!is.na(firms$ziplat),firms$ziplat,ifelse(!is.na(firms$citylat),firms$citylat,firms$guocitylat))
firms$lon = ifelse(!is.na(firms$ziplon),firms$ziplon,ifelse(!is.na(firms$citylon),firms$citylon,firms$guocitylon))


#######
# construct simplified network representation -> node table and link table

# turnover / employees may be different if a company is observed as origin and destination at two different dates?
# -> keep origin info only
length(!duplicated(firmlinks$GUOBvDIDnumber)) # guos ids are unique -> wtf?
length(!duplicated(paste0(firmlinks$GUOBvDIDnumber,firmlinks$GUOInformationdate))) # unique !
length(which(firmlinks$BvDIDnumber==firmlinks$GUOBvDIDnumber)) # 1Mio of self-ownership

firms = left_join(firms,firmlinks[!duplicated(firmlinks$BvDIDnumber),c("BvDIDnumber","Companyname","CountryISOcode","City","Zipcode",
                                     "NACERev2primarycode","NAICS2017corecode",
                                     "Nationalindustrycodeprimary","OperatingrevenueTurnoverthEURLastavailyr",
                                     "NumberofemployeesLastavailyr","Lastyear"
                                     )],by=c("id"="BvDIDnumber"))



firms = left_join(firms,firmlinks[!duplicated(firmlinks$GUOBvDIDnumber),c("GUOBvDIDnumber","GUOName","GUOCountryISOcode","GUOCity",
                                     "GUONACECorecode","GUONAICSCorecode","GUOInformationdate","GUOOperatingrevenueTurnovermEUR","GUONumberofemployees")],by=c("id"="GUOBvDIDnumber"))

# firms[which(is.na(firms$Companyname)),c("id","GUOName")] # strange company
# setdiff(bvdids$id,as.character(firmlinks$BvDIDnumber)) # ! indeed all GUOs are listed as companies also (and must be their own GUOs)
# so below operations should not be necessary

firms$name = ifelse(!is.na(firms$Companyname),firms$Companyname,firms$GUOName)
firms$country = ifelse(!is.na(firms$CountryISOcode),firms$CountryISOcode,firms$GUOCountryISOcode)
firms$city = ifelse(!is.na(firms$City),firms$City,firms$GUOCity)
firms$zipcode = ifelse(!is.na(firms$Zipcode),firms$Zipcode,NA)
firms$nacecode = ifelse(!is.na(firms$NACERev2primarycode),firms$NACERev2primarycode,firms$GUONACECorecode)
firms$naicscode = ifelse(!is.na(firms$NAICS2017corecode),firms$NAICS2017corecode,firms$GUONAICSCorecode)
firms$nationalcode = ifelse(!is.na(firms$Nationalindustrycodeprimary),firms$Nationalindustrycodeprimary,NA)
firms$turnover = ifelse(!is.na(firms$OperatingrevenueTurnoverthEURLastavailyr),firms$OperatingrevenueTurnoverthEURLastavailyr,firms$GUOOperatingrevenueTurnovermEUR)
firms$employees = ifelse(!is.na(firms$NumberofemployeesLastavailyr),firms$NumberofemployeesLastavailyr,firms$GUONumberofemployees)
firms$latestinfo = ifelse(!is.na(firms$Lastyear),firms$Lastyear,firms$GUOInformationdate) # all companies have a Lastyear

firms$turnover <- as.numeric(gsub(",","",firms$turnover))
firms$employees <- as.numeric(gsub(",","",firms$employees))

finalfirms = firms[,c("id","name","lon","lat","country","city","zipcode","nacecode",
                      "naicscode","nationalcode","turnover","employees","latestinfo")]

100*length(which(is.na(finalfirms$lon)))/nrow(finalfirms) # 11% have no location

finallinks = firmlinks[,c("GUOBvDIDnumber","BvDIDnumber","GUODirect","GUOTotal","GUOInformationdate")]
finallinks$GUODirect <- as.numeric(finallinks$GUODirect)
finallinks$GUOTotal <- as.numeric(finallinks$GUOTotal)
names(finallinks)<-c("from","to","direct_ownership","total_ownership","date")
finallinks$date[finallinks$date=="n.a."]=NA

# do we have duplicate links ?
length(which(!duplicated(paste0(finallinks$from,finallinks$to))))/nrow(finallinks)
# -> 0.2 % - ok

# Q: for self links, should be always 100%
summary(finallinks[finallinks$from==finallinks$to,c("direct_ownership","total_ownership")]) # yes
table(finallinks[finallinks$from==finallinks$to,c("date")]) # and observation is ALWAYS NA -> remove these links

# but keep the info that company is owning itself (GUO could be not observed ?)
ownowners = finallinks$from[finallinks$from==finallinks$to]
finalfirms$selfown = finalfirms$id%in%ownowners

finallinks = finallinks[finallinks$from!=finallinks$to,]

# write the data
write.table(finalfirms,file='Data/firms/amadeus_nodes.csv',sep=";",row.names = F,quote=F)
write.table(finallinks,file='Data/firms/amadeus_links.csv',sep=";",row.names = F,quote=F)


####
library(igraph)

g <- graph_from_data_frame(d = finallinks,vertices=finalfirms)




