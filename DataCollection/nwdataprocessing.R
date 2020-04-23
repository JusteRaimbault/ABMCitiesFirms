
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

# should avoid issue after having exported csv - anyway should have used quotes
finalfirms$name = gsub(';','',finalfirms$name,fixed=T)

# write the data
write.table(finalfirms,file='Data/firms/amadeus_nodes.csv',sep=";",row.names = F,quote=F)
write.table(finallinks,file='Data/firms/amadeus_links.csv',sep=";",row.names = F,quote=F)


############
############


# edits - fucking quotes and semicolons in csv
#27634;DE8310151625;B & O CONCEPT CONSULTING GMBH FÜR HOCHBAU; INGENIEURWESEN UND UMWELT;10.5156;50.0352;DE;HAßFURT;97437;7111;5413;M71110;NA;NA;2017;TRUE#
#221815;DE2290186901;COLONIAL CONSULTING GMBH; FINANCIAL SERVICES;10.3167;53.9667;DE;BAD SEGEBERG;23795;6622;5242;K66220;NA;6;2019;FALSE
#225233;NL09035291;COMPANEN: ADVIESGROEP VOOR BELEID; ONDERZOEK EN PLANNING B.V.;5.9366;51.9912;NL;ARNHEM;6824 BV;7112;5413;7112;NA;26;2017;FALSE
#249350;IT00490830460;CONCERIA VECCHIA TOSCANA S.P.A. ; GRUPPO CONCIARIO VECCHIA TOS CANA S.P.A. ; CON SIGLA COMMERCIALE GVT ; GRUPPO VECCHIA TOSCANA S.P.A.;;10.79749;43.73292;IT;FUCECCHIO;50054;1511;3161;151100;33859;52;2018;FALSE
#320930;DE8250320175;DIAFLORA, GESELLSCHAFT MIT BESCHR.HAFTUNG; GÄRTNEREI UND INTEGRATIONSBETRIEB;12.1232;47.855;DE;ROSENHEIM;83022;8130;5617;N81301;NA;7;2017;FALSE
#396848;DE2270266154;EMK PERSONAL DIENSTLEISTUNGEN;GMBH;10.7583;53.8922;DE;LÜBECK;23568;7810;5613;N78100;NA;35;2017;TRUE
# 416637;NL18073552;ERNEST BOEL; ADVIES, TRAINING EN ONTWIKKELING B.V.;4.7492;51.6645;NL;WAGENBERG;4845 GA;7022;5416;70221;NA;NA;2018;FALSE
# 448643;NL23050494;FAMILIE A.-; W.- EN W. VAN DIJK HOLDINGMAATSCHAPPIJ B.V.;4.8841;51.8245;NL;HARDINXVELD-GIESSENDAM;3372 BV;6420;5511;6420;NA;NA;2017;FALSE
# 531809;AT9090186930;GES. FÜR AUTARKE ENERGIE, TECHNISCHE INNOVATIONEN & ALTRUISMUS, KURZ: GAIA;;16.37208;48.20849;AT;WIEN;1080;9499;8139;S94990;NA;14;2019;TRUE
# 652443;PL142516831;HSIANG CHUAN MACHINERY CO.;LTD.- SP. Z O.O. PRZEDSTAWICIELSTWO W POLSCE;21.01178;52.22977;PL;WARSZAWA;02-672;7311;5418;7311;NA;5;2015;FALSE
# 689283;IT00640300554;GESTIONE ATTIVITA' TURISTICHE - SOCIETA' A RESPONSABILITA' LIMITATA ; IN FORMA ABBREVIATA G.A.T. - S.R.L.;12.11249;42.71924;IT;ORVIETO;05018;7911;5615;791100;961;2;2018;FALSE
# 703852;IT01269200695;TORRE ZAMBRA S.R.L.; AZIENDA TORRE ZAMBRA SRL ; AZIENDA TORRE ZAMBRA IDI DI MARZO SRL ; TORRE ZAMBRA IDI DI MARZO SRL ; AZIENDA IDI DI MARZO SRL ; IDI DI MARZO SRL;14.23689;42.32984;IT;VILLAMAGNA;66010;1102;3121;110210;957;5;2017;FALSE
# 721400;IT09949690011;ALA NAUTICAL DESIGN CHANCE S.R.L. SIGLABILE ED ABBREVIABILE ALTERNATIVAMENTE AI FINI COMMERCIALI IN ALA NAUTICAL S.R.L.; ALA N.D. S.R.L.; CHANCE DESIGN S.R.L.; CHANCE S.R.L.; ALA NAUTICAL DESIGN S.R.L.; A.N. DESIGN S.R.L.;7.68682;45.07049;IT;TORINO;10143;1392;3141;139200;947;9;2018;FALSE
# 802460;NL18036966;KOCK & PARTNERS B.V.; ORGANISATIE-ADVISEURS EN MANAGEMENT TRAINERS;5.4956;51.4767;NL;EINDHOVEN;5632 CW;7022;5416;70221;NA;NA;2018;FALSE
# 828409;HU13977883;LACHÁZI FAGYIZÓ KERESKEDELMI ÉS VENDÉGLÁTÓIPARI KORLÁTOLT FELELŐSSÉGŰ TÁRSASÁG &#8222;VÉGELSZÁMOLÁS ALATT&#8221;;19.0093;47.18839;HU;KISKUNLACHÁZA;2340;4724;3118;4724;NA;NA;2016;TRUE
# 1200914;GB00624771;R.P.COLMAN & CO;LIMITED;1.3246;52.6528;GB;NORWICH;NR7 9AJ;4649;4232;46499;NA;NA;2018;TRUE
# 1384229;DE2010458880;STIBB-SOZIAL-THERAP.INST. BERLIN-BRANDENBURG;HILFEN F.SEXUELL MIßBR. KINDER EV;13.3086;52.5028;DE;BERLIN;10629;8899;6241;Q88990;NA;4;2019;TRUE
# 1430712;NL10030870;TEC; TWENTE ENGINEERING CONSULTANCY B.V.;5.754;51.8022;NL;WIJCHEN;6603 LC;6420;5511;6420;NA;NA;2017;TRUE
# 1556187;DE4070579875;VEREIN FÜR BETREUUNG U. UNTERSTÜTZUNG BEHINDERTER MENSCHEN U. DEREN FAMILIEN;;7.6638;51.5923;DE;KAMEN;59174;9499;8139;S94999;NA;1;2015;TRUE
# 1666733;IT05244611009;Q PROGETTI SOCIETA' A RESPONSABILITA' LIMITATA; IN SIGLA QP S.R.L .;12.51133;41.89193;IT;ROMA;00186;7110;5413;711000;530;2;2018;FALSE
# 1685182;SK35478268;ANDREA KANDOVÁ - MINI POTRAVINY ; NON STOP;NA;NA;SK;VEĽKÝ KAMENEC;07636;4719;4522;4719;500;7;2017;TRUE
# 1733150;IT02408730642;SOCIETA' AGRICOLA TORREVIGNE COLLEANTICO SALSOLE TERRE AUREE S.R.L. CON LE SIGLE SOCIETA' AGRICOLA TORREVIGNE S.R.L. ; SOCIETA' AGRICOLA TERRE AUREE S.R.L. ; SOCIETA' AGRICOLA COLLEANTICO S.R.L. ; SOCIETA' AGRICOLA SALSOLE S.R.L. ; TORREVIGNE S.R.L. ;;14.85498;41.03691;IT;MONTEFUSCO;83030;0150;1119;015000;439;6;2017;FALSE
# 1798301;DE2250073082;GROEN & JANSSEN GMBH; HOLZ-TÜREN-PLATTEN GROßHANDEL;7.3537;53.4493;DE;SÜDBROOKMERLAND;26624;4673;4233;G46731;11691;52;2017;FALSE
# 1847052;IT02380570875;INFORMATICA CONSULENZA SERVIZI - S.R.L. ; IN BREVE I.C.S. S.R.L.;15.07041;37.49223;IT;CATANIA;95121;6202;5415;620200;322;7;2017;FALSE
# 1867220;IT02165560646;I CAPITANI SOCIETA' AGRICOLA S.R.L. IN SIGLA : SOC. AGR. I CAPITANI S.R.L; AGRICOLA I CAPITANI S.R.L.; I CAPITANI S.R.L.; ICSA S.R.L.; AGR. I CAPITANI S.R.L.; I CAPITANI S.A.R.L.;14.90934;41.02312;IT;TORRE LE NOCELLE;83030;1102;3121;110200;307;6;2017;FALSE
# 2024763;IT02375280308;EMMEPI DOORS S.R.L. (DA STATUTO ESATTA DENOMINAZIONE CONTIENE TRATTINO DI SOTTOLINEATURA FRA LE PAROLE EMMEPI E DOORS; CAUSA PROBLEMA TECNICO NON E' POSSIBILE INSERIMENTO);13.07545;46.28164;IT;TRASAGHIS;33010;2511;3323;251100;199;3;2015;FALSE
# 2050594;IT01596840346;PREVI SOCIETA' DI REVISIONE CONTABILE - SOCIETA' A RESPONSABILIT LIMITATA , O PIU' BREVEMENTE PREVI SOCIETA' DI REVISIONE CONTABILE S.R.L. O AN E PREVI S.R.L. ;;10.32618;44.79935;IT;PARMA;43121;6920;5412;692020;184;1;2018;FALSE
# 2071250;IT02357280805;SALINAUTO SOCIETA' A RESPONSABILITA' LIMITATA; IN SIGLA: SALINAUTO S.R.L.;15.758;37.98277;IT;MONTEBELLO JONICO;89064;4511;4231;451101;5881;11;2017;FALSE
# 2110553;PL770543477;RECORD R. NOWAK; M. SAWICKI; M. CHOSZCZ SP. J.;17.02872;54.46405;PL;SŁUPSK;76-206;4531;4231;4531;5510;NA;2017;TRUE
# 2112696;SK36646563;MEDICAL WEIGHT MANAGEMENT, S.R.O.;;NA;NA;SK;ZVOLEN;96001;8621;6213;8621;154;1;2017;TRUE
# 2159252;PL430722358;SALONIKA S. KONARSKI; A. WASILEWSKA SP.J.;22.56667;51.25;PL;LUBLIN;20-211;4778;4539;4778;5166;24;2018;TRUE
# 2193164;SK36237353;EURO CONSULT INVEST, S.R.O.;;NA;NA;SK;SENICA;90501;1413;3152;1413;119;7;2018;TRUE
# 2238853;IT08428031002;TECHNO SKY SOCIETA' A RESPONSABILITA' LIMITATA - TECHNOLOGIES FOR AIR TRAFFIC MANAGEMENT; IN BREVE: TECHNO SKY S.R.L.;12.51133;41.89193;IT;ROMA;00156;4321;2382;432101;91800;807;2017;FALSE
# 2250331;FI01422280;SUOMEN ADVENTTIKIRKKO;FINLANDS ADVENTKYRKA;23.78712;61.49911;FI;TAMPERE;33680;9491;8131;94910;100;175;2018;TRUE
# 2250761;FI10571253;YMPÄRISTÖKASVATUSJÄRJESTÖ FEE SUOMI RY;FÖRENINGEN FÖR MILJÖFOSTRAN FEE SUOMI RF;24.93545;60.16952;FI;HELSINKI;00520;9499;8139;94999;100;2;2018;TRUE
# 2262004;IT03548600042;ENRICO SERAFINO S.R.L. SIGLABILE ENRICO SERAFINO 1878 S.R.L, O IN SIGLA: E.S. S.R.L.; A.V.E.S. S.R.L.; C.V.E.S. S.R.L.; V.E.S. S.R.L.; GENTILE E.S. S.R.L.; C.V. GENTILE E.S. S.R.L.; CON O SENZA ANTEPOSIZIONE O POSPOSIZIONE DELLA LOCUZIONE AZIENDA VITIVINI;7.99373;44.79532;IT;CANALE;12043;1102;3121;110210;4289;8;2018;FALSE
# 2294023;SK51107953;LEKÁREŇ PETRŽALSKÁ TRŽNICA, S.R.O.;;NA;NA;SK;BRATISLAVA;81103;8211;5611;8211;84;3;2018;FALSE
# 2306006;PL100123389;SZYMAŃSCY PIEKARNICTWO, CUKIERNICTWO; PAWEŁ SZYMAŃSKI, EWA SZYMAŃSKA S.J.;NA;NA;PL;ZAWADA;97-200;1071;3118;1071;3983;NA;2018;TRUE
# 2410313;IT02067030540;ISTITUTO DI PSICOSOMATICA ABERASTURY - EIDON S.R.L.; IN FORMA ABB REVIATA ISTITUTO ABERASTURY - EIDON S.R.L.;12.38878;43.1122;IT;PERUGIA;06074;8622;6213;862209;55;2;2018;FALSE
# 2459076;GB02481354;OXFORD SPIRES MANAGEMENT CO; LIMITED;-0.5069;52.1385;GB;BEDFORD;MK40 2NR;8299;5614;82990;42;1;2017;FALSE
# 2482788;IT00744720095;ORGANIZZAZIONE PUBBLICITARIA STRADALE - S.P.A.; ABBREVIATA IN O.P.S. - S.P.A.;12.51133;41.89193;IT;ROMA;00145;7312;5418;731200;37;NA;2013;FALSE
# 2488887;IT09857171004;SOGEEA UNITA' ORGANIZZATIVA TECNICA UFFICIO SPECIALE CONDONO EDILIZIO UOT USCE S.P.A. IN BREVE: SOGEEA S.P.A.; UNITA' ORGANIZZATIVA TECNICA S.P.A.; UFFICIO SPECIALE CONDONO EDILIZIO S.P.A.; UOT S.P.A.; USCE S.P.A.;12.51133;41.89193;IT;ROMA;00195;7112;5413;711200;3067;16;2017;FALSE
# 2558756;IT04816280657;NOVA IMMOBILIARE S.R.L. (INDIVIDUATA ANCHE COME: 1) NOVA S.R.L.; 2) NOV.IMM. S.R.L.; 3) NOV.IM S.R.L.; 4) NOVIM S.R.L.);14.79328;40.67545;IT;SALERNO;84134;6831;5313;683100;24;1;2017;FALSE
# 2799379;IT07706851008;ORAL BIOCARE SOCIETA' A RESPONSABILITA' LIMITATA IN BREVE; ORAL BIOCARE - S.R.L.;12.51133;41.89193;IT;ROMA;00145;8623;3391;862300;1975;14;2018;FALSE
# 2810271;IT06687020724;AQUOS SOCIETA' A RESPONSABILITA' LIMITATA; IN FORMA ABBREVIATA AQUOS S.R.L.;16.54952;40.82664;IT;ALTAMURA;70022;4322;2382;432201;0;NA;2013;FALSE
# 2984814;IT06108080968;INTERMONTE HOLDING SIM S.P.A. SIGLABILE ANCHE INTERMONTE HOLDING - SOCIETA' DI INTERMEDIAZIONE MOBILIARE S.P.A.; INTERMONTE HOLDING - SOCIETA' DI INTERMEDIAZIONE MOBILIARE P.A.; INTERMONTE HOLDING - SIM P.A.; INTERMONTE HOLDING SIM; INTERMONTE H SIM; IH SI;9.18951;45.46427;IT;MILANO;20122;6499;5239;649910;41502;137;2017;TRUE
# 3011044;DE6070354906;AMPHION PARTNERGESELLSCHAFT; KÖGLER, KRUG-STEAD, WILKE, UNTERNEHMENSBERATER;8.2424;50.0773;DE;WIESBADEN;65185;7022;5416;M70220;NA;3;2013;TRUE
# 3050953;NL57719225;ASTIS; THE VITALITY COMPANY B.V.;5.7269;51.2542;NL;WEERT;6004 HT;4638;4244;46389;NA;NA;2018;FALSE














