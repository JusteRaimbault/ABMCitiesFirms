
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




