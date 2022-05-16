
#######
## Network Analysis
#  -> modularities of countries, of NUTS
#  -> Louvain communities

setwd(paste0(Sys.getenv("CS_HOME"),'/UrbanDynamics/Models/ABMCitiesFirms'))

library(dplyr)
library(igraph)
library(Matrix)
library(reshape2)
library(poweRlaw)

load('Data/firms/amadeus_aggregnw.RData')
load('Data/firms/amadeus_saggregnw.RData')

source('analysis/functions.R')


# Construct and export graph

g = graph_from_data_frame(aggrlinks,directed = T,vertices = saggrnodes[,1:30])
V(g)$lon = (V(g)$X - min(V(g)$X)) / (max(V(g)$X) - min(V(g)$X))
V(g)$lat = (V(g)$Y - min(V(g)$Y)) / (max(V(g)$Y) - min(V(g)$Y))
write_graph(g,file='Results/EmpiricalNetwork/fua_graph.gml',format='gml')


# Centralities 
mean(degree(g))
mean(strength(g))
plot(log(1:length(V(g))),sort(log(strength(g)),decreasing = T))
plot(log(1:length(V(g))),sort(log(strength(g,mode='in')),decreasing = T))
plot(log(1:length(V(g))),sort(log(strength(g,mode='out')),decreasing = T))
# roughly the same! balance in/out ?
summary(strength(g,mode='in')/strength(g,mode='out'))

# Metropolisation
cor.test(strength(g), V(g)$gdp)
cor.test(strength(g), V(g)$turnover)

# degree distribution (remove 2 zeros: fuas with no links)
fitdeg = fitDistrPowerLaw(strength(g)[strength(g)>0],'Weighted degree',file='Results/EmpiricalNetwork/degreeDistr.png')
#get_distance_statistic(fitdeg$powerlaw)
#get_distance_statistic(fitdeg$ln)
fitDistrPowerLaw(strength(g,mode='in')[strength(g,mode='in')>0],'Weighted in-degree',file='Results/EmpiricalNetwork/in-degreeDistr.png')
fitDistrPowerLaw(strength(g,mode='out')[strength(g,mode='out')>0],'Weighted out-degree',file='Results/EmpiricalNetwork/out-degreeDistr.png')

# edge weight distribution
fitdegweights = fitDistrPowerLaw(E(g)$weight[E(g)$weight>0],'Edge weight',file='Results/EmpiricalNetwork/edgeweight.png',
                                 y1 = 2e-4, y2 = 4e-4)

# centrality distrib: bw? # ! makes not much sense in practice



# Communities
A = get.adjacency(g,sparse = T)
g_undir = graph_from_adjacency_matrix(adjmatrix = (A+t(A))/2,weighted = T,mode = "undirected")

set.seed(42)
communities_clauset = cluster_fast_greedy(g_undir)
set.seed(42)
communities_louvain = cluster_louvain(g_undir) 
# 0.35 modularity in both cases

# directed mod
directedmodularity(communities_clauset$membership,A) # 0.3521929
directedmodularity(communities_louvain$membership,A) # 0.361524

# countries mod = internationalisation
directedmodularity(V(g)$fuacountry,A) # 0.3232149

# null model with 30 random communities
bnum=1000; mods = c()
for(b in 1:bnum){
  if(b%%100==0){show(b)}
  mods=append(mods,directedmodularity(sample.int(30,size=length(V(g)),replace = T),A))
}
mean(mods);sd(mods)
# 0.05038611 ; 0.001729387

# max modularity?
maxmodularity(communities_louvain$membership,A)

# overlap between countries / communities


###
# Community map

saggrnodes$Community = as.character(communities_louvain$membership)
#saggrnodes$Population = saggrnodes$pop # issue with ghs - fua or pop - anyway plot with turnover
saggrnodes$Turnover = saggrnodes$turnover
comsizes = as.data.frame(table(communities_louvain$membership))
keptcoms = as.character(comsizes[comsizes[,2]>5,1])
saggrnodes$Community[!saggrnodes$Community%in%keptcoms]=NA


countries <- st_read(paste0(Sys.getenv('CS_HOME'),'/Data/NaturalEarthData/ne_50m_admin_0_countries'),'ne_50m_admin_0_countries')
#countries <- st_transform(countries,st_crs(fuas)) #wgs84, so are saggrnodes (although no crs)


map(data = saggrnodes,var = "Community", sizevar = "Turnover",
    countries = countries,
    filename = 'Results/EmpiricalNetwork/map_communities_louvain.png',discrete = T,
    xlim=c(-10,30),ylim=c(35,62),
    width=22,height=18
)

