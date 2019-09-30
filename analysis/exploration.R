
setwd(paste0(Sys.getenv('CS_HOME'),'/UrbanDynamics/Models/ABMCitiesFirms/Results/Exploration'))

library(dplyr)
library(ggplot2)

source(paste0(Sys.getenv('CS_HOME'),'/Organisation/Models/Utils/R/plots.R'))

#resPrefix = '20190924_162740_ONEFACTOR_REPLICATIONS_SYNTHETIC_GRID'
resPrefix = '20190925_134404_DIRECTSAMPLING_SYNTHETIC_GRID'
resdir = paste0(resPrefix,'/')

res <- as.tbl(read.csv(paste0(resdir,'data/',resPrefix,'.csv'),stringsAsFactors = F))

# parse double arrays / rename

corrindics=c("rhoDegreeSize","rhoFlowDistance","rhoFlowDistancePos")
for(corrindic in corrindics){
  res[,corrindic]=res[,paste0(corrindic,'0')];res[,paste0(corrindic,'0')]=NULL
  res[,paste0(corrindic,'Min')]=res[,paste0(corrindic,'1')];res[,paste0(corrindic,'1')]=NULL
  res[,paste0(corrindic,'Max')]=res[,paste0(corrindic,'2')];res[,paste0(corrindic,'2')]=NULL
}
hierarchyindics = c("flowsHierarchy","networkDegreeHierarchy")
for(hierarchyindic in hierarchyindics){
  res[,paste0(hierarchyindic,'Alpha')]=res[,paste0(hierarchyindic,'0')];res[,paste0(hierarchyindic,'0')]=NULL
  res[,paste0(hierarchyindic,'RSquared')]=res[,paste0(hierarchyindic,'1')];res[,paste0(hierarchyindic,'1')]=NULL
}



####

parameters = c("gravityDecay","countryGravityDecay","gammaSectors","gammaLinks","gammaOrigin","gammaDestination")
indicators = c("internationalization","metropolization","networkAvgCommunitySize","networkDegreeEntropy",
               "flowsHierarchyAlpha","rhoDegreeSize","rhoFlowDistance","networkDegreeHierarchyAlpha"
              )
nominals = list("gravityDecay"=500,"countryGravityDecay"=500,"gammaSectors"=1,"gammaLinks"=1,"gammaOrigin"=1,"gammaDestination"=1)


#####
# one factor plot
#onefactorParamIndex = 1
#onefactorParamIndex = 4 # links : path dependency ? bof
onefactorParamIndex = 3
onefactorParam = parameters[onefactorParamIndex]

rows=rep(T,nrow(res))
for(param in parameters[-onefactorParamIndex]){rows=rows&res[,param]==nominals[[param]]}

for(indic in indicators){
  g=ggplot(res[rows,],aes_string(x=onefactorParam,y=indic))
  g+geom_point(pch='.')+geom_smooth()
  ggsave(file=paste0(resdir,indic,"-",onefactorParam,".png"),width=18,height=15,units='cm')
}

# ! with one factor, nominal value is repeated ?

# gravity decay
sres = res[rows,] %>% group_by(gravityDecay,countryGravityDecay,gammaSectors,gammaLinks,gammaOrigin,gammaDestination) %>% summarize(
  rhoDegreeSizeSd = sd(rhoDegreeSize),
  rhoDegreeSize = mean(rhoDegreeSize), 
  internationalizationSd = sd(internationalization),
  internationalization = mean(internationalization)
)

g=ggplot(sres,aes(x=gravityDecay,y=internationalization))
g+geom_point()+geom_line()+geom_errorbar(aes(ymin=internationalization-internationalizationSd,ymax=internationalization+internationalizationSd))+
  xlab('Gravity decay')+ylab('Internationalization')+stdtheme
ggsave(file=paste0(resdir,'internationalization-gravityDecay_errorbars.png'),width=18,height=15,units='cm')

g=ggplot(sres,aes(x=gravityDecay,y=rhoDegreeSize))
g+geom_point()+geom_line()+geom_errorbar(aes(ymin=rhoDegreeSize-rhoDegreeSizeSd,ymax=rhoDegreeSize+rhoDegreeSizeSd))+
  xlab('Gravity decay')+ylab(expression(rho*'['*d[i]*','*E[i]*']'))+stdtheme
ggsave(file=paste0(resdir,'rhoDegreeSize-gravityDecay_errorbars.png'),width=18,height=15,units='cm')



###

onefactorParamIndex = 3
onefactorParam = parameters[onefactorParamIndex]
rows=rep(T,nrow(res))
for(param in parameters[-onefactorParamIndex]){rows=rows&res[,param]==nominals[[param]]}

res$gammaSectors = round(res$gammaSectors,digits = 3)

sres = res[rows,] %>% group_by(gravityDecay,countryGravityDecay,gammaSectors,gammaLinks,gammaOrigin,gammaDestination) %>% summarize(
  rhoDegreeSizeSd = sd(rhoDegreeSize),
  rhoDegreeSize = mean(rhoDegreeSize), 
  internationalizationSd = sd(internationalization),
  internationalization = mean(internationalization)
)

g=ggplot(sres,aes(x=gammaSectors,y=internationalization))
g+geom_point()+geom_line()+geom_errorbar(aes(ymin=internationalization-internationalizationSd,ymax=internationalization+internationalizationSd),width=0.05)+
  xlab(expression(gamma[S]))+ylab('Internationalization')+stdtheme
ggsave(file=paste0(resdir,'internationalization-gammaSectors_errorbars.png'),width=18,height=15,units='cm')

g=ggplot(sres,aes(x=gammaSectors,y=rhoDegreeSize))
g+geom_point()+geom_line()+geom_errorbar(aes(ymin=rhoDegreeSize-rhoDegreeSizeSd,ymax=rhoDegreeSize+rhoDegreeSizeSd),width=0.05)+
  xlab(expression(gamma[S]))+ylab(expression(rho*'['*d[i]*','*E[i]*']'))+stdtheme
ggsave(file=paste0(resdir,'rhoDegreeSize-gammaSectors_errorbars.png'),width=18,height=15,units='cm')




########
##  sharpes for stoch variability

sharpes <- res %>% group_by(id) %>% summarize(
    internationalization = abs(mean(internationalization)/sd(internationalization)),
    metropolization= abs(mean(metropolization)/sd(metropolization)),
    networkAvgCommunitySize = abs(mean(networkAvgCommunitySize)/sd(networkAvgCommunitySize)),
    networkDegreeEntropy = abs(mean(networkDegreeEntropy)/sd(networkDegreeEntropy)),
    flowsHierarchyAlpha = abs(mean(flowsHierarchyAlpha)/sd(flowsHierarchyAlpha)),
    rhoDegreeSize=abs(mean(rhoDegreeSize)/sd(rhoDegreeSize)),
    rhoFlowDistance=abs(mean(rhoFlowDistance)/sd(rhoFlowDistance)),
    networkDegreeHierarchyAlpha=abs(mean(networkDegreeHierarchyAlpha)/sd(networkDegreeHierarchyAlpha))
)

summary(sharpes)



########
## Grid plots

sres <- res %>% group_by(gravityDecay,countryGravityDecay,gammaSectors,gammaLinks,gammaOrigin,gammaDestination) %>% summarize(
  internationalizationSd = sd(internationalization),
  internationalization = mean(internationalization),
  metropolizationSd = sd(metropolization),
  metropolization = mean(metropolization),
  networkAvgCommunitySizeSd = sd(networkAvgCommunitySize),
  networkAvgCommunitySize = mean(networkAvgCommunitySize),
  networkDegreeEntropySd = sd(networkDegreeEntropy),
  networkDegreeEntropy = mean(networkDegreeEntropy),
  flowsHierarchyAlphaSd = sd(flowsHierarchyAlpha),
  flowsHierarchyAlpha = mean(flowsHierarchyAlpha),
  rhoDegreeSizeSd = sd(rhoDegreeSize),
  rhoDegreeSize= mean(rhoDegreeSize),
  rhoFlowDistanceSd=sd(rhoFlowDistance),
  rhoFlowDistance = mean(rhoFlowDistance),
  networkDegreeHierarchyAlphaSd = sd(networkDegreeHierarchyAlpha),
  networkDegreeHierarchyAlpha = mean(networkDegreeHierarchyAlpha)
)


for(countryGravityDecay in unique(res$countryGravityDecay)){
  for(gammaDestination in unique(res$gammaDestination)){
    d = sres[sres$countryGravityDecay==countryGravityDecay&sres$gammaDestination==gammaDestination,]
for(indicator in indicators){
  g=ggplot(d,aes_string(x='gravityDecay',y=indicator,group='gammaSectors',color='gammaSectors'))
  g+geom_point()+geom_line()+geom_errorbar(aes_string(ymin=paste0(indicator,"-",indicator,"Sd"),ymax=paste0(indicator,"+",indicator,"Sd")))+
    facet_grid(gammaLinks~gammaOrigin)+xlab(expression(d[G]))+stdtheme
  ggsave(file=paste0(resdir,indicator,'_countryGravityDecay',countryGravityDecay,'_gammaDestination',gammaDestination,'_facetgammaLinks-gammaOrigin_colorgammaSectors.png'),width=30,height=25,units='cm')
  }
  }
}

# same but fixed gammaLinks (close to no effect -> need to change that process)
# or can we do a large number enough of simulations to see a significant effect ? -> interesting exercize !

for(countryGravityDecay in unique(res$countryGravityDecay)){
  for(gammaDestination in unique(res$gammaDestination)){
    d = sres[sres$countryGravityDecay==countryGravityDecay&sres$gammaDestination==gammaDestination&sres$gammaLinks==0,]
    for(indicator in indicators){
      g=ggplot(d,aes_string(x='gravityDecay',y=indicator,group='gammaSectors',color='gammaSectors'))
      g+geom_point()+geom_line()+geom_errorbar(aes_string(ymin=paste0(indicator,"-",indicator,"Sd"),ymax=paste0(indicator,"+",indicator,"Sd")))+
        #facet_wrap(~gammaOrigin,scales='free')+
        facet_wrap(~gammaOrigin)+
        xlab(expression(d[G]))+stdtheme
      ggsave(file=paste0(resdir,indicator,'_countryGravityDecay',countryGravityDecay,'_gammaDestination',gammaDestination,'_facetwrapgammaOrigin_colorgammaSectors.png'),width=30,height=15,units='cm')
    }
  }
}





