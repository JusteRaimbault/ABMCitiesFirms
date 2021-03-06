
setwd(paste0(Sys.getenv('CS_HOME'),'/UrbanDynamics/Models/ABMCitiesFirms/Results/Exploration'))

library(dplyr)
library(ggplot2)

source(paste0(Sys.getenv('CS_HOME'),'/Organisation/Models/Utils/R/plots.R'))

loadData <- function(pref,sep=',',resdir=NULL,addSepInName=''){
  if(is.null(resdir)){resdir = paste0(pref,'/data/')}
  res <- as.tbl(read.csv(paste0(resdir,pref,'.csv'),stringsAsFactors = F,sep=sep))

  # parse double arrays / rename
  corrindics=c("rhoDegreeSize","rhoFlowDistance","rhoFlowDistancePos")
  for(corrindic in corrindics){
    res[,corrindic]=res[,paste0(corrindic,addSepInName,'0')];res[,paste0(corrindic,addSepInName,'0')]=NULL
    res[,paste0(corrindic,'Min')]=res[,paste0(corrindic,addSepInName,'1')];res[,paste0(corrindic,addSepInName,'1')]=NULL
    res[,paste0(corrindic,'Max')]=res[,paste0(corrindic,addSepInName,'2')];res[,paste0(corrindic,addSepInName,'2')]=NULL
  }
  hierarchyindics = c("flowsHierarchy","networkDegreeHierarchy")
  for(hierarchyindic in hierarchyindics){
    res[,paste0(hierarchyindic,'Alpha')]=res[,paste0(hierarchyindic,addSepInName,'0')];res[,paste0(hierarchyindic,addSepInName,'0')]=NULL
    res[,paste0(hierarchyindic,'RSquared')]=res[,paste0(hierarchyindic,addSepInName,'1')];res[,paste0(hierarchyindic,addSepInName,'1')]=NULL
  }
  return(res)
}


####

parameters = c("gravityDecay","countryGravityDecay","gammaSectors","gammaLinks","gammaOrigin","gammaDestination")
indicators = c("internationalization","metropolization","networkAvgCommunitySize","networkDegreeEntropy",
               "flowsHierarchyAlpha","rhoDegreeSize","rhoFlowDistance","networkDegreeHierarchyAlpha"
              )
nominals = list("gravityDecay"=500,"countryGravityDecay"=500,"gammaSectors"=1,"gammaLinks"=1,"gammaOrigin"=1,"gammaDestination"=1)


#####
# one factor plots

resPrefix = '20190924_162740_ONEFACTOR_REPLICATIONS_SYNTHETIC_GRID'
resdir = paste0(resPrefix,'/')

res <- loadData(resPrefix,sep='\t')

#onefactorParamIndex = 1
#onefactorParamIndex = 4 # links : path dependency ? bof
#onefactorParamIndex = 3

for(onefactorParamIndex in c(1,3,4)){
  onefactorParam = parameters[onefactorParamIndex]

  rows=rep(T,nrow(res))
  for(param in parameters[-onefactorParamIndex]){rows=rows&res[,param]==nominals[[param]]}

  for(indic in indicators){
    g=ggplot(res[rows,],aes_string(x=onefactorParam,y=indic))
    g+geom_point(pch='.')+geom_smooth()
    ggsave(file=paste0(resdir,indic,"-",onefactorParam,".png"),width=18,height=15,units='cm')
  }
  # ! with one factor, nominal value is repeated ?
}

# gravity decay plot
onefactorParamIndex = 1
onefactorParam = parameters[onefactorParamIndex]
rows=rep(T,nrow(res))
for(param in parameters[-onefactorParamIndex]){rows=rows&res[,param]==nominals[[param]]}


# gravity decay
sres = res[rows,] %>% group_by(gravityDecay,countryGravityDecay,gammaSectors,gammaLinks,gammaOrigin,gammaDestination) %>% summarize(
  rhoDegreeSizeSd = sd(rhoDegreeSize),
  rhoDegreeSize = mean(rhoDegreeSize), 
  internationalizationSd = sd(internationalization),
  internationalization = mean(internationalization)
)

g=ggplot(sres,aes(x=gravityDecay,y=internationalization))
g+geom_point()+geom_line()+geom_errorbar(aes(ymin=internationalization-internationalizationSd,ymax=internationalization+internationalizationSd))+
  xlab(expression(d[0]))+ylab('Internationalisation')+stdtheme
ggsave(file=paste0(resdir,'internationalisation-gravityDecay_errorbars.png'),width=18,height=15,units='cm')

# note: metropolisation in the paper is correlation degree/size (not metropolization in the sim files/nl model)
g=ggplot(sres,aes(x=gravityDecay,y=rhoDegreeSize))
g+geom_point()+geom_line()+geom_errorbar(aes(ymin=rhoDegreeSize-rhoDegreeSizeSd,ymax=rhoDegreeSize+rhoDegreeSizeSd))+
  xlab(expression(d[0]))+ylab('Metropolisation')+stdtheme
ggsave(file=paste0(resdir,'metropolisation-gravityDecay_errorbars.png'),width=18,height=15,units='cm')



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
  xlab(expression(gamma[S]))+ylab('Internationalisation')+stdtheme
ggsave(file=paste0(resdir,'internationalisation-gammaSectors_errorbars.png'),width=18,height=15,units='cm')

g=ggplot(sres,aes(x=gammaSectors,y=rhoDegreeSize))
g+geom_point()+geom_line()+geom_errorbar(aes(ymin=rhoDegreeSize-rhoDegreeSizeSd,ymax=rhoDegreeSize+rhoDegreeSizeSd),width=0.05)+
  xlab(expression(gamma[S]))+ylab('Metropolisation')+stdtheme
ggsave(file=paste0(resdir,'metropolisation-gammaSectors_errorbars.png'),width=18,height=15,units='cm')




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


resPrefix = '20190925_134404_DIRECTSAMPLING_SYNTHETIC_GRID'
resdir = paste0(resPrefix,'/')
res<-loadData(resPrefix)


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

#sres$gammaOriginString = sapply(sres$gammaOrigin,function(s){expression(gamma[F]*"="*s)})
sres$gammaOriginString = paste0('gamma[O]*"="*',sres$gammaOrigin)

ylabs = indicators;names(ylabs)<-indicators
ylabs[["internationalization"]] = "Internationalisation"
ylabs[["rhoDegreeSize"]] = "Metropolisation"
ylabs[["networkAvgCommunitySize"]] = "Average community size"

for(countryGravityDecay in unique(res$countryGravityDecay)){
  for(gammaDestination in unique(res$gammaDestination)){
    d = sres[sres$countryGravityDecay==countryGravityDecay&sres$gammaDestination==gammaDestination&sres$gammaLinks==0,]
    for(indicator in indicators){
      g=ggplot(d,aes_string(x='gravityDecay',y=indicator,group='gammaSectors',color='gammaSectors'))
      g+geom_point()+geom_line()+geom_errorbar(aes_string(ymin=paste0(indicator,"-",indicator,"Sd"),ymax=paste0(indicator,"+",indicator,"Sd")))+
        #facet_wrap(~gammaOrigin,scales='free')+
        scale_color_continuous(name=expression(gamma[S]))+
        facet_wrap(~gammaOriginString,labeller = label_parsed)+
        xlab(expression(d[0]))+ylab(ylabs[[indicator]])+stdtheme
      ggsave(file=paste0(resdir,indicator,'_countryGravityDecay',countryGravityDecay,'_gammaDestination',gammaDestination,'_facetwrapgammaOrigin_colorgammaSectors.png'),width=30,height=15,units='cm')
    }
  }
}



##### hierarchy experiment

resPrefix = '20200901_142525_TARGETEDHIERARCHY_SYNTHETIC_GRID'
resdir = paste0(resPrefix,'/');dir.create(resdir)
res <- loadData(resPrefix,resdir='../../model_nl6/exploration/',addSepInName = '.')
resPrefix2 = '20200831_213603_TARGETEDHIERARCHY_SYNTHETIC_GRID'
res<-rbind(res,loadData(resPrefix2,resdir='../../model_nl6/exploration/',addSepInName = '.'))

data.frame(res %>% group_by(id) %>% summarize(count=n()))
res = res[res$id>=4&res$setupScalingExponent>=0.5,]

for(indicator in indicators){
  g=ggplot(res,mapping = aes_string(x='setupScalingExponent',y=indicator,group='gravityDecay',color='gravityDecay'))
  g+geom_point(pch='.')+geom_smooth()+xlab(expression(alpha))+stdtheme
  ggsave(file=paste0(resdir,indicator,'-alpha_colorgravityDecay.png'),width=30,height=25,units='cm')
}

sres= res %>% group_by(id) %>% summarize(
  gravityDecay = mean(gravityDecay),
  setupScalingExponent = mean(setupScalingExponent),
  internationalizationSd = sd(internationalization),internationalization=mean(internationalization),
  metropolizationSd = sd(metropolization),metropolization=mean(metropolization),
  networkAvgCommunitySizeSd = sd(networkAvgCommunitySize),networkAvgCommunitySize=mean(networkAvgCommunitySize),
  networkDegreeEntropySd = sd(networkDegreeEntropy),networkDegreeEntropy=mean(networkDegreeEntropy),
  flowsHierarchyAlphaSd = sd(flowsHierarchyAlpha),flowsHierarchyAlpha=mean(flowsHierarchyAlpha),
  rhoDegreeSizeSd = sd(rhoDegreeSize),rhoDegreeSize=mean(rhoDegreeSize),
  rhoFlowDistanceSd = sd(rhoFlowDistance),rhoFlowDistance=mean(rhoFlowDistance),
  networkDegreeHierarchyAlphaSd = sd(networkDegreeHierarchyAlpha),networkDegreeHierarchyAlpha=mean(networkDegreeHierarchyAlpha)
)

for(indicator in indicators){
  g=ggplot(sres,aes_string(x='setupScalingExponent',y=indicator,group='gravityDecay',color='gravityDecay'))
  g+geom_point()+geom_line()+geom_errorbar(aes_string(ymin = paste0(indicator,'-',indicator,'Sd'),ymax=paste0(indicator,'+',indicator,'Sd')))+
    xlab(expression(alpha))+ylab(ylabs[[indicator]])+scale_color_continuous(name=expression(d[0]))+stdtheme
  ggsave(file=paste0(resdir,indicator,'-alpha_colorgravityDecay_errorBar.png'),width=20,height=18,units='cm')
}




