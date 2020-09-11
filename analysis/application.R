
setwd(paste0(Sys.getenv('CS_HOME'),'/UrbanDynamics/Models/ABMCitiesFirms'))

library(dplyr)
library(ggplot2)

source(paste0(Sys.getenv('CS_HOME'),'/Organisation/Models/Utils/R/plots.R'))

resPrefix = '20200910_111006_APPLICATION'
resdir = paste0('Results/Application/',resPrefix,'/');dir.create(resdir,recursive = T)

#res <- as.tbl(read.csv(file='model_nl6/exploration/20200904_032159_APPLICATION.csv'))
#res <- as.tbl(read.csv(file='model_nl6/exploration/20200904_092344_APPLICATION.csv'))
res <- as.tbl(read.csv(file=paste0('model_nl6/exploration/',resPrefix,'.csv')))

params = c("countryScaleFactor","gravityScaleFactor","id")
indicators = c("internationalization","metropolization","networkAvgCommunitySize","networkDegreeEntropy",
               "flowsHierarchyAlpha","rhoDegreeSize","rhoFlowDistance","networkDegreeHierarchyAlpha"
)

tmax = 98
tstep = 500

tdata = data.frame()
for(t in seq(from=0,to=tmax,by=1)){
  inter = res[,paste0('internationalizationTS.',t)];names(inter) = c('internationalization')
  avgsize = res[,paste0('networkAvgCommunitySizeTS.',t)];names(avgsize)=c("networkAvgCommunitySize")
  rhoDegreeSize = res[,paste0('rhoDegreeSizeTS.',t,'.0')];names(rhoDegreeSize) = c("rhoDegreeSize")
  tdata=rbind(tdata,cbind(res[,params],internationalization = inter,
                          networkAvgCommunitySize = avgsize,
                          rhoDegreeSize =rhoDegreeSize ,
                          time = rep(t*tstep,nrow(res))
                          ))
}

ylabs = indicators;names(ylabs)<-indicators
ylabs[["internationalization"]] = "Internationalisation"
ylabs[["rhoDegreeSize"]] = "Metropolisation"
ylabs[["networkAvgCommunitySize"]] = "Average community size"

tdata$countryScaleFactorString = paste0('kappa[C]*"="*',tdata$countryScaleFactor)

g=ggplot(tdata,aes(x=time,y=internationalization,color=gravityScaleFactor,group=gravityScaleFactor))
g+geom_smooth()+facet_wrap(~countryScaleFactor)

g=ggplot(tdata,aes(x=time,y=networkAvgCommunitySize,color=gravityScaleFactor,group=gravityScaleFactor))
g+geom_smooth()+facet_wrap(~countryScaleFactor)

g=ggplot(tdata,aes(x=time,y=rhoDegreeSize,color=gravityScaleFactor,group=gravityScaleFactor))
g+geom_smooth()+facet_wrap(~countryScaleFactor)


# targeted plots
for(indicator in c("internationalization","rhoDegreeSize","networkAvgCommunitySize")){
  g=ggplot(tdata[tdata$countryScaleFactor%in%c(0.2,1.0),],aes_string(x="time",y=indicator,color="gravityScaleFactor",group="gravityScaleFactor"))
  g+geom_smooth()+facet_wrap(~countryScaleFactorString,labeller = label_parsed)+geom_vline(col='red',linetype=2,xintercept = tmax * tstep / 2)+
    scale_color_continuous(name=expression(kappa[G]))+ylab(ylabs[[indicator]])+xlab("Time")+stdtheme
  ggsave(file=paste0(resdir,indicator,'_Time_colorgravityScaleFactor_facetcountryScaleFactor.png'),width=30,height=15,units='cm')
}



# statistical tests

computePvalMat <- function(indicvals){
  pvalmat = matrix(0,length(unique(res$gravityScaleFactor)),length(unique(res$countryScaleFactor)))
  gravityScaleFactors = sort(unique(res$gravityScaleFactor));countryScaleFactors = sort(unique(res$countryScaleFactor))
  ref = indicvals[res$gravityScaleFactor==1.0&res$countryScaleFactor==1.0]
  for(i in 1:length(gravityScaleFactors)){
    for(j in 1:length(countryScaleFactors)){
      pvalmat[i,j] = ks.test(x=indicvals[res$gravityScaleFactor==gravityScaleFactors[i]&res$countryScaleFactor==countryScaleFactors[j]],y=ref)$p.value
    }
  }
  return(pvalmat)
}

computePvalMat(res$internationalizationTS.98)
computePvalMat(res$networkAvgCommunitySizeTS.98)
computePvalMat(res$rhoDegreeSizeTS.98.0)

