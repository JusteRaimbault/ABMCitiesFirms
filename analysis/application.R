
setwd(paste0(Sys.getenv('CS_HOME'),'/UrbanDynamics/Models/ABMCitiesFirms'))

library(dplyr)
library(ggplot2)

source(paste0(Sys.getenv('CS_HOME'),'/Organisation/Models/Utils/R/plots.R'))

#res <- as.tbl(read.csv(file='model_nl6/exploration/20200904_032159_APPLICATION.csv'))
res <- as.tbl(read.csv(file='model_nl6/exploration/20200904_092344_APPLICATION.csv'))

params = c("countryScaleFactor","gravityScaleFactor","id")

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
                          time = rep(t,nrow(res))
                          ))
}


g=ggplot(tdata,aes(x=time,y=internationalization,color=gravityScaleFactor,group=gravityScaleFactor))
g+geom_point(pch='.')+geom_smooth()+facet_wrap(~countryScaleFactor)

g=ggplot(tdata,aes(x=time,y=networkAvgCommunitySize,color=gravityScaleFactor,group=gravityScaleFactor))
g+geom_point(pch='.')+geom_smooth()+facet_wrap(~countryScaleFactor)

g=ggplot(tdata,aes(x=time,y=rhoDegreeSize,color=gravityScaleFactor,group=gravityScaleFactor))
g+geom_point(pch='.')+geom_smooth()+facet_wrap(~countryScaleFactor)




