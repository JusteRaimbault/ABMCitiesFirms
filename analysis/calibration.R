
setwd(paste0(Sys.getenv("CS_HOME"),'/UrbanDynamics/Models/ABMCitiesFirms'))
source(paste0(Sys.getenv("CS_HOME"),'/Organisation/Models/Utils/R/plots.R'))

library(dplyr)
library(ggplot2)

resdirpref='CALIBRATION_GRID_20200305_092250/'
generation='38000'

res <- as.tbl(read.csv(paste0('openmole/calibration/',resdirpref,'/population',generation,'.csv')))
resdir=paste0('Results/Calibration/',resdirpref);dir.create(resdir)

objectives = c("mselog","logmse")
parameters = c("gravityDecay","countryGravityDecay","gammaSectors","gammaLinks","gammaOrigin","gammaDestination","finalTime")

res = res[res$evolution.samples>=20,]

paramnames = list(gammaOrigin = expression(gamma[F]),
                  gammaDestination = expression(gamma[T]),
                  gammaLinks=expression(gamma[W]),
                  gammaSectors=expression(gamma[S]),
                  gravityDecay=expression(d[ij]),
                  countryGravityDecay=expression(g[ij]),
                  finalTime=expression(t[f])
                  )

for(param in parameters){
  g=ggplot(res,aes_string(x="logmse",y="mselog",color=param,size='evolution.samples'))
  g+geom_point(alpha=0.6)+xlab("log(Mean Squared Error)")+ylab("Mean Squared Error on log")+
    scale_color_continuous(name=paramnames[[param]])+scale_size_continuous(name='Samples')+stdtheme
  ggsave(paste0(resdir,'/pareto_color',param,'.png'),width=20,height=18,units='cm')
}

###
bres = as.data.frame(res[res$mselog<5.0,])
for(param in parameters){
  w = bres$evolution.samples/sum(bres$evolution.samples)
  p = sum(bres[,param]*w)
  s = sqrt(sum(w*(bres[,param]-p)^2))
  show(paste0(param," = ",p,"+-",s))
}








