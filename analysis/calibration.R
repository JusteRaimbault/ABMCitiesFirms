
setwd(paste0(Sys.getenv("CS_HOME"),'/UrbanDynamics/Models/ABMCitiesFirms'))

library(dplyr,warn.conflicts = F)
library(ggplot2)

source(paste0(Sys.getenv("CS_HOME"),'/Organisation/Models/Utils/R/plots.R'))


twores = FALSE
#resdirpref='CALIBRATION_GRID_20200305_092250/'
#resdirpref='CALIBRATION_20201128_215412/'
resdirpref = '20220519_071436_CALIBRATION_NOCOUNTRY_GRID'

generation='55000'

if(twores) res2dirpref='CALIBRATION_NOCOUNTRY_GRID_20200423_172109/'



res <- as_tibble(read.csv(paste0('openmole/calibration/',resdirpref,'/population',generation,'.csv')))
resdir=paste0('Results/Calibration/',resdirpref);dir.create(resdir)

minSamples = 20

res = res[res$evolution.samples>=minSamples,]

# add country gravity decay if not included in calibration (for genericity)
if(is.null(res[['countryGravityDecay']])) res[['countryGravityDecay']] = rep(0,nrow(res))

if (twores){
  res2 <- as.tbl(read.csv(paste0('openmole/calibration/',res2dirpref,'/population',generation,'.csv')))
  resdir2=paste0('Results/Calibration/',res2dirpref);dir.create(resdir2)
  res2 = res2[res2$evolution.samples>=minSamples,]
  allres = rbind(cbind(res,type=rep('full',nrow(res))),cbind(res2,type=rep('no countries',nrow(res2)),countryGravityDecay=rep(0,nrow(res2))))
}
  
objectives = c("mselog","logmse")
parameters = c("gravityDecay","countryGravityDecay","gammaSectors","gammaLinks","gammaOrigin","gammaDestination","finalTime")

paramnames = list(gammaOrigin = expression(gamma[O]),
                  gammaDestination = expression(gamma[D]),
                  gammaLinks=expression(gamma[W]),
                  gammaSectors=expression(gamma[S]),
                  gravityDecay=expression(d[0]),
                  countryGravityDecay=expression(c[0]),
                  finalTime=expression(t[f])
                  )

# zoom? no
#res = res[res$objective.logmse<36&res$objective.mselog<5.2,]

# full model
for(param in parameters){
  ggsave(
    ggplot(res,aes_string(x="objective.logmse",y="objective.mselog",color=param,size='evolution.samples'))+
      geom_point(alpha=0.6)+xlab(expression(epsilon[M]))+ylab(expression(epsilon[L]))+
        scale_color_continuous(name=paramnames[[param]])+scale_size_continuous(name='Samples')+stdtheme,
      file=paste0(resdir,'/pareto_color',param,'.png'),width=20,height=18,units='cm'
  )
}

# all

if (twores) {
  for(param in parameters){
    g=ggplot(allres,aes_string(x="logmse",y="mselog",color=param,size='evolution.samples',shape='type'))
    g+geom_point(alpha=0.6)++xlab(expression(epsilon[M]))+ylab(expression(epsilon[L]))+
    scale_color_continuous(name=paramnames[[param]])+scale_size_continuous(name='Samples')+stdtheme
    ggsave(paste0(resdir2,'/pareto_color',param,'.png'),width=20,height=18,units='cm')
  }
}
  
###
bres = as.data.frame(res[res$objective.mselog<5.0,])
for(param in parameters){
  w = bres$evolution.samples/sum(bres$evolution.samples)
  p = sum(bres[,param]*w)
  s = sqrt(sum(w*(bres[,param]-p)^2))
  show(paste0(param," = ",p,"+-",s))
}




#####
# Macro calibration


resdirpref='CALIBRATION_MACRO_GRID_20220331_134531/'
generation='8000'
resdir=paste0('Results/Calibration/',resdirpref);dir.create(resdir)

res <- as_tibble(read.csv(paste0('openmole/calibration/',resdirpref,'/population',generation,'.csv')))

res$relErrorMetropolisation = res$objective.errorMetropolisation / 0.96
res$relErrorInternationalisation = res$objective.errorInternationalisation / 0.32

objectives = c("relErrorMetropolisation","relErrorInternationalisation")
parameters = c("gravityDecay","countryGravityDecay","gammaSectors","gammaLinks","gammaOrigin","gammaDestination","finalTime")

paramnames = list(gammaOrigin = expression(gamma[O]),
                  gammaDestination = expression(gamma[D]),
                  gammaLinks=expression(gamma[W]),
                  gammaSectors=expression(gamma[S]),
                  gravityDecay=expression(d[0]),
                  countryGravityDecay=expression(c[0]),
                  finalTime=expression(t[f])
)



for(param in parameters){
  g=ggplot(res[res$evolution.samples>=10,],aes_string(x="relErrorMetropolisation",y="relErrorInternationalisation",color=param,size='evolution.samples'))
  g+geom_point(alpha=0.6)+xlab("Relative error on Metropolisation")+ylab("Relative error on Internationalisation")+
    scale_color_continuous(name=paramnames[[param]])+scale_size_continuous(name='Samples')+stdtheme
  ggsave(paste0(resdir,'/pareto_color',param,'.png'),width=20,height=18,units='cm')
}



