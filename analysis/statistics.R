
setwd(paste0(Sys.getenv("CS_HOME"),'/UrbanDynamics/Models/ABMCitiesFirms'))

library(dplyr)

load('Data/firms/amadeus_stataggrlinks.RData')


d = stataggrlinks[stataggrlinks$distance>0&stataggrlinks$weight>0&stataggrlinks$from_turnover>0&stataggrlinks$to_turnover>0,]
# write.csv(as_tibble(d),file=paste0(Sys.getenv("CS_HOME"),'/Teaching/2022-AnalyseSpatiale/rawdata/firmsnetwork/links.csv'))
#

## Statistical models
# Model 1: OLS - distance only
model1 = lm(data=d,log(weight)~log(distance))
summary(model1)
AIC(model1)
mean(model1$residuals^2)
#mean((log(d$weight)-model1$fitted.values)^2) # same as using residuals

# Model 2: country fixed effects
model2 = lm(data=d,log(weight)~log(distance)+from_country+to_country)
summary(model2)
AIC(model2)
mean(model2$residuals^2)

model2b =  lm(data=d,log(weight)~log(distance)+interaction(from_country,to_country))
summary(model2b)
head(summary(model2b)$coefficients)
AIC(model2b)
(length(which(summary(model2b)$coefficients[,4]<0.1))-2) / (nrow(summary(model2b)$coefficients) - 2)
mean(model2b$residuals^2)

##
# model 3: add O/D
model3 <- lm(data=d,log(weight)~log(distance)+log(from_turnover)+log(to_turnover))
summary(model3)
AIC(model3)
mean(model3$residuals^2)

##
# model 4: everything: !  include proximity (to justify multiple factors in the ABM)

model4a = lm(data=d,log(weight)~log(distance)+log(from_turnover)+log(to_turnover)+log(sim))
summary(model4a)
AIC(model4a)
mean(model4a$residuals^2)

# - fixed effects similar to constrained spatial interaction models! (check practical 2-3 spInt)

# country - ! this is not a fixed effect, should construct dummies to do it by hand
# actually it is - factors are automatically transformed to dummies
model4 = lm(data=d,log(weight)~log(distance)+log(from_turnover)+log(to_turnover)+log(sim)+from_country+to_country)
summary(model4)
AIC(model4)
mean(model4$residuals^2)


# country pair
model4b = lm(data=d,log(weight)~log(distance)+log(from_turnover)+log(to_turnover)+log(sim)+interaction(from_country,to_country))
summary(model4b)
head(summary(model4b)$coefficients)
AIC(model4b)
(length(which(summary(model4b)$coefficients[,4]<0.1))-5) / (nrow(summary(model4b)$coefficients) - 5)
mean(model4b$residuals^2)


# model 5
#  poisson models
d$intweight = floor(d$weight)
poisson <- glm(data=d,intweight~log(distance)+log(from_turnover)+log(to_turnover)+log(sim)+from_country+to_country,family = poisson(link='log'))
summary(poisson)
#mean((d$intweight - fitted(poisson))^2)
#mean((log(d$intweight) - log(fitted(poisson)))^2)
# 'hand' rsquared for the poisson model
1 - sum((d$intweight - fitted(poisson))^2) / sum((d$intweight - mean(d$intweight) )^2)
mean((log(d$intweight)-log(poisson$fitted))^2)


poisson2 <- glm(data=d,intweight~log(distance)+log(from_turnover)+log(to_turnover)+log(sim)+interaction(from_country,to_country),family = poisson(link='log'))
summary(poisson2)
head(summary(poisson2)$coefficients)
AIC(poisson2)
1 - sum((d$intweight - fitted(poisson2))^2) / sum((d$intweight - mean(d$intweight) )^2)
#1 - sum((log(d$intweight) - log(fitted(poisson2)))^2) / sum((log(d$intweight) - mean(log(d$intweight)))^2)
(length(which(summary(poisson2)$coefficients[,4]<0.1))-5) / (nrow(summary(poisson2)$coefficients) - 5)
mean((log(d$intweight)-log(poisson2$fitted))^2)

# note that origin/destination constrained models do not make sense here as we have the proportion of ownership.

##
# Zero-inflated Poisson / Hurdle regression
#
#https://en.wikipedia.org/wiki/Zero-inflated_model https://cran.r-project.org/web/packages/pscl/pscl.pdf

library(pscl)
#library(bit64)
source('analysis/functions.R')

load('Data/firms/amadeus_aggregnw.RData')
load('Data/firms/amadeus_saggregnw.RData')

dz = stataggrlinks[stataggrlinks$distance>0,]
# find missing pairs
dz$id = paste0(dz$from_fua,dz$to_fua)
fuasids = unique(saggrnodes$fua)
dmiss=data.frame(from_fua = c(sapply(fuasids,function(i){rep(i,length(fuasids))})),to_fua =rep(fuasids,length(fuasids)))
dmiss$id = paste0(dmiss$from_fua,dmiss$to_fua)
dmiss = dmiss[dmiss$from_fua!=dmiss$to_fua&!(dmiss$id%in%dz$id),]
dmiss=left_join(dmiss,distsdf,by=c("from_fua"="Var1","to_fua"="Var2"));names(dmiss)[4]<-"distance"
dmiss = left_join(dmiss,mprox,by=c('from_fua'='Var1','to_fua'='Var2'));names(dmiss)[5]<-"sim"
dmiss = left_join(dmiss,aggrnodes[,c("fua","turnover","fuacountry")],by=c('from_fua'='fua'));names(dmiss)[6:7]<-c("from_turnover","from_country")
dmiss = left_join(dmiss,aggrnodes[,c("fua","turnover","fuacountry")],by=c('to_fua'='fua'));names(dmiss)[8:9]<-c("to_turnover","to_country")
dmiss$weight = rep(0,nrow(dmiss))

dz = bind_rows(dz,as.tbl(dmiss[,names(dz)]))

# tests
#d$intweight=as.integer(floor(d$weight)) # numbers too large for ints
#d$intweight = as.integer64(floor(d$weight))
#d$intweight = round(ifelse(d$weight==0,0,log(d$weight)))

#write.csv(d,file=paste0(Sys.getenv("CS_HOME"),'/Teaching/2022-AnalyseSpatiale/rawdata/firmsnetwork/links-with-zeros.csv')) # ! 34Mo! -> add by hand if needed
# write.csv(as_tibble(saggrnodes)[-ncol(saggrnodes)],file=paste0(Sys.getenv("CS_HOME"),'/Teaching/2022-AnalyseSpatiale/rawdata/firmsnetwork/cities.csv'))


# min(dz$weight[dz$weight>0]) = 60 -> /10 keeps all non zero values after rounding, and provides integers needed for the models
dz$intweight = round(dz$weight/10)

#min(dz$sim[dz$sim>0])=0.0001680434 # -> add much smaller epsilon to zeros to take log
dz$sim[dz$sim==0]=1e-8
# taking log increases zeroinfl R2 from 0.15 to 0.16

#zinflpoisson <- zeroinfl(data = d, intweight~log(distance)+log(from_turnover)+log(to_turnover)+log(sim)+from_country+to_country, dist="poisson")
zinflpoisson <- pscl::zeroinfl(data = dz, intweight~log(distance)+log(from_turnover)+log(to_turnover)+log(sim) | 1,
                               dist="poisson")
summary(zinflpoisson)
1 - sum((dz$intweight - fitted(zinflpoisson))^2) / sum((dz$intweight - mean(dz$intweight) )^2)

zinflpoisson2 <- pscl::zeroinfl(data = dz, intweight~log(distance)+log(from_turnover)+log(to_turnover)+log(sim), dist="poisson")
summary(zinflpoisson2)
1 - sum((dz$intweight - fitted(zinflpoisson2))^2) / sum((dz$intweight - mean(dz$intweight) )^2)
mean((dz$intweight-zinflpoisson2$fitted)^2)

# taking a zero link poisson instead of binomial sligthly increases R2 but negligible
hurdle1 <- pscl::hurdle(data = dz, intweight~log(distance)+log(from_turnover)+log(to_turnover)+log(sim)|1,
                        dist="poisson", zero.dist = "poisson")
summary(hurdle1)
1 - sum((dz$intweight - fitted(hurdle1))^2) / sum((dz$intweight - mean(dz$intweight) )^2)

hurdle2 <- pscl::hurdle(data = dz, intweight~log(distance)+log(from_turnover)+log(to_turnover)+log(sim),
                        dist="poisson", zero.dist = "poisson")
summary(hurdle2)
1 - sum((dz$intweight - fitted(hurdle2))^2) / sum((dz$intweight - mean(dz$intweight) )^2)

mean((dz$intweight-hurdle2$fitted)^2)
AIC(hurdle2)

# with country fixed effects? -> pb solving, singular
hurdle3 <- pscl::hurdle(data = dz, intweight~log(distance)+log(from_turnover)+log(to_turnover)+log(sim)+from_country+to_country,
                        dist="poisson", zero.dist = "poisson")
summary(hurdle3)
1 - sum((dz$intweight - fitted(hurdle3))^2) / sum((dz$intweight - mean(dz$intweight) )^2)
# 0.399 but not valid (NA estim std errors, pvals)



##
# stratified regression
library(nlme)
lmList(data=d,(log(weight)~log(distance)+log(from_turnover)+log(to_turnover) | from_country))

##
# fixed effect with the plm library
library(plm) # ! for panel data - we have no time here - country as individual, fua as realizations
# see https://rstudio-pubs-static.s3.amazonaws.com/372492_3e05f38dd3f248e89cdedd317d603b9a.html#1_preliminaries
d$fromto = paste0(d$from_country,d$to_country)
plinks = plm::pdata.frame(d,index = c("fromto"))
fixed <- plm(log(weight)~log(distance)+log(from_turnover)+log(to_turnover), data=plinks, model="within", effect="individual")
summary(fixed)
fixef(fixed)
pFtest(fixed, ols)

# export fixed effects for ABM
dcult = - fixef(fixed)
allcountries = unique(c(as.character(d$from_country),as.character(d$to_country)))
origs=c();dests=c();dcults=c()
for(ocountry in allcountries){
  for(dcountry in allcountries){
    dcults=append(dcults,dcult[paste0(ocountry,dcountry)])
    origs=append(origs,ocountry);dests=append(dests,dcountry)
  }
}
dcultdf = data.frame(from_country = origs,to_country=dests,distance=dcults)
dcultdf$distance[is.na(dcultdf$distance)]=1000 # force no links for couples of countries where actually no link
# country names as indices
dcultdf$from_country=as.numeric(dcultdf$from_country) # actually 29 levels - ok
dcultdf$to_country=as.numeric(dcultdf$to_country)
write.table(dcultdf,file='model_nl6/setup/fixedeffects.csv',row.names = F,sep=";",quote = F)
#dcultdf <- read.table(file='model_nl6/setup/fixedeffects.csv',sep=";",header=T)
#dcultdf$distance[dcultdf$distance==1000]<-rep(NA,length(which(dcultdf$distance==1000))) # put NA again for summary stats
#summary(as.numeric(dcultdf$distance))

###
# random effects?
random <- plm(log(weight)~log(distance)+log(from_turnover)+log(to_turnover), data=plinks, model="random")
summary(random)
ranef(random)
phtest(fixed, random)


# -> links by sector ? NACE_origin, NACE_destination - aggregate at the level of broad sectors

