

directedmodularity<-function(membership,adjacency){
  m=sum(adjacency)
  kout=rowSums(adjacency);kin=colSums(adjacency)
  res = 0;k=length(unique(membership))
  for(c in unique(membership)){
    #if(c%%100==0){show(c/k)}
    inds=which(membership==c)
    res = res + sum(adjacency[inds,inds]) - sum(kin[inds])*sum(kout[inds])/m 
    #gc()
  }
  return(res/m)
}


fitDistrPowerLaw<-function(x,xlab='x',ylab='CDF',file='fitDistrPowerLaw.png'){
  degpowerlaw = conpl$new(x)
  est = estimate_xmin(degpowerlaw,xmax = max(x))
  degpowerlaw$setXmin(est)
  png(file,width=15,height=15,units='cm',res=300)
  plot(degpowerlaw,xlab=xlab,ylab=ylab);lines(degpowerlaw, col=2, lwd=2)
  degln = conlnorm$new(x)
  est = estimate_xmin(degln)
  degln$setXmin(est)
  lines(degln, col=3, lwd=2)
  text(x=min(x),y=0.005,adj=c(0,0),labels = paste0('Log-normal: mu=',round(degln$pars[1],digits=2),', sigma=',round(degln$pars[2],digits=2),', xmin=',round(degln$xmin,digits=2)),cex=0.6)
  text(x=min(x),y=0.003,adj=c(0,0),labels = paste0('Power law: alpha=',round(degpowerlaw$pars[1],digits=2),', xmin=',round(degpowerlaw$xmin,digits=2)),cex=0.6)
  dev.off()
  return(list(powerlaw=degpowerlaw,ln=degln))
}



