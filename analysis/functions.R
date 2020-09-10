

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


map<- function(data,var,sizevar,filename,discrete=FALSE,legendtitle=NULL,legendsizetitle=NULL,xlim=c(-130,150),ylim=c(-50, 60),width=30,height=12){
  #WorldData <- map_data('world') %>% filter(region != "Antarctica") %>% fortify
  #sizes = log10(data[[sizevar]]);sizes = (sizes - min(sizes,na.rm = T)) / (max(sizes,na.rm = T) - min(sizes,na.rm = T))
  #data[['sizes']]=sizes
  g=ggplot()+
    #geom_map(data = WorldData, map = WorldData,aes(group = group, map_id=region),fill = "white", colour = "#7f7f7f", size=0.1) + 
    geom_sf(data=countries,fill = "white", colour = "#7f7f7f", size=0.1)+
    geom_point(data=data,aes_string(x='X',y='Y',color=var,size=sizevar),alpha=0.5)+
    #scale_size_area(name=ifelse(is.null(legendsizetitle),sizevar,legendsizetitle))+#,trans="log10")+
    scale_size(name=ifelse(is.null(legendsizetitle),sizevar,legendsizetitle),range=c(1,8))+#,trans="log10")+
    #geom_map(data = areasmorph, map=WorldData,
    #         aes(fill=moran2015),#, map_id=region),
    #         colour="#7f7f7f", size=0.5) +
    #coord_map("mollweide")+ # coord_map("rectangular",lat0=lat0, xlim=xlim, ylim=ylim)
    #coord_sf("rectangular",xlim=xlim, ylim=ylim)+
    #scale_fill_continuous(low="thistle2", high="darkred", guide="colorbar") +
    theme_bw()+xlab("")+ylab("")+
    xlim(xlim)+ylim(ylim)+theme(axis.text = element_blank(),axis.ticks = element_blank())
  #scale_y_continuous(limits=ylim,breaks=c(),labels = c()) +
  #scale_x_continuous(limits=xlim,breaks=c(),labels = c())
  if(discrete){
    g+scale_color_discrete(name=ifelse(is.null(legendtitle),var,legendtitle))
  }else{
    g+scale_color_distiller(palette = 'Spectral',na.value ='white',name=ifelse(is.null(legendtitle),var,legendtitle))
  }
  ggsave(filename = filename,width=width,height=height,units='cm',dpi = 600)
}




