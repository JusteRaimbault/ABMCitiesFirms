
library(dplyr)

datadir = 'data3'

alldata=data.frame()
for(f in list.files(datadir)){
  show(f)
  d <- as.tbl(read.table(file=paste0(datadir,'/',f),header=T,sep="\t",fileEncoding='UTF-16',stringsAsFactors=F,quote="\""))
  # direct way
  currentid = ""
  for(i in 1:nrow(d)){
    if(i%%1000==0){show(i)}
    id = d[i,3]
    if(nchar(id)>0){currentid = id}
    d[i,3] = currentid
  }
  names(d)<-c("row","name","id","branch_name","city","postcode","country","lat","lon","employees","description")

  d[is.na(d)] = "NA"

  if(nrow(alldata)==0){alldata=as.data.frame(d)[,c(-1,-2)]}
  else{alldata = rbind(alldata,as.data.frame(d)[,c(-1,-2)])}
}

write.table(alldata,file='processed/FAME_branches.tsv',sep="\t",quote=T,row.names=F,col.names=T)

