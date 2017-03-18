#Maximum Overlap Intervals.
maxInterval=function(subdf) {
  if(nrow(subdf)<=2) return(nrow(subdf))
  relevant=lapply(strsplit(subdf$w,split = " "),function(v) v[v%in%subdf$id])
  #n id in w
  indic=unlist(lapply(subdf$id, function(v) sum(grepl(v,relevant))))
  for(i in nrow(subdf):2) {
    if(sum(indic>=i)>=i) return(i)
  }
}
require("data.table")
df=data.frame(id=seq(10,80,by=10),anest=c("baker","baker",rep("dow",6)), start=c("08:00","09:00","09:00","08:00","10:00","12:30","13:30","18:00"),end=c("11:00","13:00","15:30","13:30","11:30","13:30","14:30","19:00"),stringsAsFactors = FALSE)

anest=df$anest
appStart=as.numeric(gsub(":","",df$start))
appEnd=as.numeric(gsub(":","",df$end))

subseted=mapply(FUN=function(u,v,w) subset(df,!((u>=appEnd)|(v<=appStart)) & anest==w) ,appStart,appEnd,anest,SIMPLIFY = FALSE)
w=do.call(rbind,lapply(subseted,function(v) paste(v$id,collapse=" ")))
result=data.frame(df,w,stringsAsFactors = FALSE)

vec=lapply(subseted,function(v) subset(result,result$id %in% v$id))
s=unlist(lapply(vec,maxInterval))
result=data.frame(df,s,w,stringsAsFactors = FALSE)
