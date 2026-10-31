# Read-only dataset comparison; metadata differences are reported separately.
library(haven)
args <- commandArgs(trailingOnly=TRUE)
if(length(args)!=4) stop("Usage: Rscript scripts/audit_raw_release.R OLD_ROOT NEW_ROOT CROSSWALK_CSV REPORT_CSV")
m <- read.csv(args[3]); r <- args[2]
m$old <- file.path(args[1], sub("^raw_data/", "", m$old))
values<-function(d)lapply(d,function(x){attributes(x)<-NULL;x})
res<-vector('list',nrow(m))
for(i in seq_len(nrow(m))){
 a<-read_dta(m$old[i]);b<-read_dta(file.path(r,m$new[i]))
 res[[i]]<-data.frame(m[i,],old_rows=nrow(a),new_rows=nrow(b),old_cols=ncol(a),new_cols=ncol(b),identical_values=identical(values(a),values(b)),identical_metadata=identical(lapply(a,attributes),lapply(b,attributes)))
 if(i%%50==0){write.csv(do.call(rbind,res[seq_len(i)]),args[4],row.names=FALSE);cat('Checked',i,'of',nrow(m),'\n');flush.console()}
}
z<-do.call(rbind,res);write.csv(z,args[4],row.names=FALSE);print(table(z$identical_values))
