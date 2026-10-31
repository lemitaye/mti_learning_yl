# Aggregate comparison only: no child IDs or participant records are exported.
# Candidate renamed fields are diagnostics, not an approved harmonization rule.
local({
  args <- commandArgs(trailingOnly=TRUE)
  source('scripts/raw_data_catalog.R',local=TRUE)
  old <- if(length(args)>0) args[1] else raw_data_files[['constructed_legacy']]
  new <- if(length(args)>1) args[2] else raw_data_files[['constructed_r1_r7']]
  a<-haven::read_dta(old);b<-haven::read_dta(new);b<-b[b$round<=5,]
  key<-function(d)paste(d$childid,d$round,sep=':')
  stopifnot(!anyDuplicated(key(a)),!anyDuplicated(key(b)),setequal(key(a),key(b)))
  b<-b[match(key(a),key(b)),]
  aliases<-c(underweight='underweight_child',stunting='stunting_child',thinness='thinness_child',
    hghgrade='hghschoolgrade',sv_new='sv',cd_new='cd',drwaterq_new='drwaterq',
    toiletq_new='toiletq',elecq_new='elecq',cookingq_new='cookingq')
  output<-list()
  for(v in names(a)){
    w<-if(v%in%names(b))v else if(v%in%names(aliases))aliases[[v]]else NA_character_
    for(rd in 1:5){
      if(is.na(w)){output[[length(output)+1]]<-data.frame(old=v,new=NA,round=rd,
        mapping='absent',changed=NA,missingness_changed=NA);next}
      x<-a[[v]][a$round==rd];y<-b[[w]][a$round==rd];attributes(x)<-NULL;attributes(y)<-NULL
      valid<-!is.na(x)&!is.na(y)
      same<-is.na(x)&is.na(y)
      if(is.numeric(x)&&is.numeric(y))same[valid]<-abs(x[valid]-y[valid])<=1e-7*pmax(1,abs(x[valid]),abs(y[valid])) else same[valid]<-x[valid]==y[valid]
      output[[length(output)+1]]<-data.frame(old=v,new=w,round=rd,
        mapping=if(v==w)'same_name'else'candidate_alias',changed=sum(!same),
        missingness_changed=sum(is.na(x)!=is.na(y)))
    }
  }
  dir.create('output/raw_data_update_audit',recursive=TRUE,showWarnings=FALSE)
  write.csv(do.call(rbind,output),'output/raw_data_update_audit/constructed_differences_by_round.csv',row.names=FALSE)
  message('Historical keys match; differences reported without substituting new values into current models.')
})
