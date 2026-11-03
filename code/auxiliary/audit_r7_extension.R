# Run from the project root: Rscript code/auxiliary/audit_r7_extension.R
# Aggregate feasibility counts only; no estimation or production-data writes.
local({
source('code/helpers/raw_data_catalog.R',local=TRUE)
protected<-c(unname(raw_data_files[c('constructed_legacy','constructed_r1_r7')]),list.files('data',full.names=TRUE),'code/helpers/raw_data_catalog.R');hashes<-tools::md5sum(protected)
args<-commandArgs(trailingOnly=TRUE);out<-if(length(args))args[1]else 'output/r7_extension_reassessment';dir.create(out,recursive=TRUE,showWarnings=FALSE)
a<-haven::read_dta(raw_data_files[['constructed_legacy']]);c<-haven::read_dta(raw_data_files[['constructed_r1_r7']]);core<-list();audit<-list();coverage<-list()
valid<-function(x){!is.na(x)&!as.numeric(x)%in%c(-99,-88,-79,-77,77,79,88,99)}
for(co in c('oc','yc')) for(mode in c('in_person','phone')){
f<-file.path(raw_data_files[['round7_ethiopia']],paste0('et_r7_',co,'ch',if(mode=='phone')'_phone'else'','_arch.dta'));d<-haven::read_dta(f);d$mode_audit<-mode;d$cohort_audit<-co;core[[paste(co,mode)]]<-d
scr<-c('lst12frmr7','lst12nhsr7','lst12bsnr7'); wk<-c('lstwkfrmr7','lstwknhsr7','lstwkbsnr7')
for(v in c(scr,wk,'curjbr7','typwrk7r7','typwrk12r7','employer7r7','cntrctwk7r7','fdsec00r7','electr7','wallr7','roofr7','floorr7','flcookr7','regionr7')){
coverage[[length(coverage)+1]]<-data.frame(cohort=co,mode=mode,records=nrow(d),variable=v,present=v%in%names(d),observed=if(v%in%names(d))sum(!is.na(d[[v]]))else 0,non_special=if(v%in%names(d))sum(valid(d[[v]]))else 0)}
ann_yes<-rowSums(sapply(d[scr],function(x)x==1),na.rm=TRUE)>0;ann_no<-rowSums(sapply(d[scr],function(x)x==0),na.rm=TRUE)==3
w<-sapply(seq_along(wk),function(j){x<-as.numeric(d[[wk[j]]]);p<-as.numeric(d[[scr[j]]]);x[is.na(x)&p==0]<-0;x});wk_yes<-rowSums(w==1,na.rm=TRUE)>0;wk_no<-rowSums(w==0,na.rm=TRUE)==3
act<-as.numeric(d$typwrk7r7); annual<-if('typwrk12r7'%in%names(d))as.numeric(d$typwrk12r7)else rep(NA_real_,nrow(d))
audit[[length(audit)+1]]<-data.frame(cohort=co,mode=mode,records=nrow(d),annual_work_yes=sum(ann_yes),annual_work_no=sum(ann_no),weekly_work_yes=sum(wk_yes),weekly_work_no=sum(wk_no),weekly_screen_unresolved=sum(!wk_yes&!wk_no),weekly_valid_activity=sum(act%in%1:22),weekly_worker_activity_missing=sum(wk_yes&!act%in%1:22),annual_valid_activity=sum(annual%in%1:22),annual_worker_activity_missing=sum(ann_yes&!annual%in%1:22),annual_nonworker_activity=sum(ann_no&annual%in%1:22))
z<-c[c$round==7&c$yc==as.integer(co=='yc'),];j<-match(d$childcode,as.numeric(sub('ET','',z$childid)));for(v in c('hhsize','wi_new','hq_new','wi','hq','sv','cd','elecq','toiletq','drwaterq','cookingq','foodsec','ownhouse'))coverage[[length(coverage)+1]]<-data.frame(cohort=co,mode=mode,records=nrow(d),variable=paste0('constructed:',v),present=v%in%names(z),observed=if(v%in%names(z))sum(!is.na(z[[v]][j]))else 0,non_special=if(v%in%names(z))sum(valid(z[[v]][j]))else 0)
}
write.csv(do.call(rbind,audit),file.path(out,'outcome_coverage.csv'),row.names=FALSE);write.csv(do.call(rbind,coverage),file.path(out,'control_coverage.csv'),row.names=FALSE)

base_oc<-rbind(read.csv('data/aa_samp.csv'),read.csv('data/non_aa_samp.csv'));base_yc<-read.csv('data/ylsample_yc.csv')
ns<-list();hhchecks<-list();provenance<-list()
for(nm in names(core)){
d<-core[[nm]];co<-d$cohort_audit[1];mode<-d$mode_audit[1];b<-if(co=='oc')base_oc else base_yc;j<-match(d$childcode,b$child_id);b<-b[j,,drop=FALSE]
elig<-!is.na(j);if(co=='oc')elig<-elig&!is.na(b$raw_lang)&b$raw_lang!=0
rpath<-file.path(raw_data_files[['round7_ethiopia']],paste0('et_r7_',co,'hh',if(mode=='phone')'_phone'else'','_arch.dta'));ro<-haven::read_dta(rpath)
stopifnot(!anyDuplicated(paste(ro$childcode,ro$memidr7)))
hh<-tapply(as.numeric(ro$livhser7==1),ro$childcode,sum,na.rm=TRUE);h<-as.numeric(hh[as.character(d$childcode)])
complete<-if('cmplthhr7'%in%names(d))as.numeric(d$cmplthhr7)else rep(NA,nrow(d));h[is.na(complete)|complete!=1]<-NA
hhchecks[[nm]]<-data.frame(cohort=co,mode=mode,roster_rows=nrow(ro),participants=nrow(d),roster_complete_yes=sum(complete==1,na.rm=TRUE),reconstructed_hhsize_observed=sum(!is.na(h)),minimum=min(h,na.rm=TRUE),maximum=max(h,na.rm=TRUE))
z<-c[c$round==7&c$yc==as.integer(co=='yc'),];k<-match(d$childcode,as.numeric(sub('ET','',z$childid)));z<-z[k,]
cur<-b;cur$hhsize<-h;cur$wi_new<-as.numeric(z$wi);cur$hq_new<-as.numeric(z$hq);cur$cd_new<-as.numeric(z$cd);cur$elecq_new<-as.numeric(z$elecq)
if(mode=='phone'){cur$wi_new<-cur$hq_new<-cur$cd_new<-cur$elecq_new<-NA_real_}
common<-b;common$hhsize<-h;common$foodsec_r3<-as.numeric(d$fdsec00r7);common$foodsec_r3[!common$foodsec_r3%in%1:4]<-NA;curfood<-cur;curfood$foodsec_r3<-as.numeric(d$fdsec00r7);curfood$foodsec_r3[!curfood$foodsec_r3%in%1:4]<-NA
screen<-as.matrix(d[c('lst12frmr7','lst12nhsr7','lst12bsnr7')]);ann0<-rowSums(screen==0,na.rm=TRUE)==3
w<-sapply(seq_len(3),function(v){q<-as.numeric(d[[c('lstwkfrmr7','lstwknhsr7','lstwkbsnr7')[v]]]);q[is.na(q)&screen[,v]==0]<-0;q});wk0<-rowSums(w==0,na.rm=TRUE)==3
ctl<-c('IMTI','E_is','chsex','zbfa','stunting','caredu_r1','careage_r1','caresex_r1','hhsize','wi_new','hq_new','cd_new','elecq_new','ownlandhse_r1','foodsec_r3');if(co=='yc')ctl<-setdiff(ctl,'caresex_r1')
for(win in c('annual','weekly_job')){
act<-if(win=='annual'){if('typwrk12r7'%in%names(d))as.numeric(d$typwrk12r7)else rep(NA_real_,nrow(d))}else as.numeric(d$typwrk7r7)
act[act%in%21]<-1;act[act%in%22]<-4 # label-based aliases in R7 for crop/livestock self-employment
known0<-if(win=='annual')ann0 else wk0 & (ann0 | (!is.na(d$curjbr7)&d$curjbr7==0))
for(y in c('salary','wage')){
positive<-if(y=='salary')13 else c(5,12,13);excl<-if(y=='salary')c(8,9,10,11,19)else c(1,2,3,4,8,9,10,11,19)
yv<-rep(NA_real_,nrow(d));yv[known0]<-0;yv[act%in%1:22]<-0;yv[act%in%excl]<-NA;yv[act%in%positive]<-1
if(mode=='phone'&&win=='annual')yv[]<-NA_real_ # no annual job type in phone core: no comparable salary/wage outcome, including denominators
for(group in c('non_Addis','excluding_Amhara','Addis')){
g<-elig & !is.na(b$region)&if(group=='Addis')b$region=='Addis Ababa'else if(group=='excluding_Amhara')!b$region%in%c('Addis Ababa','Amhara')else b$region!='Addis Ababa'
ct<-c(ctl,if(group=='Addis')'entype_r4');for(spec in c('historical_controls','r7_household_controls','r7_household_plus_food','common_current_hh_food')){
dat<-switch(spec,historical_controls=b,r7_household_controls=cur,r7_household_plus_food=curfood,common_current_hh_food=common)
ok<-g & !is.na(yv) & complete.cases(dat[ct]);ns[[length(ns)+1]]<-data.frame(cohort=co,mode=mode,window=win,outcome=y,group=group,specification=spec,eligible=sum(g),outcome_observed=sum(g&!is.na(yv)),complete_cases=sum(ok),instrument_distinct=length(unique(dat$E_is[ok])),policy_regions=length(unique(dat$region[ok])))
}
}
}
}
ix<-!is.na(z$wi)&!is.na(z$hq)&!is.na(z$sv)&!is.na(z$cd);provenance[[nm]]<-data.frame(cohort=co,mode=mode,wi_identity_paired=sum(ix),wi_identity_mismatches=sum(abs(z$wi[ix]-(z$hq[ix]+z$sv[ix]+z$cd[ix])/3)>1e-6),raw_electricity_present='electr7'%in%names(d),constructed_electricity_observed=sum(!is.na(z$elecq)),constructed_hhsize_observed=sum(!is.na(z$hhsize)))
}
write.csv(do.call(rbind,ns),file.path(out,'candidate_estimation_counts.csv'),row.names=FALSE);write.csv(do.call(rbind,hhchecks),file.path(out,'roster_reconstruction_check.csv'),row.names=FALSE);write.csv(do.call(rbind,provenance),file.path(out,'index_provenance_checks.csv'),row.names=FALSE)

bridge<-do.call(rbind,lapply(c('wi','hq'),function(v){z<-c[match(paste(a$childid,a$round),paste(c$childid,c$round)),];i<-a$round==5&!is.na(a[[paste0(v,'_new')]])&!is.na(z[[v]]);data.frame(legacy_variable=paste0(v,'_new'),new_release_variable=v,round=5,paired=sum(i),mismatches=sum(abs(a[[paste0(v,'_new')]][i]-z[[v]][i])>1e-6))}));write.csv(bridge,file.path(out,'legacy_index_definition_bridge.csv'),row.names=FALSE)

f5<-haven::read_dta(raw_data_files[['r5_oc_activity']]);f7<-core[['oc in_person']];labs<-list(R5_ACTR5=attr(f5$ACTR5,'labels'),R7_annual=attr(f7$typwrk12r7,'labels'),R7_weekly=attr(f7$typwrk7r7,'labels'));cl<-do.call(rbind,lapply(names(labs),function(n){x<-labs[[n]];data.frame(source=n,code=unname(x),label=names(x))}));write.csv(cl,file.path(out,'activity_label_crosswalk.csv'),row.names=FALSE)
stopifnot(identical(hashes,tools::md5sum(protected)));write.csv(data.frame(path=names(hashes),md5=unname(hashes),unchanged=TRUE),file.path(out,'production_integrity.csv'),row.names=FALSE)
})
