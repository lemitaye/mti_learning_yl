# Run from the project root. Aggregate metadata/availability only; production files are read-only.
# Rscript code/auxiliary/inventory_extension_rounds.R
local({
root <- getwd(); args<-commandArgs(trailingOnly=TRUE);out<-if(length(args))args[1]else 'temp/r6_r7_extension_feasibility';dir.create(out,recursive=TRUE,showWarnings=FALSE)
roots<-c('rawdata/yl_r6_phone_1st_2nd_3rd','rawdata/yl_r6_phone_4th_5th','rawdata/yl_r7')
files<-unlist(lapply(roots,list.files,pattern='\\.dta$',recursive=TRUE,full.names=TRUE))
files<-files[grepl('/et_',files)&!grepl('telephone|email',basename(files))]
protected<-c(files,list.files('data',full.names=TRUE), 'code/helpers/raw_data_catalog.R','rawdata/legacy_constructed_r1_r5/ethiopia_constructed.dta'); before<-tools::md5sum(protected); schemas<-list(); inv<-list()
for(f in files){d<-haven::read_dta(f);inv[[f]]<-data.frame(file=f,rows=nrow(d),variables=ncol(d));schemas[[f]]<-data.frame(file=f,variable=names(d),label=vapply(d,function(x){z<-attr(x,'label');if(is.null(z))'' else paste(z,collapse=' | ')},''),observed=vapply(d,function(x)sum(!is.na(x)),integer(1)),codes=vapply(d,function(x){z<-attr(x,'labels');paste(paste(unname(z),names(z),sep='='),collapse='; ')},''))}
write.csv(do.call(rbind,inv),file.path(out,'inventory.csv'),row.names=FALSE)
write.csv(do.call(rbind,schemas),file.path(out,'schema.csv'),row.names=FALSE)
d<-haven::read_dta('rawdata/yl_const_r1_r7/UKDA-9543-stata/stata/stata13/ethiopia_constructed_r1r7.dta')
vars<-c('inround','incall','in_phone_r7','chsex','chethnic','chlang','region','hghschoolgrade','preprim','agegr1','work_12','work_week','no_work_job','type_act','who_work','econ_sector','typemp','written_contract','zbfa','stunting_child','caredu','careage','caresex','hhsize','wi_new','hq_new','cd','elecq','ownlandhse','foodsec','levlread','literate')
a<-list();for(r in 6:7)for(ca in unique(d$call[d$round==r]))for(y in 0:1){ix<-d$round==r & d$yc==y;if(!is.na(ca))ix<-ix&!is.na(d$call)&d$call==ca;for(v in vars)a[[length(a)+1]]<-data.frame(round=r,call=ca,yc=y,records=sum(ix),variable=v,observed=sum(!is.na(d[[v]][ix])))}
write.csv(do.call(rbind,a),file.path(out,'constructed_availability.csv'),row.names=FALSE)

base<-rbind(read.csv('data/aa_samp.csv'),read.csv('data/non_aa_samp.csv'))
controls<-c('IMTI','E_is','chsex','zbfa','stunting','caredu_r1','careage_r1','caresex_r1','hhsize','wi_new','hq_new','cd_new','elecq_new','ownlandhse_r1','foodsec_r3')
primary<-files[grepl('et_(oc|yc)_covid([2345])?_arch[.]dta$|et_r7_(oc|yc)ch(_phone)?_arch[.]dta$',basename(files))]
counts<-list();regions<-list()
for(f in primary){x<-haven::read_dta(f);n<-names(x);id<-n[tolower(n)%in%c('childcode','childid')][1];if(tolower(id)=='childid'&&'childcode'%in%n)id<-'childcode';ids<-as.numeric(x[[id]])
rd<-if(grepl('yl_r7/',f))7 else 6
ca<-if(rd==7)NA else if(grepl('1st_call',f))1 else if(grepl('2nd_call',f))2 else if(grepl('3rd_call',f))3 else if(grepl('call_4',f))4 else 5
co<-if(grepl('et_yc_|et_r7_yc',f))1 else 0
baseline<-if(co==0)base else read.csv('data/ylsample_yc.csv'); ctl<-if(co==0)controls else setdiff(controls,'caresex_r1');j<-match(ids,baseline$child_id);q<-baseline[j,,drop=FALSE];ok<-if(co==0)!is.na(j)&!is.na(q$raw_lang)&q$raw_lang!=0 else !is.na(j)
z<-d[d$round==rd&d$yc==co,];if(rd==6)z<-z[z$call==ca,];k<-match(ids,as.numeric(sub('ET','',z$childid)))
flag<-if(rd==6)z$incall[k] else z$inround[k]
dt<-n[grepl('^dint',tolower(n))][1]; dates_x<-if(is.na(dt))as.Date(NA) else if(inherits(x[[dt]],'Date'))x[[dt]]else {v<-as.character(x[[dt]]); z<-as.Date(substr(v,1,10),format='%d/%m/%Y'); ix<-is.na(z);z[ix]<-as.Date(v[ix],format='%d-%b-%y');z}; valid_date<-!is.na(dates_x)&dates_x>=as.Date(if(rd==6)'2020-01-01' else '2023-01-01')&dates_x<=as.Date(if(rd==6)'2021-12-31' else '2024-12-31');dates<-if(any(valid_date))as.character(range(dates_x[valid_date]))else c(NA,NA)
counts[[f]]<-data.frame(file=f,round=rd,call=ca,yc=co,raw_records=nrow(x),unique_ids=length(unique(ids)),duplicate_ids=sum(duplicated(ids)),baseline_sample_overlap=sum(!is.na(j)),baseline_original_filter_eligible=sum(ok),baseline_controls_complete=sum(ok&complete.cases(q[ctl])),date_out_of_window=sum(!is.na(dates_x)&!valid_date),raw_present_constructed_flag0=sum(flag==0,na.rm=TRUE),raw_missing_in_constructed=sum(is.na(k)),date_min=dates[1],date_max=tail(dates,1))
rg<-n[grepl('^REGIONCOV[12345]$|^regionr7$',n,ignore.case=TRUE)];if(length(rg)){t<-as.data.frame(table(haven::as_factor(x[[rg[length(rg)]]]),useNA='ifany'));names(t)<-c('region','records');t$file<-f;regions[[f]]<-t}
}
write.csv(do.call(rbind,counts),file.path(out,'cross_section_counts.csv'),row.names=FALSE)
write.csv(do.call(rbind,regions),file.path(out,'raw_region_counts.csv'),row.names=FALSE)
docs<-unlist(lapply(roots,list.files,pattern='[.](pdf|rtf|zip|htm)$',recursive=TRUE,full.names=TRUE));write.csv(data.frame(file=docs,bytes=file.info(docs)$size),file.path(out,'documentation_inventory.csv'),row.names=FALSE)

x<-haven::read_dta('rawdata/yl_r6_phone_1st_2nd_3rd/UKDA-8678-stata/stata/stata13/2nd_call/et_oc_covid2_arch.dta');z<-d[d$round==6&d$call==2&d$yc==0,];j<-match(x$CHILDCODE,as.numeric(sub('ET','',z$childid)));cv<-do.call(rbind,lapply(grep('CNSNT',names(x),value=TRUE),function(v){q<-as.data.frame(table(raw_response=as.character(x[[v]]),constructed_incall=z$incall[j],useNA='ifany'));q$variable<-v;q[q$Freq>0,]}));write.csv(cv,file.path(out,'call2_consent_flag_check.csv'),row.names=FALSE)
modes<-do.call(rbind,lapply(c('oc','yc'),function(co){p<-file.path('rawdata/yl_r7/UKDA-9538-stata/stata/stata13/ethiopia_r7',paste0('et_r7_',co,'ch_arch.dta'));q<-sub('ch_arch','ch_phone_arch',p);a<-haven::read_dta(p);b<-haven::read_dta(q);data.frame(cohort=co,in_person=nrow(a),phone=nrow(b),overlap=length(intersect(a$childcode,b$childcode)))}));stopifnot(all(modes$overlap==0));write.csv(modes,file.path(out,'r7_mode_key_check.csv'),row.names=FALSE)
stopifnot(identical(before,tools::md5sum(protected)));write.csv(data.frame(file=names(before),md5=unname(before),unchanged=TRUE),file.path(out,'source_integrity.csv'),row.names=FALSE)
})
