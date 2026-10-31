# Run after installing the refreshed raw-data collection.
source('temp/raw_data_catalog.R')
stopifnot(all(file.exists(raw_data_files)),
          raw_data_files[['constructed_legacy']] != raw_data_files[['constructed_r1_r7']])
# Model source files must use the catalog rather than hard-coded old folders.
for (p in c('code/01_extract_clean.R','code/02_cleaning_yc.R')) {
  s<-paste(readLines(p),collapse='\n')
  stopifnot(grepl('source("temp/raw_data_catalog.R")',s,fixed=TRUE),
            !grepl('rawdata/Round_',s,fixed=TRUE),
            !grepl('rawdata/Rounds_1-5',s,fixed=TRUE),
            grepl('raw_data_files[["constructed_legacy"]]',s,fixed=TRUE))
}
cat('PASS: all canonical input paths exist; legacy/latest constructed files remain separate; active extraction scripts use the catalog.\n')
