#this file concatenates all csvs into a master csv

setwd('/ntfsl/Dropbox/Grassroots Research Group')
#setwd('/Users/mcandocia/Dropbox/Grassroots Research Group')
source('/ntfsl/workspace/GrassrootsAnalytics/LeaderReport/fabricate_report.r')
library(dplyr)

subdir = 'Raw Data DO NOT SHARE'
csv_files = dir(subdir)

data_list = list()

for (file in csv_files){
  f = read.csv(paste0(subdir,'/',file), stringsAsFactors=FALSE)
  for (col in names(f)){
    if (is.integer(f[,col]))
      f[,col] = as.character(f[,col])
  }
  f$ORIGINAL_FILE = file
  data_list[[file]] = f
}



master_df = bind_rows(data_list)
master_df$Response.ID = 1:nrow(master_df)

master_filename = paste0(subdir,'/MASTER_DATA_FRAME.csv')
write.csv(master_df, file=master_filename, row.names=FALSE)


#load if already saved
#master_filename = paste0(subdir,'/MASTER_DATA_FRAME.csv')
#master_df = read.csv(file=master_filename)
#this directory will be changed by the R code in report_1.Rnw
setwd('/ntfsl/Dropbox/Grassroots Research Group')
source('/ntfsl/workspace/GrassrootsAnalytics/LeaderReport/fabricate_report.r')
fabricate_report('MASTER_DATA_FRAME.csv')

#other diagnostics
moveon_names = names(master_df)[grepl('move',tolower(names(master_df)))]
for (moveon_name in moveon_names){
  write.csv(table(master_df$ORIGINAL_FILE, master_df[,moveon_name]), file=paste0('diagnostic_',moveon_name,'.csv'), row.names=TRUE)
}