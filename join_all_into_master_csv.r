#this file concatenates all csvs into a master csv

setwd('/ntfsl/Dropbox/Grassroots Research Group')
#setwd('/Users/mcandocia/Dropbox/Grassroots Research Group')
source('/ntfsl/workspace/GrassrootsAnalytics/LeaderReport/fabricate_report.r')

subdir = 'Raw Data DO NOT SHARE'
csv_files = dir(subdir)

data_list = list()

for (file in csv_files){
  f = read.csv(paste0(subdir,'/',file), stringsAsFactors=FALSE)
  for (col in names(f)){
    if (is.integer(f[,col]))
      f[,col] = as.character(f[,col])
  }
  data_list[[file]] = f
}

library(dplyr)

master_df = bind_rows(data_list)

master_filename = paste0(subdir,'/MASTER_DATA_FRAME.csv')
write.csv(master_df, file=master_filename, row.names=FALSE)

fabricate_report('MASTER_DATA_FRAME.csv')
