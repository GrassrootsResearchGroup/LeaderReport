#run this file to create all of the user reports
#"Raw Data DO NOT SHARE needs to be a subdirectory of the directory in setwd(...) 
#unless you change the name of the directory containing the relevant files

#setwd('/ntfsl/Dropbox/Grassroots Research Group')
setwd('/Users/mcandocia/Dropbox/Grassroots Research Group')
source('fabricate_report.r')

subdir = 'Raw Data DO NOT SHARE'
csv_files = dir(subdir)

for (file in csv_files){
  fabricate_report(file)
}