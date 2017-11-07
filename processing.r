#setwd('/ntfsl/Dropbox/Grassroots Research Group')
setwd('/Users/mcandocia/Dropbox/Grassroots Research Group')
source('fabricate_report.r')

subdir = 'Raw Data DO NOT SHARE'
csv_files = dir(subdir)

for (file in csv_files){
  fabricate_report(file)
}