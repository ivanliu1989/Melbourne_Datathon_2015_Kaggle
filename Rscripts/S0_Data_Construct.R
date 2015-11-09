setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
# setwd('C:\\Users\\iliu2\\Documents\\datathon\\Melbourne_Datathon_2015_Kaggle')
rm(list=ls()); gc()
library(MASS);library(e1071)

### Read Data
load('../Datathon_Full_Dataset/cleaned_raw_data.RData')
ls()

### Feature generating
source('Rscripts/9_functions.R')
total <- feat.eng(dt)
total <- mbr.event
apply(total,2, function(x) mean(is.na(x)))

### output
save(total, file='data/1_complete_data_new.RData')
