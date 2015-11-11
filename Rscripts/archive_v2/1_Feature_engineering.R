setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
# setwd('C:\\Users\\iliu2\\Documents\\datathon\\Melbourne_Datathon_2015_Kaggle')
rm(list=ls()); gc()
library(MASS);library(e1071)

### Read Data
dt_1 <- read.csv('../Datathon_Full_Dataset/Datathon WC Data Games 1-10.csv', stringsAsFactors=FALSE,na.strings = "")
dt_2 <- read.csv('../Datathon_Full_Dataset/Datathon WC Data Games 11-20.csv', stringsAsFactors=FALSE,na.strings = "")
dt_3 <- read.csv('../Datathon_Full_Dataset/Datathon WC Data Games 21-30.csv', stringsAsFactors=FALSE,na.strings = "")
dt_4 <- read.csv('../Datathon_Full_Dataset/Datathon WC Data Games 31-40.csv', stringsAsFactors=FALSE,na.strings = "")
dt_5 <- read.csv('../Datathon_Full_Dataset/Datathon WC Data Games QTR Finals.csv', stringsAsFactors=FALSE,na.strings = "")
dt <- rbind(dt_1, dt_2, dt_3, dt_4, dt_5)
rm(list = c('dt_1', 'dt_2', 'dt_3', 'dt_4', 'dt_5'))
head(dt)

### Feature generating
source('Rscripts/archive_v2/9_functions.R')
total <- feat.eng(dt)
apply(total,2, function(x) mean(is.na(x)))

### output
save(total, file='data/1_complete_data_new.RData')
