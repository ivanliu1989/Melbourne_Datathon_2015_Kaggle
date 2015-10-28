setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
setwd('C:\\Users\\iliu2\\Documents\\datathon\\Melbourne_Datathon_2015_Kaggle')
rm(list=ls()); gc()

load('data/mbr_event_data.RData');ls()
length(table(mbr.event$EVENT_ID))

# Imputation
mbr.event[is.na(mbr.event$AVG_TAKEN_HOUR_INPLAY),'AVG_TAKEN_HOUR_INPLAY'] <- median(mbr.event$AVG_TAKEN_HOUR_INPLAY, na.rm=T)
mbr.event[is.na(mbr.event$AVG_TAKEN_HOUR_OUTPLAY),'AVG_TAKEN_HOUR_OUTPLAY'] <- median(mbr.event$AVG_TAKEN_HOUR_OUTPLAY, na.rm=T)

### Validation set
validation <- mbr.event[mbr.event$EVENT_ID %in% c(101183757,101183885,101184013),]
train <- mbr.event[!mbr.event$EVENT_ID %in% c(101183757,101183885,101184013),]
dim(train); dim(validation)
save(train, validation, file='data/train_validation.RData')

