setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
setwd('C:\\Users\\iliu2\\Documents\\datathon\\Melbourne_Datathon_2015_Kaggle')
rm(list=ls()); gc()

load('data/mbr_event_data.RData')
train1 <- mbr.event
load('data/mbr_event_data_BL.RData');ls()
train2 <- mbr.event

head(train1)
head(train2)
all <- merge(train2, train1[,-c(49:50)], all.x = TRUE, all.y = TRUE, by = c('ACCOUNT_ID', 'EVENT_ID'))
dim(train1); dim(train2); dim(all)

all <- all[,c(1:94, 97:142, 95:96)]
head(all)

# Imputation
table(is.na(all[,141]))
all[is.na(all$AVG_TAKEN_HOUR_INPLAY),'AVG_TAKEN_HOUR_INPLAY'] <- median(all$AVG_TAKEN_HOUR_INPLAY, na.rm=T)
all[is.na(all$AVG_TAKEN_HOUR_OUTPLAY),'AVG_TAKEN_HOUR_OUTPLAY'] <- median(all$AVG_TAKEN_HOUR_OUTPLAY, na.rm=T)
all[is.na(all$AVG_TAKEN_HOUR_INPLAY_b),'AVG_TAKEN_HOUR_INPLAY_b'] <- median(all$AVG_TAKEN_HOUR_INPLAY_b, na.rm=T)
all[is.na(all$AVG_TAKEN_HOUR_OUTPLAY_b),'AVG_TAKEN_HOUR_OUTPLAY_b'] <- median(all$AVG_TAKEN_HOUR_OUTPLAY_b, na.rm=T)
all[is.na(all$AVG_TAKEN_HOUR_INPLAY_l),'AVG_TAKEN_HOUR_INPLAY_l'] <- median(all$AVG_TAKEN_HOUR_INPLAY_l, na.rm=T)
all[is.na(all$AVG_TAKEN_HOUR_OUTPLAY_l),'AVG_TAKEN_HOUR_OUTPLAY_l'] <- median(all$AVG_TAKEN_HOUR_OUTPLAY_l, na.rm=T)

# load('data/test.RData')
# test_dt[is.na(test_dt$AVG_TAKEN_HOUR_INPLAY),'AVG_TAKEN_HOUR_INPLAY'] <- median(mbr.event$AVG_TAKEN_HOUR_INPLAY, na.rm=T)
# test_dt[is.na(test_dt$AVG_TAKEN_HOUR_OUTPLAY),'AVG_TAKEN_HOUR_OUTPLAY'] <- median(mbr.event$AVG_TAKEN_HOUR_OUTPLAY, na.rm=T)
# test <- test_dt
# total <- mbr.event

### Validation set
validation <- all[all$EVENT_ID %in% c(101183757,101183885,101184013),]
train <- all[!all$EVENT_ID %in% c(101183757,101183885,101184013),]
dim(train); dim(validation); dim(all)
save(train, validation, all, file='data/train_validation_NEW.RData')
