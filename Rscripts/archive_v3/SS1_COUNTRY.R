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

### 1. Match countries
head(dt)
MATCH_COUNTRY <- data.frame(do.call(rbind, strsplit(as.vector(dt$MATCH), split = " v ")))
names(MATCH_COUNTRY) <- c("MATCH_COUNTRY_1", "MATCH_COUNTRY_2")
MATCH_COUNTRY$MATCH_COUNTRY_1 <- as.character(MATCH_COUNTRY$MATCH_COUNTRY_1)
MATCH_COUNTRY$MATCH_COUNTRY_2 <- as.character(MATCH_COUNTRY$MATCH_COUNTRY_2)
d <- cbind(dt, MATCH_COUNTRY)

d[d$MATCH_COUNTRY_2 == 'United Arab Emirates', 'MATCH_COUNTRY_2'] <- 'UAE'
d[d$SELECTION_NAME == 'United Arab Emirates', 'SELECTION_NAME'] <- 'UAE'

### 2. Is own country
d$IS_COUNTRY <- ifelse((d$COUNTRY_OF_RESIDENCE_NAME == d$MATCH_COUNTRY_1) | (d$COUNTRY_OF_RESIDENCE_NAME == d$MATCH_COUNTRY_2), 1, 0)

### 3. Bet on own country?
d$BET_COUNTRY <- ifelse((d$SELECTION_NAME == d$COUNTRY_OF_RESIDENCE_NAME), 1, 0)

### 4. Bet oppo country?
d$BET_OP_COUNTRY <- ifelse(((d$COUNTRY_OF_RESIDENCE_NAME == d$MATCH_COUNTRY_1) | (d$COUNTRY_OF_RESIDENCE_NAME == d$MATCH_COUNTRY_2)) &
                               (d$SELECTION_NAME != d$COUNTRY_OF_RESIDENCE_NAME), 1, 0)

d$BET_COUNTRY_ONE <- ifelse(d$SELECTION_NAME == d$MATCH_COUNTRY_1, 1, 0)
d$BET_COUNTRY_TWO <- ifelse(d$SELECTION_NAME == d$MATCH_COUNTRY_2, 1, 0)

dt <- d

### Remove useless features
dt$BET_ID <- NULL
dt$BET_TRANS_ID <- NULL
dt$MATCH_BET_ID <- NULL
dt$PARENT_EVENT_ID <- NULL
dt$MATCH <- NULL
dt$EVENT_NAME <- NULL
head(dt)

### Add some useful features


save(dt, file='../Datathon_Full_Dataset/cleaned_raw_data.RData' )
