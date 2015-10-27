#----------------------------------------------
# Melbourne Datathon 2015
# Some R code to tidy up the raw file so it
# can be imported into SQL Server easier
#
# removes quotes
# removes leading and trailing spaces
# formats dates so they can be read directly into SQL Server as dates
#
# note: you will not have the raw file used here, but you do have the equivalent 
# for the other games prior to the semo final and finals
#
# enjoy!
#
#----------------------------------------------

## the location of the data
myDataFolder <- "H:\\DataScienceMelbourne\\datathon_data\\"

#the files of interest
myInFile <- 'Datathon WC Data Games SEMI Finals & Final.csv' #the original file
myOutFile <- 'Datathon_WC_Data_Games_SEMI_Finals_Final.txt' #the modified file

#append the root folder to the name
myInFile <- paste(myDataFolder,myInFile,sep="")
myOutFile <- paste(myDataFolder,myOutFile,sep="")

#read in raw file
myData <- read.csv(myInFile, stringsAsFactors=FALSE,na.strings = "")

#write out as tab delimited and removing the quotes
write.table(myData,file=myOutFile,quote=FALSE,sep="\t",row.names = FALSE,na = "")

#read back in stripping the white space
myData <- read.table(myOutFile, stringsAsFactors=FALSE,na.strings = "",strip.white = TRUE,sep="\t",header=TRUE)

#re-format the date time columns to be database friendly
myData$EVENT_DT1 <- as.character(format(as.POSIXct(strptime(myData$EVENT_DT, "%d/%m/%Y %I:%M:%S %p")), usetz=FALSE))
myData$OFF_DT1 <- as.character(format(as.POSIXct(strptime(myData$OFF_DT, "%d/%m/%Y %I:%M:%S %p")), usetz=FALSE))
myData$PLACED_DATE1 <- as.character(format(as.POSIXct(strptime(myData$PLACED_DATE, "%d/%m/%Y %I:%M:%S %p")), usetz=FALSE))
myData$TAKEN_DATE1 <- as.character(format(as.POSIXct(strptime(myData$TAKEN_DATE, "%d/%m/%Y %I:%M:%S %p")), usetz=FALSE))
myData$SETTLED_DATE1 <- as.character(format(as.POSIXct(strptime(myData$SETTLED_DATE, "%d/%m/%Y %I:%M:%S %p")), usetz=FALSE))
myData$CANCELLED_DATE1 <- as.character(format(as.POSIXct(strptime(myData$CANCELLED_DATE, "%d/%m/%Y %I:%M:%S %p")), usetz=FALSE))

#write out the table
write.table(myData,file=myOutFile,quote=FALSE,sep="\t",row.names = FALSE,na = "")