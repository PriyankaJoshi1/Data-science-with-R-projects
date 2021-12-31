getwd()
#importing dataset
teledata<-read.csv('Comcast Telecom Complaints data.csv',header = TRUE)
library(dplyr)
library(lubridate)
library(ggplot2)
library(stringi)

head(teledata)
str(teledata)
?names
#renaming fiels names
names(teledata)<-stri_replace_all(regex = "\\.",replacement = "",str = names(teledata))
head(teledata)

#finding NAs in the dataset
navector<-is.na(teledata)
navector
length(navector[navector==TRUE])
#there are no NA values in the dataset

#processing date
head(teledata$Date)
teledata$Date<-dmy(teledata$Date)
head(teledata$Date)


#quest 1
#Provide the trend chart for the number of complaints at monthly and daily granularity levels.
#to get complaints on a daily level and plotting a trend chart for it

