library(tidyverse)
library("ggplot2")
library("sf")
library(RColorBrewer)
library(rworldmap)
library(caret)
library(e1071)

df<- read.csv("https://intro-datascience.s3.us-east-2.amazonaws.com/Resort01.csv")
summary(df)
with(df, cor(IsRepeatedGuest,IsCanceled))
Cancelledtest <- df %>% filter(IsCanceled == "1") %>% select(Country, IsCanceled, CustomerType) %>% group_by(Country,CustomerType)  %>% summarize(count =n())

NotCancelledtest <- df %>%  filter(IsCanceled == "0") %>% select(Country, IsCanceled,CustomerType) %>% group_by(Country,CustomerType)  %>% summarize(count =n())



joinData <- joinCountryData2Map( Cancelledtest,
                                 joinCode = "ISO3",
                                 nameJoinColumn = "Country")
theMap<- mapCountryData(joinData, nameColumnToPlot = "CustomerType", numCats = 45, xlim = NA, ylim = NA, mapRegion = "world", catMethod = "logFixedWidth", colourPalette = "heat", addLegend = TRUE, borderCol = "grey", mapTitle = "Customer wise distrubtion of visitors with cancellations", oceanCol = NA, aspect = 1, missingCountryCol = NA, add = FALSE, lwd = 0.5)



joinData <- joinCountryData2Map( NotCancelledtest,
                                 joinCode = "ISO3",
                                 nameJoinColumn = "Country")
theMap<- mapCountryData(joinData, nameColumnToPlot = "CustomerType", numCats = 45, xlim = NA, ylim = NA, mapRegion = "world", catMethod = "logFixedWidth", colourPalette = "heat", addLegend = TRUE, borderCol = "grey", mapTitle = "Customer wise distrubtion of visitors with no cancellations", oceanCol = NA, aspect = 1, missingCountryCol = NA, add = FALSE, lwd = 0.5)

text(x = joinData2Map$longitude, y = theMap$latitude, theMap$name, pos = 4, col = "blue")
str(df)

df$IsCanceled <- as.factor(df$IsCanceled)
df$IsRepeatedGuest <- as.factor(df$IsRepeatedGuest)
df$Meal <- as.factor(df$Meal)
df$ReservedRoomType<- as.factor(df$ReservedRoomType)
df$DepositType<- as.factor(df$DepositType)
df$CustomerType<- as.factor(df$CustomerType)
df$ReservedRoomType<- as.factor(df$ReservedRoomType)
df$AssignedRoomType<- as.factor(df$AssignedRoomType)
df$Meal<-as.factor(df$Meal)
df$Country<-as.factor(df$Country)
df$MarketSegment<- as.factor(df$MarketSegment)
df$LeadTime<-as.numeric(df$LeadTime)
df$StaysInWeekendNights<-as.numeric(df$StaysInWeekendNights)
df$StaysInWeekNights<- as.numeric(df$StaysInWeekNights)
df$Adults<- as.numeric(df$Adults)
df$Children<-as.numeric(df$Children)
df$Babies<-as.numeric(df$Babies)
df$PreviousCancellations<-as.numeric(df$PreviousCancellations)
df$PreviousBookingsNotCanceled<-as.numeric(df$PreviousBookingsNotCanceled)
df$RequiredCarParkingSpaces<- as.numeric(df$RequiredCarParkingSpaces)
df$TotalOfSpecialRequests<-as.numeric(df$TotalOfSpecialRequests)
df$BookingChanges<-as.numeric(df$BookingChanges)
str(df)

library(rsample)
library(rpart)
library(rpart.plot)
library(caret)
library(kernlab)

set.seed(110)
df1 <- subset(df, select = -c(Country) )
trainList<- createDataPartition(y=df1$IsCanceled,p=.66,list=FALSE)
str(trainList)

trainSet<- df1[trainList,]
str(trainSet)

testSet<- df1[-trainList,]



cartTree <- rpart(IsCanceled ~ ., data = trainSet)

rpart.plot(cartTree)
predictValues <- predict(cartTree, newdata=testSet, type = "class")
confMatrix<- confusionMatrix(predictValues,testSet$IsCanceled)
confMatrix
varImp(cartTree)

