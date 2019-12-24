setwd("/Volumes/Transcend/PSU/STAT 380 DATA/um")

library(data.table)
library(caret)
library(Metrics)
library(glmnet)
library(plotmo)
library(xgboost)

appeal <- fread("appeal.csv")
donor <- fread("donor.csv")
trans <- fread("trans.csv")

#task 1
#Submit a single number estimating the total donations from the donors 
#during the one year target period.
trans$period <- 0
trans$period[grep("04",trans$giftdate)]<-1

trans_sub1 <- trans[period == 1]

#donor$period <- 0
#donor$period[grep("04",donor$firstgift)]<-1
#donor_sub1 <- donor[period == 1]

TotalDonation <- merge(donor,trans_sub1, by.x = 'id', by.y = 'id')
#TotalDonation <- TotalDonation[,c(id,giftdate,amt)]

sum(TotalDonation$amt)#149922.5

TotalDonation_sub <- TotalDonation[,.(donation_sum = sum(amt)), by = id]

#Task 2: 
#Best estimates of individual-level donations, 
#as measured by the mean squared error (MSE) of the logged donations

sub <- TotalDonation[,.(donation_sum = sum(amt)), by = giftdate]

total <- trans[,.(donation_sum = sum(amt)), by = giftdate]

#=========================================================================
user <- data.table(unique(donor$id))
setnames(user,"id")
#=========================================================================
Count <- trans[,.N, by = id]
Count$do <- 1
Count$do[grep(1,Count$N)]<-0

Count <- Count[,.(id,do)]
#=========================================================================

#appeal$Year98 <-0 
#appeal$Year98[grep("98",appeal$appdate)]<-1

appeal$Year99 <-0 
appeal$Year99[grep("99",appeal$appdate)]<-1

appeal$Year00 <-0 
appeal$Year00[grep("00",appeal$appdate)]<-1

appeal$Year01 <-0 
appeal$Year01[grep("01",appeal$appdate)]<-1

appeal$Year02 <-0 
appeal$Year02[grep("02",appeal$appdate)]<-1

appeal$Year03 <-0 
appeal$Year03[grep("03",appeal$appdate)]<-1

appeal$Year04 <-0 
appeal$Year04[grep("04",appeal$appdate)]<-1

AppSum <- appeal[,.(id,Year99)]
AppSum <- AppSum[,.(sum99 = sum(Year99)), by = id]

AppSum00 <- appeal[,.(id,Year00)]
AppSum00 <- AppSum00[,.(sum00 = sum(Year00)), by = id]
AppSum$sum00 <- AppSum00$sum00

AppSum01 <- appeal[,.(id,Year01)]
AppSum01 <- AppSum01[,.(sum01 = sum(Year01)), by = id]
AppSum$sum01 <- AppSum01$sum01

AppSum02 <- appeal[,.(id,Year02)]
AppSum02 <- AppSum02[,.(sum02 = sum(Year02)), by = id]
AppSum$sum02 <- AppSum02$sum02

AppSum03 <- appeal[,.(id,Year03)]
AppSum03 <- AppSum03[,.(sum03 = sum(Year03)), by = id]
AppSum$sum03 <- AppSum03$sum03

AppSum04 <- appeal[,.(id,Year04)]
AppSum04 <- AppSum04[,.(sum04 = sum(Year04)), by = id]
AppSum$sum04 <- AppSum04$sum04
#=========================================================================

trans$gift99 <- 0
trans$gift99[grep("99",trans$giftdate)]<-1

trans$gift00 <- 0
trans$gift00[grep("00",trans$giftdate)]<-1

trans$gift01 <- 0
trans$gift01[grep("01",trans$giftdate)]<-1

trans$gift02 <- 0
trans$gift02[grep("02",trans$giftdate)]<-1

trans$gift03 <- 0
trans$gift03[grep("03",trans$giftdate)]<-1

trans$gift04 <- 0
trans$gift04[grep("04",trans$giftdate)]<-1

gift <- trans[,.(id,gift00)]
gift <- gift[,.(Gsum00 = sum(gift00)), by = id]

gift01 <- trans[,.(id,gift01)]
gift01 <- gift01[,.(Gsum01 = sum(gift01)), by = id]
gift$Gsum01 <- gift01$Gsum01

gift02 <- trans[,.(id,gift02)]
gift02 <- gift02[,.(Gsum02 = sum(gift02)), by = id]
gift$Gsum02 <- gift02$Gsum02

gift03 <- trans[,.(id,gift03)]
gift03 <- gift03[,.(Gsum03 = sum(gift03)), by = id]
gift$Gsum03 <- gift03$Gsum03

gift04 <- trans[,.(id,gift04)]
gift04 <- gift04[,.(Gsum04 = sum(gift04)), by = id]
gift$Gsum04 <- gift04$Gsum04

#=========================================================================

amtSum <- trans[gift00 == 1]
amtSum <- amtSum[,.(id,gift00,amt)]
amtSum <- amtSum[,.(amt00 = sum(amt)), by = id]

amtSum1 <- trans[gift01 == 1]
amtSum1 <- amtSum1[,.(id,gift01,amt)]
amtSum1 <- amtSum1[,.(amt01 = sum(amt)), by = id]
amtSum <- merge(amtSum,amtSum1,all.x = TRUE)

amtSum2 <- trans[gift02 == 1]
amtSum2 <- amtSum2[,.(id,gift02,amt)]
amtSum2 <- amtSum2[,.(amt02 = sum(amt)), by = id]
amtSum <- merge(amtSum,amtSum2,all.x = TRUE)

amtSum3 <- trans[gift03 == 1]
amtSum3 <- amtSum3[,.(id,gift03,amt)]
amtSum3 <- amtSum3[,.(amt03 = sum(amt)), by = id]
amtSum <- merge(amtSum,amtSum3,all.x = TRUE)

amtSum4 <- trans[gift04 == 1]
amtSum4 <- amtSum4[,.(id,gift04,amt)]
amtSum4 <- amtSum4[,.(amt04 = sum(amt)), by = id]
amtSum <- merge(amtSum,amtSum4,all.x = TRUE)

#train[is.na(train)]<-0
amtSum[is.na(amtSum)]<- 0
#=========================================================================
user <- merge(user,Count, all.x = TRUE)
user <- merge(user,AppSum,all.x = TRUE)
user <- merge(user,gift,all.x = TRUE)
user <- merge(user,amtSum,all.x = TRUE)

user[is.na(user)]<- 0

write.csv(user,"user.csv",row.names=FALSE)
#drop <-c('','BldgType','CentralAir')
#=========================================================================
test<- user
train <- user
drop <-c('sum00','Gsum00','amt00')
test <- test[, !drop, with = FALSE]
drop <-c('sum04','Gsum04','amt04')
train <- train[, !drop, with = FALSE]


