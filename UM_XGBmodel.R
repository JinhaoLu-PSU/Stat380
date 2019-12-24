setwd("/Volumes/Transcend/PSU/STAT 380 DATA/um")

library(data.table)
library(caret)
library(Metrics)
library(glmnet)
library(plotmo)
library(xgboost)

train <- fread("train.csv")
test <- fread("test.csv")
example <- data.table(train$id)
setnames(example,"V1","id")

drop <- c("sequence")
train <- train[, !drop, with = FALSE]
test <- test[, !drop, with = FALSE]
#-------------------------------------------------
setnames(train,"sum00","sum1")
setnames(train,"sum01","sum2")
setnames(train,"sum02","sum3")
setnames(train,"sum03","sum4")

setnames(train,"Gsum00","Gsum1")
setnames(train,"Gsum01","Gsum2")
setnames(train,"Gsum02","Gsum3")
setnames(train,"Gsum03","Gsum4")

setnames(train,"amt00","amt1")
setnames(train,"amt01","amt2")
setnames(train,"amt02","amt3")
setnames(train,"amt03","amt4")
#-------------------------------------------------
setnames(test,"sum01","sum1")
setnames(test,"sum02","sum2")
setnames(test,"sum03","sum3")
setnames(test,"sum04","sum4")

setnames(test,"Gsum01","Gsum1")
setnames(test,"Gsum02","Gsum2")
setnames(test,"Gsum03","Gsum3")
setnames(test,"Gsum04","Gsum4")

setnames(test,"amt01","amt1")
setnames(test,"amt02","amt2")
setnames(test,"amt03","amt3")

drop <- c("amt04")
test <- test[, !drop, with = FALSE]
#-------------------------------------------------

x.train <- as.matrix(train[,c(2:14)])
y.train <- as.matrix(train$amt4)
x.test <- as.matrix(test[,c(2:14)])

dtrain <- xgb.DMatrix(x.train,label=y.train,missing=NA)
dtest <- xgb.DMatrix(x.test,missing=NA)


param <- list(  objective           = "reg:linear",
                gamma               = 0.01,
                booster             = "gbtree",
                eval_metric         = "rmse",
                eta                 = 0.01,
                max_depth           = 5,
                subsample           = 0.9,
                colsample_bytree    = 0.9,
                tree_method = 'hist'
)

XGBm<-xgb.cv(params=param,nfold=5,nrounds=650,missing=NA,data=dtrain,print_every_n=1)

XGBm<-xgb.train(params=param,nrounds=1000,missing=NA,data=dtrain,print_every_n=1)

pred<-predict(XGBm, newdata = dtest)
#-------------------------------------------------
#example <- data.table(train$id)
example$amt <- pred

write.csv(example,"submit.csv",row.names =FALSE)
#-------------------------------------------------
task2 <- fread("submit.csv")

sum(donate$amt)#127918.4

#submit$donate <- 0
donate <- task2[amt >=10]
donate$tag <- 1
donate <- donate[,.(id,tag)]
task2 <- merge(task2,donate,all.x = TRUE)
task2[is.na(task2)]<- 0

setnames(task2,"tag","donate")

write.csv(task2,"task2.csv",row.names =FALSE)






