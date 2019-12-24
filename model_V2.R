setwd("/Volumes/Transcend/PSU/STAT 380 DATA/HousePrice2")
library(data.table)
library(caret)
library(Metrics)
library(glmnet)
library(plotmo)
library(xgboost)


test <- fread("Stat_380_test.csv")
train <- fread("Stat_380_train.csv")
example_sub <- fread("Stat_380_sample_submission.csv")

train[is.na(train)]<-0
test[is.na(test)]<- 0

#test$SalePrice <- 0
#test$train<-0
#train$train<-1

#master<-rbind(train,test)
#setkey(master,id)
#setkey(card_tab,id)
#=============================================================================
#train$Lot <- 0
#test$Lot <- 0
#train$Lot[train$LotFrontage != 0]<-"1"
#test$Lot[test$LotFrontage != 0]<-"1"
#drop <- c('LotFrontage')
#train <- train[, !drop, with = FALSE]
#test <- test[, !drop, with = FALSE]
#=============================================================================
#train$Pool <- 0
#test$Pool <- 0
#train$Pool[train$PoolArea != 0]<-1
#test$Pool[test$PoolArea != 0]<-1
#drop <- c('PoolArea')
#train <- train[, !drop, with = FALSE]
#test <- test[, !drop, with = FALSE]
#=============================================================================
#Build Type
train_Heat <- train[,.(Id,Heating)]
train_Heat <- melt(train_Heat,id.vars = "Id")
train_Heat <- train_Heat[!is.na(train_Heat$value)]
train_Heat$True <- 1
train_Heat <- dcast(train_Heat,Id ~ value,length,value.var="True")


test_Heat <- test[,.(Id,Heating)]
test_Heat <- melt(test_Heat,id.vars = "Id")
test_Heat <- test_Heat[!is.na(test_Heat$value)]
test_Heat$True <- 1
test_Heat <- dcast(test_Heat,Id ~ value,length,value.var="True")

train <- merge(train,train_Heat,by.x = 'Id', by.y='Id')
test <- merge(test,test_Heat,by.x = 'Id', by.y='Id')

drop <- c('Heating')
train <- train[, !drop, with = FALSE]
test <- test[, !drop, with = FALSE]
#=============================================================================
train_year <- train[,.(Id,YrSold)]
train_year <- melt(train_year,id.vars = "Id")
train_year <- train_year[!is.na(train_year$value)]
train_year$True <- 1
train_year <- dcast(train_year,Id ~ value,length,value.var="True")


test_year <- test[,.(Id,YrSold)]
test_year <- melt(test_year,id.vars = "Id")
test_year <- test_year[!is.na(test_year$value)]
test_year$True <- 1
test_year <- dcast(test_year,Id ~ value,length,value.var="True")

train <- merge(train,train_year,by.x = 'Id', by.y='Id')
test <- merge(test,test_year,by.x = 'Id', by.y='Id')

drop <- c('YrSold','BldgType','CentralAir')
train <- train[, !drop, with = FALSE]
test <- test[, !drop, with = FALSE]

x.train <- as.matrix(train[, c(2:13,15:23)])
y.train <- as.matrix(train$SalePrice)
x.test <- as.matrix(test[,c(2:22)])

dtrain <- xgb.DMatrix(x.train,label=y.train,missing=NA)
dtest <- xgb.DMatrix(x.test,missing=NA)


param <- list(  objective           = "reg:linear",
                gamma               = 0,
                booster             = "gbtree",
                eval_metric         = "rmse",
                eta                 = 0.02,
                max_depth           = 5,
                subsample           = 0.9,
                colsample_bytree    = 0.9,
                tree_method = 'hist'
)


XGBm<-xgb.cv(params=param,nfold=5,nrounds=600,missing=NA,data=dtrain,print_every_n=1)

XGBm<-xgb.train(params=param,nrounds=1000,missing=NA,data=dtrain,print_every_n=1)

saveRDS(XGBm,'XGBm.model')

pred<-predict(XGBm, newdata = dtest)

example_sub$SalePrice <- pred

write.csv(example_sub,"submit_xgb.csv",row.names =FALSE)
