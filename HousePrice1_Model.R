library(data.table)

setwd("/Volumes/Transcend/PSU/STAT 380 DATA")
train <- fread("Stat_380_train.csv")
test <- fread("Stat_380_test.csv")

train$BudAge <- train[,.(YrSold - YearBuilt)]

test$BudAge <- test[,.(YrSold-YearBuilt)]

fn <- lm(SalePrice~LotArea+OverallQual+OverallCond+FullBath+HalfBath+TotRmsAbvGrd+YearBuilt+TotalBsmtSF+BedroomAbvGr+GrLivArea+PoolArea+YrSold+BudAge, data = train)
summary(fn)

test$SalePrice <- 47158.03 + test$LotArea * 0.4999425 + test$OverallQual * 22499.86 + test$OverallCond * 4735.436 + test$FullBath * 11969.06 + test$HalfBath * 7960.236 + test$TotRmsAbvGrd * 4338.301 + test$TotalBsmtSF * 35.21965 + test$BedroomAbvGr * (-873.7006) + test$GrLivArea * -0.3166828 + test$BudAge* -286.2018 + test$YrSold* -58.61925

RData <- test[,.(Id, SalePrice)]

write.csv(RData, file = "RData.csv")

saveRDS(fn, file = "HousePriceModel.model")
