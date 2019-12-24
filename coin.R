library(data.table)

train <- fread("train_file.csv")
test <- fread("test_file.csv")

#test$result <- test[,.(V1+V10)]

logr_model<-glm(result ~ V1+V2+V3+V4+V5+V6+V7+V8+V9+V10,data=train, family=binomial)

summary(logr_model)

saveRDS(logr_model,"logr_lm.model")

pred<-predict(logr, newdata = test, type="response")

submit<-data.table(id=c(1:100000))
submit$result<-pred

fwrite(submit,"submit_logr.csv")