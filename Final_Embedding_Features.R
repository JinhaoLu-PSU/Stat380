setwd("/Volumes/Transcend/PSU/STAT 380 DATA/380FinalProject")

library(data.table)
library(Metrics)
library(caret)
library(Rtsne)
library(xgboost)

#data <-fread("training_data.csv")
train <- fread("training_data.csv")
test <- fread("test_file.csv")
emb_train <- fread("Emb_train.csv") #train's text after embedding
emb_test <- fread("Emb_test.csv")#test's text after embedding

#===================================================================================
#Embeffing Function
#
#getEmbeddings<-function(text){
#input <- list(
#  instances =list( text)
#)
#res <- POST("https://dsalpha.vmhost.psu.edu/api/use/v1/models/use:predict", body = input,encode = "json", verbose())
#emb<-unlist(content(res)$predictions)
#emb
#}

#emb_dt<-NULL
#as.data.frame.table(emb_dt)

#for (i in 1:length(data$text)){
#  emb_dt<-rbind(emb_dt,getEmbeddings(data$text[i]))
  
#}
#emb_dt<-data.table(emb_dt)
#===================================================================================
#Python section --- mrjob 
#Use hadoop mrjob to do the map reduce job and sum up the number of keyword appear in 
#the train text and test text
from mrjob.job import MRJob

class WordCount(MRJob):
	def mapper(self,key,line):
		words = line.split()
		for w in words:
			yield(w,1)

	def reducer(self,key,values):
		yield key,sum(values)

if __name__ == "__main__"
	WordCount.run()
#data <- fread("keyword.csv")  #-4184
#data3 <- as.data.table(tstrsplit(data$word, "\t", fixed = TRUE))

#data <- fread("keyword.csv")  #-4184
#data[!grepl("\"", data$text),]

#Car
#"commander","Mercedes","steering","Kia","Mazda","tire","trucks","Enzo","Ferraris"
#car <- c("commander","Mercedes","steering","Kia","Mazda","tire","trucks","Enzo","Ferraris")
#car <- as.data.table(car)
#test$Season_1[grep(4,test$season,test$season)]<-"1"
#test$subcar <- 0
#ID_Price_tab <- price_tab[id %in% train$id]
#test$subcar[grep("commander",test$text)]<- "1"
#test$subcar[grep("Mercedes",test$text)]<- "1"
#test$subcar[grep("Kia",test$text)]<- "1"
#test$subcar[grep("Mazda",test$text)]<- "1"

#test$subcar[grep("tire",test$text)]<- "1"
#test$subcar[grep("Enzo",test$text)]<- "1"
#test$subcar[grep("Ferraris",test$text)]<- "1"


# pass the dec 8 benchmark !!
#keyword is useless since I can not pick that much keyword for each subreddit.
#===================================================================================
train$tag <- 0
train$tag[grep("1",train$subredditcars)]<- 1
train$tag[grep("1",train$subredditCooking)]<- 2
train$tag[grep("1",train$subredditMachineLearning)]<- 3
train$tag[grep("1",train$subredditmagicTCG)]<- 4
train$tag[grep("1",train$subredditpolitics)]<- 5
train$tag[grep("1",train$subredditReal_Estate)]<- 6
train$tag[grep("1",train$subredditscience)]<- 7
train$tag[grep("1",train$subredditStockMarket)]<- 8
train$tag[grep("1",train$subreddittravel)]<- 9
train$tag[grep("1",train$subredditvideogames)]<- 10

train$tag <- train$tag - 1

#===================================================================================
sub_data <- rbind(emb_train,emb_test)
j_data <- data.frame(lapply(sub_data,jitter,factor = 0.0001))
pca <- prcomp(j_data, center = TRUE, scale = TRUE)
pca_x <- as.data.table(pca$x)

#===================================================================================
tsne <- Rtsne(pca_x,perplexity = 30, pca = FALSE ,verbose = TRUE, 
	          max_iter = 3000, check_duplicates =  FALSE)

data_y <- as.data.table(tsne$Y) #data train in model = emb+y+features 

subTrain <- data_y[1:200]
subTest <- data_y[201:20755]

setnames(subTrain,"V1","tsne1")
setnames(subTrain,"V2","tsne2")

setnames(subTest,"V1","tsne1")
setnames(subTest,"V2","tsne2")

result<- as.data.table(train$tag)
setnames(result,"V1","tag")

sub_emb_train <- cbind(emb_train,subTrain,result)
sub_emb_test <- cbind(emb_test,subTest)

write.csv(sub_emb_train,"train_model.csv",row.names = FALSE)
write.csv(sub_emb_test,"test_model.csv",row.names = FALSE)
write.csv(result,"tag.csv",row.names = FALSE)






