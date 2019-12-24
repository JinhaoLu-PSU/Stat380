setwd("/Volumes/Transcend/PSU/STAT 380 DATA/Mid Project/MTG2")
library(data.table)
library(caret)
library(Metrics)
library(glmnet)
library(plotmo)
library(lubridate)

train<-fread("start_train.csv")
test<-fread("start_test.csv")
card_tab<-fread("card_tab.csv")
price_tab<-fread("price_tab.csv")

#=============================================================================
#未来三天价格
train$Tday <- train$current_date
test$Tday <- test$current_date

train$Tday <- ymd(train$Tday)+days(3)
test$Tday <- ymd(test$Tday)+days(3)

train$Tdayid <- paste(train$id,train$Tday) 
test$Tdayid <- paste(test$id,test$Tday)

ID_Price_tab <- price_tab[id %in% train$id]
ID_Price_tab_1 <- ID_Price_tab[ymd(date) %in% train$Tday]
ID_Price_tab_1$Tdayid <- paste(ID_Price_tab_1$id,ID_Price_tab_1$date)
train <- merge(train,ID_Price_tab_1, all.x = T)

ID_Price_tab <- price_tab[id %in% test$id]
ID_Price_tab_1 <- ID_Price_tab[ymd(date) %in% test$Tday]
ID_Price_tab_1$Tdayid <- paste(ID_Price_tab_1$id,ID_Price_tab_1$date)
test <- merge(test,ID_Price_tab_1, all.x = T)

drops<-c('Tdayid','Tday','date')
train<-train[, !drops, with = FALSE]
test<-test[, !drops, with = FALSE]
#=============================================================================
test$future_price<-0
test$train<-0
train$train<-1
#=============================================================================
#季节
train$season <- quarter(ymd(train$current_date))
train$Season_1 <- 0

train$Season_1[grep(4,train$season,train$season)]<-"1"

train$season_2 <- quarter(ymd(train$future_date))
train$Season_2 <- 0

train$Season_2[grep(4,train$season,train$season)]<-"1"

test$season <- quarter(ymd(test$current_date))
test$season_2 <- quarter(ymd(test$future_date))

test$Season_1 <- 0
test$Season_1[grep(4,test$season,test$season)]<-"1"

test$Season_2 <- 0
test$Season_2[grep(4,test$season,test$season)]<-"1"


master<-rbind(train,test)
setkey(master,id)
setkey(card_tab,id)
#=============================================================================
#subtypes
#subtypes_tab<-as.data.table(card_tab$subtypes)
#subtypes_tab$id<-card_tab$id

#m_subtypes_tab<-melt(subtypes_tab,id.vars = "id")
#m_subtypes_tab<-m_subtypes_tab[!is.na(m_subtypes_tab$value)]
#m_subtypes_tab$True<-1
#subtypes_tab<-dcast(m_subtypes_tab,id ~ value,length,value.var="True")

#master<-merge(master,subtypes_tab,all.x=T)

#keeps<- c('id','current_date','future_date','train','season','season_2','Season_1','Season_2','future_price','current_price','Teferi','Ugin'
#	,'Dinosaur Beast')

#master<-master[, keeps, with = FALSE]


#=============================================================================
#卡牌种类
types_tab<-as.data.table(tstrsplit(card_tab$types," "))
types_tab$id<-card_tab$id

m_types_tab<-melt(types_tab,id.vars = "id")
m_types_tab<-m_types_tab[!is.na(m_types_tab$value)]
m_types_tab$True<-1
types_tab<-dcast(m_types_tab,id ~ value,length,value.var="True")

master<-merge(master,types_tab,all.x=T)
#挑选特征
drops <- c('Instant','Sorcery','Creature','Artifact')
master<-master[, !drops, with = FALSE]

#=============================================================================
#卡牌supertype
#master<-merge(master,card_tab[,.(id,supertypes)],all.x=T)
#master$Legendary <- 0
#master$Legendary[grep("Legendary",master$supertypes)]<-1

#drops <- c('supertypes')
#master<-master[, !drops, with = FALSE]
#=============================================================================
#稀有度
#master<-merge(master,card_tab[,.(id,rarity)],all.x=T)
#master$Mythic <- 0
#master$Mythic[grep("Mythic",master$rarity)]<-1

#drops <- c('rarity')
#master<-master[, !drops, with = FALSE]

#=============================================================================
#卡组
set_tab<-as.data.table(card_tab$set)
set_tab$id<-card_tab$id

m_set_tab<-melt(set_tab,id.vars = "id")
m_set_tab<-m_set_tab[!is.na(m_set_tab$value)]
m_set_tab$True<-1
set_tab<-dcast(m_set_tab,id ~ value,length,value.var="True")
# /MBS M12 AVR ISD DKA /BFZ /SOI /GTC THS
drops <- c('AER','AKH','BNG','DGM','DOM','DTK','ELD','EMN','FRF','GRN'
	,'M13','HOU','JOU','KLD','KTK','M13','M14','M15','M19','M20'
	,'NPH','OGW','ORI','RIX','RNA','RTR','WAR','XLN','BFZ'
	,'DKA','MBS','ISD','GTC')
set_tab<-set_tab[, !drops, with = FALSE]

master<-merge(master,set_tab,all.x=T)
#=============================================================================
#颜色
#color_tab<-as.data.table(tstrsplit(card_tab$color," "))
#color_tab$id<-card_tab$id

#m_color_tab<-melt(color_tab,id.vars = "id")
#m_color_tab<-m_color_tab[!is.na(m_color_tab$value)]
#m_color_tab$True<-1
#color_tab<-dcast(m_color_tab,id ~ value,length,value.var="True")

#master<-merge(master,color_tab,all.x=T)

#drops <- c('Red','White')
#master<-master[, !drops, with = FALSE]

#=============================================================================
#卡牌攻击
power_tab <- data.table(card_tab$power)
power_tab$id <- card_tab$id

m_power_tab<-melt(power_tab,id.vars = "id")
m_power_tab<-m_power_tab[!is.na(m_power_tab$value)]
m_power_tab$True<-1
power_tab<-dcast(m_power_tab,id ~ value,length,value.var="True")

#master<-merge(master,card_tab[,.(id,toughness)],all.x=T)
master<-merge(master,power_tab,all.x=T)

drops <- c('1','2','3','4','5','6','8','9','10','11','15','16','V1','11','*','0','7')
master<-master[, !drops, with = FALSE]

#=============================================================================
#卡牌强度
toughness_tab <- data.table(card_tab$toughness)
toughness_tab$id <- card_tab$id

m_toughness_tab<-melt(toughness_tab,id.vars = "id")
m_toughness_tab<-m_toughness_tab[!is.na(m_toughness_tab$value)]
m_toughness_tab$True<-1
toughness_tab<-dcast(m_toughness_tab,id ~ value,length,value.var="True")

#master<-merge(master,card_tab[,.(id,toughness)],all.x=T)
master<-merge(master,toughness_tab,all.x=T)
#=============================================================================
#卡牌费用特征

#card_tab$cost<-0
#card_tab$cost[grep(0,card_tab$cmc,card_tab$cmc)]<-"raw"
#card_tab$cost[grep(1,card_tab$cmc,card_tab$cmc)]<-"raw"
#card_tab$cost[grep(2,card_tab$cmc,card_tab$cmc)]<-"raw"
#card_tab$cost[grep(3,card_tab$cmc,card_tab$cmc)]<-"raw"
#card_tab$cost[grep(4,card_tab$cmc,card_tab$cmc)]<-"mid"
#card_tab$cost[grep(5,card_tab$cmc,card_tab$cmc)]<-"mid"
#card_tab$cost[grep(6,card_tab$cmc,card_tab$cmc)]<-"mid"
#card_tab$cost[grep(7,card_tab$cmc,card_tab$cmc)]<-"mid"
#card_tab$cost[grep(8,card_tab$cmc,card_tab$cmc)]<-"well"
#card_tab$cost[grep(9,card_tab$cmc,card_tab$cmc)]<-"well"
#card_tab$cost[grep(10,card_tab$cmc,card_tab$cmc)]<-"well"
#card_tab$cost[grep(11,card_tab$cmc,card_tab$cmc)]<-"well"
#card_tab$cost[grep(12,card_tab$cmc,card_tab$cmc)]<-"well"
#card_tab$cost[grep(13,card_tab$cmc,card_tab$cmc)]<-"well"

#master<-merge(master,card_tab[,.(id,cost)],all.x=T)

#master$Cost <- 0
#master$Cost[grep("well",master$cost)]<-1

#drops <- c('cost')
#master<-master[, !drops, with = FALSE]
#=============================================================================

master$current_price[is.na(master$current_price)]<-mean(master$current_price,na.rm=T)

master[is.na(master)]<-0

train<-master[train==1]
test<-master[train==0]

#drops <- c('season','season_2')
#train<-train[, !drops, with = FALSE]
#test<-test[, !drops, with = FALSE]

train$train<-NULL
test$train<-NULL
test$future_price<-NULL

fwrite(train,"train_v1.csv")
fwrite(test,"test_v1.csv")
#=============================================================================
setwd("/Volumes/Transcend/PSU/STAT 380 DATA/Mid Project/MTG2")
library(data.table)
library(caret)
library(Metrics)
library(glmnet)
library(plotmo)
library(lubridate)

train<-fread("train_v1.csv")
test<-fread("test_v1.csv")
example_sub<-fread("example_submission.csv")

test$future_price<-0

year_num<-as.numeric(as.factor(year(as_date(train$current_date))))

drops<- c('id','future_date','current_date','season','season_2','V1',
	'1','10','11','15','16','2','3','4','5','6','7','9','*','0','12.y','8')
train<-train[, !drops, with = FALSE]
test<-test[, !drops, with = FALSE]

train_y<-train$future_price
#train_cmc<-train$cmc
#test_cmc <- test$cmc

#setnames(train,'V1','types')
#setnames(test,'V1','types')


#最重要的一步
dummies <- dummyVars(future_price ~ ., data = train)
train<-predict(dummies, newdata = train)
test<-predict(dummies, newdata = test)

#Sesson_1winter <- 0
#Sesson_2winter <- 0
#test <- cbind(test,Sesson_1winter,Sesson_2winter)


train<-as.matrix(train)

gl_model<-cv.glmnet(train, train_y, alpha = 1,family="gaussian",foldid = year_num,nfolds = length(unique(year_num)))

plot(gl_model)

bestlam<-gl_model$lambda.min

gl_model<-glmnet(train, train_y, alpha = 1,family="gaussian")

plot_glmnet(gl_model)

test<-as.matrix(test)

#use the full model
pred<-predict(gl_model,s=bestlam, newx = test)

example_sub$future_price<-pred

fwrite(example_sub,"submit_TEST.csv")

#季节非常牛逼 0.334138
#=============================================================================


