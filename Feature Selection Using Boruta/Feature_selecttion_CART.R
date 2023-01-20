#Importing Libraries And reading The file ----
library(caTools)
library(rpart)
library(caret)
library(rpart.plot)
f<-read.csv("train.csv")
#head(f)
f$blue <- as.factor(f$blue)
f$dual_sim <- as.factor(f$dual_sim)
f$four_g <- as.factor(f$four_g)
f$three_g<- as.factor(f$three_g)
f$touch_screen <- as.factor(f$touch_screen)
f$wifi <- as.factor(f$wifi)
f$price_range <- as.factor(f$price_range)
#splitting it into train and test data----
set.seed(123)
split<-sample.split(f,SplitRatio=0.8)
tr_data<-subset(f,split==TRUE)
ts_data<-subset(f,split==FALSE)

#set.seed(12345)
# Training with classification tree
model <- rpart(price_range ~ram+battery_power+px_height+px_width+mobile_wt, data=tr_data, method="class")
#print(model, digits = 3)
rpart.plot(model)
printcp(model)
plotcp(model)
pred <- predict(model, ts_data, type = "class")
#pred
# Accuracy and other metrics
t1<-table(ts_data$price_range, pred)
confusionMatrix(t1, mode="everything", positive="1")

#ROC
pred <- as.integer(pred)
tr_data$price_range<- as.integer(tr_data$price_range)
s6 <- roc(ts_data$price_range, pred)
s6
