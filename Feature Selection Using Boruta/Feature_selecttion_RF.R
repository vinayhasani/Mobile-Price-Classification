library(randomForest)
library(caTools)
library(caret)
f<-read.csv("train.csv")
head(f)
f$blue <- as.factor(f$blue)
f$dual_sim <- as.factor(f$dual_sim)
f$four_g <- as.factor(f$four_g)
f$three_g<- as.factor(f$three_g)
f$touch_screen <- as.factor(f$touch_screen)
f$wifi <- as.factor(f$wifi)
f$price_range <- as.factor(f$price_range)
#spliting it into train and test data----
set.seed(123)
split<-sample.split(f,SplitRatio=0.8)
tr_data<-subset(f,split==TRUE)
ts_data<-subset(f,split==FALSE)
#Building Random Forest classifier
rf1 <- randomForest(price_range ~ ram+battery_power+px_height+px_width+mobile_wt,data=tr_data, importance=TRUE)
importance(rf1)
#Predicting 
pred1 = predict(rf1, ts_data)
#pred = as.numeric(pred)
cm1 = table(ts_data$price_range, pred1)
confusionMatrix(cm1, mode = "everything", positive = "1")

#ROC
pred1 <- as.integer(pred1)
tr_data$price_range<- as.integer(tr_data$price_range)
s4 <- roc(ts_data$price_range, pred1)
s4