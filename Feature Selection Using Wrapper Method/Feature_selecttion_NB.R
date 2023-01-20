#Importing Libraries And reading The file ----
library(e1071)
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

#Applying Multinomial Logestic Regression----
model <- naiveBayes(price_range ~ram+battery_power+px_height+px_width+mobile_wt+dual_sim+int_memory, data = tr_data)

#Summarize the model----
print(summary(model))

#prediction part----
predicted <- predict(model, ts_data)
head(predicted)

#final part of Confusion Matrix And Accuracy and all----
accu<-mean(predicted == ts_data$price_range)  #accuracy of the multinomial logistic regression
t1<-table(ts_data$price_range, predicted)
confusionMatrix(t1, mode="everything", positive="1")

#ROC
predicted <- as.integer(predicted)
tr_data$price_range<- as.integer(tr_data$price_range)
s5 <- roc(ts_data$price_range, predicted)
s5