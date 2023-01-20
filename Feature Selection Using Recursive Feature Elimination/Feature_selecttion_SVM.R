library(e1071)
library(caTools)
library(caret)
f<-read.csv("train.csv")
#print(head(f))
f$blue <- as.factor(f$blue)
f$dual_sim <- as.factor(f$dual_sim)
f$four_g <- as.factor(f$four_g)
f$three_g<- as.factor(f$three_g)
f$touch_screen <- as.factor(f$touch_screen)
f$wifi <- as.factor(f$wifi)
f$price_range <- as.factor(f$price_range)
#splitting the data into train and test----
set.seed(123)
split<-sample.split(f,SplitRatio=0.8)
tr_data<-subset(f,split==TRUE)
ts_data<-subset(f,split==FALSE)

svmfita = svm(price_range~ram+battery_power+px_height+px_width+fc+pc, data = tr_data, kernel = "linear", cost = 10, scale = FALSE)
print(svmfita)
preda <- predict(svmfita, ts_data)
t11<- table(ts_data$price_range, preda)
confusionMatrix(t11, mode="everything", positive ="1")

#ROC
preda <- as.integer(preda)
tr_data$price_range<- as.integer(tr_data$price_range)
s3 <- roc(ts_data$price_range, preda)
s3