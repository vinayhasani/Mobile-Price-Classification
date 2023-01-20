#Importing Libraries And reading The file----
library(e1071)
library(caTools)
library(caret)
library(class)
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
#Applying knn for k = 1----
classifier_knn <- knn(train = tr_data, test = ts_data, cl = tr_data$price_range, k = 1)
#classifier_knn
#summary(classifier_knn)
#Confusion Matric And Accuracy For k = 1----
cm <- table(ts_data$price_range, classifier_knn)
confusionMatrix(cm)

#Applying knn for k = 3----
classifier_knn <- knn(train = tr_data, test = ts_data, cl = tr_data$price_range, k = 3)
#classifier_knn
#summary(classifier_knn)
#Confusion Matric And Accuracy For k = 3----
cm <- table(ts_data$price_range, classifier_knn)
confusionMatrix(cm)


#Applying knn for k = 5----
classifier_knn <- knn(train = tr_data, test = ts_data, cl = tr_data$price_range, k = 5)
#classifier_knn
#summary(classifier_knn)
#Confusion Matric And Accuracy For k = 5----
cm <- table(ts_data$price_range, classifier_knn)
confusionMatrix(cm)

#Applying knn for k = 7----
classifier_knn <- knn(train = tr_data, test = ts_data, cl = tr_data$price_range, k = 7)
#classifier_knn
#summary(classifier_knn)
#Confusion Matric And Accuracy For k = 7----
cm <- table(ts_data$price_range, classifier_knn)
confusionMatrix(cm)
#Applying knn for k = 15----
classifier_knn <- knn(train = tr_data, test = ts_data, cl = tr_data$price_range, k = 15)
#classifier_knn
#summary(classifier_knn)
#Confusion Matric And Accuracy For k = 15----
cm <- table(ts_data$price_range, classifier_knn)
confusionMatrix(cm)
confusionMatrix(cm, mode = "everything", positive="1")

#but with below we get only for class 0
#library(MLmetrics)
#res = F1_Score(classifier_knn,ts_data$price_range)
classifier_knn <- as.integer(classifier_knn)
tr_data$price_range<- as.integer(tr_data$price_range)
s2 <- roc(ts_data$price_range, classifier_knn)
s2