library(e1071)
library(caTools)
library(caret)
library(class)
f<-read.csv("train.csv")
#print(head(f))
set.seed(149)
f$blue <- as.factor(f$blue)
f$dual_sim <- as.factor(f$dual_sim)
f$four_g <- as.factor(f$four_g)
f$three_g<- as.factor(f$three_g)
f$touch_screen <- as.factor(f$touch_screen)
f$wifi <- as.factor(f$wifi)
f$price_range <- as.factor(f$price_range)
df<- f[,c(1,3,5,7,8,9,10,11,12,13,14,15,16,17)]
a<-f[,c(2,4,6,18,19,20)]
df <- df[order(runif(2000)),]
n <- function(b){
  (b-min(b))/(max(b)-min(b))
}
df_new <- as.data.frame(lapply(df, n))
df_new <- cbind(df_new, a)
df_new <- cbind(df_new, f$price_range)
#df_new<- cbind(df_new,f$price_range)
#splitting the data into train and test----
set.seed(149)
split<-sample.split(df_new,SplitRatio=0.8)
tr_data<-subset(df_new,split==TRUE)
ts_data<-subset(df_new,split==FALSE)
#split<-sample.split(df_new,SplitRatio=0.8)
#tr_data<-subset(df_new,split==TRUE)
#ts_data<-subset(df_new,split==FALSE)
#Applying knn for k = 1----
classifier_knn <- knn(train = tr_data, test = ts_data, cl = tr_data$`f$price_range`, k = 1)
#classifier_knn
#summary(classifier_knn)
cm <- table(ts_data$`f$price_range`, classifier_knn)
confusionMatrix(cm, mode="everything", positive="1")
#Applying knn for k = 3----
classifier_knn <- knn(train = tr_data, test = ts_data, cl = tr_data$`f$price_range`, k = 3)
#classifier_knn
#summary(classifier_knn)
cm <- table(ts_data$`f$price_range`, classifier_knn)
confusionMatrix(cm, mode="everything", positive="1")
#Applying knn for k = 5----
classifier_knn <- knn(train = tr_data, test = ts_data, cl = tr_data$`f$price_range`, k = 5)
#classifier_knn
#summary(classifier_knn)
cm <- table(ts_data$`f$price_range`, classifier_knn)
confusionMatrix(cm, mode="everything", positive="1")
