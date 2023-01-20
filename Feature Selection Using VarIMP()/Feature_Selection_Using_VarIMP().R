# ensure results are repeatable
set.seed(7)
# load the library
library(mlbench)
library(caret)
# load the dataset
df <- read.csv("train.csv")
df$blue <- as.factor(df$blue)
df$dual_sim <- as.factor(df$dual_sim)
df$four_g <- as.factor(df$four_g)
df$three_g<- as.factor(df$three_g)
df$touch_screen <- as.factor(df$touch_screen)
df$wifi <- as.factor(df$wifi)
df$price_range <- as.factor(df$price_range)
# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(price_range~., data=df, method="lvq", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)

