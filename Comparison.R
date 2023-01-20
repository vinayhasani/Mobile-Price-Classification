library(ggplot2)
data<-read.csv("comparison.csv")
gr<-ggplot(data, aes(x = Classifiers, y = Performance, fill = Performance.Parameters)) + geom_col(position = position_dodge()) + scale_fill_manual(values = c("#191970", "#0000FF", "#1E90FF", "#87CEFA" )) + ggtitle("Comparision of Accuracy, Precision, Recall and Specificity for Various Classifiers")
print(gr)

gr1<-ggplot(data, aes(x = Classifiers, y = performance, fill = performance.parameters)) + geom_col(position = position_dodge(), alpha = 0.75) + scale_fill_manual(values = c("#006400", "#2E8B57", "#32CD32", "#98FB98" )) + ggtitle("Comparision of Specificity, F-Measure and AUC for Various Classifiers")
print(gr1)
