library(Boruta)
boruta_output <- Boruta(price_range ~ ., data=tr_data, doTrace=0) 
names(boruta_output)
boruta_signif <- getSelectedAttributes(boruta_output, withTentative = TRUE)
print(boruta_signif) 
# Do a tentative rough fix
roughFixMod <- TentativeRoughFix(boruta_output)
boruta_signif <- getSelectedAttributes(roughFixMod)
print(boruta_signif)
# Variable Importance Scores
imps <- attStats(roughFixMod)
imps2 = imps[imps$decision != 'Rejected', c('meanImp', 'decision')]
head(imps2[order(-imps2$meanImp), ])  # descending sort
# ploting
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")  

