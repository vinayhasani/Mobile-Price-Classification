library(data.table)    # provides enhanced data.frame
library(ggplot2)       # plotting
library(glmnet) 
x = model.matrix(price_range~., tr_data)       # matrix of predictors
y = tr_data$price_range                       # vector y values
set.seed(123)                                # replicate  results
lasso_model <- cv.glmnet(x, y, alpha=1)      # alpha=1 is lasso
best_lambda_lasso <- lasso_model$lambda.1se  # largest lambda in 1 SE
lasso_coef <- lasso_model$glmnet.fit$beta[,  # retrieve coefficients
                                          lasso_model$glmnet.fit$lambda  # at lambda.1se
                                          == best_lambda_lasso]
coef_l = data.table(lasso = lasso_coef)      # build table
coef_l[, feature := names(lasso_coef)]       # add feature names
to_plot_r = melt(coef_l                      # label table
                 , id.vars='feature'
                 , variable.name = 'model'
                 , value.name = 'coefficient')
ggplot(data=to_plot_r,                       # plot coefficients
       aes(x=feature, y=coefficient, fill=model)) +
  coord_flip() +         
  geom_bar(stat='identity', fill='brown4', color='blue') +
  facet_wrap(~ model) + guides(fill=FALSE) 