library(MASS) #for stepAIC (stepwise)
library(tidyverse) # for data manipulation
library(corrplot) # for correlation plot
library(ggplot2) # for plotting
library(gridExtra) # for grid.arrange
library(correlation) # for partial correlation
library(stats) # for anova 
library(caret) # for confusion matrix
library(car) # for vif package
library(ROCR) # for roc curve
library(ROSE) # for oversampling (just in case)
library(leaps) # for stepwise selection

#read data
#CHOOSE BETWEEN:
#data.csv = original dataset
#data_without_null_and_all_numeric.csv = dataset without null values and all numeric

data = read.csv("data.csv")
#drop X column
data = data %>% select(-X)
attach(data)
correlations = read.csv("correlations.csv")
partial_corr = read.csv("partial_corr.csv")
df_top3 = read.csv("df_top3.csv")


#compute the leverage statistic
leverage = hatvalues(lm(satisfaction ~ ., data = data))

#save them on a csv file
write.csv(leverage, file = "leverage.csv")

#plot the leverage statistic
plot(leverage, pch = 19, col = "blue", main = "Leverage Statistic", xlab = "Observation Number", ylab = "Leverage Statistic")

#COLLINEARITY
#It refers to the situation in which 2 or more predictor variables are closely
#related to one another. The presence of collinearity can destabilize the model
#and make it harder to estimate the effect of each predictor variable.

#LOW collinearity is desirable.

#VIF (Variance Inflation Factor) is a measure of collinearity among predictor variables
#within a multiple regression. It is calculated by taking the the ratio of the variance
#of all a given model's betas divide by the variance of a single beta if it were fit alone.

#VIF = 1: no collinearity
#1 < VIF < 5: moderate collinearity
#VIF > 5: high collinearity (this is the case we want to avoid)

#compute the VIF

glm_compl = glm(satisfaction ~ ., data = data, family = "binomial")

#We compute the reference level R-squared

s = summary(glm_compl)
r2 = 1 - (s$deviance/s$null.deviance)

1/(1-r2)


vif = vif(glm_compl)
vif
#Depending on what value of VIF you deem to be too high to include in the model, 
#you may choose to remove certain predictor variables and see if the corresponding 
#R-squared value or standard error of the model is affected.

#We decide to remove Departure delay and arrival delay because
#they seems to be more correlated each other than with the target variable

#remove departure delay and arrival delay
#TODO: check if it's correct to remove them, for example maybe only one of them is enough (remove and check VIF again)
data = data %>% select(-Departure_Delay_in_Minutes, -Arrival_Delay_in_Minutes)

#anova
anova = anova(logistic_model, test = "Chisq")
#plot anova



















































#train test split
set.seed(123)
train_index = sample(1:nrow(data), 0.8*nrow(data))
# 80% of data is used for training
train = data[train_index,]
# 20% of data is used for testing
test = data[-train_index,]

# Set X and Y, where Y is the target variable "satisfaction"
X_train = train %>% select(-satisfaction)
y_train = train$satisfaction

X_test = test %>% select(-satisfaction)
y_test = test$satisfaction








#LOGISTIC REGRESSION

#fit logistic regression model
logistic_model = glm(satisfaction ~.  , data = train, family = "binomial")
summary(logistic_model)

#predict on test data
logistic_pred = predict(logistic_model, newdata = test, type = "response")
logistic_pred = ifelse(logistic_pred > 0.5, 1, 0)

#show confusion matrix
confusionMatrix(as.factor(logistic_pred), as.factor(y_test))
table(y_test, logistic_pred)

#show accuracy
mean(logistic_pred == y_test)



#LOGISTIC REGRESSION WITH STEPWISE SELECTION
#forward, backward, mixed, we choose mixed (both)
logistic_model_stepwise = stepAIC(logistic_model, direction = "both", trace = FALSE)
summary(logistic_model_stepwise)

#predict on test data
logistic_pred_stepwise = predict(logistic_model_stepwise, newdata = test, type = "response")
logistic_pred_stepwise = ifelse(logistic_pred_stepwise > 0.5, 1, 0)

#show confusion matrix
confusionMatrix(as.factor(logistic_pred_stepwise), as.factor(y_test))
table(y_test, logistic_pred_stepwise)

#show accuracy
mean(logistic_pred_stepwise == y_test)

install.packages('e1071')
library(e1071)

#Build a simple svm model
svm_model = svm(satisfaction ~ ., data = train)
summary(svm_model)

#predict on test data
svm_pred = predict(svm_model, newdata = test)
svm_pred = ifelse(svm_pred > 0.5, 1, 0)

#show confusion matrix
confusionMatrix(as.factor(svm_pred), as.factor(y_test))
table(y_test, svm_pred)

