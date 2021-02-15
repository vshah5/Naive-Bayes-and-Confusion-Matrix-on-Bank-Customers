

#Naive Bayes Code
install.packages('e1071', dependencies=TRUE)
library(e1071)

#point of Naive Bayes is determine if x is more likely than y
bank.df <- read.csv("C:/Users/valay/OneDrive/Desktop/Ivey/Winter Term 2021/Big Data Analytics/Class 6/Individual Assignment/Bank-1.csv")
head(bank.df)


# Create training and validation sets
selected.var <- c(11, 14, 15)

#from the 1000 data entries, sample will be 60% of that (600)
train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6) 
#c means to concatenate

train.df <- bank.df[train.index, selected.var]
head(train.df)
head(train.index)
valid.df <- bank.df[-train.index, selected.var]

# run naive bayes
bank.nb <- naiveBayes(as.factor(Personal_Loan) ~ ., data = train.df)
bank.nb

# predict probabilities
pred.prob <- predict(bank.nb, newdata = valid.df, type = "raw")
head(pred.prob)

# predict class membership
pred.class <- predict(bank.nb, newdata = valid.df)
head(pred.class)
head(valid.df)

#Install a new package-caret-and make functions available
install.packages("caret")
library(caret)

install.packages("ROCR")
library(ROCR)
#Confusion Matrix

classification <- ifelse(pred.prob[,2]>0.1, 1, 0)
actual = valid.df$Personal_Loan
confusionMatrix(as.factor(classification), as.factor(actual))



#Output <- data.frame(actual, predprob=pred, classification)



# Receiver Operating Characteristic (ROC) Curve
#Install New Package

#use predict() with type = "response" to compute predicted probabilities.
#Displaying Actual and Predicted Probability together

#Output<-data.frame(actual = test.df$Personal_Loan, predicted = pred[,2])
#head(pred)
#head(Output)

#Choose a threshold and classify records




predObj <- prediction(pred.prob[,2], train.df$Personal_Loan)

rocObj = performance(predObj, measure="tpr", x.measure="fpr")
aucObj = performance(predObj, measure="auc")  

plot(rocObj, main = paste("Area under the curve:", round(aucObj@y.values[[1]] ,4)))





