#Loading the dataset
library("tidyverse")
library(caTools)
dataset=read_csv("Data/Social_Network_Ads.csv")


set.seed(123)
split=sample.split(dataset$Purchased,SplitRatio=3/4)
training_set = subset(dataset,split==TRUE)
test_set=subset(dataset,split==FALSE)

#Feature Scaling
training_set[,1:2] =scale(training_set[,1:2])
test_set[,1:2]=scale(test_set[,1:2])


classifier = glm(formula=Purchased~.,
                 family=binomial,
                 data=training_set)

prob_pred = predict(classifier,type='response',newdata=test_set[-3])
y_pred=ifelse(prob_pred>0.5,1,0)
print(y_pred)
matrixcm = table(test_set$Purchased,y_pred)

class(test_set[,3])
class(y_pred)
classtest