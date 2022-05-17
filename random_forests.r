# RANDOM FORESTS
rm(list=ls())

library(ROSE)
library(caret)
library(randomForest)

d <- read.csv(file.choose(), header = TRUE, stringsAsFactors = FALSE)
attach(d)

# Splitting data into training and testing
d2 = d[,c("age", "hypertension", "heart_disease","avg_glucose_level", 
          "stroke")]
str(d2)

splitindex2 = createDataPartition(d2$stroke, p = 0.7, 
                                  list = FALSE)
train2 = d2[splitindex2,]
test2  = d2[-splitindex2,]

#oversampling data
set.seed(123)
drose2 <- ROSE(stroke ~., data=train2, seed=123, p=0.5)$d
table(drose2$stroke)

set.seed(123)
rf <- randomForest(stroke ~., data=drose2)
rf
importance(rf)
varImpPlot(rf)

#predict on train and test
rf_train.pred <- predict(rf)
rf_test.pred <- predict(rf, newdata = test2)

x1 = table(Pred = rf_train.pred, Actual = drose2$stroke)
x1

y1 = table(Pred = rf_test.pred, Actual= test2$stroke)
y1

fourfoldplot(x1, color = c("red3", "seagreen"), 
             conf.level = 0, margin = 1, main = "Confusion Matrix - Train")
fourfoldplot(y1, color = c("red3", "seagreen"),
             conf.level = 0, margin = 1, main = "Confusion Matrix - Test")

#Percentages (train)
acc1 <- ( x1[2,2] + x1[1,1] ) / (length(drose2$stroke))
acc1
Precision1 =  x1[2,2] / (x1[2,2] + x1[1,2])
Precision1
Recall1 =  x1[2,2] / ( x1[2,2] + x1[2,1] )
Recall1

acc2 <- (y1[2,2] + y1[1,1]) / (length(test2$stroke))
acc2
Precision2 =  y1[2,2] / (y1[2,2] + y1[1,2])
Precision2
Recall2 =  y1[2,2] / (y1[2,2] + y1[2,1])
Recall2

#10 fold cross validation
cv_model3 = train(stroke~., data=drose2,method="rf",
                  trControl=trainControl(method="cv",number=10),
                  family="binomial");
cv_model3
pred2=predict(cv_model3)
confusionMatrix(data=pred2,drose2$stroke)
