# Used data columns
d1 = d[,c("age", "hypertension", "heart_disease",
          "avg_glucose_level", "stroke")]
str(d1)

# Train and Test Partition
set.seed(123)
splitindex = createDataPartition(d1$stroke, p = 0.7, 
                                 list = FALSE)
d_train1 = d1[splitindex,]
d_test1 = d1[-splitindex,]

# Balance data by oversampling
drose = ROSE(stroke ~., data=d_train1, seed=123,p=0.5)$d
table(drose$stroke)

set.seed(123)
glm.fit = glm(stroke~.,family = binomial, data = drose)
summary(glm.fit)

glm.probs <- predict(glm.fit,type = "response")
glm.pred <- ifelse(glm.probs > 0.5, 1, 0)
x=table(pred=glm.pred, actual=drose$stroke)
x

glm.probs2 = predict(glm.fit, newdata = d_test1, type="response")
glm.pred2 = ifelse(glm.probs2 > 0.5, 1, 0)
y=table(pred=glm.pred2, actual=d_test1$stroke)
y

fourfoldplot(x, color = c("turquoise", "pink"),
             conf.level = 0, margin = 1, main = "Train Confusion Matrix")
fourfoldplot(y, color = c("turquoise", "pink"),
             conf.level = 0, margin = 1, main = "Test Confusion Matrix")

acc1 <- ( x[2,2] + x[1,1] ) / (length(drose$stroke))
acc1
Precision1 =  x[2,2] / (x[2,2] + x[1,2])
Precision1
Recall1 =  x[2,2] / ( x[2,2] + x[2,1] )
Recall1

acc2 <- (y[2,2] + y[1,1]) / (length(d_test1$stroke))
acc2
Precision2 =  y[2,2] / (y[2,2] + y[1,2])
Precision2
Recall2 =  y[2,2] / (y[2,2] + y[2,1])
Recall2

# 10 k-fold Cross validation
set.seed(123)
drose$stroke=as.factor(drose$stroke)

cv_model2 = train(stroke~., data=drose,method="glm",
                  trControl=trainControl(method="cv",number=10),
                  family="binomial");
print(cv_model2)
pred1 = predict(cv_model2)
confusionMatrix(data=pred1, drose$stroke)
