## 
rm(list=ls())

d <- read.csv(file.choose(), header = TRUE, stringsAsFactors = TRUE)
attach(d)
View(d)

# Gender and Stroke grouped bar grap
gender_type <- table(d$stroke_t, d$bmi)
barplot(gender_type, 
        main="Stroke Dist",
        xlab = "Stroke", 
        ylab = "Counts",
        col=c("green","red"), breaks=10,
        legend=rownames(gender_type),
        beside=T)

# Work Type Bar Graph
work_type <- table(d$work_type)
barplot(work_type, 
        main="Work Distribution",
        xlab = "Type of Work", 
        col=c("lightblue"))

# Age Histogram
hist(d$age, 
     main = "Age Distribution",
     xlab = "Age", prob=TRUE,
     freq=FALSE, xlim=c(0,95))
lines(density(d$age), col="black")

# Avg_glucose Histogram
hist(d$avg_glucose_level, breaks = 25, col="lightgreen",
     main = "Histogram of Glucose Density", 
     xlab = "Glucose Levels", prob=TRUE,
     freq=FALSE, xlim=c(50,300))
lines(density(d$avg_glucose_level),col="red")

# Avg_glucose BMI
hist(d$bmi, breaks = 25, col="lightpink",
     main = "Histogram of BMI Density",
     xlab = "BMI", prob=TRUE,
     freq=FALSE)
lines(density(na.omit(d$bmi)), col="black")

d$gender = ifelse(d$gender == "Male", 1, 0)
d$gender = as.numeric(d$gender)

d$hypertension = as.numeric(d$hypertension)

d$heart_disease = as.numeric(d$heart_disease)

d$bmi = as.numeric(d$bmi)
d$bmi[is.na(d$bmi)] = mean(d$bmi, na.rm = TRUE)

#Giving smoking status a numeric classification 
d$smoking_status = as.character(d$smoking_status)
for(i in 1:length(d$id)){
  if (d$smoking_status[i] == "never smoked"){
    d$smoking_status[i] = 0;
  }
  else if(d$smoking_status[i]=="smokes"){
    d$smoking_status[i] = 10;
  }
  else if(d$smoking_status[i]=="formerly smoked"){
    d$smoking_status[i] = 3;
  }
  else if (d$smoking_status[i]=="Unknown"){
    d$smoking_status[i] = 5;
  }
}
d$smoking_status = as.numeric(d$smoking_status)
d$stroke = ifelse(d$stroke == 1, 'Stroke', 'No Stroke')
d$stroke = factor(d$stroke)
