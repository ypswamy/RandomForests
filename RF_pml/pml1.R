training <- read.csv("C:/Users/Pandu/Desktop/Pandu_ course/CASE_STUDY/RF_pml/pml-training.csv")

#attaching data frame to reduce the length of the variable names associated to it
attach(training) 

colnames(training)
summary(training)
str(training)
head(training)


testing <- read.csv("C:/Users/Pandu/Desktop/Pandu_ course/CASE_STUDY/RF_pml/pml-testing.csv")

colnames(testing)
summary(testing)
str(testing)
head(testing)


tail(names(training),24)


#The response variable is "classe" and the rest of the variables are all potential ##predictors of this response variable. To get an idea of the size of this dataset, here #are some basic numbers:
# the number of variables is 160
#the number of observations in this dataset is 19622

	
summary(training$classe)

#check missing values
colSums(is.na(training))
colSums(is.na(testing))

is.factor(kurtosis_roll_belt)
str(kurtosis_roll_belt)


library(ggplot2); 
library(caret)

## Loading required package: lattice
#selecting a few of the more promising predictors to be plotted
colSelection<- c("roll_belt","pitch_belt", "yaw_belt", "roll_arm", "pitch_arm","yaw_arm")
 
#creating a feature plot 
featurePlot(x=training[,colSelection],y = training$classe,plot="pairs")

qplot(roll_belt, roll_forearm, colour=classe, data=training)

par(mfrow=c(1,2))
hist(roll_belt, main = "roll_belt")
hist(roll_forearm, main="roll_forearm")

training <- training[,-c(1,2,3,4,5,6,7)]

#removed all the columns with missing values from the dataset:
training <-training[,colSums(is.na(training))==0]

#found all the columns that are factors, while ignoring the last column which was the #response variable "classe."

col_names <- c()
n <- ncol(training)-1
for (i in 1:n) {
     if (is.factor(training[,i])){
           col_names <- c(col_names,i)
           }
}

training <- training[,-col_names]

#used a for loop to set up cross validation using random subsampling to fit three random #forest models to random subsets of the training data, called "trainingSet". I then used #these models to predict the "classe" variable of the testing subsets, called #"testingSet". I was hoping for an out of sample error of less than 20%.


library(randomForest)
first_seed <- 123355
accuracies <-c()
for (i in 1:3){
       set.seed(first_seed)
       first_seed <- first_seed+1
       trainIndex <- createDataPartition(y=training$classe, p=0.75, list=FALSE)
       trainingSet<- training[trainIndex,]
       testingSet<- training[-trainIndex,]
       modelFit <- randomForest(classe ~., data = trainingSet)
       prediction <- predict(modelFit, testingSet)
       testingSet$rightPred <- prediction == testingSet$classe
       t<-table(prediction, testingSet$classe)
       print(t)
       accuracy <- sum(testingSet$rightPred)/nrow(testingSet)
       accuracies <- c(accuracies,accuracy)
       print(accuracy)
}


library(randomForest)
first_seed <- 123355
accuracies <-c()
for (i in 1:3){
       set.seed(first_seed)
       first_seed <- first_seed+1
       trainIndex <- createDataPartition(y=training$classe, p=0.75, list=FALSE)
       trainingSet<- training[trainIndex,]
       testingSet<- training[-trainIndex,]
       modelFit <- randomForest(classe ~., data = trainingSet)
       prediction <- predict(modelFit, testingSet)
       testingSet$rightPred <- prediction == testingSet$classe
       t<-table(prediction, testingSet$classe)
       print(t)
       accuracy <- sum(testingSet$rightPred)/nrow(testingSet)
       accuracies <- c(accuracies,accuracy)
       print(accuracy)
}

