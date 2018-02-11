library(MASS)
data(cats)
?cats
library(vegan) 
cats1 <- decostand(cats[,-1],"range")

head(cats)
cats1['Sex']=cats$Sex
cats1<-subset(cats1,select=c(Sex,Bwt,Hwt))
cats = cats1
inputData<-cats
rm(cats,cats1)
head(inputData)
inputData=inputData[sample(1:144,10),]

library(e1071)
model <- svm(Sex~., data = cats1, kernel = "linear",cost = 100)
summary(model)
plot(model, cats1,xlim=c(0,1),ylim=c(0,1))
table(cats1$Sex,cats1$Sex)
compareTable <- table(predict(model),cats1$Sex)  
compareTable


model_kernel <- svm(Sex~., data = cats1, kernel = "radial",cost = 10)
print(model_kernel)
summary(model_kernel)
plot(model_kernel, cats1)
compareTable <- table(predict(model_kernel),inputData$Sex) 
mean(inputData$Sex != predict(model)) #misclassification error

### Tuning
# Prepare training and test data
set.seed(100) # for reproducing results
rowIndices <- 1 : nrow(cats1) # prepare row indices
sampleSize <- 0.8 * length(rowIndices) # training sample size
trainingRows <- sample (rowIndices, sampleSize) # random sampling
trainingData <- cats1[trainingRows, ] # training data
testData <- cats1[-trainingRows, ] # test data

tuned <- tune.svm(Sex ~., data = trainingData, gamma = 10^(-6:-1), cost = 10^(1:2)) # tune

summary (tuned) # to select best gamma and cost

svmfit <- svm (Sex ~ ., data = trainingData, kernel = "radial", cost = 100, gamma=0.01) # radial svm
print(svmfit)
plot(svmfit, trainingData)
compareTable <- table (testData$Sex, predict(svmfit, testData))  # comparison table
mean(testData$Sex != predict(svmfit, testData)) # 13.79% misclassification error


model1 <- svm(Sex~., data = trainingData,gamma=0.1,cost=100)
prediction1 <- predict(model1, testData)
tab1 <- table(pred = prediction1, true = testData[,3])
tab1 

