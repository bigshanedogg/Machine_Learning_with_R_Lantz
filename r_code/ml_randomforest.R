install.packages("randomForest")
library(randomForest)

random <- sample(150,120)
iris_train <- iris[random,]
iris_test <- iris[-random,]
m <- randomForest(Species~.,data=iris_train, importance=TRUE)
#m <- randomForest(iris[,1:4],iris[,5]) #변수 직접 지정
importance(m) #각 변수의 중요성(Gini, Accuray) 파악
varImpPlot(m, main="varImpPlot of iris") #변수의 중요성 시각화
iris_pre <- predict(m, newdata=iris_test)

library(gmodels)
head(iris_pre)
CrossTable(iris[-random,]$Species,iris_pre, prop.chisq=FALSE, prop.c=FALSE, prop.r=FALSE, dnn=c('actual default','predicted default'))

install.packages("lattice")
library(lattice)
install.packages("robustbase")
library(robustbase)
install.packages("cvTools")
library(cvTools)
install.packages("foreach")
library(foreach)
set.seed(719)

K=10
R=3
cv <- cvFolds(nrow(iris),K=K,R=R)
grid <- expand.grid(ntree=c(10,100,200),mtry=c(3,4))

cv <- cvFolds(nrow(iris),K=10,R=3)
cv
result <- foreach(g=1:nrow(grid),.combine=rbind) %do% {
  foreach(r=1:R, .combine=rbind) %do% {
    validation_idx <- cv$subsets[which(cv$which==K),R]
    train <- iris[-validation_idx,]
    validation <- iris[validation_idx,]
    
    #training
    m <- randomForest(Species ~., 
                      data=train,
                      ntree=grid[g,"ntree"],
                      mtry=grid[g,"mtry"])
    #prediction
    predicted <- predict(m, newdata=validation)
    #estimating performance
    precision <- sum(predicted == validation$Species)/nrow(predicted)
    return(data.frame(g=g,precision=precision))
  }
}















