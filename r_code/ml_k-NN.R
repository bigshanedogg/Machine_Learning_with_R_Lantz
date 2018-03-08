setwd("/Users/hodong/Desktop/ml_prac/raw_data/3. kNN")
wbcd <- read.csv("wisc_bc_data.csv",header=TRUE)

#---------------데이터 전처리 과정---------------#
#View(wbcd) #str(wbcd)
wbcd <- wbcd[-1]
wbcd[,1] <- factor(wbcd[,1], levels=c("B","M"), labels=c("Benign","Malignant"))
#table(wbcd[,1])
#round(prop.table(table(wbcd[,1]))*100,digits=1)

#표준화 함수 설정
normalize <- function(x){
  #x <- as.numeric(as.character(x))
  return((x-min(x))/(max(x)-min(x)))
}
#표준화 테스트
#normalize(c(1,2,3,4,5))
#normalize(c(10,20,30,40,50))
wbcd_n <- as.data.frame(lapply(wbcd[2:31],normalize))
#str(wbcd) #str(wbcd_n)

wbcd_train <- wbcd_n[1:469,]
wbcd_test <- wbcd_n[470:569,]
wbcd_train_labels <- wbcd[1:469,1]
wbcd_test_labels <- wbcd[470:569,1]

head(wbcd_train)
head(wbcd_train_labels)
#---------------데이터 전처리 과정---------------#

#---------------데이터 분류 및 성과 측정---------------#
#kNN 적용
install.packages("class")
library(class)
k <- floor(sqrt(nrow(wbcd_train)))
#knn 함수 인자 구성 p <- knn(train_set, test_set, class(검사할 테스트 라벨), k)
wbcd_test_pred <- knn(train=wbcd_train,test=wbcd_test,cl=wbcd_train_labels, k=k)

#성과 측정
install.packages("gmodels")
library(gmodels)
CrossTable(x=wbcd_test_labels, y=wbcd_test_pred,prop.chisq=FALSE)
#비교해보기
#round(prop.table(table(wbcd_test_labels))*100,digits=1)
#---------------데이터 분류 및 성과 측정---------------#

#---------------데이터 분류 개선---------------#
#Z-score로 전환
wbcd <- read.csv("wisc_bc_data.csv",header=FALSE)
wbcd[,1] <- factor(wbcd[,1], levels=c("B","M"), labels=c("Benign","Malignant"))
wbcd_z <- as.data.frame(scale(wbcd[c(-1,-2)]))
k <- floor(sqrt(nrow(wbcd_train)))

wbcd_train <- wbcd_z[1:469,]
wbcd_test <- wbcd_z[470:569,]
wbcd_train_labels <- wbcd[1:469,2]
wbcd_test_labels <- wbcd[470:569,2]
wbcd_test_pred <- knn(train = wbcd_train, test=wbcd_test, cl=wbcd_train_labels, k=k)
CrossTable(x=wbcd_test_labels, y=wbcd_test_pred, prop.chisq=FALSE)

#다양한 k값 시도
wbcd_test_pred <- knn(train = wbcd_train, test=wbcd_test, cl=wbcd_train_labels, k=1)
CrossTable(x=wbcd_test_labels, y=wbcd_test_pred, prop.chisq=FALSE)
wbcd_test_pred <- knn(train = wbcd_train, test=wbcd_test, cl=wbcd_train_labels, k=9)
CrossTable(x=wbcd_test_labels, y=wbcd_test_pred, prop.chisq=FALSE)
wbcd_test_pred <- knn(train = wbcd_train, test=wbcd_test, cl=wbcd_train_labels, k=17)
CrossTable(x=wbcd_test_labels, y=wbcd_test_pred, prop.chisq=FALSE)
wbcd_test_pred <- knn(train = wbcd_train, test=wbcd_test, cl=wbcd_train_labels, k=25)
CrossTable(x=wbcd_test_labels, y=wbcd_test_pred, prop.chisq=FALSE)
wbcd_test_pred <- knn(train = wbcd_train, test=wbcd_test, cl=wbcd_train_labels, k=33)
CrossTable(x=wbcd_test_labels, y=wbcd_test_pred, prop.chisq=FALSE)
#---------------데이터 분류 개선---------------#

























































