setwd("/Users/hodong/Desktop/ml_prac/raw_data/6. Regression")
wine <- read.csv("whitewines.csv")
#View(wine) #str(wine)
#hist(wine$quality) #summary(wine)

#훈련/테스트용 데이터 나눔 (75:25)
train_sample <- sample(4898,3750)
wine_train <- wine[train_sample,]
wine_test <- wine[-train_sample,]

install.packages("rpart")
library(rpart)

#regression tree를 사용하기 위해rpart를 이용한 모델 생성
m.rpart <- rpart(quality~.,data=wine_train)
#Decission Tree처럼 가장 중요한 속성부터 먼저 나눈다.
m.rpart
#결과 표시 : 
#노드의 속성 및 조건 / 노드에 분류된 인스턴스의 개수 / ~~ / ~~ / 해당 조건에 속해있을 경우의 종속변수
# * 는 terminal 혹은 leaf node에 붙는다.
summary(m.rpart) #평균/표준오차 등도 함께 확인하려면

#rpart 모델을 시각화하기
install.packages("rpart.plot")
library(rpart.plot)

rpart.plot(m.rpart,digits=3)
rpart.plot(m.rpart, digits=4, fallen.leaves=TRUE, type=3, extra=101)
#digits는 소수점 몇자리까지 보여줄 것인지, type, extra는 보여지는 옵션 조정하는 parameter이고,
#fallen.leaves=TRUE는 leaf 노드를 분류해서 아래로 정렬시키는 것

p.rpart <- predict(m.rpart, wine_test)

#테스트를 진행했는데, 실제 데이터의 범위보다 더 좁은 범위로 계산된다.
summary(p.rpart)
summary(wine_test$quality)
#평균으로 측정하다보니 어쩔 수 없다. 실제로 1Q ~ 3Q값은 꽤 괜찮게 나왔다. 

cor(p.rpart, wine_test$quality)
# cor() 함수를 이용해 성능을 비교한다. 그러나, 이것도 증감 경향성이 일치하는지만 보여주지, 실제값들과 예측값이 어느 정도 차이나는지를 보여주진 못한다.

#실제값, 예측값 사이의 error의 평균을 구해서 차이 정도를 알아본다.
MAE <- function(actual, predicted){  mean(abs(actual-predicted))  }
MAE(wine_test$quality, p.rpart)
MAE(mean(wine_train$quality),wine_test$quality)
#모델을 사용하지 않고 단순히 평균으로 몰빵한 후, MAE(mean absolute error)를 비교해보면 어느 정도 성능을 감잡을 수 있다.

library(RWeka)
#model tree 실습
#regression tree와 다르게, terminal 혹은 leaf node가 숫자 예측으로 끝나지 않고, 
#linear model 식으로 반환된다는 것이다.
m.m5p <- M5P(quality~.,data=wine_train)
m.m5p
summary(m.m5p)

#test
p.m5p <- predict(m.m5p,wine_test)
summary(p.m5p)
cor(p.m5p,wine_test$quality)
MAE(p.m5p,wine_test$quality)

