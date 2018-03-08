install.packages("neuralnet")
library(neuralnet)

setwd("/Users/hodong/Desktop/ml_prac/raw_data/7. Neural_Networks")
concrete <- read.csv("concrete.csv",header=TRUE)
str(concrete)

#인공신경망은 해당 값이 0에 가까운 좁은 범위일수록 더 효과적으로 작동하므로, 변형해야 한다.
#데이터가 정규분포를 따르면 정규화를, 그렇지 않은 경우는 표준화를 사용한다.
normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
concrete_norm <- as.data.frame(lapply(concrete,normalize))

#훈련용 데이터와 테스트 데이터로 나누기
concrete_train <- concrete_norm[1:773,]
concrete_test <- concrete_norm[774:1030,]

#hidden 노드가 1개인 single-layer feedforwared neural network 훈련용 모델 생성
concrete_model <- neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,data=concrete_train)
#인공신경망 모델 시각화
#1번 동그라미는 bias term으로, 선형 부등식에서 intercept처럼 값을 전체적으로 특정 정도만큼 
#내리거나 올리도록 하는 값이다.
#Error는 SSE(예측값-실제값의 제곱합)를 의미하고, Steps는 훈련 과정의 수를 의미한다. 
plot(concrete_model)

model_result <- compute(concrete_model, concrete_test[1:8])
predicted_strength <- model_result$net.result
#분류 문제라면 confusion matrix를 사용하겠지만, 숫자형 자료이기 때문에 상관계수로 확인한다.
cor(predicted_strength, concrete_test$strength)

#모델 개선하기 
#hidden 노드가 5개인 single-layer feedforwared neural network 훈련용 모델 생성
concrete_model2 <- neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,data=concrete_train, hidden=5)
plot(concrete_model2)
model_result2 <- compute(concrete_model2, concrete_test[1:8])
predicted_strength2 <- model_result2$net.result
cor(predicted_strength2, concrete_test$strength)











