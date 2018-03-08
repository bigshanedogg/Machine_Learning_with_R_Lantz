setwd("/Users/hodong/Desktop/ml_prac/raw_data/5. Decision_Tree")

#결정트리 연습
credit <- read.csv("credit.csv",header=TRUE)
#str(credit) #table(credit$checking_balance) #table(credit$savings_balance)
#summary(credit$months_loan_duration) #summary(credit$amount) #table(credit$default)
set.seed(123) #동일한 테스트 결과를 확인하기 위해 random sequence에 대한 고유 번호 지정
#train & test data set
train_sample <- sample(1000,900)
credit_train <- credit[train_sample,]
credit_test <- credit[-train_sample,]
#prop.table(table(credit_train$default)) #prop.table(table(credit_test$default))

#C5.0은 데이터를 처리하고 결정트리를 형성하는 여러가지 함수를 가지고 있으므로, 선택지가 많다.
install.packages("C50")
library(C50)

#결정트리 모델 생성
credit_train$default <- as.factor(credit_train$default) #class는 factor 변수여야 하고,
#levels(credit_train$default) <- list(yes=1, no=2) #1,2보다는 보기 편하게 이름을 바꿔준다.
credit_model <- C5.0(credit_train[-17], credit_train$default)
#credit_model #형성된 dt 확인
summary(credit_model) #결과 확인
credit_pred <- predict(credit_model, credit_test)

install.packages("gmodels")
library(gmodels)
CrossTable(credit_test$default, credit_pred, prop.chisq=FALSE, prop.c=FALSE, prop.r=FALSE, dnn=c('actual default','predicted default'))
#정확도가 형편없다....

#모델 성능 개선하기
credit_boost10 <- C5.0(credit_train[-17], credit_train$default, trials=10)
#credit_boost10 #summary(credit_boost10)
#(이경우는 데이터가 적지만) DT를 생성/분류하는 것 자체가 비용이 크므로, 처음부터 부스팅을 하는 건 좋지 않다.
#또, 데이터에 noisy data가 많을 경우 역시 좋지 않은 선택이다.
#하지만 뭐, 어차피 하는 거 한번쯤 해보고 결과 비교하는 것도 나쁘지 않음!
credit_pred_boost10 <- predict(credit_boost10, credit_test)
CrossTable(credit_test$default, credit_pred_boost10, prop.chisq=FALSE, prop.c=FALSE, prop.r=FALSE, dnn=c('actual default','predicted default'))

#오류에 따른 비용 가중치 설정하기
matrix_d <- list(c("no","yes"),c("no","yes"))
names(matrix_d) <- c("predicted","actual")
error_cost <- matrix(c(0,1,4,0),nrow=2,dimnames=matrix_d)
#error_cost
credit_cost <- C5.0(credit_train[,-17], credit_train$default, costs=error_cost)
credit_pred_cost <- predict(credit_cost, credit_test)
CrossTable(credit_test$default, credit_pred_cost, prop.chisq=FALSE, prop.c=FALSE, prop.r=FALSE, dnn=c('actual default','predicted default'))

















