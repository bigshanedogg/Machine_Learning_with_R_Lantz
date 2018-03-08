setwd("/Users/hodong/Desktop/ml_prac/raw_data/7. Neural_Networks")
letters <- read.csv("letterdata.csv",header=TRUE)
str(letters)

#다양한 SVM 패키지
#e1071 : C++ 기반으로 사용된 패키지로, LIBSVM 라이브러리와 익숙할 경우 추천
#klaR : R 내에서 직접 SVM을 구현한 패키지로, SVMlight 알고리즘에 대한 지식이 있을 경우 추천
#kernlab : R로 개발되어 변형하기 쉽고, caret 패키지와 함께 사용되면서 다양한 방법으로 훈련/평가 가능
library(ggplot2)
install.packages("kernlab")
library(kernlab)
#데이터 전처리를 해야하는데, 해당 데이터는 이미 모든 속성이 integer이고,
#사용할 kernlab 패키지가 rescaling을 자동으로 수행하기 때문에 바로 본론으로 넘어간다. 
#kernlab은 디폴트는 Gaussian RBF kernel을 사용한다.
letters_train <- letters[1:16000,]
letters_test <- letters[16001:20000,]

#kernel 인자인 vanilladot는 simple linear SVM classifier를 의미한다. 
letter_classifier <- ksvm(letter~., data=letters_train, kernel="vanilladot")
letter_classifier

#predict 함수의 type 인자는 "response"와 "probabilities"를 가질 수 있으며, 디폴트는 response이다.
#response는 예상되는 class를, probabilities는 각 class별로 분류될 확률을 반환한다.
letter_predictions <- predict(letter_classifier, letters_test, type="response")
head(letter_predictions)

#실제 예측 결과를 표 형태로 보여줌
table(letter_predictions, letters_test$letter)

#에러의 타입(false-positive, false-negative)에 상관 없이 예측된 값과 실제 테스트 값이 같은지만 확인 
agreement <- letter_predictions==letters_test$letter
table(agreement)
prop.table(table(agreement))

#모델 개선
#RBF 커널 이용
letter_classifier_rbf <- ksvm(letter~.,data=letters_train,kernel="rbfdot")
letter_predictions_rbf <- predict(letter_classifier_rbf,letters_test)
agreement_rbf <- letter_predictions_rbf==letters_test$letter
table(agreement_rbf)
prop.table(table(agreement_rbf))













