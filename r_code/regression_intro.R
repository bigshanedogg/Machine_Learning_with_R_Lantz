setwd("/Users/hodong/Desktop") #" " 안에 작업할 디렉토리 입력
Reyeme <- read.csv("Reyeme.csv", stringsAsFactors=TRUE, header=TRUE) #" " 안에 불러들일 csv 파일 입력
#xls 파일 열어서 csv로 저장하기를 누르면 csv 파일 생성할 수 있음
#참고 : header(각 열의 이름)는 영어로 바꾸는 게 편하다

#변수 형태 확인
str(Reyeme)
View(Reyeme)
summary(insurance$charges) #insurance$charges는 insurance 데이터의 charges 열을 선택하는 명령어
hist(insurance$charges) 
#정규분포일 때, 회귀분석 효과가 더 정확하다. 타겟(종속) 변수가 정규 분포인지 얼추 확인해본다.

#각 변수들 간의 상관 관계 확인
cor(insurance[c("age","bmi","children","charges")])

#선형회귀 모델 생성
ins_model <- lm(charges ~age+children+bmi+sex+smoker+region, data=insurance)
#insurance 데이터에서 추출, 종속 변수 : charges, 독립 변수 : age, children, bmi, sex, smoker, region

#vif() 함수 구글링해서 다중공선성 확인하기!
#데이터에서 변수 제거하는 방법 : insurance의 3번째 항목 제거 = insurance <- insurance[,-3]

#변수 치환하기
insurance$age2 <- insurance$age^2 #age 열에 기존 값을 제곱하여 치환한다.
#응용으로 log도 사용가능
#종종 log를 사용하면 변화 정도를 완화(단위 변환)시켜줘서 유의미한 관계를 발견하게 되기도 한다.
insurance$bmi30 <- ifelse(insurance$bmi >=30, 1, 0) #bmi가 30보다 크면 1, 작으면 0 치환
#총 200개의 행 중에 1~29의 값이 100개, 30~1000가 100개이면, 그대로 1~1000까지의 값을 사용하면
#아웃라이어에 의해 결과가 왜곡될 수 있다. 그런 경우를 위한 치환.
#또는, 예를 들어 특정 숫자이면 그 논리적 의미가 무의미해지는 경우
#eg. 자식의 수가 0명과 1명은 차이가 크지만, 7명과 8명은 그다지 차이가 없다.

step(ins_model)
#단계적 탐색으로 변수들을 다양하게 조합해 AIC가 제일 높은 모델을 가장 아래에 보여준다.
#(절대적인 건 아니지만) 보통 AIC가 제일 높은 모델이 가장 유의한 모델이라고 판단.
#각 변수들의 *** 수(유의미한 정도), 생성된 모델의 수정계수, F값, P값을 보고 잘 판단해서 괜찮은 모델인지 확인

#추가 : 보통 처음 데이터셋을 8:2 정도 비율로 훈련/테스트 데이터로 나눈다.
#80%로 모델 생성하고, 생성된 모델에 20%의 테스트 데이터를 넣어서 어느 정도 예측하는지 확인하는 것.
#predict(ins_model,newdata=ROSA) 로 모델에 테스트데이터를 입력한 값을 확인할 수 있고, 본래 테스트 데이터의 종속 변수 값과 비교해보면 된다.
#cor() 등을 이용해서...!




