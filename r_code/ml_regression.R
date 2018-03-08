setwd("/Users/hodong/Desktop/ml_prac/raw_data/6. Regression")
insurance <- read.csv("insurance.csv", stringsAsFactors=TRUE)

#----------------------#회귀분석 전 데이터에 대한 이해#----------------------#
str(insurance)
View(insurance)
summary(insurance$charges)
hist(insurance$charges) #정규분포일 때, 회귀분석 효과가 더 정확하다.
#그러므로 사전에 파악하고 분석하는 것이 이후의 정확도 향상에 더 도움이 된다.
#table(insurance$region)
#pairwise 관계를 확인하여 각 변수 간의 상관관계를 확인한다.
cor(insurance[c("age","bmi","children","charges")])
#독립변수 1개, 종속변수 1개의 관계를 보여주는 scatterplot과 달리, 
#여러 개의 독립변수와 종속변수 1개의 관계를 시각적으로 보여주기 위해 scatterplot matrix를 사용한다.
pairs(insurance[c("age","bmi","children","charges")])
#install.packages("psych")
library(psych)
pairs.panels(insurance[c("age","bmi","children","charges")])
#빨간선으로 표시되는 loess curve를 이용해 비선형 관계도 대략적으로 파악할 수 있다.
#----------------------#회귀분석 전 데이터에 대한 이해#----------------------#


#----------------------#회귀분석 시작#----------------------#
ins_model <- lm(charges ~age+children+bmi+sex+smoker+region, data=insurance)
#ins_model <- lm(charges ~., data=insurance) #위와 동일
ins_model #자동으로 Dummy Coding해주는 고마운 R
#R은 factor 변수에서 첫번째 level을 reference group으로 삼는다.
#여기서 모든 독립변수값이 0일 때의 종속변수값을 의미하는 intercept가 음수가 나왔는데, 
#현실적으로 나이=0, 성별=0, bmi=0인 사람이 존재할 수 없다. 그런 이유로 intercept는 종종 무시하기도 한다.

summary(ins_model) 
#p값은 회귀분석에서 얻은 추정값이 주어졌을 때, true coefficient가 0일 확률을 뜻한다. (가설 검정)
#p값이 작다는 것은, true coefficient가 0이 아닐 확률, 즉 회귀분석의 추정된 회귀계수가 유의미할 확률을 의미한다. (가설 기각)
#R-squared값은 종속변수의 값을 얼마나 잘 설명하고 있는지에 대한 측정치. 1에 가까울수록 설명을 잘하고 있는 것으로 볼 수 있다.
#eg. R-squared = 0.75 : 모델이 75%의 종속변수를 반영한다.
#독립변수가 많을수록, 모델은 더 높은 설명력을 가질 수 밖에 없다. 
#adjusted R-squared는 독립변수의 개수에 따라 R-squared값을 조정한 값이다.
#독립변수의 개수가 서로 다른 모델들을 비교할 때 유용하다.

#non-linear 관계를 표현하기 위해 변수의 제곱을 넣어 비교해본다.
insurance$age2 <- insurance$age^2
age2_lm <- lm(charges~age+age2,data=insurance)
summary(age2_lm)

#현실에서는 특정 변수는 일정 기준점을 넘을 때까지 효과가 없다가 일정 기준점을 지나야 관계를 보이는 변수도 있다.
#예를 들어, BMI는 정상 범위에 있을 때까지는 보험비에 큰 관계가 없다가, 과체중 범위로 넘어갈 때부터 관계가 생기기 시작한다.
#이럴 경우, 기준점을 이용해 binary coding을 하여 반영할 수 있다.
insurance$bmi30 <- ifelse(insurance$bmi >=30, 1, 0)

#두 변수의 상호작용을 고려하기 위한 모델 설정
inter_lm <- lm(charges~bmi30*smoker,data=insurance)
#inter_lm <- lm(charges~bmi30+smoker+bmi30:smoker,data=insurance) #위와 동일
summary(inter_lm)

#개선 사항 반영 후 전체 모델 분석
ins_model2 <- lm(charges ~ age+age2+children+bmi+sex+bmi30*smoker+region, data=insurance)
summary(ins_model2)
#----------------------#회귀분석 시작#----------------------#

































































