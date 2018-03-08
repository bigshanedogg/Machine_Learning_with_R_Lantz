setwd("/Users/hodong/Desktop/ml_prac/raw_data/9. k_Means")
teens <- read.csv("snsdata.csv", header=TRUE)
#str(teens) #View(teens) 
View(teens)
table(teens$gender) #성별에 NA가 있다…. missing value를 의미. 즉, 성별이 무엇인지 모르는 상태…!
table(teens$gender, useNA="ifany") 
summary(teens$age) #나이도 missing value가...
teens$age <- ifelse(teens$age >= 13 & teens$age < 20, teens$age, NA) 
summary(teens$age) #나이의 최소/최대값이 비현실적이다. ifelse()를 이용하여 비현실적인 값은 NA로 바꿔준다.

#결측치 처리 방법 - 2) Dummy Coding
teens$female <- ifelse(teens$gender=="F" & !is.na(teens$gender),1,0)
teens$no_gender <- ifelse(!is.na(teens$gender),1,0)
#table(teens$gender) #table(teens$female) #table(teens$no_gender)

#결측치 처리 방법 - 3) Imputation
mean(teens$age) #NA값을 포함한 연산의 결과는 NA로 산출된다. 
mean(teens$age,na.rm=TRUE)#na.rm 파라미터를 통해 NA값을 제외하고 연산한다.
aggregate(data=teens,age~gradyear, mean, na.rm=TRUE)
#teens에서 NA값은 제외하고, 속성의 값별(졸업연도별)로 group화하여 특정 속성에 함수(age의 평균)를 사용한다
#aggregate는 결과값으로 data frame형을 출력하기 때문에 처리 작업이 복잡하다. 
#다음과 같은 방법으로 한 번에 해결 가능하다. (필요할 때 자세히 알아보자.)
ave_avg <- ave(teens$age, teens$gradyear, FUN=function(x) mean(x,na.rm=TRUE)) 
#vector값 출력하는 ave 함수 이용
teens$age <- ifelse(is.na(teens$age),ave_avg,teens$age)
#summary(teens$age)

install.packages("stats")
library(stats)
#k-means에 사용하는 데이터의 속성은 모두 numeric data여야 한다.
interests <- teens[5:40]
interests_z <- as.data.frame(lapply(interests, scale)) #kNN처럼 속성을 표준화해주어야 한다.

set.seed(2345)
teen_clusters <- kmeans(interests_z,5)

#Model Evaluating
teen_clusters$size #size of each clustered group
#가장 작은 그룹과 큰 그룹의 size가 다르다는 것은 
#'실제와 일치하는 결과'일 수도 있고 - 10대의 대다수가 특정한 코드를 공유하는 것,
#'kmeans의 시작점을 임의로 설정하기 때문에 발생한 우연한 결과'일 수도 있다.
teen_clusters$center #각 클러스터 centroid의 좌표
#centroid의 각 속성별 좌표(z-score)를 통해, 각 클러스터의 특징에 대해 유추할 수 있다. 
#각 클러스터의 속성값 중 특성으로 여겨질만한 값을 찾는 것!
#(그러나 어디까지나 유추이기 때문에 정확하지 않다.)


#Model Improving
#앞에서 제외한 개인적 특성과 cluster 결과를 비교하며 모델 성능을 확인한다.
teens$cluster <- teen_clusters$cluster
teens[1:5, c("cluster","gender","age","friends")] 

#각 결과값을 확인하며, 유의미한 차이를 가져오는지 점검한다.
aggregate(data=teens, age~cluster, mean)
aggregate(data=teens, female~cluster, mean)
aggregate(data=teens, friends~cluster, mean)
aggregate(data=teens, gradyear~cluster, mean)














































