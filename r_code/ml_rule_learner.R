setwd("/Users/hodong/Desktop/ml_prac/raw_data/5. Decision_Tree")

#현재 데이터에 있는 버섯이 존재하는 모든 버섯에 대한 정보라고 가정한다.
#그렇지 않으면…테스트를 위해 추가 정보 수집하는 것을 막기 위해....
mushrooms <- read.csv("mushrooms.csv", stringsAsFactors=TRUE, header=TRUE)
#str(mushrooms)
mushrooms$veil_type <- NULL #veli_type의 factor는 1개이므로, 불필요하다. 지운다.
table(mushrooms$type)
#데이터셋의 범주가 앞의 1글자만 사용한다….주의

#설치하는데만 엄청 고생한 패키지…. 구글링해도 안나오고….
#RWeka를 깔기 위해선 rJava가 필요한데, 
#Java가 제대로 설치 안되는 것을 보아 아마 맥 자체에 Java가 업데이트되지 않은 것이라고 판단
#홈브류에 #brew update #brew cask install java 
#커맨드를 입력하여 Java를 설치하고 차례대로 설치했더니 잘 진행된다!
install.packages("rJava") 
install.packages("RWeka") 
library(rJava)
library(RWeka)

#OneR 사용
mushroom_1R <- OneR(type~., data=mushrooms)
mushroom_1R 
#결과를 살펴보면, 냄새(odor)가 가장 주요한(정확도가 높은) 규칙으로 선정되었으며, 
#괜찮은(?) 향일 수록 edible, 구린 향일수록 poisonous하게 판단되었다.
summary(mushroom_1R)

#Model Improving with JRip, because false positive can bring out dangerous result.
#JRip = RIPPER algorithm based on Java
mushroom_RIPPER <- JRip(type~., data=mushrooms)
mushroom_RIPPER #프로그래밍 언어의 if-else문과 유사한 형태의 규칙이 생성된다. (짱좋!)
#규칙 => 분류되는 타입 (분류된 instance의 수 / 잘못 분류한 수)





























































