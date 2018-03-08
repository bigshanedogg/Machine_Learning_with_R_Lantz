setwd("/Users/hodong/Desktop/ml_prac/raw_data/8. Association_Rule")
grocery <- read.csv("groceries.csv", header=TRUE)
View(grocery) 
str(grocery) #for finding insight easily, considering transactions by product category, not brand name
#data frame structure may not be proper, because It cause the memory waste 
#in aligning all feature space according to amount of the longest instance.
#and whole milk in 1st column of first instance and whole milk in 3rd column of 20th instance may
#be interpreted to different meaning.
#Sparse Matrix will be suitable in this reasons.

install.packages("arules")
library(arules)
groceries <- read.transactions("groceries.csv", sep=",")
summary(groceries)
#density means acual purchased items of all cells in matrix
#(9835*169)*0.02609146=1662115*0.02609146=43367 items had been actually purchased
#43367/9835 means the average purchased items per transaction
#size means the number of purchased items per each transaction, and fivenums with those sizes.

inspect(groceries[1:5]) #showing items of each instances in set data structure of python
itemFrequency(groceries[,1:5]) #columns of sparse matrix shows alphabetically-ordered items

#Visualizing the data
#Visualizing helps revealing hidden data insights such as seasonal character
itemFrequencyPlot(groceries,support=0.1) #showing items & support over 0.1
itemFrequencyPlot(groceries,topN=20) #showing top N items in support

image(groceries[1:5]) #showing each transaction's item in string shape
image(sample(groceries,100)) #100 randomly sampled rows shown by image function

#Analysis
#minlen means the minimum required rule items
#minlen = 1 inlcudes '{} -> whole milk', which just represent commonly purchased item case. 
#It is fact, but not actionable.
apriori(groceries) #default minimum support is 0.1, in this case it means only 8 items can be involved

#Setting support
#setting levels too high can shows no rules or too generic rules,
#while setting levels too low can shows unwieldy rules or running out of memory
#1달 간의 거래 내역이라고 가정할 때, 하루에 2번 구매 = 총 60번 거래
#총 9835건 중 60번 거래 = 0.006, minimum support > 0.006으로 설정하면 된다..

#Setting confidence
#too low confidence make unreliable rules which containing not specific relationship, 
#just common items (which is not associated with others) can be included
#too high confidence make too natural rules which can be found by just intuition.

groceryrules <- apriori(groceries, parameter=list(support=0.006,confidence=0.25,minlen=2))
groceryrules
summary(groceryrules)
#if support & confidence is located near minimun value, It is because bar had been setted too high.
#lift(X->Y) = confidence(X->Y)/support(Y) = confidence(Y->X)/support(X) = lift(Y->X)
#lift > 1 : lhs와 rhs의 물품이 함께 구매될 확률이, 하나의 물품만 구매될 확률보다 높다.
#lift < 1 : lhs와 rhs의 물품이 함께 구매될 확률이, 하나의 물품만 구매될 확률보다 낮다.
inspect(groceryrules[1:3])
#첫번째 룰 : {pot plants} => {whole milk}, support 0.007 / confidence 0.400 / lift 1.565
#0.7%의 거래에서 룰을 발견할 수 있으며, pot plant가 포함된 거래의 40%에서 일치한다.
#whole milk만 거래하는 경우보다, pot plant를 구매한 상태에서 구매할 확률이 56.5% 높다.
#=> 일반적인 whole milk 구매자보다, pot plant까지 구매하는 거래자가 whole milk를 1.56배 더 많이 구매한다.

#그러나 데이터마이닝의 AR 결과로만 판단하기엔 어려운 상황이 있다.
#분석 결과와 논리적 직관이 일치하는지 확인한다. 
#기준 : Actionable-실제 마케팅에 활용가능한지 / Trivial-너무 당연하지 않은지 /
#Inexplicable-데이터로는 나오는데, 논리적 연결은 부족한 것
#eg. 짱구DVD와 초코비과자 사이의 AR, 유의미한 듯하지만 사실은 Trivial한 무용한 룰이다.

#진짜 유의미한 룰은 숨겨진 보석이다. (이래서 Knowledge Discovery라고 하나보다.)

#Sorting rules
inspect(sort(groceryrules, by="lift")[1:5]) #"lift", "support", "confidence" 사용 가능
#parameterdecreasing = TRUE : 내림차순(default), parameterdecreasing = FALSE : 오름차순

#특정 아이템이 들어간 룰의 부분집합 구하기
berryrules <- subset(groceryrules, items %in% "berries") #berries가 포함된
berryrules <- subset(groceryrules, items %in% "berries", confidence > 0.50) 
#support, confidence, lift 등을 인자로 사용해 제한할 수 있다.
berryrules <- subset(groceryrules, items %in% c("berries","yogurt")) #berries 또는 yogurt이 포함된
berryrules <- subset(groceryrules, items %ain% c("berries","yogurt")) #berries와 yogurt가 모두 포함된
berryrules <- subset(groceryrules, items %pin% "fruits") 
#citrus fruits든 tropical fruits든 fruits가 포함된 단어면 모두
#이외에 &, |, ! 등의 논리 연산자 사용 가능
inspect(berryrules)
#items 인자는 lhs와 rhs를 모두 포함한다. lhs, rhs만 사용가능

#saving rules file
write(groceryrules, file="groceryrules.csv",sep=",",quote=TRUE,row.names=FALSE)
groceryrules_df <- as(groceryrules,"data.frame")
str(groceryrules_df)
View(groceryrules_df)























