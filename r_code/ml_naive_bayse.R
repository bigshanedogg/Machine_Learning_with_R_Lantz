install.packages("tm",dependencies=TRUE)
library(tm)

#---------------데이터 전처리 과정---------------#
#파일 입력
setwd("/Users/hodong/Desktop/ml_prac/raw_data/4. Naive_Baysean")
sms_raw <- read.csv("sms_spam.csv",stringsAsFactors=FALSE, header=FALSE)
#View(sms_raw) #str(sms_raw)

#파일 기본 자료형 손질
names(sms_raw) <- c('type','text')
sms_raw$type <- factor(sms_raw$type)
sms_raw$text<-iconv(enc2utf8(sms_raw$text),sub="byte")
#table(sms_raw$type)
#levels(factor(sms_raw$type))
#sms_raw <- sms_raw[c(-63,-101,-827,-1710,-2970,-3118,-3264,-3356,-3493,-4367),]

#불용 단어 제거, 단어 간 order 제거, 단어의 출현 여부만을 검증하기 위해 변수화 
#Corpus(언어 사전집) 생성 (VectorSource로 text의 벡터값을 모아, VCorpus 함수에 전달)
#언어 사전집 = 각 행별 문장으로 만들어낸 사전들의 모음
#PCorpus는 실제 메모리에 저장되어 있는 파일(웹상/디스크상)을 불러와 만드는 것.
#VCorpus는 가상의 공간의 데이터를 불러와 만드는 것. (이번엔 이미 R에 불러왔으니 이걸 사용)
sms_corpus <- VCorpus(VectorSource(sms_raw$text))
#print(sms_corpus)
#corpus는 복합적으로 구성된(여러 자료형이 한 번에 저장된) list 자료형이기 때문에 inspect로 검사한다.
#inspect(sms_corpus[1:2])
#본문을 보려면, character화 시켜야 함. [[1]]의 의미 : 첫번째 다큐먼트의 첫번째 요소
#as.character(sms_corpus[[1]]) #lapply(sms_corpus[1:2],as.character)

#'hello! 3to'와 'HELLO'와 'hello'를 동일하게 만들기 위해, Transformation 진행
sms_corpus_clean <- tm_map(sms_corpus,content_transformer(tolower)) #소문자화
sms_corpus_clean <- tm_map(sms_corpus_clean,removeNumbers) #숫자 지우기
#stopwords() #불용어 list 확인하기 #사용자 임의 지정 가능
sms_corpus_clean <- tm_map(sms_corpus_clean,removeWords,stopwords()) #불용어(stop word) 지우기
sms_corpus_clean <- tm_map(sms_corpus_clean,removePunctuation) #구두점 지우기

#어간 추출
install.packages("SnowballC") 
library(SnowballC) #wordstem()도 있는데, 여기선 stemDocument 사용 (SnoballC도 설치되어 있어야 작동 가능)
sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument) #어간 추출 #에러일 경우 mc.cores=1 인자 추가
sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace) #whitespace 제거 
#as.character(sms_corpus_clean[1:3]) #대망의 확인... 

#tokenization - 텍스트 문서집을 개별 단어들의 집합으로
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)
sms_dtm2 <- DocumentTermMatrix(sms_corpus, control = list(tolower=TRUE,removeNumbers=TRUE,stopwords=TRUE,removePunctuation=TRUE,stemming=TRUE))
#그래도 DTM 함수의 control 인자를 통해 한 번에 해결할 수 있음.여러 개의 작업을 처리할 때는 이게 더 좋겠다!
#결과값에서 일부 차이를 보이는 것은 후자는 단어를 쪼갠 뒤에 처리하기 때문에 사용하는 불용어 리스트가 조금 다르기 때문.
#DTM의 stopwords 인자를 기존의 stopwords() 함수로 교체하면 같은 값을 얻을 수 있다.
#"처리 순서에 따라 유의미한 결과값 차이가 발생한다!" #케이스마다 잘 고려해야 한다.
#---------------데이터 전처리 과정---------------#

#---------------단어를 통한 확률 추출 및 나이브 베이즈 분류---------------#
sms_dtm_train <- sms_dtm[1:4169,] #75%
sms_dtm_test <- sms_dtm[4169:5559,] #25%
sms_train_labels <- sms_raw[1:4169,]$type
sms_test_labels <- sms_raw[4169:5559,]$type
#prop.table(table(sms_train_labels)) #prop.table(table(sms_test_labels))

#worldcloud를 통해, 우리가 설정한 스팸과 햄의 규격이 맞는지 확인한다.
#혹은, 이 wordcloud에서 인사이트를 얻어 나이브 베이즈 필터를 설계한다.
#스팸으로 분류된 SMS의 상위 빈출 단어와 나이브 베이즈에서 사전확률로 설정한 속성(filter)을 비교한다.
install.packages("RColorBrewer")
install.packages("wordcloud")
library(RColorBrewer)
library(wordcloud)
wordcloud(sms_corpus_clean, min.freq=50, random.order=FALSE) #50은 전체 관측치(SMS)의 약 1%에 해당하는 수치
spam <- subset(sms_raw,type=="spam")
ham <- subset(sms_raw,type=="ham")
wordcloud(spam$text, max.words=40, scale=c(3,0.5)) #TOP40의 단어들을 보여줘
wordcloud(ham$text, max.words=40, scale=c(3,0.5)) #최대 폰트 사이즈 3, 최소 0.5으로 보여줘

#전체 6000개 이상의 단어 중, 불필요한 속성(차원)을 줄이기 위해 최소 5번 이상 관측되는 단어만 추출한다.
sms_freq_words <- findFreqTerms(sms_dtm_train,5)
#str(sms_freq_words)
sms_dtm_freq_train <- sms_dtm_train[,sms_freq_words] #약 6500개의 속성(차원)에서
sms_dtm_freq_test <- sms_dtm_test[,sms_freq_words] #약 1137개의 속성(차원)으로 줄었다!
#관측 횟수(연속형 자료)를 출현 여부(범주형 자료)로 변환하는 함수 : 이진 나이브 베이즈 분류이기 때문
conver_counts <- function(x) { x<- ifelse(x>0,"Yes","No") }
#MARGIN=1 : 행별 작업, MARGIN=2 : 열별 작업
sms_train <- apply(sms_dtm_freq_train, MARGIN=2, conver_counts)
sms_test <- apply(sms_dtm_freq_test, MARGIN=2, conver_counts)


#나이브 베이즈 분류기 패키지 사용하기 
install.packages("e1071") 
library(e1071)
#install.packages("klaR") install.packages("MASS") library(MASS) library(klaR) #거의 동일한 기능 제공
#naiveBayes()는 각 속성별 도수를 가지고 있는 행들의 집합, 각 행의 실제 타입의 집합을 인자로 받는다.
#sms_train은 처음에 sms_raw$text를 가공해서 만들었고, sms_train_labels는 sms_raw$type을 가공해서 만들었다.
#두 데이터 벡터들의 행 맞춤은 정확도와 직결된다. 
#nrow(sms_train) #length(sms_train_labels)
sms_classifier <- naiveBayes(sms_train, sms_train_labels, laplace=0)
#predict() 함수는 적용할 모델과 데이터를 각각 인자로 받는다.
sms_test_pred <- predict(sms_classifier, sms_test)
#head(sms_test_pred) #head(sms_test_labels)

#교차 테이블을 이용해 결과 확인하기 (성과 측정)
install.packages("gmodels")
library(gmodels)
CrossTable(sms_test_pred, sms_test_labels, prop.chisq=FALSE, prop.t=FALSE,dnn=c("predicted","actual"))
#---------------단어를 통한 확률 추출 및 나이브 베이즈 분류---------------#

#---------------모델 개선하기---------------#
#라플라스 추정기 값 조정하기
#라플라스 값을 통해 공격적/수동적 필터링을 조정해야 한다.
#이 경우, false-positive의 리스크가 false-negative의 리스크보다 더 크기 때문이다.
#사람들은 spam이 ham으로 분류되는 false-negative보다 ham이 spam으로 분류되는 false-positive를 더 경계한다.
sms_classifier2 <- naiveBayes(sms_train, sms_train_labels, laplace=1)
sms_test_pred2 <- predict(sms_classifier2, sms_test)
CrossTable(sms_test_pred2, sms_test_labels, prop.chisq=FALSE, prop.t=FALSE,dnn=c("predicted2","actual2"))
#---------------모델 개선하기---------------#
sms_raw_train <- sms_raw[1:4169,]
sms_raw_test <- sms_raw[4170:5558,]
sms_dtm_train <- corpus_clean[1:4169]
sms_dtm_test <- corpus_clean[4170:5558]

prop.table(table(sms_raw_train$type))
prop.table(table(sms_raw_test$type))
wordcloud(sms_corpus_train,min.freq=40,random.order=F)
spam <- subset(sms_raw_train, type=="spam")
ham <- subset(sms_raw_train, type=="ham")

wordcloud(spam$text, max.words = 40, scale=c(3,0.5))
wordcloud(ham$text, max.words = 40, scale=c(5,0.5))
findFreqTerms(sms_dtm_train,5)

sms_raw_train <- sms_raw[1:4169,]
sms_raw_test <- sms_raw[4170:5558,]
sms_dtm_train <- corpus_clean[1:4169]
sms_dtm_test <- corpus_clean[4170:5558]

prop.table(table(sms_raw_train$type))
prop.table(table(sms_raw_test$type))
wordcloud(sms_corpus_train,min.freq=40,random.order=F)
spam <- subset(sms_raw_train, type=="spam")
ham <- subset(sms_raw_train, type=="ham")

wordcloud(spam$text, max.words = 40, scale=c(3,0.5))
wordcloud(ham$text, max.words = 40, scale=c(5,0.5))
findFreqTerms(sms_dtm_train,5)

sms_raw_train <- sms_raw[1:4169,]
sms_raw_test <- sms_raw[4170:5558,]
sms_dtm_train <- corpus_clean[1:4169]
sms_dtm_test <- corpus_clean[4170:5558]

prop.table(table(sms_raw_train$type))
prop.table(table(sms_raw_test$type))
wordcloud(sms_corpus_train,min.freq=40,random.order=F)
spam <- subset(sms_raw_train, type=="spam")
ham <- subset(sms_raw_train, type=="ham")

wordcloud(spam$text, max.words = 40, scale=c(3,0.5))
wordcloud(ham$text, max.words = 40, scale=c(5,0.5))
findFreqTerms(sms_dtm_train,5)

