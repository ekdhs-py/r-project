' 나이브 베이즈 분류
  : 지계 학습 분야에서 나이브 베이즈는 특성들 사이의 독립을 가정하는 베이즈 정리를 적용한 확률 분류기의 일종 
'

# 패키지 로드
install.packages('tm') #tm 패키지 : 텍스트 마이닝 패키지
library(tm)

# 데이터 로드
sms_raw <- read.csv('c:/R_Project_hdo/sms_spam.csv', stringsAsFactors = FALSE)
str(sms_raw)
str(sms_raw$type)

table(sms_raw$type) ##ham : 4812, spam : 747

# 텍스트 문서를 corpus로 변환(tm 패키지 기능)
## => 텍스트 벡터를 말뭉치로 바꿔서 자연어 처리를 할 수 있도록 준비하는 단계
sms_corpus <- VCorpus(VectorSource(sms_raw$text)) #corpus : 언어 모음을 만들어 놓은 것
sms_corpus ##문서개수 : 5559

## 첫 번째와 두 번째 문서만 추출
inspect(sms_corpus[1:2])
## 첫번째 문서를 문자형으로 변환해서 실제 텍스트 내용만 추출
as.character(sms_corpus[[1]]) 
## lapply : 벡터, 리스트, 표현식, df 등에 함수를 적용하고 그 결과를 리스트로 반환  
lapply(sms_corpus[1:2], as.character)

#텍스트 전처리(정제) - 자연어처리에서 대소문자 구분을 제거하는 작업
' 반응형함수
  - 데이터 정제를 할 때 자주 쓰이는 함수
  - 문서에 함수를 적용해서 변환된 결과를 반환, 텍스트를 변환해주는 함수 

  tm_map() : corpus내의 각 문서에 어떤 함수를 적용할 때 사용
  content_transformer(tolower) : 소문자 변환 함수'
sms_corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))
## 소문자 변환 확인
as.character(sms_corpus_clean[[1]]) 

sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers) #숫자제거
sms_corpus_clean <- tm_map(sms_corpus_clean, removeWords, stopwords()) #불용어 제거
stopwords() #불용어 : 의미없게 쓰이는 단어
sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation) #특수문자제거

# 단어 비교
install.packages('SnowballC')
'SnowballC 패키지 : 어휘 비교를 지원하기 위해서 단어를 공통 어근으로 축소하는 단어형태소 알고리즘 구현 패키지 '
library(SnowballC)
wordStem(c('learn', 'learned', 'learning', 'learns'))
' wordStem() : 벡터에서 주어진 각 단어의 공통 어근을 찾는 함수
** 단어가 기본 구성요소로 줄어들어야 단어 비교가 쉽다.
** 어근만으로 변환을 해야 나이브 베이즈 형태를 구축할 수 있다.
'

## 어간 추출
' stemDocument : Porter 스테밍 알고리즘을 사용하여 같은 의미의 단어들을 공통된 형태로 축약 '
sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)
## 공백 제거
sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace)

## DTM 문서 단어 행렬
'DocumentTermMatrix : 여러 문서들의 집단에서 단어를 추출한 뒤,
                      행에 출현했던 단어 리스트를 나열하고, 열에 각 문서들을 나열한 행렬
  - 텍스트 마이닝의 핵심구조
  - 각 문서를 행, 단어를 열로 배치하고 그 교차점에는 해당 단어가 문서에서 나타난 빈도를 기록
  => 텍스트를 숫자 형태로 변환하는 단계
'
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)
sms_dtm # 모델 학습, 분류, 분석 등에 바로 사용 가능한 수치형 데이터

# 방식 비교
sms_dtm2 <- DocumentTermMatrix(sms_corpus, control = list(
  tolower = TRUE, removeNumbers = TRUE, stopwords = TRUE, removePunctuation = TRUE, stemming = TRUE))
sms_dtm
sms_dtm2
' 기존 방식 : tm_map()을 여러번 써서 단계별 전처리 후 DocumentTermMatrix() 적용

  지금 방식 : DocumentTermMatrix() 안의 control 옵션으로 전처리 + DTM생성을 한번에 처리 
'

# 머신러닝 모델학습
## 데이터 분할
' 일반적인 분할 비율
  - 70:30
  - 75:25 
  - 80:20
  
  => 75%를 훈련용, 25%를 테스트용으로 설정
     5559 * 0.75 = 4179로 설정 
'
sms_dtm_train <- sms_dtm[1:4179,] #모델이 학습할 데이터
sms_dtm_test <- sms_dtm[4180:5559,] #모델의 예측성능을 검증할 데이터

## 라벨(정답) 분리
sms_train_labels <- sms_raw[1:4179,]$type #훈련용 문서에 대한 정답(스팸/일반 여부)
sms_test_labels <- sms_raw[4180:5559,]$type #테스트용 문서에 대한 정답

## 라벨 비율 구하기
'R에서 비율을 구하는 방법
  matrix 테이블을 한번에 proportion테이블로 변환 후,
  prop.table()을 이용해서 proportion/비율/퍼센트를 구함
'
prop.table(table(sms_train_labels)) ##ham : 0.8645609, spam : 0.1354391
prop.table(table(sms_test_labels)) ##ham : 0.8688406, spam : 0.1311594

# 워드 클라우드 생성
library(wordcloud)
wordcloud(sms_corpus_clean, 
          min.freq = 50, #min.freq : 50미만의 빈도를 가진 단어는 출력x 
          random.order = FALSE) #random.order : 단어 무작위 배치

# 스팸, 일반 추출하여 워드클라우드 생성
spam <- subset(sms_raw, type == 'spam') #스팸만
ham <- subset(sms_raw, type == 'ham') #일반만

wordcloud(spam$text, 
          max.words = 40, #max.words : 플롯할 최대 단어 수, 빈도가 낮은 용어는 삭제  
          scale = c(3, 0.5))
wordcloud(ham$text, max.words = 40, scale = c(3, 0.5))
'
  - 스팸 메시지에는 "call", "free", "now", "mobile", "text", "prize" 등 광고성 단어가 많음
  - 햄 메시지에서는 "just", "will", "know", "get", "come", "you" 등 일상적인 단어가 많음 
'

# 문서-단어 행렬에서 자주 사용되는 용어 추출 
sms_freq_words <- findFreqTerms(sms_dtm_train, 5) #하한값이 5
str(sms_freq_words)

# 자주 나오는 단어로만 문서-단어 행렬 필터링
sms_dtm_freq_train <- sms_dtm_train[ , sms_freq_words]
sms_dtm_freq_test <- sms_dtm_test[ , sms_freq_words]

# 함수 정의(어떤 단어가 메시지에 등장하면 'yes', 안하면 'no')
convert_counts <- function(x) {x <- ifelse(x > 0, 'yes', 'no')}
## 훈련용, 테스트용에 적용
sms_train <- apply(sms_dtm_freq_train, MARGIN = 2, convert_counts)
sms_test <- apply(sms_dtm_freq_test, MARGIN = 2, convert_counts)

# 패키지 설치
install.packages('e1071') #svm구현체로 잘 알려진 libsvm을 R에서 사용할 수 있게 하는 패키지
library(e1071)

# 나이브베이즈 모델학습 - 단어들의 출현 여부로 스팸인지 아닌지 예측
sms_classifier <- naiveBayes(sms_train, sms_train_labels)
' 오류 수정(인자 개수 다름)
nrow(sms_train) ##4179
length(sms_train_labels) ##4169 => 데이터 개수 4179로 설정'

## 테스트 데이터에 예측 모델 적용
sms_test_pred <- predict(sms_classifier, sms_test)
## 예측결과 출력
sms_test_pred

# 패키지 로드
install.packages('gmodels') # 교차표 생성
library(gmodels)
# 빈도기반의 교차표 생성
CrossTable(sms_test_pred, sms_test_labels, ## 모델이 예측한 결과, 실제 정답 비교
           prop.chisq = FALSE, #카이제곱 기여도 포함 여부
           prop.t = FALSE, #TRUE테이블의 비율 포함 여부
           dmm = c('predicted', 'actual')) #결과의 차원에 부여할 이름 설정 
' 해석
   Cell Contents
|-------------------------|
|                       N |
|           N / Row Total |
|           N / Col Total |
|-------------------------|

 
Total Observations in Table:  1380 

 
              | sms_test_labels 
sms_test_pred |       ham |      spam | Row Total | 
--------------|-----------|-----------|-----------|
          ham |      1193 |        29 |      1222 | 
              |     0.976 |     0.024 |     0.886 | 
              |     0.995 |     0.160 |           | 
--------------|-----------|-----------|-----------|
         spam |         6 |       152 |       158 | 
              |     0.038 |     0.962 |     0.114 | 
              |     0.005 |     0.840 |           | 
--------------|-----------|-----------|-----------|
 Column Total |      1199 |       181 |      1380 | 
              |     0.869 |     0.131 |           | 
--------------|-----------|-----------|-----------|

  총 테스트 데이터 수 : 1380개
  실제 행 : 1199개
  실제 스팸 : 181개
  
  - 모델 예측 결과 요약
    yes : 1193 (ham 예측, ham 실제) / 152 (spam 예측, spam 실제)
    no : 6 (spam 예측, ham 실제) / 29 (ham 예측, spam 실제)
    
  - 성능 지표 계산
    1. 정확도(Accuracy) - 전체 중 맞춘 비율
      (1193 + 152) / 1380 = 0.9746377
      => 97.5%
    2. 정밀도(Precision) - 스팸이라고 예측한 것 중 실제로 스팸인 비율
      152 / 158 = 0.9620253
      => 96.2%
    3. 재현율(Recall) - 실제 스팸 중에서 제대로 예측한 비율
      152 / 181 = 0.839779
      => 84.0%
    4. F1스코어 - 정밀도와 재현율의 조화 평균
      2 * (0.962 * 0.839) / (0.962 + 0.839) = 0.8962998
      => 89.6
      
    => 정확도와 정밀도가 아주 높은 걸로 보아 스팸 예측 시 거의 정확하다는 것을 알 수 있음.
       재현율도 높지만 29개의 스팸은 놓쳤으며,
       일상적인 메시지를 스팸으로 오예측한 건 단 6건으로 매우 적음
'

## 라플라스 보정
sms_classifier2 <- naiveBayes(sms_train, sms_train_labels, laplace = 1)
sms_test_pred2 <- predict(sms_classifier2, sms_test)
sms_test_pred2
CrossTable(sms_test_pred2, sms_test_labels,
           prop.chisq = FALSE,
           prop.t = FALSE,
           prop.r = FALSE,
           dmm = c('predicted', 'actual'))
' laplace smoothing
  : 기존의 나비스베이즈는 어떤 단어가 학습 데이터에서 한번도 안나왔을 때 그 단어가 나오면 확률 = 0으로 계산하는데,
    이럴 경우 0 * 나머지 확률 = 0으로 전체 문서 예측에 오류가 날 수 있다.
    즉 어떤 단어가 학습 데이터에 없었을 때 전체 확률이 0이 되는 것을 방지한다.
    
  <결과>
 Cell Contents
|-------------------------|
|                       N |
|           N / Col Total |
|-------------------------|

 
Total Observations in Table:  1380 

 
               | sms_test_labels 
sms_test_pred2 |       ham |      spam | Row Total | 
---------------|-----------|-----------|-----------|
           ham |      1181 |        16 |      1197 | 
               |     0.985 |     0.088 |           | 
---------------|-----------|-----------|-----------|
          spam |        18 |       165 |       183 | 
               |     0.015 |     0.912 |           | 
---------------|-----------|-----------|-----------|
  Column Total |      1199 |       181 |      1380 | 
               |     0.869 |     0.131 |           | 
---------------|-----------|-----------|-----------|
  
  - yes : 1181(ham - ham) / 165(spam - spam)
  - no : 18(spam - ham) / 16(ham - spam)
  
  1. 정확도
    (1181 + 165) / 1380 = 0.9753623
    => 97.5% "이전 모델과 거의 동일"
  2. 정밀도
    165 / 183 = 0.9016393
    => 90.2% "이전 모델에 비해 약간 감소"
  3. 재현율
    165 / 181 = 0.9116022
    => 91.2% "이전 모델보다 재현율 상승"
  4.F1스코어
    89.5%
    
  ** 재현율 상승 - 스팸을 더 잘 잡는게 중요하다면 라플라스 보정 모델이 나음
  ** 정밀도 하락 - 오탐 최소화가 더 중요하다면 기존 모델도 좋음
' 
'나이브 베이즈 분류 결과
  => 스팸 메일을 스팸 메일이라 분류하는 확률은 0.912,
     스팸 처리가 안된 메일을 스팸 처리가 안된 메일이라 분류하는 확률은 0.98
     전체적으로 1346 : 34 의 비율이 되므로 괜찮은 분류결과를 내는 모델이라 볼 수 있다.
'

