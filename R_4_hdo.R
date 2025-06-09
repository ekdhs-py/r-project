' 텍스트 마이닝
  : 문자로 된 데이터에서 가치 있는 정보를 얻어 내는 기법 

  형태소 분석 
  : 문장을 구성하는 어절들이 어떤 품사로 되어 있는지 파악하는 작업 
'

install.packages('rJava') #저수준 R에서 Java 인터페이스로 객체 생성, 메서드 호출, 필드 접근을 허용
install.packages('memoise') #함수 결과를 저장하여 같은 인수로 다시 호출할 때 이전에 계산된 값을 반환
install.packages('Sejong') #KoNLP 정적 사전 및 세종 프로젝트 리소스
install.packages('hash') #해시 테이블/연관 배열/사전의 모든 기능을 갖춘 구현
install.packages('tau') #텍스트 분석 유틸리티
install.packages('devtools') #R 패키지 개발을 더 쉽게 만들어주는 도구 

Sys.setenv(JAVA_HOME = "C:\\Program Files\\Amazon Corretto\\jdk11.0.20_8")

install.packages('KoNLP') #한국어 형태소 분석 R패키지

# KoNLP 수동설치
install.packages('multilinguer') #java, python등 다른 언어의 설치기능을 제공
multilinguer::install_jdk() ##벡터 오류 발생
## 오류 해결 (JDK 수동 설치)
Sys.setenv(JAVA_HOME = "C:/Program Files/Java/jdk-17")  # 설치 경로에 맞게 조정
Sys.setenv(JAVA_HOME = "/opt/homebrew/opt/openjdk@17")
### JDK 설치 확인
system("java -version")

install.packages(c('hash', 'tau', 'RSQLite', 'bit','rex','lazyeval',
                   'htmlwidgets', 'crosstalk', 'promises', 'later',
                   'sessioninfo', 'xopen', 'bit64', 'blob', 'DBI',
                   'plogr', 'covr', 'DT', 'rcmdcheck', 'rversions'),
                 type = 'binary')

remotes::install_github('haven-jeon/KoNLP',
                        upgrade = 'never',
                        INSTALL_opts = c('--no-multiarch'),
                        force = TRUE)
library(KoNLP)
useNIADic()

extractNoun('안녕하세요 R을 공부중입니다.')

#----------------------------------------------------------------------------------------------------

' 국정원 트윗 텍스트 마이닝 '
# 패키지 로드
library(stringr)
library(dplyr)
install.packages("wordcloud")
library(wordcloud)
library(RColorBrewer)

# 데이터 로드
twitter <- read.csv('c:/R_Project_hdo/twitter.csv',
                    header = T,
                    stringsAsFactors = F,
                    fileEncoding = 'UTF-8')

# 변수명 수정
str(twitter)
twitter <- rename(twitter,
                  no = 번호,
                  id = 계정이름,
                  date = 작성일,
                  tw = 내용)
str(twitter)

# 문자열 전처리
## 특수문자 제거
twitter$tw <- str_replace_all(twitter$tw, '\\W', ' ') # '\\W : 영어, 한글, 숫자를 모두 찾는 패턴
head(twitter$tw) ## 트윗 텍스트에서 단어문자가 아닌 모든 문자를 공백으로 대체 

# 단어 빈도표 생성
## 명사 추출
nouns <- extractNoun(twitter$tw)
nouns

### 오류 수정
#### 특수 문자 제거
twitter$tw_clean <- str_replace_all(twitter$tw, "[^가-힣0-9\\s]", " ")
#### 여러 공백을 하나로 축소
twitter$tw_clean <- str_squish(twitter$tw_clean)
extractNoun(twitter$tw_clean)
#### 빈 문자열이나 na 처리
twitter$tw_clean <- twitter$tw_clean[!is.na(twitter$tw_clean) & twitter$tw_clean != ""]
#### 인코딩 확인 및 변환
twitter$tw_clean <- iconv(twitter$tw_clean, from="CP949", to="UTF-8", sub="")
#### KoNLP 사전 초기화
library(KoNLP)
useSejongDic() # 혹은 다른 사전 설정

nouns <- extractNoun(twitter$tw_clean)

## 추출한 명사 list를 문자열 벡터로 치환, 단어별 빈도표 생성
wordcount <- table(unlist(nouns)) #unlist() : 리스트를 벡터로 평탄화(하나의 벡터로 모든 명사를 합침) 
warnings()
wordcount

## 데이터 프레임
df_word <- as.data.frame(wordcount, stringsAsFactors = F) ##factor타입으로 변환 X
df_word

### 변수명 수정
df_word <- rename(df_word,
                  word = Var1,
                  freq = Freq)
df_word

## 두 글자 이상 단어만 추출
df_word <- filter(df_word, nchar(word) >= 2)
df_word

## 상위 20개 추출
top20 <- df_word %>%
  arrange(desc(freq)) %>%
  head(20)
top20

# 단어 빈도 막대그래프 생성
library(ggplot2)

## 빈도 순서 변수 생성
order <- arrange(top20, freq)$word
order
top20

## 빈도 막대그래프 생성
ggplot(data = top20, aes(x=word, y=freq)) +
  ylim(0, 2500) + # y축(빈도수)을 0부터 2500까지만 표시
  geom_col() +
  coord_flip() + # x축과 y축을 뒤집어서 가로막대그래프로 생성
  scale_x_discrete(limit = order) + #x축(단어)의 순서를 수동으로 지정
  geom_text(aes(label = freq), hjust = -0.3) ##빈도 표시
' 해석
  - 북한 관련 언급이 압도적인 것으로 보아 북한과 관련된 이슈를 다루고 있음
  - 비판적 언어가 다수 존재
  - 특정 단체 또는 진영에 대한 비판적 시각이 반영되었을 가능성이 큼
  => 북한 및 관련 이념 세력에 대한 경계와 비판을 담은 정치적 성향의 텍스트 
'

# 워드 클라우드
'brewer.pal(색상의 수, 색)
  : RColorBrewer 패키지에서 제공하는 색상 팔레트 함수'
pal <- brewer.pal(9, 'Blues')[5:9] #9개의 색상 중 5번부터 9번까지의 색상(1:4는 너무 연해서 눈에 잘 안띔) 
brewer.pal.info
set.seed(1234)


wordcloud(words = df_word$word,
          freq = df_word$freq,
          min.freq = 11,
          max.words = 200,
          random.order = F,
          rot.per = .1,
          scale = c(6, 0.2),
          colors = pal)

# 오류 수정
warnings()
## min.freq = 10이 너무 높음
summary(df_word$freq)  # 빈도 분포 확인

##결측치 존재 확인
any(is.na(df_word$word)) ## 없음
any(is.na(df_word$freq)) ## 없음

## colors = pal 확인
print(pal)

## 시각화 창 문제 확인
plot.new()

# 새 그래픽 장치를 크게 열기 (Windows 예시)
windows(width=10, height=10)  # macOS: quartz(), Linux: X11()

#--------------------------------------------------------------------------------------------
' 통계 분석 기법 - 가설 검정 
  
  1. 기술 통계 : 데이터를 요약해서 설명하는 통계기법
  2. 추론 통계 : 숫자를 요약하는 것을 넘어 특정 값이 발생할 확률을 계산하는 통계 기법
  
  ex) 성별에 따라 월급 차이가 존재
    
    1. 이런 차이가 우연히 나타날 확률이 "작다"
    -> 월급 차이가 통계적으로 유의하다.
    
    2. 이런 차이가 우연히 나타날 확률이 "크다"
    -> 월급 차이가 통계적으로 유의하지 않다.
    
  - 통계적 가설 검정 : 유의확률을 사용해 가설을 검정하는 방법
 
  - 유의확률 : 실제로는 집단 간 차이가 없는데 우연히 차이가 있는 데이터가 추출될 확률(기준값 : 0.05)
'

' t 검정 
  : 두 집단의 평균 비교 '
# compact 자동차와 suv 자동차의 도시 연비 t 검정
## mpg데이터 로드
mpg <- as.data.frame(ggplot2::mpg)
mpg

## 데이터 전처리
library(dplyr) #데이터 전처리 패키지
mpg_diff <- mpg %>%
  select(class, cty) %>%   
  filter(class %in% c('compact', 'suv'))

head(mpg_diff)
table(mpg_diff$class) ## compact : 47, suv : 62

## t-test
t.test(data = mpg_diff, cty ~ class, var.equal = T) #var.equal : 두 집단의 분산이 동일한지
' 결과 해석법
  1. p-value < 2.2e-16
  - p-value : 유의확률
  -> 2.2e-16 는 유의확률이 2.2앞에 0이 16개 있는 값보다 작다라는 뜻이며,
    이 분석 결과는 "compact"와 "suv" 간 평균 도시 연비 차이가 통계적으로 유의함.
    
  2. sample estimates:
  mean in group compact     mean in group suv 
               20.12766              13.50000
  
  - 각 집단의 도시연비 평균
  -> compact는 20인 반면, suv는 14이므로 suv보다 compact의 도시연비가 더 높음.
'

# 일반 휘발유와 고급 휘발유의 도시 연비 t 검정
## 데이터 전처리
mpg_diff2 <- mpg %>%
  select(fl, cty) %>%
  filter(fl %in% c("r", "p")) # regular, premium

table(mpg_diff2$fl) ## p : 52, r : 168

## t-test
t.test(data = mpg_diff2, cty ~ fl, var.equal = T)
' 해석
  - p-value = 0.2875
  -> p-value가 0.05 보다 큰 0.2875라는 것은 실제로는 차이가 없는데 우연히 차이가 관찰될 확률이 28.75%라는 뜻
    따라서 "일반 휘발유와 고급휘발유를 사용하는 자동차간 도시 연비 차이가 통계적으로 유의하지 않음.'

#-------------------------------------------------------------------------------------------------------------------

' 상관분석 
  : 두 연속 변수가 서로 관련이 있는지 검정하는 통계 분석 기법
  
  1. 상관계수로 두 변수가 얼마나 관련되어 있는지 관련성의 정도를 파악 가능
  2. 상관계수는 0~1 사이의 값을 지니고 1에 가까울수록 관련성이 큼
  3. 상관계수가 양수면 정비례, 음수면 반비례
'

# 실업자 수(unemploy)와 개인 소비 지출(pce)의 상관 관계
## 데이터 로드
economics <- as.data.frame(ggplot2::economics)

## 상관분석 시행
cor.test(economics$unemploy, economics$pce)
' 결과 해석법
  - p-value < 2.2e-16 
  -> p-value가 0.05 미만이므로 개인 소비 지출과 실업자 수의 상관이 통계적으로 유의함.
  
  - cor(상관계수) : 0.6145176
  -> cor이 양수 0.61이므로 실업자 수와 개인 소비 지출은
    한 변수가 증가하면 다른 변수도 증가하는 정비례 관계이다. 
'

## 상관행렬 히트맵 만들기
'mtcars( R에 기본적으로 내장된 데이터 )
  : 자동차 32종의 11개 속성에 대한 정보를 담고 있는 데이터 '
head(mtcars)

### 피어슨 상관계수 - cor()
car_cor <- cor(mtcars) # -1 ~ 1의 값
round(car_cor, 2)

### 패키지 다운로드 
install.packages('corrplot') # 상관행렬과 신뢰구간의 그래프 패키지
library(corrplot)

### 시각화
corrplot(car_cor)

#### 숫자로 확인
corrplot(car_cor, method ='number')

#### 다양한 옵션 추가
corrplot(car_cor,
         method = 'color', #색깔로 표현
         type = 'lower', #왼쪽 아래 행렬만 표시
         order = 'hclust', #유사한 상관계수끼리 군집화
         addCoef.col = 'black', #상관계수 색깔
         tl.col = 'black', #변수명 색깔
         tl.srt = 45, #변수명 기울기
         diag = F) #대각 행렬 제외
' 히트맵 해석
  1. 양의 상관관계
    - disp(배기량) vs cyl(실린더 수) : 0.90 
    -> 실린더 수가 많을수록 배기량도 큼
    - hp(마력) vs cyl : 0.83
    -> 실린더 수가 많을수록 마력도 큼
    - wt(무게) vs disp : 0.89
    -> 무게가 클수록 배기량이 많음
    - gear(기어 수) vs am(변속기 종류) : 0.79
    -> 수동변속기일수록 기어 수가 많음
    - am vs drat(리어 액슬 비율) : 0.71
    -> 수동변속기일수록 리어 액슬 비율이 높음
    - gear vs drat : 0.70
    -> 기어 수가 많을 수록 리어 액슬 비율도 높음
    
  2. 음의 상관관계
    - mpg(연비) vs wt : -0.87
    -> 차가 무거울수록 연비가 낮아짐
    - mpg vs cyl : -0.78
    -> 실린더가 많을수록 연비가 낮음
    - mpg vs disp : -0.85
    -> 배기량이 많을수록 연비가 낮음
    - mpg vs hp : -0.78
    -> 마력이 높을수록 연비가 낮음
    - vs(엔진 형태) vs cyl : -0.81
    -> V형 엔진일수록 실린더 수가 많음
    - qsec(1/4마일 시간) vs hp : - 0.71
    -> 마력이 높을수록 시간이 짧음
    
  => 연비(mpg)는 여러 변수에서 음의 상관관계를 보이며,
     특히 무게, 실린더수, 배기량, 마력과 밀접하게 연결되어 있음.
     
  => 자동차 성능 지표(배기량, 마력, 실린더 수)는 서로 양의 상관관계를 가지며,
     전반적으로 차량이 크고 강할수록 연비가 낮아지는 경향을 보임
     
  => 변속기 관련 변수들(am, gear, drat)도 서로 강한 양의 상관관계를 보임    
'

#--------------------------------------------------------------------------------------------------------------------------

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

inspect(sms_corpus[1:2]) #첫 번째와 두 번째 문서만 추출
as.character(sms_corpus[[1]]) #첫번째 문서를 문자형으로 변환해서 실제 텍스트 내용만 추출
lapply(sms_corpus[1:2], as.character) #lapply : 벡터, 리스트, 표현식, df 등에 함수를 적용하고 그 결과를 리스트로 반환  
