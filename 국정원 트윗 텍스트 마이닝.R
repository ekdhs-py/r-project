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
library(KoNLP)

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