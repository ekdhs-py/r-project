' 데이터 파악 - 다루기 쉽게 수정 '

' 데이터 파악 함수
  1. head() : 데이터의 앞 부분 출력
  2. tail() : 데이터의 뒷 부분 출력
  3. view() : 뷰어 창에서 데이터를 확인
  4. dim() : 데이터의 차원을 출력
  5. str() : 데이터의 속성 출력
  6. summary() : 요약통계량 출력
'
# 데이터 로드
exam <- read.csv('c:/R_Project_hdo/csv_exam.csv')
## 데이터 파악
head(exam, 10) ## 앞의 10행
tail(exam, 10) ## 뒤의 10행
dim(exam) ## 20 5 // 20행 5열
str(exam) ## 모두 int형인 것을 파악
summary(exam) ## 각 속성마다 최소, 최대, 평균값 등을 파악 

# mpg 데이터 파악
library(ggplot2) ## ggplot2패키지 안의 mpg데이터 로드
## 데이터 프레임 형태로 로드
mpg <- as.data.frame(ggplot2::mpg) ## as.data.frame() : 데이터프레임으로 변환
head(mpg,20)
tail(mpg,20)
str(mpg)
summary(mpg)

' 데이터 수정 - 변수명 수정 '
' dplyr 패키지 
  : 데이터 처리에 특화된 패키지, 데이터 프레임 조작 
'
install.packages('dplyr') # 데이터 셋의 부분 집합화, 요약, 재정렬 및 결합을 위한 필수 패키지
library(dplyr)

#데이터 프레임 만들기
df_raw <- data.frame(var1 = c(1,2,1),
                     var2 = c(2,3,2))
df_raw

### 기존의 데이터를 수정 시 기존 데이터를 복사 후 복사본을 사용하여 수정 ###

# 데이터 복사본 생성
df_new <- df_raw ## 복사

## 변수명 수정 - rename(복사본, 새 변수명 = 기존 변수명 )
df_new <- rename(df_new, v2 = var2) ##var2를 v2로 수정
df_new

'
Q1. mpg 데이터 복사본을 생성하시오.
Q2. cty는 city로, hwy는 highway로 변수명을 수정하고 일부를 10행만 출력하시오.
'
# 1) 복사본 생성
mpg_new <- mpg

# 2) 변수명 수정
mpg_new <- rename(mpg_new, city = cty)
mpg_new <- rename(mpg_new, highway = hwy)

# 3) 10행만 출력
head(mpg_new, 10)


' 파생변수 생성
  : 기존 변수를 활용하여 새로 만드는 변수 '

# mpg 데이터에서 통합 연비 변수 생성
mpg$total <- (mpg$cty + mpg$hwy) / 2 ## '$'를 이용하여 접속
head(mpg, 10)

# 조건문을 활용한 파생변수 생성
## 기준값 정하기
summary(mpg$total) # 문자로 파악(최소, 평균, 최대값을 파악하기 용이)
hist(mpg$total) # 그래프로 파악(값의 분포를 파악하고 비교하기 용이)

## 합격 판정 변수 생성
### 통합 연비가 20 이상이면 pass, 아니면 fail
mpg$test <- ifelse(mpg$total >= 20, "pass", "fail") ## ifelse(조건문, true, false)
head(mpg, 20)

# 빈도표 생성 - table()
table(mpg$test) ## fail : 106, pass : 128
library(ggplot2)
qplot(mpg$test)

' 중첩 조건문 : 조건문 안의 조건문
  A : 30 이상
  B : 20 ~ 29
  C : 20 미만
'
# total 기준으로 a, b, c 등급 부여
mpg$grade <- ifelse(mpg$total >= 30, "A",
                    ifelse(mpg$total >= 20, "B", "C"))
head(mpg,20)
table(mpg$grade) ## A : 10, B : 118, C : 106
qplot(mpg$grade) ## 막대 그래프

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

' 데이터 가공 
  : 데이터를 원하는 형태로 만드는 것(전처리)
  
  전처리 함수 - dplyr패키지 함수
  1. filter() : 행 추출 함수
  2. select() : 열 추출 함수
  3. arrange() : 정렬
  4. mutate() : 변수 추가
  5. summarise() : 통계치 산출
  6. group_by() : 집단별로 나누기
  7. left_join() : 데이터 합치기(열 기준)
  8. bint_rows() : 데이터 합치기(행 기준)
'

# 조건에 맞는 데이터만 추출 - filter()
' %>% 파이프 기호 : 전처리 함수들을 연결 '
exam %>% filter(class == 2) ## class가 2인 행 추출
exam %>% filter(class != 1) ## class가 1이 아닌 행
exam %>% filter(math > 50) ## 수학 점수가 50점 초과 

## 중첩 조건
exam %>% filter(class == 2 & english >= 80) ## 2반이고 영어가 80점 이상
exam %>% filter(math >= 90 | english >= 90) ## 수학이 90점 이상이거나 영어가 90점 이상
### 1반, 3반, 5반 추출
exam %>% filter(class == 1 | class == 3 | class == 5) 
exam %>% filter(class %in% c(1,3,5)) ## %in% : 포함 연산자 

# 1반 수학 점수 평균
class1 <- exam %>% filter(class == 1) ## class1변수에 1반인 학생의 점수 생성
mean(class1$math) ## 1반 수학 점수 평균


' R에서 사용하는 기호

  1. 논리 연산자
    1) < : 작다
    2) <= : 작거나 같다
    3) > : 크다
    4) >= : 크거나 같다
    5) == : 같다
    6) != : 같지 않다
    7) %in% : 포함연산자, 매칭
    
  2. 산술 연산자
    1) +, - : 더하기, 빼기
    2) / : 나누기
    3) * : 곱하기
    4) ^, ** : 제곱
    5) %/% : 나눗셈의 몫
    6) %% : 나눗셈의 나머지
'

# 필요한 변수(열)만 추출 - select()
exam %>% select(math) ## 수학 점수만 추출
exam %>% select(class, math, english)
exam %>% select(-math, -english) ## 변수 제외


' dplyr 패키지 함수들 조합 '

## 1반 학생들의 영어 점수만 조회
exam %>% filter(class == 1) %>% select(english)

## id, math 추출 후 앞 6행까지 추출
id_math <- exam %>% select(id, math)
head(id_math, 6)

exam %>% select(id, math) %>%
  head(6)

# 순서 정렬 - arrange()
exam %>% arrange(math) ## 기본값 오름차순
exam %>% arrange(desc(math)) ## 내림차순

# 파생변수 추가 - mutate()
exam %>% mutate(total = math + english + science) %>%
  head()
## 여러 파생변수 한번에 추가
exam %>% mutate(total = math + english + science,
                mean = (math + english + science)/3) %>%
  head

## 조건문 활용하여 파생변수 추가
exam %>% mutate(test = ifelse(science >= 60, 'pass', 'fail')) %>%
    head

## total 변수 추가 후 정렬
exam %>% mutate(total = math + english + science) %>%
  arrange(total) %>%
  head

# 요약 - summarise()
exam %>% summarise(mean_math = mean(math)) ## 수학 평균 산출

# 집단별 요약 - group_by()
exam %>% 
  group_by(class) %>% ## 각 반 별
  summarise(mean_math = mean(math)) ## 수학 평균

## 여러가지 요약 통계량 한 번에 산출
exam %>%
  group_by(class) %>% ## 반 별 분리
  summarise(mean_math = mean(math), ## 수학 평균
            sum_math = sum(math), ## 수학 합계
            median_math = median(math), ## 수학 중앙값
            stud = n()) ## 학생 수
' 자주 사용하는 요약 통계량 함수
  1. mean() : 평균
  2. sd() : 표준편차
  3. sum() : 합계
  4. median() : 중앙값
  5. min(), max() : 최소값, 최대값
  6. n() : 빈도
'

## 각 집단별로 다시 집단 나누기
### 제조사 별 구동방식 별 도심 연비 평균
mpg %>%
  group_by(manufacturer, drv) %>%
  summarise(mean_cty = mean(cty)) %>%
  head(20)

'Q1. 제조사별로 "SUV"자동차의 도시 및 고속도로 통합연비 평균을 구해서
    내림차순으로 정렬하고, 1~5위까지 출력하시오.'
mpg %>%
  group_by(manufacturer) %>% ## 제조사별로
  filter(class == "suv") %>% ## "suv"자동차의
  mutate(tot = (cty + hwy)/2) %>% ## 도시 및 고속도로 통합 연비
  summarise(mean_tot = mean(tot)) %>% ## 평균을 구해서
  arrange(desc(mean_tot)) %>% ## 내림차순으로 정렬하고
  head(5) ## 1~5위까지 출력하시오


# 가로로 데이터 합치기 - left_join()
## 데이터 생성
### 중간고사
test1 <- data.frame(id = c(1,2,3,4,5),
                    midterm = c(60,80,70,90,85))
### 기말고사
test2 <- data.frame(id = c(1,2,3,4,5),
                    final = c(70,83,65,95,80))
test1
test2
## 열을 기준으로 가로로 합치기
total <- left_join(test1, test2, by = 'id') ## id를 기준으로 합쳐서 total에 할당
total

## 매칭 - left_join()
### 반 별로 담임 교사 추가
name <- data.frame(class = c(1,2,3,4,5),
                   teacher = c('kim','lee','park','choi','jung'))
name
exam_new <- left_join(exam, name, by='class')
exam_new


# 세로로 합치기 - bind_rows()
## 데이터 생성
group_a <- data.frame(id = c(1,2,3,4,5),
                      test = c(60,80,70,90,85))
group_b <- data.frame(id = c(6,7,8,9,10),
                      test = c(70,83,65,95,80))
group_a
group_b

## 행을 기준으로 합치기( 두 데이터 변수명이 같아야 함!!)
group_all <- bind_rows(group_a, group_b)
group_all

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
' 데이터 정제
  : 빠진 데이터(결측치), 이상한 데이터(이상치) 제거 '

' 결측치(Missing Value)
  1. 누락된 값, 비어있는 값
  2. 함수에 적용 불가, 분석 결과를 왜곡
  3. 제거한 후 분석 실시
'

# 결측치 찾기 - is.na()
## 결측치 데이터 생성
df <- data.frame(s = c('M', 'F', NA, 'M', 'F'),
                 score = c(5,4,3,4,NA))
df
## 결측치 확인
is.na(df)

# 결측치 빈도 출력
table(is.na(df)) ## false : 8, true : 2
table(is.na(df$score)) ## false : 4, true : 2

# 결측치 포함 분석
mean(df$score) ## NA

' 결측치 제거
  : 결측치가 있는 행을 제거(데이터의 크기에 여유가 있는 경우)
'
df %>% filter(is.na(score)) ## score변수에서 널값만 출력
df %>% filter(!is.na(score)) ## score변수에서 널값이 아닌 경우 출력

df_nomiss <- df %>% filter(!is.na(score))
mean(df_nomiss$score) ## 결측치 제거 후 분석

# 여러 변수에 결측치가 있을 경우
df_nomiss <- df %>% filter(!is.na(score) & !is.na(s))
df_nomiss

# 결측치가 하나라도 있으면 제외 - na.omit()
'분석에 필요한 데이터까지 손실될 우려가 있음.'
df_nomiss2 <- na.omit(df)
df_nomiss2

# 함수의 결측치 제외 기능 - na.rm = T
mean(df$score, na.rm = T) 

## 결측치 데이터 생성
exam_na <- read.csv('c:/R_Project_hdo/csv_exam.csv')
exam_na[c(3,8,15), 'math'] <- NA ## math의 3, 8, 15번 행에 NA 할당
exam_na

## 평균 산출
exam_na %>% summarise(mean_math = mean(math, na.rm = T))

exam_na %>% summarise(mean_math = mean(math, na.rm = T),
                      sum_math = sum(math, na.rm = T),
                      median_math = median(math, na.rm = T))


' 결측치 대체(다른 값으로 채워넣기)
  1. 대표값(평균, 최빈값 등)으로 일괄 대체
  2. 통계 분석 기법, 예측값 추정해서 대체
'
# 평균값 대체
mean(exam_na$math, na.rm = T) ## 55.23529 : 결측치 제외 평균값

exam_na$math <- ifelse(is.na(exam_na$math), 55, exam_na$math) ## math의 값이 널값이면 55로 대체
table(is.na(exam_na$math)) ## false : 20
exam_na


' 이상치(Outlier) 
  : 정상범주에서 크게 벗어난 값
  ex_1) 성별 변수에 3 -> 결측처리
  ex_2) 몸무게 변수에 200 -> 정상범위 기준 정해서 결측처리
'
# 이상치 포함 데이터 생성
out <- data.frame(s = c(1,2,1,3,2,1),
                  score = c(5,4,3,4,2,6))
out

# 이상치 확인
table(out$s) ## 1:3, 2:2, 3:1
table(out$score) ## 2:1, 3:1, 4:2, 5:1, 6:1

# 이상치 결측 처리
out$s <- ifelse(out$s == 3, NA, out$s) ## 성별이 3이면 결측 처리
out

out$score <- ifelse(out$score > 5, NA, out$score) ## score가 5이상이면 결측처리
out

# 결측치 제외 분석
out %>%
  filter(!is.na(s) & !is.na(score)) %>%
  group_by(s) %>%
  summarise(mean_score = mean(score))


' 극단치
  
  논리적 판단 : 성인 몸무게 40kg ~ 150kg 벗어나면 극단적인 값으로 간주
  통계적 판단 : 상하위 0.3% 극단치 또는 상자그림 1.5 IQR 벗어나면 극단치로 간주
'
# 상자그림 해석하기
mpg <- as.data.frame(ggplot2 :: mpg)
boxplot(mpg$hwy)
'
  1) 상자 아래 세로 점선(아래수염) : 하위 0% ~ 25% 안에 해당하는 값
  2) 상자 밑면(1사분위수 - Q1) : 하위 25%에 위치한 값
  3) 상자 내 굵은 선(2사분위수 - Q2) : 중앙값 50%
  4) 상자 윗면(3사분위수 - Q3) : 하위 75%에 위치한 값
  5) 상자 위 세로 점선(윗 수염) : 하위 75% ~ 100% 안에 해당하는 값
  
  6) 상자 밖 가로 선(극단치 경계) : Q1, Q3 밖 1.5 IQR 내 최대값
  7) 상자 밖 점 표식(극단치) : Q1, Q3 밖 1.5 IQR을 벗어난 값
  
  8) 1.5 IQR : 1사분위수와 3사분위수에 1.5를 곱한 값
'

# 상자그림 통계치 출력
boxplot(mpg$hwy)$stats ## 최소값(12), 1사분위수(18), 2사분위수(24), 3사분위수(27), 최대값(37)

## 12 ~ 37 벗어나면 NA 할당
mpg$hwy <- ifelse(mpg$hwy < 12 | mpg$hwy > 37, NA, mpg$hwy)
table(is.na(mpg$hwy)) ## na 3개

## 결측치 제외 분석
mpg %>%
  group_by(drv) %>% ## 구동방식 별
  summarise(mean_hwy = mean(hwy, na.rm = T)) ## 고속도로 평균 연비

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
' R에서 만들 수 있는 그래프 - ggplot2 패키지 심화 

  1. 2차원, 3차원 그래프
  2. 지도 그래프
  3. 네트워크 그래프
  4. 모션 차트
  5. 인터랙티브 그래프
'
' qplot() vs ggplot()
  1. qplot() : 전처리 단계 데이터 확인용 - 문법 간단, 기능 단순
  2. ggplot() : 최종 보고용 - 색, 폰트, 크기 등 세부 조작 가능
'

' ggplot2의 레이어 구조
  1단계) 배경 설정(축)
  2단계) 그래프 추가(산점도, 막대, 선)
  3단계) 설정 추가(축 범위, 색, 표식)
'

' 산점도 - geom_point()
  : 데이터를 x축과 y축에 점으로 표현한 그래프
  - 나이와 소득 등 연속값으로 된 두 변수의 관계를 표현할 때 자주 사용
'
# 배경 설정
ggplot(data = mpg, aes(x=displ, y=hwy)) ## x축, y축 설정
# 그래프 추가
ggplot(data = mpg, aes(x=displ, y=hwy)) + geom_point() ## 산점도
# 축 범위를 조절하는 설정 추가
ggplot(data = mpg, aes(x=displ, y=hwy)) + geom_point() + xlim(2, 6) ## xlim() : x축 좌표값의 범위

ggplot(data = mpg, aes(x=displ, y=hwy)) +
  geom_point() +
  xlim(2,6) +
  ylim(10,30)

'Q1. mpg 데이터에서 cty(도시 연비)와 hwy(고속도로 연비)간에 어떤 관계가 있는 지 알아보려 한다.
    x축은 cty, y축은 hwy로 이루어진 산점도를 그려보시오.
'
ggplot(data = mpg, aes(x = cty, y = hwy)) + geom_point()


' 막대 그래프
  1. 평균 막대 그래프 - geom_col()
    : 집단의 평균값을 막대 길이로 표현한 그래프
    - 데이터를 요약한 평균표를 먼저 만든 후 그래프 생성
  2. 빈도 막대 그래프 : geom_bar()
    : 값의 개수(빈도)로 막대의 길이를 표현한 그래프
    - 별도로 표를 만들지 않고 원자료를 이용해서 바로 그래프 생성
'
# 평균 막대 그래프
library(dplyr) 
mpg <- as.data.frame(ggplot2 :: mpg) 

## 고속도로 평균 연비
df_mpg <- mpg %>% group_by(drv) %>% ## 구동방식 별
                  summarise(mean_hwy = mean(hwy)) ## 고속도로 연비 평균
df_mpg
## 평균연비 막대그래프
ggplot(data = df_mpg, aes(x=drv, y=mean_hwy)) + geom_col()
### 크기순으로 정렬(내림차순)
ggplot(data = df_mpg, aes(x=reorder(drv, -mean_hwy), y= mean_hwy)) + geom_col() ## -mean_hwy : 내림차순

# 빈도 막대 그래프
## x축 범주 변수, y축 빈도
ggplot(data = mpg, aes(x=drv)) + geom_bar()

## x축 연속 변수, y축 빈도
ggplot(data = mpg, aes(x=hwy)) + geom_bar()

#------------------------------------------------------------------------------------------------------------------------------------
' - Review

1. 조건에 맞는 데이터만 추출 - filter
  exam %>% filter(english >= 80)

2. 여러 조건 동시 충족 - &, |
  exam %>% filter(class == 1 & math >= 50)

3. 필요한 변수만 추출 - select
  exam %>% select(math)
  
4. 함수 조합
  exam %>%
    select(id, math) %>%
    head(10)

5. 순서대로 정렬 - arrange
  exam %>% arrange(math) # 수학점수 오름차순
  exam %>% arrange(desc(math)) # 수학점수 내림차순

6. 파생변수 추가 - mutate
  exam %>% mutate(total = eng + math + sci)
  exam %>% mutate(test = ifelse(science >/ 60, "pass", "fail)) # 조건

7. 집단별로 요약 - group_by
  exam %>% 
    group_by(class) %>%
    summarise(mean_math = mean(math)) # 요약통계량
# 각 집단별로 다시 집단 나누기
  mpg %>%
    group_by(manufacturer, drv) %>%
    summarise(mean_cty = mean(cty)) %>%
    head(10)
    
8. 데이터 합치기
# 가로로 합치기 - left_join
  total <- left_join(test1, test2, by = "id")
    
# 세로로 합치기 - bind_rows
  group_all <- bind_rows(group_a, goup_b)
  
9. 결측치
  1) 결측치 확인 - is.na
  table(is.na(df$score))
  
  2) 결측치 제거 - !is.na
  df_nomiss <- df %>% filter(!is.na(score))
  
  3) 함수의 결측치 제외 기능
  mean(df$score, na.rm = T)
  exam %>% summarise(mean_math = mean(math, na.rm = T))
  
10. 이상치
  1) 이상치 확인
  table(out$s)
  
  2) 결측처리
  out$s <- ifelse(out$s == 3, NA, out$s)
  
  3) boxplot으로 극단치 기준 찾기
  boxplot(mpg$hwy)
  boxplot(mpg$hwy)$stats # 박스플롯 여러 값을 수치로 결과 출력
  
  4) 극단치 결측 처리
  mpg$hwy <- ifelse(mpg$hwy < 12 | mpg$hwy > 37, NA, mpg$hwy)
  
11. ggplot2 함수 요약
  1) 산점도
  ggplot(data == mpg, aes(x=displ, y=hwy)) + geom_point()
  
  2) 평균 막대 그래프
    # 1단계 - 평균표 생성
    df_mpg <- mpg %>%
                group_by(drv) %>%
                summarise(mean_hwy = mean(hwy))
                
    # 2단계 - 그래프 생성, 크기순 정렬
    ggplot(data = df_mpg, aes(x=reorder(drv, -mean_hwy), y=mean_hwy)) + geom_col()
    
  3) 빈도 막대 그래프
  ggplot(data=mpg, aes(x=drv)) + geom_bar()
  