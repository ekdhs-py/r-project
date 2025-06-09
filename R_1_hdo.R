' 기본 함수 및 파일 로드 '

' 
주석 : 코드에 영향을 미치지 않고 해당 코드에 대한 설명이나
      작성자의 의견을 적을 때 사용한다.
  1. 한줄 주석 : #
  2. 장문 주석 : '', ""
  
'
print('Hello World')

a <- 1 + 134
sum(54 + 574)

' c() : 생성 함수 '
a = c(1,2)
b = c(1,3)
plot(a,b) # 그래프 함수

' 논리형 자료 '
# 값이 없으면 FALSE, 값이 존재하면 TRUE 다.
# 조건이 맞으면 TRUE, 조건이 틀리면 FALSE 다.
a = 4
b = 5
a>b ## FALSE
a<b ## TRUE

' 벡터 
  : 데이터 구조의 가장 기본유형(1차원) '
# 숫자형 벡터
ex_vector1 <- c(-1,0,1) ## 데이터 생성
ex_vector1
# 숫자형 벡터 속성, 길이 확인
' 함수 : 기능을 하는 수'
mode(ex_vector1) ## numeric형
str(ex_vector1)
length(ex_vector1) ## 데이터 길이 : 값의 개수

# 문자형 벡터
ex_vector2 <- c("Hello", "Hi~!")
ex_vector2
ex_vector3 <- c("1", "2", "3")
ex_vector3
# 문자형 속성, 길이 확인
mode(ex_vector2) ## character형
str(ex_vector2)
mode(ex_vector3) ## character형
str(ex_vector3)

### 범주형 자료(범위)
ex_vector5 <- c(2,1,3,2,1)
'factor() : 범주형 자료 입력'
cate_vector5 <- factor(ex_vector5, labels = c('Apple',                                              'Banana', 'Cherry')) 
# 범주화할 자료(ex_vector5), 라벨 욥션 -> 범주화
cate_vector5

' 행렬 - matrix() 
  : 행과 열로 구성(2차원) '
x <- c(1,2,3,4,5,6)
matrix(x, nrow = 2, ncol = 3) ## 2행 3열
matrix(x, nrow = 3, ncol = 2) ## 3행 2열 ##기본 값 열 먼저 입력
matrix(x, nrow = 3, ncol = 2, byrow = T) ## byrow : 행 먼저 값 입력

' 배열 
  : 행렬과 유사하지만 2차원 이상의 구조'
y <- c(1,2,3,4,5,6)
array(y, dim = c(2,2,3)) ## dim = c(행, 열, 층)

' 리스트 
  : 다양한 유형의 데이터를 하나의 변수에 저장'
list1 <- list(c(1,2,3), "Hello") 
list1

' 데이터 프레임
  : 다양한 데이터를 하나의 테이블에 저장 '
ID <- c(1,2,3,4,5,6,7,8,9,10)
SEX <- c("F", "M","F","M","M","F","F","F","M","F")
AGE <- c(50,40,28,50,27,23,56,47,20,38)
AREA <- c('서울', '경기', '제주','서울', '서울', '서울',
          '경기', '서울', '인천', '경기')
dataframe_ex <- data.frame(ID,SEX,AGE,AREA)
dataframe_ex

str(dataframe_ex) ## ID : num, SEX : chr, AGE : num, AREA : chr

' 변수 (Variable) 
  1. 다양한 값을 지니고 있는 하나의 속성
  2. 결국 데이터 분석의 대상
'

#변수 연산
a <- -1
b <- 2
c <- 3
d <- 3.5

a+b
a + b + c
4/b
5*d

# 여러 값으로 구성된 변수 활용
var1 <- c(1,2,5,7,8)
var2 <- c(1:100)

var1
var2

# seq() : 짝수, 홀수 또는 간격을 만들고 싶을 때
var3 <- seq(1,5)
var4 <- seq(1,100, by=2) # 1-100까지 2간격 연속값 생성
var5 <- seq(1,100, by=3)
var5

# 연속값 변수로 연선
var1 + 2
var1 + var2

### 문자열 변수는 연산할 수 없다. ###

' 함수 : 값을 넣으면 특정한 기능을 수행해 다른 값을 출력 '

# 숫자를 다루는 함수
x <- c(1,2,3)
x
mean(x) # 평균
max(x) # 최대값
min(x) # 최소값

# 문자를 다루는 함수
str5 <- c('Hello!', 'World', 'is', 'good !')
str5
' paste() : 문자열을 연결하는 함수'
paste(str5, collapse = ',') # 쉼표를 구분자로 str5의 단어들을 하나로 합쳐준다,

### collapse = , by = 이런 값들을 옵션 또는 파라미터라 한다 ###

# 함수의 결과값으로 새로운 변수 생성
x_mean <- mean(x)
str5_paste <- paste(str5, collapse = " ")
str5_paste

' 패키지 (Packages)
  1. 함수가 여러 개 들어 있는 꾸러미
  2. 하나의 패키지 안에 다양한 "함수"가 존재함
  3. 함수를 사용하려면 패키지를 먼저 설치하고 불러와야 함
'

# 패키지 설치법(ggplot2)
'ggplot2 : R에서 그래프와 관련된 함수 제공 패키지'
install.packages('ggplot2') # 패키지 설치
library(ggplot2) # 패키지 로드

x <- c('a','a','b','c')
qplot(x) # 빈도 그래프

' 패키지마다 패키지의 함수를 테스트해 볼 수 있는 테스트 데이터가 존재한다.'
# ggplot2의 mpg 데이터로 그래프 만들기
qplot(data=mpg, x=hwy)
qplot(data=mpg, x=cty)

# x축(drv = 구동방식), y축(hwy = 고속도로 연비) 직접 지정
qplot(data=mpg, x=drv, y=hwy)
qplot(data=mpg, x=drv, y=hwy, geom="line") # 선형태
qplot(data=mpg, x=drv, y=hwy, geom = "boxplot") # 상자그림
qplot(data=mpg, x=drv, y=hwy, geom = "boxplot", colour = drv)

' 데이터 프레임
  1. 열 = 속성
  2. 행 = 한 사람의 정보
  3. 데이터가 크다 : 행 또는 열이 많다
'

# 성적 데이터 프레임 작성
english <- c(90,80,60,70)
math <- c(50,60,100,20)
df_midterm <- data.frame(english, math)
df_midterm

class <- c(1,1,2,2)
df_midterm <- data.frame(english, math, class)
df_midterm

' 데이터 프레임 변수 접속 : $ 기호 사용'
mean(df_midterm$english)

'Q1, data.frame()과 c() 함수를 조합해서 그림의 내용을 데이터 프레임으로 만들어 출력하고,
      과일 가격의 평균과 판매량 평균을 구하세요. '

fruit = c('사과', '딸기', '수박')
price = c(1800, 1500, 3000)
v = c(24,38,13)

s <- data.frame(fruit,price,v)

mean(s$price)
mean(s$v)

'외부 데이터 - 시험 성적 데이터 로드'
# 엑셀 파일 불러오기
install.packages('readxl')
library(readxl)

df_exam <- read_excel('excel_exam.xlsx')
str(df_exam)
mean(df_exam$english)
mean(df_exam$math)

# 경로 지정해서 불러오기
'R에서는 디스크명을 소문자로 쓰고(c:) 경로를 구분할 때는 슬래쉬(/)를 씀'
df_exam_ex <- read_excel("c:/Program Files/test/excel_exam.xlsx")

# 엑셀 파일 첫 행이 변수명이 아닌 경우
df_exam_novar <- read_excel("c:/R_Project_hdo/excel_exam_novar.xlsx")
df_exam_novar ## 첫 행의 값이 변수명이 됨
df_exam_novar <- read_excel("c:/R_Project_hdo/excel_exam_novar.xlsx", 
                            col_names = F) # 열 이름이 없음
df_exam_novar ## 변수 명을 없애고 값들이 그대로 출력

# 엑셀에 시트가 여러 개 있는 경우
df_exam_sh <- read_excel("c:/R_Project_hdo/excel_exam_sheet.xlsx", sheet = 3) ## 3번째 시트 선택
df_exam_sh

' csv 파일 로드
  1. 값 사이를 쉼표로 구분한 파일
  2. 용량이 작음, 다양한 소프트웨어에서 사용함
'
df_csv_exam <- read.csv('c:/R_Project_hdo/csv_exam.csv')
df_csv_exam

#------------------------------------------------------------------------------------------------------------------------------------------
' - Review 
  1. 변수 : 변하는 수
  a <- 1
  
  2. 함수 : 기능을 하는 수( 입력을 받아서 출력을 내는 기능 )
  c(), mean()
  
  3. 패키지 : 함수들의 꾸러미, 하나의 패키지엔 여러개의 함수가 존재
  install.packages("패키지 명")
  library(패키지명)

  4. 데이터 프레임
    1) 열 = 속성, 행 = 대상의 정보
    2) 데이터가 크다 : 행과 열의 개수가 많다.
    3) 데이터 프레임 생성 함수 : data.frame()

  5. 외부 데이터 이용
    1) 엑셀 파일 : read_excel("파일명.확장자")
    2) CSV파일 : read.csv("파일명.확장자")
'
