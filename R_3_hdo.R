' 한국복제패널데이터 분석 
  -> 한국보건사회연구원에서 가구의 경제활동을 연구해서 정책 지원에 반영할 목적으로 발간하는 조사 자료,
    2006~2015년까지 7천여 가구를 추적 조사한 자료 '

# Using Packages
' foreign 패키지
  : SPSS, SAS, STATA 등 통계분석 소프트웨어의 파일 '
install.packages("foreign")

library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)

# 데이터 로드
raw_welfare <- read.spss(file='c:/R_Project_hdo/Koweps_hpc10_2015_beta1.sav',
                         to.data.frame = T)
str(raw_welfare)
summary(raw_welfare)

# 변수명 변경 - rename
welfare <- rename(raw_welfare, 
                  sex = h10_g3, #성별
                  birth = h10_g4, #태어난 연도
                  marriage = h10_g10, #결혼 유무
                  religion = h10_g11, #종교
                  income = p1002_8aq1, #월급
                  code_job = h10_eco9, #직종코드
                  code_region = h10_reg7) #지역코드
welfare$birth

' 데이터 분석 절차
  1. 변수 검토 및 전처리
  -> 변수의 특성을 파악하고 이상치를 정제한 다음 파생변수 생성

  2. 변수 간 관계를 분석
  -> 요약표 만들고 그래프 생성
'

' 성별에 따른 월급 차이 - 성별과 월급의 상관관계 파악 '
# 성별 변수 검토
table(welfare$sex) ## 1 -> 7578, 2 -> 9086 ==> 이상치 없음

## 결측치 확인
table(is.na(welfare$sex)) ## false -> 16664 => 결측치 없음

## 성별 항목에 이름 부여
welfare$sex <- ifelse(welfare$sex == 1, 'male', 'female')
table(welfare$sex) # 열 변경 확인
qplot(welfare$sex) # 빈도 막대 그래프

# 월급 변수 검토 및 전처리
class(welfare$income) #변수 형 numeric형
summary(welfare$income) ## 최소값 -> 0.0, 평균 -> 241.6, 최대값 -> 2400.0, 결측치 -> 12030 => 이상치, 결측치 존재 확률높음
qplot(welfare$income) + xlim(0, 1000)

## 이상치 결측처리
welfare$income <- ifelse(welfare$income %in% c(0, 9999), NA, welfare$income)

## 결측치 확인
table(is.na(welfare$income)) ## 결측치 -> 12044

# 성별에 따른 월급 차이 분석
## 성별 월급 평균표 생성
sex_income <- welfare %>%
  filter(!is.na(income)) %>% #월급 변수 중 결측치 제외
  group_by(sex) %>% #성별
  summarise(mean_income = mean(income)) #월급 평균
sex_income ## female -> 163., male -> 312.

## 시각화
ggplot(data = sex_income, aes(x=sex, y=mean_income)) + geom_col()
' 해석
  - 여성은 9086명, 남성은 7578명으로 여성이 남성보다 수가 많지만,
    여성은 163만원, 남성은 312만원으로 평균 월급이 남성이 더 높은 것을 알 수 있다.'


' 나이와 월급의 상관관계 - 월급을 가장 많이 받는 나이 '
# 나이 변수 검토
## 이상치 확인
summary(welfare$birth) ## 최소 -> 1907, 최대 -> 2014 => 이상치 없음

## 결측치 확인
table(is.na(welfare$birth)) ##결측치 없음

## 파생변수 '나이' 생성
welfare$age <- 2015 - welfare$birth + 1
summary(welfare$age) ## 48세가 가장 많음
qplot(welfare$age)

# 나이와 월급 관계 분석
## 나이 월급 평균표
age_income <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(age) %>%
  summarise(mean_income = mean(income))
age_income

## 그래프 생성
ggplot(data = age_income, aes(x = age, y = mean_income)) + geom_line()
' 해석
  - 20살부터 100만원 가량의 월급을 받고 이후 증가하는 추세를 보임.
  - 50대 무렵 300만원 가량의 월급을 받고 이후 지속적으로 감소하는 추세를 보임.
  - 70세 이후 20대보다 더 적은 월급을 받음.
'

' 연령대에 따른 월급 차이 - 월급이 가장 많은 연령대 '
# 파생변수 '연령대' 생성
welfare <- welfare %>%
  mutate(ageg = ifelse(age < 30, 'young',
                       ifelse(age <= 59, 'middle', 'old')))
table(welfare$ageg) ##초년 -> 4334, 중년 -> 6049, 노년 -> 6281

# 연령대와 월급의 상관관계
ageg_income <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(ageg) %>%
  summarise(mean_income = mean(income))
ageg_income ##초년 -> 164, 중년 -> 282, 노년 -> 125

# 시각화
ggplot(data = ageg_income, aes(x=ageg, y=mean_income)) + geom_col() 
### 그래프 x축의 순서가 중년 노년 초년 순으로 되어있음.

# x축 순서 직접 지정 - scale_x_discrete()
ggplot(data = ageg_income, aes(x=ageg, y=mean_income)) +
  geom_col() +
  scale_x_discrete(limits = c('young', 'middle', 'old'))
' 해석
  - 초년 4334명, 중년 6049년, 노년 6281명으로 경제활동인구가 노년층이 가장 많은 것으로 보아 고령화와 연관이 있는 것으로 보임.
  - 연령대별 평균 월급은 중년층이 초년,노년층보다 약 2배가량 많고, 노년층이 가장 적은 것을 알 수 있음.
'

' 연령대 및 성별 월급 차이 - 성별 월급차이는 연령대별로 다를까? '
# 연령대 및 성별 월급 평균표 생성
s_income <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(ageg, sex) %>%
  summarise(mean_income = mean(income))
s_income

# 시각화
ggplot(data = s_income, aes(x=ageg, y=mean_income, fill=sex)) +
  geom_col() +
  scale_x_discrete(limits = c('young', 'middle', 'old')) # 함께 표현되어 있어 비교하기 어려움

# 막대그래프 성별 나누기
ggplot(data = s_income, aes(x=ageg, y=mean_income, fill=sex)) +
  geom_col(position = "dodge") + #막대 분리 옵션
  scale_x_discrete(limits = c('young', 'middle', 'old'))
' 해석
  - 초년층의 경우 여성과 남성의 평균 월급이 크게 차이가 나지 않는 반면에,
    중년층과 노년층의 경우 여성이 남성보다 2배가량 평균 월급이 적음.
'

' 나이 및 성별 월급 차이 분석 '
# 성별 연령별 월급 평균표
s_age <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(age, sex) %>%
  summarise(mean_income = mean(income))
s_age

# 시각화 - 선그래프
ggplot(data = s_age, aes(x=age, y=mean_income, fill=sex)) + geom_col() #막대그래프 - fill =
ggplot(data = s_age, aes(x=age, y=mean_income, col=sex)) + geom_line() #선그래프 - col =
' 해석
  - 20대와 80대는 여성과 남성의 월급차이가 거의 나지 않지만,
    30대 이후부터 50대이후까지 성별 월급차이가 많이 나고 60대부터 점차 줄어드는 추세를 보임.
'

' 직업별 월급 차이 - 월급을 가장 많이 받는 직업 '
# 직업 변수 검토
class(welfare$code_job) ##numeric
table(is.na(welfare$code_job)) ##결측치 -> 9135

# 전처리
library(readxl)
## 코드별 직업엑셀 불러오기
list_job <- read_excel('c:/R_Project_hdo/Koweps_Codebook.xlsx', col_names = T, sheet = 2)
head(list_job)
## welfare에 list_job 합치기
welfare <- left_join(welfare, list_job, by ='code_job')

# 직업 월급 평균표 작성
job_income <- welfare %>%
  filter(!is.na(job) & !is.na(income)) %>%
  group_by(job) %>%
  summarise(mean_income = mean(income))
head(job_income)

# 월급이 낮은 직업 10곳
bottom10 <- job_income %>%
  arrange(mean_income) %>%
  head(10)
bottom10 

ggplot(data=bottom10, aes(x=reorder(job, -mean_income), #reorder() : 정렬
                          y=mean_income)) +
  geom_col() +
  coord_flip() + #x축과 y축 위치를 바꿈
  ylim(0,850) #y축 값의 범위 지정
' 해석
  - 월급이 가장 낮은 직업은 가사 및 육아도우미이며,
    월급이 낮은 직업 10곳은 모두 200만원이 안됨.
'

' 성별 직업 빈도 - 성별로 가장 많은 직업 '
# 성별 직업 빈도표
## 남성
job_male <- welfare %>%
  filter(!is.na(job) & sex == 'male') %>%
  group_by(job) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  head(10)
job_male

## 여성
job_female <- welfare %>%
  filter(!is.na(job) & sex == 'female') %>%
  group_by(job) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  head(10)
job_female

# 그래프 생성
## 남성
ggplot(data = job_male, aes(x=reorder(job, n), y=n)) +
  geom_col() +
  coord_flip()

## 여성
ggplot(data = job_female, aes(x=reorder(job, n), y=n)) +
  geom_col() +
  coord_flip()
' 해석
  - 남성과 여성 모두 작물재배 종사자가 600명 이상으로 가장 많음.
  - 남성은 자동차 운전원, 경영관련 사무원, 영업 종사자, 경비원, ㄱㄴ설 및 광업 단순 종사원, 행정 사무원 등이 높은 반면,
    여성은 회계 및 경리 사무원, 음식관련 종사자, 가사 및 육아도우미, 의료복지 관련 종사자가 높음.
  - 공통적으로는 청소원 및 환경 미화원, 매장 판매 종사자, 제조관련 단순 종사원이 높은 편임.
'

' 종교 유무에 따른 이혼율 '
# 종교 변수 검토
class(welfare$religion) ##numeric
table(welfare$religion) ## 1->8047, 2->8617

## 종교 유무에 이름 부여
welfare$religion <- ifelse(welfare$religion == 1, 'yes', 'no')
table(welfare$religion) 

# 혼인 상태 검토
class(welfare$marriage) #numeric
table(welfare$marriage)

## 파생변수 "이혼여부" 생성
welfare$group_marriage <- ifelse(welfare$marriage == 1, 'marriage',
                                 ifelse(welfare$marriage == 3, 'divorce', NA))
table(welfare$group_marriage) ##divorce->712, marriage->8431
table(is.na(welfare$group_marriage)) ##결측치 -> 7521

# 종교유무에 따른 이혼율 분석
religion_marriage <- welfare %>%
  filter(!is.na(group_marriage)) %>%
  group_by(religion, group_marriage) %>%
  summarise(n = n()) %>% ##빈도수
  mutate(total_group = sum(n)) %>% ##전체 빈도
  mutate(pct = round(n/total_group*100, 1)) ##백분율
religion_marriage

# 이혼율만 추출
divorce <- religion_marriage %>%
  filter(group_marriage == 'divorce') %>%
  select(religion, pct)
divorce ##no->8.3, yes->7.2
' 해석
  - 종교의 유무는 이혼율에 크게 영향을 미치지 않음.
'

' 지역별 연령대 비율 - 노년층이 가장 많은 지역 '
# 지역 변수 검토
class(welfare$code_region) #numeric
table(welfare$code_region)

## 전처리
###지역 코드 목록 생성
list_region <- data.frame(code_region = c(1:7),
                          region = c("서울",
                                     "수도권(인천/경기)",
                                     "부산/경남/울산",
                                     "대구/경북",
                                     "대전/충남",
                                     "강원/충북",
                                     "광주/전남/전북/제주도"))
list_region

### 지역별 변수를 데이터에 추가
welfare <- left_join(welfare, list_region, by = 'code_region')

# 지역별 연령대 비율표 생성
region_ageg <- welfare %>%
  group_by(region, ageg) %>%
  summarise(n = n()) %>%
  mutate(tot_group = sum(n)) %>%
  mutate(pct = round(n/tot_group*100, 2)) #지역별 연령대 비율
head(region_ageg)

# 그래프 생성
ggplot(data = region_ageg, aes(x=region, y=pct, fill=ageg)) +
  geom_col() +
  coord_flip()

# 노년층 비율이 높은 순으로 막대 정렬
list_order_old <- region_ageg %>%
  filter(ageg == 'old') %>%
  arrange(pct)
list_order_old

# 지역별 순서 변수 만들기(그래프 입력값)
order <- list_order_old$region
order

# 시각화
ggplot(data = region_ageg, aes(x=region, y=pct, fill=ageg)) +
  geom_col() +
  coord_flip() +
  scale_x_discrete(limits = order)
'해석
  - 노년층 비율이 가장 높은 지역은 대구/경북지역이며,
    가장 적은 지역은 수도권(인천/경기)임.
'

#-------------------------------------------------------------------------------------------------------------------------

' 인터렉티브 그래프
  : 마우스 움직임에 반응해서 실시간으로 형태가 변하는 그래프
  -> 그래프를 HTML 포맷으로 저장하면 일반 사용자들도 웹 브라우저를 이용해서 자유롭게 조작하면서 살펴볼 수 있음.
'
install.packages('plotly') # 동적 인터랙티브 시각화
library(plotly)

# 배기량과 고속도로 연비 산점도
p <- ggplot(data = mpg, aes(x=displ, y=hwy, col=drv)) + geom_point()
ggplotly(p)
' 해석
  - 배기량이 높을수록 고속도로 연비가 떨어짐,
  - 전륜구동(f)이 배기량은 낮고 고속도로 연비가 높은 편이고 사륜구동이 대체적으로 고속도로 연비가 낮은 편임.
'

# 인터렉티브 막대 그래프 - 컷팅품질에 따른 투명도의 분포
' 변수 소개 
  - cut : 절단면 품질
  - color : 색상
  - clarity : 투명도
  - carat : 다이아몬드의 무게'
p <- ggplot(data = diamonds, aes(x=cut, fill=clarity)) +
  geom_bar(position = "dodge")
ggplotly(p)
' 해석
  - cut 등급별 다이아몬드 개수 파악 -> Ideal 컷으로 갈수록 다이아몬드가 가장 많음.
  - clarity 분포 비교 -> Ideal컷 내에서는 SI1투명도가 가장 많고, I1이 가장 적음.
  - cut과 clarity사이의 상관관계 -> cut등급이 높을수록 clarity도 높은 등급이 많음.
'

# dygraphs 패키지로 인터랙티브 시계열 그래프 생성
install.packages('dygraphs')
library(dygraphs)

# ggplot2의 economics데이터셋 불러오기
economics <- ggplot2::economics
head(economics)
' 해당 패키지를 사용해서 시계열 그래프를 만들려면 데이터가 시간순서의 속성을 지니는 xts데이터 타입이어야 함.'

## 데이터타입 변경 - xts()
library(xts)
eco <- xts(economics$unemploy, order.by = economics$date) # eco : 시간 순서가 있는 실업자 수 시계열 데이터  

## 시간에 따른 미국의 실업자 수 변화 그래프
dygraph(eco)
' 해석
  - 실업자수는 2008년부터 급증하다 2010년에 15325명으로 가장 많았고 이후 점진적으로 감소하는 것으로 보임. 
'

### 날짜 범위 선택 기능 추가 - dyRangeSelector()
dygraph(eco) %>% dyRangeSelector()

### 여러 값 표시하기 - psavert : 개인 저축률
eco_a <- xts(economics$psavert, order.by = economics$date)
eco_b <- xts(economics$unemploy/1000, order.by = economics$date) ##실업자수를 1000으로 나눠 백만명으로 변환

### cbind()를 사용해서 가로로 결합 후 변수명 수정
eco2 <- cbind(eco_a, eco_b) # 열방향 데이터 결합
colnames(eco2) <- c("psavert", "unemploy") # 변수명 수정
head(eco2)

### 개인 저축률과 실업자수 사이의 관계
dygraph(eco2) %>% dyRangeSelector()
' 해석
  - 2008년 경제위기로 실업자수가 급격히 증가할 때 개인 저축률도 같이 증가하는 경향을 보임.
  - 개인 저축률은 장기적으로 완만한 하락 추세를 보임 -> 소득대비 저축비중이 감소 , 변동이 비교적 적고 점진적임
  - 실업자수는 경기 사이클에 따라 급격한 상하강이 빈번함 -> 경기 침체기마다 급등, 회복기에 감소
'

#----------------------------------------------------------------------------------------------------------------------------------------------------------------

' 지도 시각화 '

' 단계 구분도
  : 지역별 통계치를 색깔의 차이로 표현한 지도 
'

# 패키지 로드
install.packages("ggiraphExtra") #단계구분도
library(ggiraphExtra)

# 미국 주별 범죄 데이터 로드
'USArrests Data 
  : R에 기본적으로 내장되어 었는 1970년도 미국 주별 강력 범죄율 정보를 담고 있는 데이터  
'
str(USArrests)
head(USArrests) ## 데이터프레임 형태가 아님
'tibble 패키지의 rownames_to_column()를 이용해서 행 이름을 state 변수로 바꿔 새로운 데이터프레임 생성'
library(tibble)
crime <- rownames_to_column(USArrests, var = 'state')
crime$state <- tolower(crime$state) # tolower() : 소문자 변환 함수
str(crime) ## state 변수 생성됨

'
단계 구분도를 만드려면 위도, 경도 정보가 있는 지도 데이터가 필요
 - maps 패키지의 미국 주별 위경도를 나타낸 state 데이터
  -> 해당 데이터를 ggplot2의 map_Data() 함수를 써서 DF형태로 가져오기   
'
# 미국 주 지도 데이터 로드
states_map <- map_data('state')
str(states_map)

# 단계 구분도 생성 - 지역별 범죄율
ggChoropleth(data = crime, # 지도에 표현할 데이터
             aes(fill = Murder, # 색깔로 표현할 변수
                 map_id = state), # 지역 기준 변수
             map = states_map) # 지도 데이터

# 인터렉티브 단계 구분도 생성
ggChoropleth(data = crime,
             aes(fill = Murder,
                 map_id = state),
             map = states_map,
             interactive = T) # 인터렉티브 여부
' 해석
  - 조지아 지역이 17.4로 범죄율이 가장 높은 지역임
  - 전체적으로 밑지방 주들이 범죄율이 높음
'

' 대한민국 시도별 인구, 결핵환자 수 단계 구분도 '
install.packages('stringi') #빠른 문자열 처리 기능
install.packages('devtools') #R패키지 개발을 더 쉽게 만들어주는 도구
devtools::install_github('cardiomoon/kormaps2014')
'kormaps2014 패키지 : 대한민국의 지역 통계 데이터와 지도 데이터를 사용하게 해줌'
library(kormaps2014)

# 데이터 로드
' korpop1 : 2015년 센서스 데이터(시도별)
  korpop2 : 2015년 센서스 데이터(시군구별)
  korpop3 : 2015년 센서스 데이터(읍면동별)
'
str(korpop1)

# 변수명 수정
library(dplyr)
korpop1 <- rename(korpop1,
                  pop = 총인구_명,
                  name = 행정구역별_읍면동)
str(korpop1)

# 대한민국 시도 지도 데이터 로드
str(kormap1)

# 단계 구분도 생성
ggChoropleth(data = korpop1, # 지도에 표현할 데이터
             aes(fill = pop, # 색깔로 표현할 변수
                 map_id = code, # 지역 기준 변수
                 tooltip = name), # 지도 위에 표시할 지역명
             map = kormap1, # 지도 데이터
             interactive = T) # 인터렉티브 여부
' 해석
  - 경기도에 12,479,061명으로 인구가 가장 많이 분포되어 있고, 그 뒤 서울이 9,904,312명으로 두번째로 많이 분포되어 있는 걸로 보아
  수도권에 인구가 몰려있는 것을 알 수 있음.'

# 결핵 환자 수 단계 구분도
'tbc 데이터 : 지역별 결핵 환자 수 데이터 -> NewPts(결핵 환자 수) 변수 사용'
str(tbc)
ggChoropleth(data = tbc,
             aes(fill = NewPts,
                 map_id = code,
                 tooltip = name),
             map = kormap1,
             interactive = T)
' 해석
  - 서울에 11,178명으로 결핵환자가 가장 많이 몰려있고 경기도에 4,843명으로 두번째로 많음.
  - 지역별 인구수와 거의 유사한 분포이지만 서울과 경기가 반대
'

