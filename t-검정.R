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

