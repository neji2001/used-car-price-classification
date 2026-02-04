library(ggplot2) ; library(dplyr) ; library(car) ; library(C50) ; library(MASS)
library(randomForest) ; library(caret) ; library(Epi) ; library(pROC) ; library(PropCIs)

#----------------------------------------------------------------------------------------#

hyundai = read.csv("C:/Rscript/hyundi.csv")

#----------------------------------------------------------------------------------------#

### 데이터 전처리
hyundai$year = 2020-hyundai$year  # 등록연도 -> 연식

## 가격 이상치 제거
summary(hyundai$price)
hyundai[hyundai$price==max(hyundai$price),] # 주행거리 많은데 가격 높게 설정
hyundai=hyundai[-4248,]
summary(hyundai$price) # max값 변화

## 가격 등급 나누기(기준 : 3사분위)
cutline = quantile(hyundai$price, probs = c(0.75))
hyundai$price_G = ifelse(hyundai$price > cutline, 1, 0)
hyundai$price=NULL
hyundai$price_G=as.factor(hyundai$price_G)

## engineSize 결측
table(hyundai$model,hyundai$engineSize) # engine size 0인 값들 존재(최빈값 대체)
hyundai[hyundai$model==" I10" & hyundai$engineSize==0,8]=1
hyundai[hyundai$model==" I20" & hyundai$engineSize==0,8]=1.2
hyundai[hyundai$model==" I30" & hyundai$engineSize==0,8]=1.6
hyundai[hyundai$model==" I40" & hyundai$engineSize==0,8]=1.7
hyundai[hyundai$model==" I800" & hyundai$engineSize==0,8]=2.5
hyundai[hyundai$model==" IX20" & hyundai$engineSize==0,8]=1.4
hyundai[hyundai$model==" IX35" & hyundai$engineSize==0,8]=1.7
hyundai[hyundai$model==" Tucson" & hyundai$engineSize==0,8]=1.6
table(hyundai$model,hyundai$engineSize) # 최빈값 대체 후

## transmission (Other값 제거)
table(hyundai$model,hyundai$transmission)
hyundai = hyundai %>% filter(transmission != "Other")

## fuelType (Other값 Hybrid로 대체)
table(hyundai$model,hyundai$fuelType)
hyundai[hyundai$fuelType=="Other",5]="Hybrid"

## mpg
summary(hyundai$mpg) # min, max값 특이값 존재 (min값 : 리터당 0.4km / max값 : 리터당 109km)
hyundai[hyundai$mpg==min(hyundai$mpg),] # Ioniq 1.1 특이값
hyundai[hyundai$mpg==max(hyundai$mpg),] # Ioniq 256.8 특이값
hyundai[hyundai$mpg==1.1,7]=78.5 # Ioniq mpg 이상치 최빈값으로 대체
hyundai[hyundai$mpg==256.8,7]=78.5 # Ioniq mpg 이상치 최빈값으로 대체

ggplot(hyundai) +
  geom_histogram(aes(x=mpg,fill=price_G),colour=TRUE) +
  theme_bw()
## 연비가 좋지 않은데 비싼 중고차 -> SUV 가능성 높음 (차량 무게)

## tax
# 오른쪽 끝에 존재하는 최대값 특이값처럼 보임
ggplot(hyundai) +
  geom_boxplot(aes(x=tax), outlier.size=4, outlier.color = "red") +
  theme_bw()

hyundai[hyundai$tax==555,6]=325 # 가장 이상적이라 생각
