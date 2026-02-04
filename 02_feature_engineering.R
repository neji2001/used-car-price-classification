## SUV 분류
# SUV : Kona, Santa Fe, Tucson, Terracan

hyundai = hyundai %>%
  mutate(SUV = case_when(model == " Santa Fe" ~ "Yes",
                         model == " Terracan" ~ "Yes",
                         model == " Kona" ~ "Yes",
                         model == " Tucson" ~ "Yes",
                         TRUE ~ "No"),
         SUV = factor(SUV, levels = c("No", "Yes")))

suv=xtabs(~SUV + price_G,data=hyundai)
suv=suv[c(2,1),c(2,1)] ; suv

chisq.test(suv) # 독립성 검정결과 귀무가설 기각 = 중고차 등급과 suv여부는 관련있음
chisq.test(suv)$stdres # 표준화잔차

## pi_1 - pi_0
prop.test(c(813,401),c(1061+813,2582+401),correct = FALSE) # wald 신뢰구간
# wald 신뢰구간 0포함 X -> suv이냐 아니냐 두 비율의 차이 존재
diffscoreci(813,1061+813,401,2582+401,conf.level = 0.95) # score 신뢰구간

riskscoreci(813,1061+813,401,2582+401,conf.level = 0.95) 
# suv가 suv가 아닌 그룹보다 1에 속할 확률이 4.49배와 5.44배 사이에 존재

OR = 813*2582/(401*1061) # suv가 그룹1에 속할 오즈는 suv가 아닐 그룹이 그룹1에 속할 오즈보다 4.947배 높다.
se = sqrt(sum(1/suv))
exp(c(log(OR)-1.96*se,log(OR)+1.96*se)) # wald 신뢰구간 (오즈비)
orscoreci(813,813+1061,401,401+2582,conf.level = 0.95) # score 신뢰구간 (오즈비)

#----------------------------------------------------------------------------------------#

## 차량크기에 따른 분류(소형 + 경형 / 중형 + 준중형 / 준대형 + 대형)
# 경차 + 소형 : I10, I20, Ix20, Accent, Getz, Amica, Kona
# 준중형 + 중형 : I30, I40, Ix35, Ioniq, Veloster, Santa Fe, Tucson
# 준대형 + 대형 : Terracan, I800

hyundai = hyundai %>%
  mutate(Size = case_when(model == " I800" ~ "Large",
                          model == " Terracan" ~ "Large",
                         model == " Tucson" ~ "Medium",
                         model == " Santa Fe" ~ "Medium",
                         model == " I40" ~ "Medium",
                         model == " IX35" ~ "Medium",
                         model == " I30" ~ "Medium",
                         model == " Ioniq" ~ "Medium",
                         model == " veloster" ~ "Medium",
                         TRUE ~ "Small"),
         Size = factor(Size, levels = c("Small", "Medium","Large")))

ggplot(hyundai) +
  geom_point(aes(x=Size,y=engineSize), size=5, pch=11, colour="red") +
  theme_bw()

## size의 경우 엔진크기와 연관성이 커서 사용 X
hyundai$Size=NULL
