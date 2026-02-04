set.seed(20221205)
SL = sample(1:nrow(hyundai),nrow(hyundai)*0.7)
Train = hyundai[SL,]
Test = hyundai[-SL,]

#-------------------------------------------------------------------------------------------#
## 연속형 자료 잔차 이탈도 카이제곱에 수렴 X (다른 방식으로 적합도 검정 필요)
## 상관관계 확인

## 로지스틱 회귀분석
GLM = glm(formula = price_G ~ ., family = binomial(link = "logit"), data = Train)
summary(GLM)
## (1 not defined because of singularities)
## model을 이용해서 SUV를 만들다보니 위와 같은 문제 존재한다고 판단

# 전(SUV 제거)
GLM_b = glm(formula = price_G ~ model + year + transmission + mileage + 
            fuelType + mpg + tax + engineSize , family = binomial(link = "logit"), 
          data = Train)
summary(GLM_b)
vif(GLM_b) # model의 다중공선성문제 발생 -> model 제거

GLM_b = glm(formula = price_G ~  year + transmission + mileage + 
              fuelType + mpg + tax + engineSize , family = binomial(link = "logit"), data = Train)
summary(GLM_b)
vif(GLM_b) # fuelType 제거

GLM_b = glm(formula = price_G ~  year + transmission + mileage + 
               mpg + tax + engineSize , family = binomial(link = "logit"), data = Train)
summary(GLM_b)
vif(GLM_b) # 다중공선성 문제 해결

stepAIC(GLM_b)  # AIC 결과 (mpg 선택 X)
GLM_b = glm(formula = price_G ~ year + transmission + mileage + tax + 
              engineSize, family = binomial(link = "logit"), data = Train)
summary(GLM_b)
Anova(GLM_b)

Predicted = predict(GLM_b, newdata = Test, type = 'response')
Predicted_C = ifelse(Predicted > 0.5, 1, 0)
confusionMatrix(factor(Predicted_C, levels = c(1,0)),
                factor(Test$price_G, levels = c(1,0)))

rocplot=roc(price_G ~ Predicted,Test)
plot.roc(rocplot,legacy.axes = TRUE)
auc(rocplot)

# 후 (model 제거)
GLM_a = glm(formula = price_G ~ year + transmission + mileage + 
      fuelType + mpg + tax + engineSize + SUV, family = binomial(link = "logit"), 
    data = Train)
summary(GLM_a)
vif(GLM_a) # fuelType 제거

GLM_a = glm(formula = price_G ~ year + transmission + mileage + 
               mpg + tax + engineSize + SUV, family = binomial(link = "logit"), data = Train)
summary(GLM_a)
vif(GLM_a) # 다중공선성 문제 해결

stepAIC(GLM_a) # 모든 변수 선택
