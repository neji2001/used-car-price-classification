Predicted = predict(GLM_a, newdata = Test, type = 'response')
Predicted_C = ifelse(Predicted > 0.5, 1, 0)
confusionMatrix(factor(Predicted_C, levels = c(1,0)),
                factor(Test$price_G, levels = c(1,0)))

rocplot=roc(price_G ~ Predicted,Test)
plot.roc(rocplot,legacy.axes = TRUE)
auc(rocplot)
# 성능 향상

exp(GLM_a$coefficients) # 회귀계수 해석 : 다른 변수 고정 각 변수 오즈비 증가
