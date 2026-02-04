library(caret)
library(pROC)

evaluate_model <- function(model, Test) {

  probs <- predict(model, newdata = Test, type = "response")
  preds <- ifelse(probs > 0.5, 1, 0)

  cm <- confusionMatrix(factor(preds, levels = c(1,0)),
                        factor(Test$price_G, levels = c(1,0)))

  roc_obj <- roc(Test$price_G, probs)

  print(cm)
  print(auc(roc_obj))
}
