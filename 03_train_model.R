library(caret)
library(car)
library(MASS)

train_model <- function(df) {

  set.seed(20221205)
  train_idx <- createDataPartition(df$price_G, p = 0.7, list = FALSE)

  Train <- df[train_idx, ]
  Test <- df[-train_idx, ]

  model <- glm(price_G ~ year + transmission + mileage + mpg + tax + engineSize + SUV,
               family = binomial(link = "logit"),
               data = Train)

  return(list(model = model, Train = Train, Test = Test))
}
