library(readr)
source("01_clean_data.R")
source("02_feature_engineering.R")
source("03_train_model.R")
source("04_evaluate_model.R")

df <- read_csv("sample_data.csv")

df <- clean_data(df)
df <- make_features(df)

model_obj <- train_model(df)
evaluate_model(model_obj$model, model_obj$Test)
