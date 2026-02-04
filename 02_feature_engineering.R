library(dplyr)

make_features <- function(df) {

  # 가격 등급 생성
  cutline <- quantile(df$price, 0.75)
  df$price_G <- as.factor(ifelse(df$price > cutline, 1, 0))
  df$price <- NULL

  # SUV 변수 생성
  df <- df %>%
    mutate(SUV = case_when(
      model == " Santa Fe" ~ "Yes",
      model == " Terracan" ~ "Yes",
      model == " Kona" ~ "Yes",
      model == " Tucson" ~ "Yes",
      TRUE ~ "No"
    ),
    SUV = factor(SUV, levels = c("No", "Yes")))

  return(df)
}
