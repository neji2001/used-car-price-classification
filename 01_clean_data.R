library(dplyr)

clean_data <- function(df) {

  # 연식 변환
  df$year <- 2020 - df$year

  # transmission 정리
  df <- df %>% filter(transmission != "Other")

  # fuelType 정리
  df$fuelType[df$fuelType == "Other"] <- "Hybrid"

  # mpg 이상치 완화
  df$mpg <- ifelse(df$mpg < 5 | df$mpg > 150,
                   median(df$mpg, na.rm = TRUE),
                   df$mpg)

  # tax 이상치 완화
  df$tax <- ifelse(df$tax > quantile(df$tax, 0.99),
                   median(df$tax, na.rm = TRUE),
                   df$tax)

  return(df)
}
