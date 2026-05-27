# B0_data_prep_time.R
# Подготовка временных признаков

library(tidyverse)
library(lubridate)

# Загрузка данных
sales <- read.csv("data/sales.csv")
sales$date <- as.Date(sales$date)

# Создание временных признаков
sales_time <- sales %>%
  mutate(
    weekday_name = wday(date, label = TRUE, locale = "ru_RU"),
    weekday_num = wday(date),
    day_type = ifelse(weekday_name %in% c("сб", "вс"), "weekend", "weekday"),
    month = month(date, label = TRUE, locale = "ru_RU"),
    year = year(date),
    week = week(date)
  )

# Сохранение
write.csv(sales_time, "data/sales_time.csv", row.names = FALSE)

# Проверка
print("✅ data/sales_time.csv создан!")
print(paste("Строк:", nrow(sales_time)))
print(paste("Столбцов:", ncol(sales_time)))
print("Первые строки:")
head(sales_time)