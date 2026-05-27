# B0_data_prep_time.R - ПРОСТАЯ ВЕРСИЯ
library(tidyverse)

# Загрузка данных
sales <- read.csv("data/sales.csv")
sales$date <- as.Date(sales$date)

# Создание временных признаков (без русских букв!)
sales_time <- sales %>%
  mutate(
    weekday_num = wday(date),           # номер дня недели (1-7)
    weekday_name = case_when(           # название на русском вручную
      weekday_num == 1 ~ "вс",
      weekday_num == 2 ~ "пн",
      weekday_num == 3 ~ "вт",
      weekday_num == 4 ~ "ср",
      weekday_num == 5 ~ "чт",
      weekday_num == 6 ~ "пт",
      weekday_num == 7 ~ "сб"
    ),
    day_type = ifelse(weekday_num %in% c(1, 7), "weekend", "weekday"),
    month_num = month(date),
    year = year(date),
    week = week(date)
  )

# Сохранение
write.csv(sales_time, "data/sales_time.csv", row.names = FALSE)

# Проверка
print("✅ ГОТОВО! Файл data/sales_time.csv создан")
print(paste("Количество строк:", nrow(sales_time)))
head(sales_time, 3)