# create_metrics_B.R
# Создаёт недостающие файлы студента Б

library(tidyverse)
library(lubridate)

# Загружаем данные
sales <- read.csv("data/sales.csv")
sales$date <- as.Date(sales$date)

# Добавляем дни недели (русские названия)
sales <- sales %>%
  mutate(
    weekday_num = wday(date),
    weekday = case_when(
      weekday_num == 1 ~ "вс",
      weekday_num == 2 ~ "пн",
      weekday_num == 3 ~ "вт",
      weekday_num == 4 ~ "ср",
      weekday_num == 5 ~ "чт",
      weekday_num == 6 ~ "пт",
      weekday_num == 7 ~ "сб"
    ),
    day_type = ifelse(weekday %in% c("сб", "вс"), "weekend", "weekday")
  )

# Метрики по дням недели
sales_by_weekday <- sales %>%
  group_by(weekday, weekday_num, day_type) %>%
  summarise(
    revenue = sum(revenue),
    transactions = n_distinct(transaction_id),
    avg_check = revenue / transactions,
    .groups = "drop"
  ) %>%
  arrange(weekday_num)

# Будни vs выходные
weekday_vs_weekend <- sales %>%
  group_by(day_type) %>%
  summarise(
    revenue = sum(revenue),
    transactions = n_distinct(transaction_id),
    avg_check = revenue / transactions,
    .groups = "drop"
  )

# Создаём список metrics_B
metrics_B <- list(
  sales_by_weekday = sales_by_weekday,
  weekday_vs_weekend = weekday_vs_weekend
)

# Создаём папку если её нет
if(!dir.exists("outputs")) dir.create("outputs")

# Сохраняем файл
saveRDS(metrics_B, "outputs/metrics_B.rds")

cat("✅ Файл outputs/metrics_B.rds создан!\n")
cat("   Размер:", file.size("outputs/metrics_B.rds"), "байт\n")
