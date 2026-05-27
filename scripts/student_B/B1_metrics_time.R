# B1_metrics_time.R
# Расчёт временных метрик

library(tidyverse)
library(lubridate)

sales <- read.csv("data/sales_time.csv")
sales$date <- as.Date(sales$date)

# Метрика 1: Продажи по дням недели
sales_by_weekday <- sales %>%
  group_by(weekday_name, weekday_num, day_type) %>%
  summarise(
    revenue = sum(revenue),
    transactions = n_distinct(transaction_id),
    avg_check = revenue / transactions,
    .groups = "drop"
  ) %>%
  arrange(weekday_num)

saveRDS(sales_by_weekday, "outputs/sales_by_weekday.rds")

# Метрика 2: Будни vs Выходные
weekday_vs_weekend <- sales %>%
  group_by(day_type) %>%
  summarise(
    revenue = sum(revenue),
    transactions = n_distinct(transaction_id),
    avg_check = revenue / transactions,
    .groups = "drop"
  )

saveRDS(weekday_vs_weekend, "outputs/weekday_vs_weekend.rds")

# Метрика 3: Ежедневная динамика
daily_sales <- sales %>%
  group_by(date, day_type) %>%
  summarise(
    revenue = sum(revenue),
    transactions = n_distinct(transaction_id),
    .groups = "drop"
  )

saveRDS(daily_sales, "outputs/daily_sales.rds")

# Метрика 4: Лучший и худший день
best_day <- daily_sales %>% arrange(desc(revenue)) %>% head(1)
worst_day <- daily_sales %>% arrange(revenue) %>% head(1)

saveRDS(best_day, "outputs/best_day.rds")
saveRDS(worst_day, "outputs/worst_day.rds")

print("✅ Все метрики рассчитаны и сохранены!")
