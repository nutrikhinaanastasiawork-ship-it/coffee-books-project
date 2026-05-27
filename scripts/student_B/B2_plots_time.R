# B2_plots_time.R - УПРОЩЁННАЯ ВЕРСИЯ
library(tidyverse)
library(ggplot2)
library(scales)

# Загрузка метрик
sales_by_weekday <- readRDS("outputs/sales_by_weekday.rds")
weekday_vs_weekend <- readRDS("outputs/weekday_vs_weekend.rds")
daily_sales <- readRDS("outputs/daily_sales.rds")

# ============================================================
# Упорядочиваем дни недели
# ============================================================

sales_by_weekday$weekday_name <- factor(
  sales_by_weekday$weekday_name,
  levels = c("пн", "вт", "ср", "чт", "пт", "сб", "вс"),
  ordered = TRUE
)

# ============================================================
# ГРАФИК 1: Выручка по дням недели
# ============================================================

p1 <- ggplot(sales_by_weekday, aes(x = weekday_name, y = revenue, group = 1)) +
  geom_line(size = 1.5, color = "steelblue") +
  geom_point(size = 4, color = "steelblue") +
  geom_text(aes(label = round(revenue, 0)), vjust = -1, size = 4) +
  labs(title = "Выручка по дням недели", x = "День недели", y = "Выручка (руб.)") +
  theme_minimal()

ggsave("outputs/plots_B/revenue_by_weekday.png", p1, width = 10, height = 6)
print("✅ График 1 сохранён")

# ============================================================
# ГРАФИК 2: Средний чек по дням недели
# ============================================================

p2 <- ggplot(sales_by_weekday, aes(x = weekday_name, y = avg_check, fill = day_type)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = round(avg_check, 0)), vjust = -0.5, size = 4) +
  labs(title = "Средний чек по дням недели", x = "День недели", y = "Средний чек (руб.)") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("weekday" = "#a6cee3", "weekend" = "#1f78b4"))

ggsave("outputs/plots_B/avg_check_by_weekday.png", p2, width = 10, height = 6)
print("✅ График 2 сохранён")

# ============================================================
# ГРАФИК 3: Будни vs Выходные (средний чек)
# ============================================================

p3 <- ggplot(weekday_vs_weekend, aes(x = day_type, y = avg_check, fill = day_type)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = round(avg_check, 0)), vjust = -0.5, size = 6) +
  labs(title = "Средний чек: будни vs выходные", x = "", y = "Средний чек (руб.)") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("weekday" = "#a6cee3", "weekend" = "#1f78b4"))

ggsave("outputs/plots_B/weekend_vs_weekday.png", p3, width = 8, height = 6)
print("✅ График 3 сохранён")

# ============================================================
# ГРАФИК 4: Динамика продаж
# ============================================================

p4 <- ggplot(daily_sales, aes(x = as.Date(date), y = revenue)) +
  geom_line(color = "darkgreen", size = 1) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "Динамика ежедневной выручки", x = "Дата", y = "Выручка (руб.)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("outputs/plots_B/daily_revenue_trend.png", p4, width = 12, height = 6)
print("✅ График 4 сохранён")

# ============================================================
# ФИНАЛЬНАЯ ПРОВЕРКА
# ============================================================

print("=" %>% rep(50) %>% paste(collapse = ""))
print("🎉 ВСЕ 4 ГРАФИКА СОЗДАНЫ!")
print("Проверьте папку: outputs/plots_B/")
list.files("outputs/plots_B/")