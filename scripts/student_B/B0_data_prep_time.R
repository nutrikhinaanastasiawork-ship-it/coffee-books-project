# B3_insights_time.R
# Аналитические выводы по временным паттернам

library(tidyverse)

# Загрузка метрик
sales_by_weekday <- readRDS("outputs/sales_by_weekday.rds")
weekday_vs_weekend <- readRDS("outputs/weekday_vs_weekend.rds")
daily_sales <- readRDS("outputs/daily_sales.rds")
best_day <- readRDS("outputs/best_day.rds")
worst_day <- readRDS("outputs/worst_day.rds")

# ============================================================
# ФУНКЦИЯ ДЛЯ РАСЧЁТА ОТКЛОНЕНИЙ
# ============================================================

avg_revenue <- mean(sales_by_weekday$revenue)

# ============================================================
# ВЫВОД 1: Дни-лидеры и дни-аутсайдеры
# ============================================================

top_days <- sales_by_weekday %>%
  filter(revenue == max(revenue)) %>%
  pull(weekday_name)

bottom_days <- sales_by_weekday %>%
  filter(revenue == min(revenue)) %>%
  pull(weekday_name)

max_revenue <- max(sales_by_weekday$revenue)
min_revenue <- min(sales_by_weekday$revenue)
pct_diff <- round((max_revenue - min_revenue) / min_revenue * 100, 1)

insight_1 <- sprintf("
## 📊 ВЫВОД 1: Контраст между днями недели

**Самый прибыльный день:** %s (%.0f руб.)

**Самый слабый день:** %s (%.0f руб.)

**Разница:** %s показывает выручку на %.1f%% выше, чем %s

**Рекомендация:** 
- За 1-2 дня до %s увеличить закупки популярных товаров
- В %s запустить стимулирующие акции для роста продаж
", top_days, max_revenue, bottom_days, min_revenue, 
                     top_days, pct_diff, bottom_days, top_days, bottom_days)

# ============================================================
# ВЫВОД 2: Будни vs Выходные
# ============================================================

weekday_check <- weekday_vs_weekend %>% filter(day_type == "weekday") %>% pull(avg_check)
weekend_check <- weekday_vs_weekend %>% filter(day_type == "weekend") %>% pull(avg_check)
check_diff <- round(weekend_check - weekday_check, 0)
check_pct <- round((weekend_check - weekday_check) / weekday_check * 100, 1)

insight_2 <- sprintf("
## 📊 ВЫВОД 2: Выходные приносят более высокий чек

**Средний чек в будни:** %.0f руб.

**Средний чек в выходные:** %.0f руб.

**Разница:** +%.0f руб. (+%.1f%%) в выходные

**Рекомендация:**
- В выходные дни предлагать более дорогие позиции из меню
- Создать 'Weekend Special' — премиум-наборы
- Усилить промо выcокомаржинальных товаров в субботу и воскресенье
", weekday_check, weekend_check, check_diff, check_pct)

# ============================================================
# ВЫВОД 3: Лучший и худший день за весь период
# ============================================================

best_revenue <- best_day$revenue
best_date <- best_day$date
worst_revenue <- worst_day$revenue
worst_date <- worst_day$date

insight_3 <- sprintf("
## 📊 ВЫВОД 3: Рекордные и провальные дни

**Абсолютный рекорд:** %s — выручка %.0f руб.

**Худший день:** %s — выручка %.0f руб.

**Соотношение:** рекордный день принёс в %.1f раз больше, чем худший

**Рекомендация:**
- Проанализировать, что особенного было в рекордный день (акция? погода? событие?)
- Воспроизвести успешные факторы по возможности
", as.Date(best_date), best_revenue, as.Date(worst_date), worst_revenue, 
                     round(best_revenue / worst_revenue, 1))

# ============================================================
# ВЫВОД 4: Общий тренд и аномалии
# ============================================================

total_revenue <- sum(daily_sales$revenue)
avg_daily <- mean(daily_sales$revenue)
days_above_avg <- sum(daily_sales$revenue > avg_daily)
days_below_avg <- sum(daily_sales$revenue < avg_daily)

insight_4 <- sprintf("
## 📊 ВЫВОД 4: Стабильность и тренды

**Всего за период:** выручка %.0f руб. за %d дней

**Средняя дневная выручка:** %.0f руб.

**Дней выше среднего:** %d (%.0f%%)
**Дней ниже среднего:** %d (%.0f%%)

**Рекомендация:**
- Сгладить 'ямы' с помощью промо-активностей в слабые дни
- Планировать инвентаризацию и профилактику на historically слабые дни
", total_revenue, nrow(daily_sales), avg_daily, 
                     days_above_avg, days_above_avg / nrow(daily_sales) * 100,
                     days_below_avg, days_below_avg / nrow(daily_sales) * 100)

# ============================================================
# ОБЪЕДИНЕНИЕ ВСЕХ ВЫВОДОВ
# ============================================================

final_insights <- paste(
  "# 🔍 ВРЕМЕННОЙ АНАЛИЗ: ВЫВОДЫ И РЕКОМЕНДАЦИИ\n",
  paste(rep("=", 60), collapse = ""),
  insight_1,
  insight_2,
  insight_3,
  insight_4,
  paste(rep("=", 60), collapse = ""),
  "\n## 📌 ИТОГОВАЯ ТАБЛИЦА ПО ДНЯМ НЕДЕЛИ\n",
  sep = "\n\n"
)

# Добавим таблицу с данными
table_output <- sales_by_weekday %>%
  mutate(
    revenue = round(revenue, 0),
    avg_check = round(avg_check, 0),
    share_of_revenue = revenue / sum(revenue) * 100
  ) %>%
  select(День = weekday_name, 
         Выручка = revenue, 
         Чеков = transactions,
         Средний_чек = avg_check,
         Доля = share_of_revenue)

# Сохраняем выводы в файл
writeLines(final_insights, "outputs/insights_B.txt")

# И сохраняем таблицу отдельно
write.csv(table_output, "outputs/weekday_table_B.csv", row.names = FALSE)

print("✅ Выводы сохранены в outputs/insights_B.txt")
print("✅ Таблица сохранена в outputs/weekday_table_B.csv")

# Покажем таблицу
print(table_output)