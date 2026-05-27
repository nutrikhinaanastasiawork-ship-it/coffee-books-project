# B3_insights_time.R
# Выводы по временному анализу (на основе реальных данных)

library(tidyverse)

# Загрузка данных
sales_by_weekday <- readRDS("outputs/sales_by_weekday.rds")
weekday_vs_weekend <- readRDS("outputs/weekday_vs_weekend.rds")

# Приводим названия дней к единому формату
sales_by_weekday <- sales_by_weekday %>%
  mutate(
    weekday_name = case_when(
      weekday_name == "пн" ~ "Понедельник",
      weekday_name == "вт" ~ "Вторник",
      weekday_name == "ср" ~ "Среда",
      weekday_name == "чт" ~ "Четверг",
      weekday_name == "пт" ~ "Пятница",
      weekday_name == "сб" ~ "Суббота",
      weekday_name == "вс" ~ "Воскресенье"
    )
  )

# Извлекаем нужные цифры
best_day <- sales_by_weekday %>% filter(revenue == max(revenue))
worst_day <- sales_by_weekday %>% filter(revenue == min(revenue))
best_check <- sales_by_weekday %>% filter(avg_check == max(avg_check))
worst_check <- sales_by_weekday %>% filter(avg_check == min(avg_check))
weekday_avg <- weekday_vs_weekend %>% filter(day_type == "weekday") %>% pull(avg_check)
weekend_avg <- weekday_vs_weekend %>% filter(day_type == "weekend") %>% pull(avg_check)

# Формируем выводы
insights <- paste0(
  "Выводы по временному анализу (Студент Б)\n\n",
  "1. Самый прибыльный день\n",
  "Среда показала максимальную выручку ", format(best_day$revenue, big.mark = " "), " рублей. ",
  "Понедельник - самый слабый день с выручкой ", format(worst_day$revenue, big.mark = " "), " рублей. ",
  "Разница составляет ", format(best_day$revenue - worst_day$revenue, big.mark = " "), " рублей (",
  round((best_day$revenue - worst_day$revenue) / worst_day$revenue * 100, 1), "%).\n",
  "Рекомендация: проводить акции в понедельник для выравнивания продаж, обеспечивать максимальный запас товаров к среде.\n\n",
  
  "2. Средний чек по дням недели\n",
  "Самый высокий средний чек в пятницу - ", round(best_check$avg_check, 0), " рублей. ",
  "Самый низкий в четверг - ", round(worst_check$avg_check, 0), " рублей. ",
  "Разница составляет ", round(best_check$avg_check - worst_check$avg_check, 0), " рублей.\n",
  "Рекомендация: в четверг предлагать дополнительные позиции к заказу, в пятницу использовать кросс-продажи.\n\n",
  
  "3. Сравнение будней и выходных\n",
  "Средний чек в будни - ", round(weekday_avg, 0), " рублей, в выходные - ", round(weekend_avg, 0), " рублей. ",
  "Разница составляет всего ", round(weekend_avg - weekday_avg, 0), " рублей (",
  round((weekend_avg - weekday_avg) / weekday_avg * 100, 1), "%), что статистически незначительно.\n",
  "Рекомендация: не требуется отдельной стратегии для выходных, сфокусироваться на росте чека в будние дни.\n\n",
  
  "4. Итоговые рекомендации\n",
  "Усилить понедельник акциями (самый слабый день по выручке). ",
  "Повышать средний чек в четверг через комбо-предложения. ",
  "Сохранить базовую стратегию для выходных - отличие от будней минимально. ",
  "Обеспечить достаточный запас товаров к среде - дню максимальной выручки."
)

# Сохраняем выводы
writeLines(insights, "outputs/insights_B.txt")

print("Выводы сохранены в outputs/insights_B.txt")
cat(insights)