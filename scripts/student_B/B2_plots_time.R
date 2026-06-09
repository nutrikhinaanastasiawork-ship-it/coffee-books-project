# ============================================
# B2_plots_time.R - Графики временного анализа
# СТУДЕНТ Б
# ============================================

library(tidyverse)
library(ggplot2)
library(scales)
library(lubridate)

# Загрузка метрик
sales_by_weekday <- readRDS("outputs/sales_by_weekday.rds")
weekday_vs_weekend <- readRDS("outputs/weekday_vs_weekend.rds")
daily_sales <- readRDS("outputs/daily_sales.rds")

# Градиент из 7 цветов: красный -> оранжевый -> жёлтый
gradient_colors <- c(
  "#D32F2F",  # Понедельник
  "#E53935",  # Вторник
  "#F57C00",  # Среда
  "#FB8C00",  # Четверг
  "#FFC107",  # Пятница
  "#FFD54F",  # Суббота
  "#FFEB3B"   # Воскресенье
)

# Создаём папку для графиков
if(!dir.exists("outputs/plots_B")) dir.create("outputs/plots_B", recursive = TRUE)

# ============================================
# ПРЕОБРАЗОВАНИЕ ДАННЫХ (русские названия)
# ============================================

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
    ),
    weekday_name = factor(weekday_name, 
                          levels = c("Понедельник", "Вторник", "Среда", 
                                     "Четверг", "Пятница", "Суббота", "Воскресенье"))
  )

weekday_vs_weekend <- weekday_vs_weekend %>%
  mutate(day_type = case_when(
    day_type == "weekday" ~ "Будни",
    day_type == "weekend" ~ "Выходные"
  ))

# ============================================
# ГРАФИК 1: Выручка по дням недели (линия)
# ============================================
p1 <- ggplot(sales_by_weekday, aes(x = weekday_name, y = revenue, group = 1)) +
  geom_line(size = 1.5, color = gradient_colors[3]) +
  geom_point(size = 4, aes(color = weekday_name)) +
  scale_color_manual(values = setNames(gradient_colors, levels(sales_by_weekday$weekday_name)), name = NULL) +
  geom_text(aes(label = label_number(big.mark = " ", decimal.mark = ",")(revenue)), 
            vjust = -1.2, size = 4) +
  scale_y_continuous(
    labels = label_number(big.mark = " ", decimal.mark = ","),
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(title = "Выручка по дням недели",
       x = "", y = "Выручка (руб.)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        legend.position = "none")
ggsave("outputs/plots_B/revenue_by_weekday.png", p1, width = 10, height = 6, dpi = 150)

# ============================================
# ГРАФИК 2: Средний чек по дням недели (столбцы с градиентом)
# ============================================
p2 <- ggplot(sales_by_weekday, aes(x = weekday_name, y = avg_check, fill = weekday_name)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = round(avg_check, 0)), vjust = -0.5, size = 4) +
  scale_fill_manual(values = setNames(gradient_colors, levels(sales_by_weekday$weekday_name)), name = NULL) +
  scale_y_continuous(
    labels = label_number(big.mark = " ", decimal.mark = ","),
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(title = "Средний чек по дням недели",
       x = "", y = "Средний чек (руб.)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        legend.position = "none")
ggsave("outputs/plots_B/avg_check_by_weekday.png", p2, width = 10, height = 6, dpi = 150)

# ============================================
# ГРАФИК 3: Будни vs Выходные
# ============================================
p3 <- ggplot(weekday_vs_weekend, aes(x = day_type, y = avg_check, fill = day_type)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = round(avg_check, 0)), vjust = -0.5, size = 5) +
  scale_fill_manual(values = c(gradient_colors[1], gradient_colors[7]), name = NULL) +
  scale_y_continuous(
    labels = label_number(big.mark = " ", decimal.mark = ","),
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(title = "Средний чек: будни vs выходные",
       x = "", y = "Средний чек (руб.)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(face = "bold"),
        legend.position = "none")
ggsave("outputs/plots_B/weekend_vs_weekday.png", p3, width = 8, height = 6, dpi = 150)

# ============================================
# ГРАФИК 4: Динамика продаж
# ============================================
daily_sales <- daily_sales %>%
  mutate(date = as.Date(date))

p4 <- ggplot(daily_sales, aes(x = date, y = revenue)) +
  geom_line(size = 0.8, color = gradient_colors[1], alpha = 0.8) +
  geom_smooth(method = "loess", se = TRUE, color = gradient_colors[3], fill = gradient_colors[5], alpha = 0.2) +
  scale_y_continuous(
    labels = label_number(big.mark = " ", decimal.mark = ",")
  ) +
  scale_x_date(date_breaks = "1 week", date_labels = "%d.%m") +
  labs(title = "Динамика ежедневной выручки",
       x = "Дата", y = "Выручка (руб.)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
ggsave("outputs/plots_B/daily_revenue_trend.png", p4, width = 12, height = 6, dpi = 150)

cat("✅ 4 графика сохранены в outputs/plots_B/\n")
cat("   - revenue_by_weekday.png\n")
cat("   - avg_check_by_weekday.png\n")
cat("   - weekend_vs_weekday.png\n")
cat("   - daily_revenue_trend.png\n")