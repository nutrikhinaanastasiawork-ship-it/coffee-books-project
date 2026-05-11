# ============================================
# A2_plots_products.R - Графики товарного анализа
# СТУДЕНТ А
# ============================================

library(tidyverse)

# Загрузка метрик
metrics_A <- readRDS("outputs/metrics_A.rds")
sales <- read.csv("data/sales.csv")

# Создаём папку для графиков
if(!dir.exists("outputs/plots_A")) dir.create("outputs/plots_A", recursive = TRUE)

# График 1: Выручка по категориям
p1 <- ggplot(metrics_A$revenue_by_category, 
             aes(x = reorder(category, -revenue), y = revenue, fill = category)) +
  geom_col() +
  geom_text(aes(label = paste0(round(share, 1), "%")), vjust = -0.5, size = 6) +
  labs(title = "Выручка по категориям товаров",
       x = "Категория", y = "Выручка (руб.)") +
  theme_minimal() +
  theme(legend.position = "none")
ggsave("outputs/plots_A/revenue_by_category.png", p1, width = 8, height = 5)

# График 2: Топ-5 товаров по выручке
p2 <- ggplot(metrics_A$top_products, 
             aes(x = reorder(product, revenue), y = revenue, fill = category)) +
  geom_col() +
  coord_flip() +
  labs(title = "Топ-5 товаров по выручке",
       x = "", y = "Выручка (руб.)") +
  theme_minimal()
ggsave("outputs/plots_A/top_products.png", p2, width = 8, height = 5)

# График 3: ABC-анализ
p3 <- ggplot(metrics_A$abc, 
             aes(x = reorder(product, -revenue), y = revenue, fill = class)) +
  geom_col() +
  coord_flip() +
  labs(title = "ABC-анализ товаров",
       subtitle = "A — 70% выручки, B — 20%, C — 10%",
       x = "", y = "Выручка (руб.)") +
  theme_minimal()
ggsave("outputs/plots_A/abc_analysis.png", p3, width = 10, height = 6)

# График 4: Цены товаров
price_data <- sales %>%
  group_by(product, category) %>%
  summarise(price = first(price_per_unit), .groups = "drop")

p4 <- ggplot(price_data, 
             aes(x = reorder(product, price), y = price, fill = category)) +
  geom_col() +
  coord_flip() +
  labs(title = "Цены на товары",
       x = "", y = "Цена (руб.)") +
  theme_minimal()
ggsave("outputs/plots_A/prices.png", p4, width = 10, height = 6)

cat("✅ 4 графика сохранены в outputs/plots_A/\n")
cat("   - revenue_by_category.png\n")
cat("   - top_products.png\n")
cat("   - abc_analysis.png\n")
cat("   - prices.png\n")