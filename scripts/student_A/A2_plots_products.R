# ============================================
# A2_plots_products.R - Графики товарного анализа
# СТУДЕНТ А
# ============================================

library(tidyverse)
library(scales)

# Загрузка метрик
metrics_A <- readRDS("outputs/metrics_A.rds")
sales <- read.csv("data/sales.csv")

# Создаём папку
if(!dir.exists("outputs/plots_A")) dir.create("outputs/plots_A", recursive = TRUE)

# ============================================================
# График 1: Выручка по категориям (текст ВНУТРИ)
# ============================================================

p1 <- ggplot(metrics_A$revenue_by_category, 
             aes(x = reorder(category, -revenue), y = revenue, fill = category)) +
  geom_col() +
  geom_text(aes(label = paste0(round(share, 1), "%")), 
            vjust = -0.5,          # ← над столбцом, но с отступом
            size = 5,
            fontface = "bold") +
  scale_fill_manual(values = c("Кофе" = "#F39C12",
                               "Выпечка" = "#FFD700",
                               "Книги" = "#D35400")) +
  labs(title = "Выручка по категориям товаров",
       x = "Категория", y = "Выручка (руб.)") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 14),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12)) +
  scale_y_continuous(
    labels = scales::comma,
    limits = c(0, max(metrics_A$revenue_by_category$revenue) * 1.2),  # ← 20% запаса сверху
    expand = c(0, 0)  # ← убирает лишние отступы снизу
  )
ggsave("outputs/plots_A/revenue_by_category.png", p1, width = 8, height = 6, dpi = 150)

# ============================================================
# График 2: Топ-5 товаров (горизонтальный)
# ============================================================
p2 <- ggplot(metrics_A$top_products, 
             aes(x = reorder(product, revenue), y = revenue, fill = category)) +
  geom_col() +
  geom_text(aes(label = comma(revenue)), 
            hjust = -0.1,           # ← текст справа от столбца
            size = 4) +
  coord_flip() +
  scale_fill_manual(values = c(
    "Кофе" = "#D35400",      # тёмно-оранжевый
    "Выпечка" = "#E69A2C",   # золотисто-жёлтый
    "Книги" = "#D35400"      # голубой
  )) +
  labs(title = "Топ-5 товаров по выручке",
       x = "", y = "Выручка (руб.)") +
  theme_minimal() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.1)))  # ← место для подписей
ggsave("outputs/plots_A/top_products.png", p2, width = 10, height = 6, dpi = 150)

# ============================================================
# График 3: ABC-анализ (горизонтальный)
# ============================================================
p3 <- ggplot(metrics_A$abc, 
             aes(x = reorder(product, -revenue), y = revenue, fill = class)) +
  geom_col() +
  geom_text(aes(label = comma(revenue)), 
            hjust = -0.1,
            size = 3.5) +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "A" = "#D35400",   # тёмно-оранжевый (лидеры)
      "B" = "#F1C40F",   # жёлтый (середняки)
      "C" = "#E69A2C"    # голубой (аутсайдеры)
    ),
    name = "Класс(ABC)"   # ← название легенды на русском
  ) +
  labs(title = "ABC-анализ товаров",
       subtitle = "A — 70% выручки, B — 20%, C — 10%",
       x = "", y = "Выручка (руб.)") +
  theme_minimal() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.1)))
ggsave("outputs/plots_A/abc_analysis.png", p3, width = 10, height = 7, dpi = 150)

# ============================================================
# График 4: Цены товаров (горизонтальный)
# ============================================================
price_data <- sales %>%
  group_by(product, category) %>%
  summarise(price = first(price_per_unit), .groups = "drop")

p4 <- ggplot(price_data, 
             aes(x = reorder(product, price), y = price, fill = category)) +
  geom_col() +
  geom_text(aes(label = comma(price)), 
            hjust = -0.1,
            size = 4) +
  coord_flip() +
  scale_fill_manual(values = c(
    "Кофе" = "#D35400",      # тёмно-оранжевый
    "Выпечка" = "#F1C40F",   # золотисто-жёлтый
    "Книги" = "#A0522D"      # голубой
  ),
  name = "Категория") +
  labs(title = "Цены на товары",
       x = "", y = "Цена (руб.)") +
  theme_minimal() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.1)))
ggsave("outputs/plots_A/prices.png", p4, width = 10, height = 7, dpi = 150)

cat("✅ 4 графика сохранены в outputs/plots_A/\n")
<<<<<<< HEAD
=======
cat("   - revenue_by_category.png\n")
cat("   - top_products.png\n")
cat("   - abc_analysis.png\n")
cat("   - prices.png\n")

>>>>>>> f868217cb177e65f5c79bb233afa6021e4829b77
