# ============================================
# A2_plots_products.R - Графики товарного анализа
# СТУДЕНТ А
# ============================================

library(tidyverse)
library(scales)

# Загрузка метрик
metrics_A <- readRDS("outputs/metrics_A.rds")
sales <- read.csv("data/sales.csv")

# Цветовая палитра: красный -> оранжевый -> жёлтый
my_colors <- c("#D32F2F", "#F57C00", "#FFC107", "#FFEB3B")

# Создаём папку для графиков
if(!dir.exists("outputs/plots_A")) dir.create("outputs/plots_A", recursive = TRUE)

# ============================================
# ГРАФИК 1: Выручка по категориям
# ============================================
revenue_by_category_ru <- metrics_A$revenue_by_category %>%
  mutate(category = case_when(
    category == "Coffee" ~ "Кофе",
    category == "Pastry" ~ "Выпечка",
    category == "Books" ~ "Книги"
  ))

p1 <- ggplot(revenue_by_category_ru, 
             aes(x = reorder(category, -revenue), y = revenue, fill = category)) +
  geom_col() +
  geom_text(aes(label = paste0(round(share, 1), "%")), 
            vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_manual(values = my_colors, name = "Категория") +
  scale_y_continuous(
    labels = label_number(big.mark = " ", decimal.mark = ","),
    limits = c(0, max(revenue_by_category_ru$revenue) * 1.2),
    expand = c(0, 0)
  ) +
  labs(title = "Выручка по категориям товаров",
       x = "Категория", y = "Выручка (руб.)") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 0, size = 11))
ggsave("outputs/plots_A/revenue_by_category.png", p1, width = 8, height = 6, dpi = 150)

# ============================================
# ГРАФИК 2: Топ-5 товаров по выручке
# ============================================
top_products_ru <- metrics_A$top_products %>%
  mutate(category = case_when(
    category == "Coffee" ~ "Кофе",
    category == "Pastry" ~ "Выпечка",
    category == "Books" ~ "Книги"
  ))

p2 <- ggplot(top_products_ru, 
             aes(x = reorder(product, revenue), y = revenue, fill = category)) +
  geom_col() +
  geom_text(aes(label = label_number(big.mark = " ", decimal.mark = ",")(revenue)), 
            hjust = -0.1, size = 4) +
  coord_flip() +
  scale_fill_manual(values = my_colors, name = "Категория") +
  scale_y_continuous(
    labels = label_number(big.mark = " ", decimal.mark = ","),
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(title = "Топ-5 товаров по выручке",
       x = "", y = "Выручка (руб.)") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"))
ggsave("outputs/plots_A/top_products.png", p2, width = 9, height = 5, dpi = 150)

# ============================================
# ГРАФИК 3: ABC-анализ
# ============================================
p3 <- ggplot(metrics_A$abc, 
             aes(x = reorder(product, -revenue), y = revenue, fill = class)) +
  geom_col() +
  geom_text(aes(label = label_number(big.mark = " ", decimal.mark = ",")(revenue)), 
            hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_fill_manual(values = c("#D32F2F", "#F57C00", "#FFC107"),
                    name = "Класс") +
  scale_y_continuous(
    labels = label_number(big.mark = " ", decimal.mark = ","),
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(title = "ABC-анализ товаров",
       subtitle = "A — 70% выручки, B — 20%, C — 10%",
       x = "", y = "Выручка (руб.)") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 10, color = "gray40"),
        axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"))
ggsave("outputs/plots_A/abc_analysis.png", p3, width = 11, height = 6, dpi = 150)

# ============================================
# ГРАФИК 4: Цены на товары
# ============================================
price_data <- sales %>%
  group_by(product, category) %>%
  summarise(price = first(price_per_unit), .groups = "drop") %>%
  mutate(category = case_when(
    category == "Coffee" ~ "Кофе",
    category == "Pastry" ~ "Выпечка",
    category == "Books" ~ "Книги"
  ))

p4 <- ggplot(price_data, 
             aes(x = reorder(product, price), y = price, fill = category)) +
  geom_col() +
  geom_text(aes(label = label_number(big.mark = " ", decimal.mark = ",")(price)), 
            hjust = -0.1, size = 4) +
  coord_flip() +
  scale_fill_manual(values = my_colors, name = "Категория") +
  scale_y_continuous(
    labels = label_number(big.mark = " ", decimal.mark = ","),
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(title = "Цены на товары",
       x = "", y = "Цена (руб.)") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"))
ggsave("outputs/plots_A/prices.png", p4, width = 11, height = 6, dpi = 150)

cat("✅ 4 графика сохранены в outputs/plots_A/\n")
cat("   - revenue_by_category.png\n")
cat("   - top_products.png\n")
cat("   - abc_analysis.png\n")
cat("   - prices.png\n")