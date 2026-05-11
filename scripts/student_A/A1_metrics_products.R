# ============================================
# A1_metrics_products.R - Товарные метрики
# СТУДЕНТ А
# ============================================

library(tidyverse)

# Загрузка данных
sales <- read.csv("data/sales.csv")
sales$date <- as.Date(sales$date)

cat("📊 РАСЧЁТ ТОВАРНЫХ МЕТРИК\n")
cat("========================================\n")

# 1. Выручка по категориям
revenue_by_category <- sales %>%
  group_by(category) %>%
  summarise(
    revenue = sum(revenue),
    share = round(revenue / sum(sales$revenue) * 100, 1)
  ) %>%
  arrange(desc(revenue))

cat("\n1️⃣ Выручка по категориям:\n")
print(revenue_by_category)

# 2. Топ-5 товаров по выручке
top_products <- sales %>%
  group_by(product, category) %>%
  summarise(revenue = sum(revenue), .groups = "drop") %>%
  arrange(desc(revenue)) %>%
  head(5)

cat("\n2️⃣ Топ-5 товаров по выручке:\n")
print(top_products)

# 3. Топ-5 товаров по количеству продаж
top_quantity <- sales %>%
  group_by(product) %>%
  summarise(quantity = sum(quantity), .groups = "drop") %>%
  arrange(desc(quantity)) %>%
  head(5)

cat("\n3️⃣ Топ-5 товаров по количеству:\n")
print(top_quantity)

# 4. Ценовая статистика
price_stats <- sales %>%
  group_by(product, category) %>%
  summarise(price = first(price_per_unit), .groups = "drop") %>%
  summarise(
    cheapest = min(price),
    cheapest_product = product[which.min(price)],
    most_expensive = max(price),
    expensive_product = product[which.max(price)],
    avg_price = mean(price)
  )

cat("\n4️⃣ Ценовая статистика:\n")
print(price_stats)

# 5. ABC-анализ
abc <- sales %>%
  group_by(product) %>%
  summarise(revenue = sum(revenue), .groups = "drop") %>%
  arrange(desc(revenue)) %>%
  mutate(
    cum_percent = cumsum(revenue) / sum(revenue) * 100,
    class = case_when(
      cum_percent <= 70 ~ "A",
      cum_percent <= 90 ~ "B",
      TRUE ~ "C"
    )
  )

cat("\n5️⃣ ABC-анализ (A=70% выручки, B=20%, C=10%):\n")
print(abc %>% select(product, revenue, class))

# Сохраняем все метрики в один список
metrics_A <- list(
  revenue_by_category = revenue_by_category,
  top_products = top_products,
  top_quantity = top_quantity,
  price_stats = price_stats,
  abc = abc
)

# Сохраняем в файл
if(!dir.exists("outputs")) dir.create("outputs")
saveRDS(metrics_A, "outputs/metrics_A.rds")

cat("\n========================================\n")
cat("✅ Метрики сохранены в outputs/metrics_A.rds\n")