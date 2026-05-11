# ============================================
# 01_data_prep.R - Генерация данных
# ВЫПОЛНЯЕТ: студент А ИЛИ Б (один из них)
# ============================================

library(tidyverse)
library(lubridate)

set.seed(2024)  # для воспроизводимости

# Товары и категории
products <- data.frame(
  product = c(
    "Эспрессо", "Американо", "Капучино", "Латте", "Раф",
    "Круассан", "Маффин", "Сырник", "Печенье",
    "Война и мир", "Мастер и Маргарита", "Преступление и наказание"
  ),
  category = c(
    rep("Кофе", 5),
    rep("Выпечка", 4),
    rep("Книги", 3)
  ),
  price_per_unit = c(150, 180, 220, 250, 280, 120, 150, 200, 80, 450, 600, 500)
)

# Период: 90 дней
dates <- seq.Date(from = as.Date("2024-01-01"), 
                  to = as.Date("2024-03-30"), 
                  by = "day")

# Генерация продаж
n_transactions <- 2000
sales_data <- data.frame()

for (i in 1:n_transactions) {
  date <- sample(dates, 1)
  weekday <- wday(date, label = TRUE)
  
  # Коэффициент для дня недели (в выходные продаж больше)
  day_factor <- ifelse(weekday %in% c("Sat", "Sun"), 1.5, 1.0)
  
  # Количество товаров в чеке (от 1 до 5)
  n_items <- sample(1:5, 1, prob = c(0.3, 0.25, 0.2, 0.15, 0.1))
  
  # Выбор товаров с учётом дня недели
  if (weekday %in% c("Sat", "Sun")) {
    # в выходные чаще покупают книги и кофе
    probs <- ifelse(products$category %in% c("Кофе", "Книги"), 2, 1)
  } else {
    # в будни чаще выпечку
    probs <- ifelse(products$category == "Выпечка", 2, 1)
  }
  
  selected_products <- sample(1:nrow(products), n_items, 
                              prob = probs, replace = TRUE)
  
  for (item in selected_products) {
    quantity <- sample(1:3, 1)
    revenue <- quantity * products$price_per_unit[item]
    
    sales_data <- rbind(sales_data, data.frame(
      transaction_id = i,
      date = date,
      product = products$product[item],
      category = products$category[item],
      quantity = quantity,
      price_per_unit = products$price_per_unit[item],
      revenue = revenue
    ))
  }
}

# Сохранение данных
write.csv(sales_data, "data/sales.csv", row.names = FALSE)

cat("✅ Данные созданы! Всего строк:", nrow(sales_data), "\n")
cat("   Сохранено в data/sales.csv\n")

