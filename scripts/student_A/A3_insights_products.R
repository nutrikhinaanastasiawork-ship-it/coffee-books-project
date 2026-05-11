# ============================================
# A3_insights_products.R - Выводы студента А
# ============================================

metrics_A <- readRDS("outputs/metrics_A.rds")

# Формируем выводы на основе реальных цифр
revenue_top_category <- metrics_A$revenue_by_category$category[1]
revenue_top_share <- metrics_A$revenue_by_category$share[1]

abc_a_count <- sum(metrics_A$abc$class == "A")
abc_c_count <- sum(metrics_A$abc$class == "C")

cheapest <- metrics_A$price_stats$cheapest_product
expensive <- metrics_A$price_stats$expensive_product

insights <- sprintf("
## 📊 Выводы по товарному анализу (Студент А)

### 1. Главный драйвер выручки
Категория **'%s'** приносит %.1f%% всей выручки.
- **Рекомендация:** расширить ассортимент в этой категории, выделить больше места на витрине.

### 2. Аутсайдеры (категория C)
%d товаров (категория C) приносят всего 10%% выручки.
- **Рекомендация:** провести распродажу, сделать скидку 20-30%% или убрать из ассортимента.

### 3. Ценовая политика
- Самый дорогой товар: **%s** (%d руб.)
- Самый дешёвый товар: **%s** (%d руб.)
- **Рекомендация:** добавить комбо-предложения, например 'Кофе + Выпечка = скидка 15%%'.

### 4. Топ-5 товаров
%s
- **Рекомендация:** всегда держать эти товары в наличии, сделать отдельную витрину.

### 5. ABC-анализ
- Товары категории A: %d штук (70%% выручки)
- Товары категории B: %d штук (20%% выручки)
- Товары категории C: %d штук (10%% выручки)
",
                    revenue_top_category, revenue_top_share,
                    abc_c_count,
                    expensive, metrics_A$price_stats$most_expensive,
                    cheapest, metrics_A$price_stats$cheapest,
                    paste(metrics_A$top_products$product, collapse = ", "),
                    sum(metrics_A$abc$class == "A"),
                    sum(metrics_A$abc$class == "B"),
                    sum(metrics_A$abc$class == "C")
)

cat(insights)

# Сохраняем в файл
writeLines(insights, "outputs/insights_A.txt")
cat("\n✅ Выводы сохранены в outputs/insights_A.txt\n")