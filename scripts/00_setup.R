# ============================================
# 00_setup.R - Установка и загрузка пакетов
# ВЫПОЛНЯЮТ: оба студента (каждый на своём компьютере)
# ============================================

# Список необходимых пакетов
packages <- c("tidyverse", "lubridate", "ggplot2", "rmarkdown", "knitr", "here")

# Установка недостающих пакетов
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Создание структуры папок
dir.create("data", showWarnings = FALSE)
dir.create("scripts/student_A", showWarnings = FALSE, recursive = TRUE)
dir.create("scripts/student_B", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs/plots_A", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs/plots_B", showWarnings = FALSE, recursive = TRUE)

# Установка рабочей директории (для here пакета)
library(here)

cat("✅ Готово! Все пакеты установлены, папки созданы.\n")
