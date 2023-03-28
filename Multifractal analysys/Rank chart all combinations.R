# подключение необходимых библиотек
library(tidyverse)
library(xlsx)
library(microbenchmark)
library(stringr)

# Функция для расчета кумулятивных значений по парам, тройкам и т.д. площадок
# Единственный аргумент (df) - датафрейм с данными по площадкам.
# Возвращает датафрейм аналогичный исходному.
SuperCum <- function(df) {
  # Функции для расчета кумулятивных сумм
  combinationNUM <- function(x) {
    go <- function(a, i, k){
      if (k == 0) return(0)
      
      b <- numeric(0)
      for (j in seq(i, length(a) - k + 1)) {
        for (p in go(a, j + 1, k - 1)) {
          b <- c(b, p + a[j])
        }
      }
      return(b)
    }
    
    d <- numeric(0)
    for (k in seq(1, length(x))) {
      d <- c(d, go(x, 0, k))
    }
    d
  }
  combinationSTR <- function(x) {
    go <- function(a, i, k){
      if (k == 0) return(character(0))
      
      b <- character(0)
      for (j in seq(i, length(a) - k + 1)) {
        s <- go(a, j + 1, k - 1)
        for (p in s) {
          b <- c(b, paste(a[j], p))
        }
        if (length(s) == 0) b <- c(b, a[j])
      }
      return(b)
    }
    
    d <- character(0)
    for (k in seq(1, length(x))) {
      d <- c(d, go(x, 1, k))
    }
    d
  }
  # обертка для первых двух
  combination <- function(x) {
    if (is.numeric(x)) combinationNUM(x) else
      if (is.character(x)) combinationSTR(x) else
        cat('Некорректные данные')
  }
  
  # Добавление столбца с номерами строк
  df$area <- 1
  df$name <- as.character(1:nrow(df))
  
  # Расчет всех сумм и отображение времени расчета
  cat('Расчет кумулятивных значений занял', 
      microbenchmark(df2 <- as.data.frame(lapply(df, FUN = combination)), times = 1)$time / 10^9,
      'секунд(ы)\n')
  df2 # Возвращение значения
}

# Пример
df <- read.xlsx('Lug_sum.xls', sheetIndex = 1)
df2 <- SuperCum(df)

# Функция для построения серии кумулятивных ранговых графиков
# Первый аргумент (name) - имя файла с данными.
# Данные на первом листе в книге (по счету, не по названию)
# В первой строке - названия видов, 
# в первом столбце - названия площадок
# Второй аргумент (saveMD) - при True создаст папку "Ранговые диаграммы %имя файла%",
# в котрую будут сохранены ранговые диаграммы для каждой площадки
# ВНИМАНИЕ! При saveMD = T выполнение может занять много времени
# Третий аргумент (mainDSave) - при True построит диаграммы зависимости intercept, slope и adjasted R-squared
# для степенной и показательной моделей от площади площадки
ManyRangDiagram <- function(filename, saveMD = F, mainDSave = T) {
  # Загрузка данных
  df <- read.xlsx(filename, sheetIndex = 1)
  cat('Данные загружены\n') # вывод сообщения
  
  df2 <- SuperCum(df) # Расчет кумулятивных значений
  
  # создание пустого датафрема для последующего заполнения
  data <- data.frame(area = numeric(0), R_exp = numeric(0), intercept_exp = numeric(0), slope_exp = numeric(0),
                     R_step = numeric(0), intercept_step = numeric(0), slope_step = numeric(0))
  
  # Функция для построения ранговых диаграмм
  plotRang <- function(v, saveDG){
    name <- v[length(v)] # Выделение имени площадки из вектора
    area <- v[length(v) - 1] # Выделение площади из вектора
    v <- v %>% 
      .[1:(length(v) - 3)] %>%  # Обрезка технических значений: имя, площадь и порядковый номер
      as.numeric() %>% 
      (function(v) 100 * v / sum(v)) %>% # переход к процентам
      sort(decreasing = T) # сортировка
    
    # формирование датафрейма для построения диаграммы
    df <- data.frame(fitomassa = v,
                     rang = 1:length(v)) %>%  # формирование датафрейма
      filter(fitomassa != 0) # отбрасывание нулевых фитомасс
    
    if (nrow(df) > 1) {  
      options(scipen = 3) # для корректного отображения значений
      
      # Расчет и сохранение параметров регрессии
      # fitomassa = intercept_exp * e ^ (slope_exp * rang) Показательная
      stat <- summary(lm(log(fitomassa) ~ rang, df))
      
      # fitomassa = intercept_step * rang ^ slope_step Степенная
      stat2 <- summary(lm(log(fitomassa) ~ log(rang), df))
      
      data <<- add_row(data, area = area, R_exp = stat[["adj.r.squared"]], 
                       intercept_exp = exp(stat[["coefficients"]][1, 1]), 
                       slope_exp = stat[["coefficients"]][2, 1],
                       R_step = stat2[["adj.r.squared"]], 
                       intercept_step = exp(stat2[["coefficients"]][1, 1]), 
                       slope_step = stat2[["coefficients"]][2, 1])
      
      
      if (saveDG) {
        # график зависисмости относительной фитомассы от ранга логарифм
        ggplot(df, aes(x = rang, y = fitomassa)) +
          geom_line() + # отрисовка линии
          geom_point() + # построение точек
          labs(x = "Ранг вида", y = "Фитомасса, %", title = name) + # подписи осей
          theme_bw() + # задание темы
          scale_y_log10(lim = c(0.001, 100)) + # логарифмический масштаб
          geom_smooth(method = "lm", se = F, formula = y ~ log(x), colour = "red") + # показательная
          geom_smooth(method = "lm", se = F, colour = "blue") + # степенная
          annotate("text", x = 7, y = 0.01, color = 'blue', 
                   label = paste0('y = ', round(exp(stat[["coefficients"]][1, 1]), 3), 
                                  ' * e ^ (Ранг * ', round(stat[["coefficients"]][2, 1], 3), 
                                  ')\nAdjusted R-squared: ', round(stat[["adj.r.squared"]], 3))) +
          annotate("text", x = 7, y = 0.1, color = 'red', 
                   label = paste0('y = ', round(exp(stat2[["coefficients"]][1, 1]), 3), 
                                  ' * Ранг ^ ', round(stat2[["coefficients"]][2, 1], 3), 
                                  '\nAdjusted R-squared: ', round(stat2[["adj.r.squared"]], 3)))
        
        ggsave(paste('Диаграмма', name, '.jpeg'), width = 10, height = 8, 
               path = paste('Ранговые диаграммы', str_remove(filename, '.xlsx|.xls'))) # сохранение в файл
        cat(name, 'диаграмма сохранена\n') # вывод сообщения
      }
    }
  }
  
  # Создание папки для сохранения диаграмм, если требуется
  if (saveMD) {
    # Проверка наличия папки. Если есть, удаление
    if (dir.exists(paste('Ранговые диаграммы', str_remove(filename, '.xlsx|.xls')))) {
      unlink(paste('Ранговые диаграммы', str_remove(filename, '.xlsx|.xls')), recursive = TRUE)
    }
    dir.create(paste('Ранговые диаграммы', str_remove(filename, '.xlsx|.xls'))) # Создание папки для сохранения диаграмм
  }
 
  
  # Построение ранговых диаграмм и выведение времени выполнения
  cat('Построение графиков заняло', 
      microbenchmark(apply(df2, 1, plotRang, saveDG = saveMD), times = 1)$time / 10^9, 
      'секунд(ы)\n')
  
  # сохранение данных
  write.xlsx(data, paste0(str_remove(filename, '.xlsx|.xls'), '_rangs.xlsx'), sheetName = 'Лист 1')
  cat('Файл', paste0(str_remove(filename, '.xlsx|.xls'), '_rangs.xlsx'), 'сохранен\n')

  
  # построение диаграмм
  if (mainDSave) {
    # ДЛЯ СТЕПЕННОЙ
    # Зависимость R-squared от площади площадки
    ggplot(data, aes(x = area, y = R_step)) +  
      # geom_line() + # отрисовка линии
      geom_point() + # построение точек
      labs(x = "Площадь площадки", y = "R-squared", title = 'Степенная') + # подписи осей
      theme_bw() # задание темы
    
    ggsave(paste0(str_remove(filename, '.xlsx|.xls'), '_R_step.jpeg'), width = 10, height = 8) # сохранение в файл
    cat('График зависимости R-squared от площади площадки для степенной сохранен\n')
    
    
    # Зависимость Intercept от площади площадки
    ggplot(data, aes(x = area, y = intercept_step)) +  
      # geom_line() + # отрисовка линии
      geom_point() + # построение точек
      labs(x = "Площадь площадки", y = "Intercept", title = 'Степенная') + # подписи осей
      theme_bw() # задание темы
    
    ggsave(paste0(str_remove(filename, '.xlsx|.xls'), '_I_step.jpeg'), width = 10, height = 8) # сохранение в файл
    cat('График зависимости Intercept от площади площадки для степенной сохранен\n')
    
    
    # Зависимость Slope от площади площадки
    ggplot(data, aes(x = area, y = slope_step)) +  
      # geom_line() + # отрисовка линии
      geom_point() + # построение точек
      labs(x = "Площадь площадки", y = "Slope", title = 'Степенная') + # подписи осей
      theme_bw() # задание темы
    
    ggsave(paste0(str_remove(filename, '.xlsx|.xls'), '_S_step.jpeg'), width = 10, height = 8) # сохранение в файл
    cat('График зависимости Slope от площади площадки для степенной сохранен\n')
    
    
    #ДЛЯ ПОКАЗАТЕЛЬНОЙ
    # Зависимость R-squared от площади площадки
    ggplot(data, aes(x = area, y = R_exp)) +  
      # geom_line() + # отрисовка линии
      geom_point() + # построение точек
      labs(x = "Площадь площадки", y = "R-squared", title = 'Показательная') + # подписи осей
      theme_bw() # задание темы
    
    ggsave(paste0(str_remove(filename, '.xlsx|.xls'), '_R_exp.jpeg'), width = 10, height = 8) # сохранение в файл
    cat('График зависимости R-squared от площади площадки для показательной сохранен\n')
    
    
    # Зависимость Intercept от площади площадки
    ggplot(data, aes(x = area, y = intercept_exp)) +  
      # geom_line() + # отрисовка линии
      geom_point() + # построение точек
      labs(x = "Площадь площадки", y = "Intercept", title = 'Показательная') + # подписи осей
      theme_bw() # задание темы
    
    ggsave(paste0(str_remove(filename, '.xlsx|.xls'), '_I_exp.jpeg'), width = 10, height = 8) # сохранение в файл
    cat('График зависимости Intercept от площади площадки для показательной сохранен\n')
    
    
    # Зависимость Slope от площади площадки
    ggplot(data, aes(x = area, y = slope_exp)) +  
      # geom_line() + # отрисовка линии
      geom_point() + # построение точек
      labs(x = "Площадь площадки", y = "Slope", title = 'Показательная') + # подписи осей
      theme_bw() # задание темы
    
    ggsave(paste0(str_remove(filename, '.xlsx|.xls'), '_S_exp.jpeg'), width = 10, height = 8) # сохранение в файл
    cat('График зависимости Slope от площади площадки для показательной сохранен\n')
  }
  cat('\nВыполнение завершено!\n\n') # вывод сообщения
}

# Пример
# Будут сохранены ранговые диаграммы для каждой площадки, включая кумклятивные.
# Но не будут сохранены диаграммы зависимости intercept, slope и adjasted R-squared
ManyRangDiagram('Elnik_2009-2010-.xlsx', T, F)
