# подключение необходимых библиотек
library(tidyverse)
library(xlsx)

# Функция для построения ранговой диаграммы
# Первый аргумент (filename) - имя файла с данными.
# Данные на первом листе в книге (по счету, не по названию)
# В первой строке - названия видов, 
# в первом столбце - названия площадок
# Второй аргумент (n) - номер площадки, по которой будет построена диаграмма (необязательный)
# Третий аргумент (kumulative) - если T, переход к кумулятивным значениям (необязательный)
RMFM <- function(filename, n = NA, kumulative = T) {
  # загрузка массива
  data <- read.xlsx(filename, sheetIndex = 1) # загрузка массива данных из таблицы
  if (!is.numeric(data[1,1])) # если в первом столбце названия видов
    data <- data[,-1] # убрать первый столбец
  cat('Данные загружены\n\n') # вывод сообщения
  
  if (kumulative) { # если указано, переход к кумклятивному массиву
    # создание кумулятивного массива
    df <- data[1,]
    for (i in 2:nrow(data)) {
      df[i,] <- df[i - 1,] + data[i,]
    }
    cat('Кумулятивный массив создан\n') # вывод сообщения
  }  else df <- data
  
  # по умолчанию последняя строка, но если указан номер строки, то по этой строке
  if (is.na(n)) v <- df[nrow(df),] else v <- df[n,]
  v <- v %>% 
    as.numeric() %>% 
    (function(v) 100 * v / sum(v)) %>% # переход к процентам
    sort(decreasing = T) # сортировка
  
  df <- data.frame(fitomassa = v,
                   rang = 1:length(v)) %>%  # формирование датафрейма
    filter(fitomassa != 0) # отбрасывание нулевых фитомасс

  # Расчет и сохранение параметров регрессии
  # fitomassa = intercept_exp * e ^ (slope_exp * rang) Показательная
  stat <- summary(lm(log(fitomassa) ~ rang, df))
  cat('Показательная модель') # вывод сообщения
  print(stat) # вывод результатов модели
  
  # fitomassa = intercept_step * rang ^ slope_step Степенная
  stat2 <- summary(lm(log(fitomassa) ~ log(rang), df))
  cat('Степенная модель') # вывод сообщения
  print(stat2) # вывод результатов модели
  
  options(scipen = 3) # для корректного отображения значений
  
  ggplot(df, aes(x = rang, y = fitomassa)) +
    geom_line() + # отрисовка линии
    geom_point() + # построение точек
    labs(x = "Ранг вида", y = "Фитомасса, %", title = filename) + # подписи осей
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
  ggsave(paste(str_remove(filename, '.xlsx|.xls'), 'ранговая диаграмма.jpeg'), width = 10, height = 8) # сохранение в файл
  cat('Диаграмма сохранена\n') # вывод сообщения
  cat('\nВыполнение завершено!\n\n') # вывод сообщения
}

# Пример
# Построение диаграммы по второй площадке
RMFM(filename = 'Lug_sum.xls', n = 2, kumulative = F)
# Построение диаграммы по последней площадке
RMFM(filename = 'Lug_sum.xls', kumulative = F)
# Построение диаграммы по сумме первых 5 площадок
RMFM(filename = 'Lug_sum.xls', n = 5)
# Построение диаграммы по сумме всех площадок
RMFM(filename = 'Elnik_sum.xlsx')


# Функция для построения ранговой диаграммы для нескольких файлов
# Первый аргумент (files) - вектор с именами файлов
# Данные на первом листе в книге (по счету, не по названию)
# В первой строке - названия видов, 
# в первом столбце - названия площадок
# Второй аргумент (names) - вектор с названиями, которые будут отображены на диаграмме (необязательный)
# Построение идет по сумме всех площадок в файле
RMFMmany <- function(files, names = files) {
  
  # в цикле добавить все таблицы
  for (i in 1:length(files)) {
      # загрузка массива
      data <- read.xlsx(files[i], sheetIndex = 1) # загрузка массива данных из таблицы
      if (!is.numeric(data[1,1])) # если в первом столбце названия видов
        data <- data[,-1] # убрать первый столбец
      
      # создание кумулятивного массива
      df <- data[1,]
      for (j in 2:nrow(data)) {
        df[j,] <- df[j - 1,] + data[j,]
      }
      
      # по умолчанию последняя строка, но если указан номер строки, то по этой строке
      v <- df[nrow(df),] %>% 
        as.numeric() %>% 
        (function(v) 100 * v / sum(v)) %>% # переход к процентам
        sort(decreasing = T) # сортировка
      
      df <- data.frame(fitomassa = v,
                       rang = 1:length(v),
                       name = names[i]) %>% # формирование датафрейма
      filter(fitomassa != 0) # отбрасывание нулевых фитомасс
      
      cat(files[i], ' загружен\n') # вывод сообщения
      
      # Расчет и сохранение параметров регрессии
      # fitomassa = intercept_exp * e ^ (slope_exp * rang) Показательная
      stat <- summary(lm(log(fitomassa) ~ rang, df))
      cat('Показательная модель') # вывод сообщения
      print(stat) # вывод результатов модели
      
      # fitomassa = intercept_step * rang ^ slope_step Степенная
      stat2 <- summary(lm(log(fitomassa) ~ log(rang), df))
      cat('Степенная модель') # вывод сообщения
      print(stat2) # вывод результатов модели
      
      # объединение в единую таблицу
      if (i == 1) DF = df else DF <- rbind(DF, df)
        
       
    }
  
  options(scipen = 3) # для корректного отображения значений
  ggplot(DF, aes(x = rang, y = fitomassa, col = name)) +
    geom_line() + # отрисовка линии
    geom_point() + # построение точек
    labs(x = "Ранг вида", y = "Фитомасса, %", col = 'Файл') + # подписи осей
    theme_bw() + # задание темы
    scale_y_log10() + # логарифмический масштаб
    geom_smooth(method = "lm", se = F)
  ggsave('Диаграмма.jpeg', width = 10, height = 8) # сохранение в файл
  cat('Диаграмма сохранена\n') # вывод сообщения
}

# Пример
files <- c('Осинники Средний Урал 2.xlsx', 'Elnik_sum.xls', 'Lug_sum.xls', 
           'P7-KP_bereznyak_ne_polny.xlsx', 'Korennye_Sbr_yag.xlsx', 'Korennye_Srtr.xlsx')
names <- c('Осинники Средний Урал', 'Ельники', 'Луга', 
           'Березняк', 'Коренные СБР', 'Коренные СРТР')
RMFMmany(files, names)