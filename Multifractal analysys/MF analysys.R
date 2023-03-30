# library for Excel, charts, data tables and strings
library(xlsx)
library(ggplot2)
library(tidyverse)
library(stringr)

# Function for plotting 6 charts:
# 1) The power dependence of the accumulation of the number of species S on the sample size N;
# 2) The power dependence of the moments of the distribution by species Mq on the sample size N;
# 3) The nonlinearity of the dependence of scaling indicators t on the order of the moment q;
# 4) The nonlinearity of the dependence of generalized Renyi dimensions Dq from the order of the moment q;
# 5) Invariance of generalized dimensions Dq with respect to the sample size;
# 6) Multifractal spectrum.
# All diagrams are saved to the working directory.
# The first argument (filename) is the name of the data file.
# Data on the first sheet in the file (by count, not by name)
# In the first line - the names of species, 
# in the first column - the names of the sites
# The second argument (Q) is an array of q values for 2 and 5 diagrams (optional)

MFAF <- function(filename, Q = seq(from = -2, to = 2, by = 1)){
  # загрузка массива
  data <- read.xlsx(filename, sheetIndex = 1) # загрузка массива данных из таблицы
  if (!is.numeric(data[1,1])) # если в первом столбце названия видов
    data <- data[,-1] # убрать первый столбец
  cat('Данные загружены\n') # вывод сообщения
  
  # создание кумулятивного массива
  df <- data[1,]
  for (i in 2:nrow(data)) {
    df[i,] <- df[i - 1,] + data[i,]
  }
  cat('Кумулятивный массив создан\n') # вывод сообщения
  
  filename <- basename(str_remove(filename, '.xlsx|.xls')) # исключение расширения файла из имени
 
  cat(rep('-', 80), sep = '') # Разделитель раздела
  cat('\nЭтап 1. Степенная зависимость накопления видового богатства S от объема выборки N\n')
  # Этап 1 (S от N) ----
  df1 <- data.frame(N = apply(df, 1, sum), # подсчет фитомассы по площадкам
                    S = apply(df, 1, function(x) length(x[x > 0]))) # подсчет количества видов по площадкам
  
  ggplot(df1, aes(x = N, y = S)) +  
    # geom_line() + # отрисовка линии
    geom_point() + # построение точек
    labs(x = "N", y = "S", 
         title = 'Степенная зависимость накопления видового богатства S от объема выборки N') + # подписи осей
    theme_bw() + # задание темы
    scale_x_log10() + # билогарифмический масштаб
    scale_y_log10() 
  ggsave(paste0('Этап 1 ', filename, '.jpeg'), width = 10, height = 8) # сохранение в файл
  cat('График первого этапа построен\n') # вывод сообщения
  
  # Вывод параметров регрессии
  stat <- summary(lm(log(S) ~ log(N), df1))
  cat('Зависимость видового богатства S от объема выборки N\n',
      'S = ', exp(stat[["coefficients"]][1, 1]), ' * N ^ ', stat[["coefficients"]][2, 1],
      '; Adjusted R-squared:  ', stat[["adj.r.squared"]], '\n\n', sep = '')
  
  
  cat(rep('-', 80), sep = '') # Разделитель раздела
  cat('\nЭтап 2. Степенная зависимость моментов распределения особей по видам Mq от объема выборки N\n')
  # Этап 2 (Mq от N) ----
  # функция рассчитывающая моменты
  graph2 <- function(v){
    v <- v[v != 0]
    p <- v/sum(v) # переходим к относительным частотам
    M <- sapply(Q, function(i) sum(p^i)) # считаем моменты
  }
  
  df2 <- as.data.frame(cbind(t(apply(df, 1, graph2)), apply(df, 1, sum))) # создание массива с моментами
  colnames(df2) <- c(Q, 'N') # переименовывание столбцов
  
  # перевод датафрейма из широкого в длинный
  df2  <-  df2 %>% 
    pivot_longer(cols = colnames(df2)[1]:colnames(df2)[length(Q)], 
                 names_to = 'q', 
                 values_to = 'Mq') %>% 
    filter(q != 1)
  
  ggplot(df2, aes(x = N, y = Mq, col = q)) +
    # geom_line() + # отрисовка линии
    geom_point() + # построение точек
    labs(x = "N", y = "Mq", 
         title = 'Степенная зависимость моментов распределения особей по видам Mq от объема выборки N') + # подписи осей
    theme_bw() + # задание темы
    scale_x_log10() + # билогарифмический масштаб
    scale_y_log10() 
  ggsave(paste0('Этап 2 ', filename, '.jpeg'), width = 10, height = 8) # сохранение в файл
  cat('График второго этапа построен\n') # вывод сообщения
  
  # Вывод параметров регрессии
  cat('Зависимость моментов Mq от объема выборки N\n')
  for (i in 1:length(Q)) {
    try({
      stat <- summary(lm(log(Mq) ~ log(N), df2, q == Q[i]))
      cat('q = ', Q[i], '; ',
          'Mq = ', exp(stat[["coefficients"]][1, 1]), ' * N ^ ', stat[["coefficients"]][2, 1],
          '; Adjusted R-squared: ', stat[["adj.r.squared"]], '\n', sep = '')
    }, silent = T)
  } 
  cat('\n')
  
  
  cat(rep('-', 80), sep = '') # Разделитель раздела
  cat('\nЭтап 3. Нелинейность зависимости скейлинговых показателей t от порядка момента q\n')
  # Этап 3 (t от q) ----
  # функция для МФА
  mfa <- function(v, Qmin = -7, Qmax = 5, step = 0.01){ 
    v <- v[v != 0]
    p <- v/sum(v) # переходим к относительным частотам
    q <- seq(Qmin, Qmax, step) # создаем вектор порядков моментов распределения особей (фитомассы) по видам
    
    M <- sapply(q, function(i) sum(p^i)) # считаем моменты
    t <- log(M)/log(sum(v)) # рассчитываем скейлинговые показатели
    D <- t/(1 - q) # рассчитываем обобщенные размерности Реньи
    
    a <- (t[1:(length(t) - 1)] - t[2:length(t)])/step # дискретно считаем производную от показателе массы (индексы сингулярности)
    f <- q[-1]*a + t[-1] # считаем спектр сингулярностей
    
    return(list(vars = data.frame(q = q, M = M, t = t, D = D), mfs = data.frame(a = a, f = f))) 
  }
  # создание датафрейма со скейлинговыми показателями
  df3 <- mfa(df[nrow(df),])$vars
  
  ggplot(df3, aes(x = q, y = t)) +
    geom_line() + # отрисовка линии
    labs(x = "q", y = "t", 
         title = 'Нелинейность зависимости скейлинговых показателей t от порядка момента q') + # подписи осей
    theme_bw() # задание темы
  ggsave(paste0('Этап 3 ', filename, '.jpeg'), width = 10, height = 8) # сохранение в файл
  cat('График третьего этапа построен\n\n') # вывод сообщения
  
  
  cat(rep('-', 80), sep = '') # Разделитель раздела
  cat('\nЭтап 4. Нелинейность зависимости обобщенных размерностей реньи Dq от порядка момента q\n')
  # Этап 4 (Dq от q) ----
  # используется данные из датафрейма, полученного на предыдущем шаге
  ggplot(df3[df3$q != 1,], aes(x = q, y = D)) +
    geom_line() + # отрисовка линии
    labs(x = "q", y = "Dq", 
         title = 'Нелинейность зависимости обобщенных размерностей реньи Dq от порядка момента q') + # подписи осей
    theme_bw() # задание темы
  ggsave(paste0('Этап 4 ', filename, '.jpeg'), width = 10, height = 8) # сохранение в файл
  cat('График четвертого этапа построен\n\n') # вывод сообщения
  
  
  cat(rep('-', 80), sep = '') # Разделитель раздела
  cat('\nЭтап 5. Инвариантность обобщенных размерностей относительно числа видов и численности сообщества\n')
  # Этап 5 (Dq от N) ----
  # функция рассчитывающая размерности
  graph5 <- function(v){
    v <- v[v != 0]
    p <- v/sum(v) # переходим к относительным частотам
    D <- sapply(Q, function(i) log(sum(p^i))/((1 - i) * log(sum(v)))) # считаем размерности
  }
  
  df5 <- as.data.frame(cbind(t(apply(df, 1, graph5)), apply(df, 1, sum))) # создание массива с размерностями
  colnames(df5) <- c(Q, 'N') # переименовывание столбцов
  
  # перевод датафрейма из широкого в длинный
  df5  <-  df5 %>% 
    pivot_longer(cols = colnames(df5)[1]:colnames(df5)[length(Q)], 
                 names_to = 'q', 
                 values_to = 'Dq')
  
  ggplot(df5, aes(x = N, y = Dq, col = q)) +
    # geom_line() + # отрисовка линии
    geom_point() + # построение точек
    labs(x = "N", y = "Dq", 
         title = 'Инвариантность обобщенных размерностей относительно числа видов и численности сообщества') + # подписи осей
    theme_bw() + # задание темы
    scale_x_log10() + # билогарифмический масштаб
    scale_y_log10() 
  ggsave(paste0('Этап 5 ', filename, '.jpeg'), width = 10, height = 8) # сохранение в файл
  cat('График пятого этапа построен\n') # вывод сообщения
  
  # Вывод параметров регрессии
  cat('Инвариантность обобщенных размерностей Dq\n')
  for (i in 1:length(Q)) {
    try({
      stat <- summary(lm(Dq ~ 1, df5, q == Q[i], na.action = NULL))
      cat('q = ', Q[i], '; ',
          'Dq = ', exp(stat[["coefficients"]][1, 1]),
          '; Residual standard error: ', stat[["sigma"]], 
          '; p-value: ', stat[["coefficients"]][1, 4], '\n', sep = '')
    }, silent = T)
  }
  cat('\n')
  
  
  cat(rep('-', 80), sep = '') # Разделитель раздела
  cat('\nЭтап 6. Мультифрактальный спектр\n')
  # Этап 6 (мультифрактальный спектр) ----
  v <- data[nrow(data),] # выборка вектора с суммарными значениями фитомасс
  
  mf <- mfa(v, step = 0.01) # расчет мультифрактального спектра
  
  ggplot(mf$mfs, aes(x = a, y = f)) +
    geom_line() + # отрисовка линии
    labs(x = "\u03B1", y = "f(\u03B1)", title = 'Мультифрактальный спектр') + # подписи осей
    theme_bw() # задание темы
  ggsave(paste0('Этап 6 ', filename, '.jpeg'), width = 10, height = 8) # сохранение в файл
  cat('График шестого этапа построен\n\n') # вывод сообщения
  cat('Выполнение завершено!\n\n') # вывод сообщения
}

# Пример
MFAF('Input data/Elnik_2009-2010.xlsx')


# Функция для построения диаграммы 
# "Степенная зависимость накопления видового богатства S от объема выборки N"
# для нескольких массивов данных для сравнения
# Первый аргумент (file) - вектор с именами файлов
# Второй аргумент (names) - вектор с названиями, которые будут отображены на диаграмме (необязательный)
SNmany <- function(files, names = files) {
  
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
    
    if (i == 1) 
      df1 <- data.frame(N = apply(df, 1, sum),
                        S = apply(df, 1, function(x) length(x[x > 0])),
                        sp = names[i]) else
      df1 <- rbind(df1, data.frame(N = apply(df, 1, sum),
                                   S = apply(df, 1, function(x) length(x[x > 0])),
                                   sp = names[i]))
    cat(files[i], ' загружен\n') # вывод сообщения
  }
  
  ggplot(df1, aes(x = N, y = S, col = sp)) +
    geom_line() + # отрисовка линии
    geom_point() + # построение точек
    labs(x = "N", y = "S", col = 'Файл', 
         title = 'Степенная зависимость накопления видового богатства S от объема выборки N') + # подписи осей
    theme_bw() + # задание темы
    scale_x_log10() + # билогарифмический масштаб
    scale_y_log10() 
  ggsave('Диаграмма.jpeg', width = 10, height = 8) # сохранение в файл
  cat('Диаграмма сохранена\n') # вывод сообщения
}

# Пример
files <- c('Input data/Осинники Средний Урал 2.xlsx', 'Input data/Elnik_sum.xls', 
           'Input data/Lug_sum.xls', 'Input data/P7-KP_bereznyak_ne_polny.xlsx', 
           'Input data/Korennye_Sbr_yag.xlsx', 'Input data/Korennye_Srtr.xlsx')
names <- c('Осинники Средний Урал', 'Ельники', 'Луга', 
           'Березняк', 'Коренные СБР', 'Коренные СРТР')
SNmany(files, names)

