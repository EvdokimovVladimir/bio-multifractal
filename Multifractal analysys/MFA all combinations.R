# Увеличение памяти (Цифра - количество гигабайт. Должны быть свободны.)
options(java.parameters = '-Xmx8g')

# подключение необходимых библиотек
library(xlsx)
library(tidyverse)
library(microbenchmark)


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


# Функция для построения 6 диаграмм:
# 1) Степенная зависимость накопления видового богатства S от объема выборки N;
# 2) Степенная зависимость моментов распределения особей по видам Mq от объема выборки N;
# 3) Нелинейность зависимости скейлинговых показателей t от порядка момента q;
# 4) Нелинейность обобщенных размерностей реньи Dq от порядка момента q;
# 5) Инвариантность обобщенных размерностей относительно числа видов и численности сообщества;
# 6) Мультифрактальный спектр.
# Все диаграммы сохраняются в рабочую директорию.
# Первый аргумент (filename) - имя файла с данными.
# Данные на первом листе в книге (по счету, не по названию)
# В первой строке - названия видов, 
# в первом столбце - названия площадок
# Второй аргумент (Q) - массив значений q для 2 и 5 диаграмм (необязательный)
# Третий аргумент (savefile) - при True сохранит результаты в файл Excel (необязательный)
# ВНИМАНИЕ! При savefile = T может потребоваться много опреативной памяти 
# и расчет будет занимать значительно больше времени
MFAFallCum <- function(filename, Q = seq(from = -2, to = 2, by = 1), savefile = F){
  
  cat('Загрузка данных\n') # вывод сообщения
  # загрузка массива
  data <- read.xlsx(filename, sheetIndex = 1) # загрузка массива данных из таблицы
  if (!is.numeric(data[1,1])) # если в первом столбце названия видов
    data <- data[,-1] # убрать первый столбец
  cat('Данные загружены\n') # вывод сообщения
  filename <- str_remove(filename, '.xlsx|.xls') # исключение расширения файла из имени
  filename2 <- paste0(filename, '_analyse.xlsx')
  
  # создание кумулятивного массива
  df <- SuperCum(data)
  df$sum <- apply(df[, 1:(ncol(df) - 2)], 1, sum) # Расчет сумм по строкам
  df <- arrange(df, sum) # сортировка по сумме
  cat('Кумулятивный массив создан\n') # вывод сообщения
  
  if (savefile) {
    # Сохранение данных в файл
    write.xlsx(df, filename2, sheetName = 'Кумулятивный')
    cat('Файл', filename2, 'сохранен\n\n') # вывод сообщения
  }

  df <- select(df, -sum, -name, -area) # исключение столбцов
  
  
  cat(rep('-', 80), sep = '') # Разделитель раздела
  cat('\nЭтап 1. Степенная зависимость накопления видового богатства S от объема выборки N\n')
  # Этап 1 (S от N) ----
  df1 <- data.frame(N = apply(df, 1, sum), # подсчет фитомассы по площадкам
                    S = apply(df, 1, function(x) length(x[x > 0]))) # подсчет количества видов по площадкам
  
  if (savefile) {
    # Сохранение данных в файл
    write.xlsx(df1, filename2, sheetName = 'Виды и фитомассы', append = T)
    cat('Файл', filename2, 'обновлен\n\n') # вывод сообщения
  }
  
  ggplot(df1, aes(x = N, y = S)) +  
    # geom_line() + # отрисовка линии
    geom_point() + # построение точек
    labs(x = "N", y = "S", title = 'Степенная зависимость накопления видового богатства S от объема выборки N') + # подписи осей
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
  
  if (savefile) {
    # Сохранение данных в файл
    write.xlsx(df2, filename2, sheetName = 'Моменты', append = T)
    cat('Файл', filename2, 'обновлен\n') # вывод сообщения
  }
  
  # перевод датафрейма из широкого в длинный
  df2  <-  df2 %>% 
    pivot_longer(cols = colnames(df2)[1]:colnames(df2)[length(Q)], 
                 names_to = 'q', 
                 values_to = 'Mq') %>% 
    filter(q != 1)
  
  ggplot(df2, aes(x = N, y = Mq, col = q)) +
    # geom_line() + # отрисовка линии
    geom_point() + # построение точек
    labs(x = "N", y = "Mq", title = 'Степенная зависимость моментов распределения особей по видам Mq от объема выборки N') + # подписи осей
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
    
    M <- sapply(q, FUN = function(i) sum(p^i)) # считаем моменты
    t <- log(M)/log(sum(v)) # рассчитываем скейлинговые показатели
    D <- t/(1 - q) # рассчитываем обобщенные размерности Реньи
    
    a <- (t[1:(length(t) - 1)] - t[2:length(t)])/step # дискретно считаем производную от показателе массы (индексы сингулярности)
    f <- q[-1]*a + t[-1] # считаем спектр сингулярностей
    
    return(list(vars = data.frame(q = q, M = M, t = t, D = D), mfs = data.frame(a = a, f = f))) 
  }
  # создание датафрейма со скейлинговыми показателями
  df3 <- mfa(df[nrow(df),])$vars
 
  if (savefile) {
    # Сохранение данных в файл
    write.xlsx(df3, filename2, sheetName = 'Скейлинговые показатели', append = T)
    cat('Файл', filename2, 'обновлен\n') # вывод сообщения
  }
  
  ggplot(df3, aes(x = q, y = t)) +
    geom_line() + # отрисовка линии
    labs(x = "q", y = "t", title = 'Нелинейность зависимости скейлинговых показателей t от порядка момента q') + # подписи осей
    theme_bw() # задание темы
  ggsave(paste0('Этап 3 ', filename, '.jpeg'), width = 10, height = 8) # сохранение в файл
  cat('График третьего этапа построен\n\n') # вывод сообщения
  
  
  cat(rep('-', 80), sep = '') # Разделитель раздела
  cat('\nЭтап 4. Нелинейность зависимости обобщенных размерностей реньи Dq от порядка момента q\n')
  # Этап 4 (Dq от q) ----
  # используется данные из датафрейма, полученного на предыдущем шаге
  ggplot(df3[df3$q != 1,], aes(x = q, y = D)) +
    geom_line() + # отрисовка линии
    labs(x = "q", y = "Dq", title = 'Нелинейность зависимости обобщенных размерностей реньи Dq от порядка момента q') + # подписи осей
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
  
  if (savefile) {
    # Сохранение данных в файл
    write.xlsx(df5, filename2, sheetName = 'Размерности', append = T)
    cat('Файл', filename2, 'обновлен\n') # вывод сообщения
  }
  
  # перевод датафрейма из широкого в длинный
  df5  <-  df5 %>% 
    pivot_longer(cols = colnames(df5)[1]:colnames(df5)[length(Q)], 
                 names_to = 'q', 
                 values_to = 'Dq')
  
  ggplot(df5, aes(x = N, y = Dq, col = q)) +
    # geom_line() + # отрисовка линии
    geom_point() + # построение точек
    labs(x = "N", y = "Dq", title = 'Инвариантность обобщенных размерностей относительно числа видов и численности сообщества') + # подписи осей
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
  
  if (savefile) {
    # Сохранение данных в файл
    write.xlsx(mf$mfs, filename2, sheetName = 'Мультифрактальный спектр', append = T)
    cat('Файл', filename2, 'обновлен\n') # вывод сообщения
  }
  
  ggplot(mf$mfs, aes(x = a, y = f)) +
    geom_line() + # отрисовка линии
    labs(x = "\u03B1", y = "f(\u03B1)", title = 'Мультифрактальный спектр') + # подписи осей
    theme_bw() # задание темы
  ggsave(paste0('Этап 6 ', filename, '.jpeg'), width = 10, height = 8) # сохранение в файл
  cat('График шестого этапа построен\n\n') # вывод сообщения
  
  cat(rep('-', 80), sep = '') # Разделитель раздела
  cat('\nРасчет индексов разнообразия\n')
  # Расчет индексов разнообразия ----
  
  # Функция для расчета индексов разнообразия
  indexes <- function(df){
    library(xlsx) # подгружаем пакет для работы с таблицами
    library(vegan) # подгружаем пакет для подсчета некоторых коэффициентов
    diversity.zhiv <- function(x, MARGIN = 1) # функция для вычисления индексов Животовского, 
      # напаписанная по аналогии с функцией diversity пакета vegan.
    {
      x <- drop(as.matrix(x)) # преобразование из датафрейма в матрицу
      if (!is.numeric(x)) # проверка некорректных значений
        stop("input data must be numeric")
      if (any(x < 0, na.rm = TRUE)) # проверка некорректных значений
        stop("input data must be non-negative")
      if (length(dim(x)) > 1) { # если несколько строк
        total <- apply(x, MARGIN, sum) # посчет сумм
        x <- sweep(x, MARGIN, total, "/") # переход от абсолютных значений к относительным
      }
      else { # если одна строка
        x <- x/(total <- sum(x)) # переход от абсолютных значений к относительным
      }
      x <- sqrt(x) # замена значений на квадратные корни
      if (length(dim(x)) > 1) # если несколько строк
        H <- apply(x, MARGIN, sum, na.rm = TRUE) # посчет суммы корней
      else H <- sum(x, na.rm = TRUE)
      H <- H * H # возведение в квадрат
      if (any(NAS <- is.na(total)))  # проверка некорректных значений
        H[NAS] <- NA
      H # возвращение результата
    }
    
    result <- data.frame( # собираем датафрейм с показателями
      N = apply(df, FUN = sum, na.rm = TRUE, MARGIN = 1), # общая численность, как сумма всех значений по строкам
      S = specnumber(df)) # видовое богатство, функция из пакета vegan
    result$k <- log(result$S)/log(result$N) # показатель Маргалефа, расчет по формуле
    result$d <- (result$S - 1)/log(result$N) # индекс видового богатства Маргалефа
    result$H <- diversity(df, index = "shannon") # индекс Шеннона, функция из пакета vegan
    result$e <- result$H/log(result$S) # индекс выравненности Пиелу, расчет по формуле
    result$С <- 1 - diversity(df, index = "simpson") # индекс доминирования Симпсона, функция из пакета vegan
    result$m <- diversity.zhiv(df) # индекс Животовского
    result$h <- 1 - result$m / result$S # доля редких видов Животовского, расчет по формуле
    
    result # возвращаем результаты
  }
  
  if (savefile) {
  # Сохранение данных в файл
  write.xlsx(indexes(df), filename2, sheetName = 'Индексы', append = T)
  cat('Файл', filename2, 'обновлен\n\n') # вывод сообщения
  }
  cat('Выполнение завершено!\n\n') # вывод сообщения
}

# Пример
# После выполнения выведет затраченное время
microbenchmark(MFAFallCum('Elnik_2009-2010.xlsx', savefile = T), times = 1)
