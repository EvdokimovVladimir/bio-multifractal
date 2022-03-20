library(xlsx) # подгружаем пакет для работы с таблицами
library(vegan) # подгружаем пакет для подсчета некоторых коэффициентов

indexes <- function(name, sheetindex){
  diversity.zhiv <- function(x, MARGIN = 1) # функция для вычисления индексов Животовского, 
    # написанная по аналогии с функцией diversity пакета vegan.
  {
    x <- drop(as.matrix(x)) # преобразование из датафрейма в матрицу
    if (!is.numeric(x)) # проверка некорректных значений
      stop("input data must be numeric")
    if (any(x < 0, na.rm = TRUE)) # проверка некорректных значений
      stop("input data must be non-negative")
    if (length(dim(x)) > 1) { # если несколько строк
      total <- apply(x, MARGIN, sum) # подсчет сумм
      x <- sweep(x, MARGIN, total, "/") # переход от абсолютных значений к относительным
    }
    else { # если одна строка
      x <- x/(total <- sum(x)) # переход от абсолютных значений к относительным
    }
    x <- sqrt(x) # замена значений на квадратные корни
    if (length(dim(x)) > 1) # если несколько строк
      H <- apply(x, MARGIN, sum, na.rm = TRUE) # подсчет суммы корней
    else H <- sum(x, na.rm = TRUE)
    H <- H * H # возведение в квадрат
    if (any(NAS <- is.na(total)))  # проверка некорректных значений
      H[NAS] <- NA
    H # возвращение результата
  }
  
  df <- read.xlsx(name, sheetIndex = sheetindex) # загрузка массива данных из таблицы
  if (!is.numeric(df[1,1])) # если в первом столбце названия видов
    df <- df[-1] # убрать первый столбец
  
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
  
  write.xlsx(result, file = paste("result", name)) # записываем результаты в файл
  
  result # возвращаем результаты
}

indexes('data.xlsx', 2) # проверка работы/шаблон


