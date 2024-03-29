# ����������� ����������� ���������
library(tidyverse)
library(xlsx)

# ������� ��� ���������� �������� ���������
# ������ �������� (filename) - ��� ����� � �������.
# ������ �� ������ ����� � ����� (�� �����, �� �� ��������)
# � ������ ������ - �������� �����, 
# � ������ ������� - �������� ��������
# ������ �������� (n) - ����� ��������, �� ������� ����� ��������� ��������� (��������������)
# ������ �������� (kumulative) - ���� T, ������� � ������������ ��������� (��������������)
RMFM <- function(filename, n = NA, kumulative = T) {
  # �������� �������
  data <- read.xlsx(filename, sheetIndex = 1) # �������� ������� ������ �� �������
  if (!is.numeric(data[1,1])) # ���� � ������ ������� �������� �����
    data <- data[,-1] # ������ ������ �������
  cat('������ ���������\n\n') # ����� ���������
  
  if (kumulative) { # ���� �������, ������� � ������������� �������
    # �������� ������������� �������
    df <- data[1,]
    for (i in 2:nrow(data)) {
      df[i,] <- df[i - 1,] + data[i,]
    }
    cat('������������ ������ ������\n') # ����� ���������
  }  else df <- data
  
  # �� ��������� ��������� ������, �� ���� ������ ����� ������, �� �� ���� ������
  if (is.na(n)) v <- df[nrow(df),] else v <- df[n,]
  v <- v %>% 
    as.numeric() %>% 
    (function(v) 100 * v / sum(v)) %>% # ������� � ���������
    sort(decreasing = T) # ����������
  
  df <- data.frame(fitomassa = v,
                   rang = 1:length(v)) %>%  # ������������ ����������
    filter(fitomassa != 0) # ������������ ������� ��������

  # ������ � ���������� ���������� ���������
  # fitomassa = intercept_exp * e ^ (slope_exp * rang) �������������
  stat <- summary(lm(log(fitomassa) ~ rang, df))
  cat('������������� ������') # ����� ���������
  print(stat) # ����� ����������� ������
  
  # fitomassa = intercept_step * rang ^ slope_step ���������
  stat2 <- summary(lm(log(fitomassa) ~ log(rang), df))
  cat('��������� ������') # ����� ���������
  print(stat2) # ����� ����������� ������
  
  options(scipen = 3) # ��� ����������� ����������� ��������
  
  ggplot(df, aes(x = rang, y = fitomassa)) +
    geom_line() + # ��������� �����
    geom_point() + # ���������� �����
    labs(x = "���� ����", y = "���������, %", title = filename) + # ������� ����
    theme_bw() + # ������� ����
    scale_y_log10(lim = c(0.001, 100)) + # ��������������� �������
    geom_smooth(method = "lm", se = F, formula = y ~ log(x), colour = "red") + # �������������
    geom_smooth(method = "lm", se = F, colour = "blue") + # ���������
    annotate("text", x = 7, y = 0.01, color = 'blue', 
             label = paste0('y = ', round(exp(stat[["coefficients"]][1, 1]), 3), 
                            ' * e ^ (���� * ', round(stat[["coefficients"]][2, 1], 3), 
                            ')\nAdjusted R-squared: ', round(stat[["adj.r.squared"]], 3))) +
    annotate("text", x = 7, y = 0.1, color = 'red', 
             label = paste0('y = ', round(exp(stat2[["coefficients"]][1, 1]), 3), 
                            ' * ���� ^ ', round(stat2[["coefficients"]][2, 1], 3), 
                            '\nAdjusted R-squared: ', round(stat2[["adj.r.squared"]], 3)))
  ggsave(paste(str_remove(filename, '.xlsx|.xls'), '�������� ���������.jpeg'), width = 10, height = 8) # ���������� � ����
  cat('��������� ���������\n') # ����� ���������
  cat('\n���������� ���������!\n\n') # ����� ���������
}

# ������
# ���������� ��������� �� ������ ��������
RMFM(filename = 'Lug_sum.xls', n = 2, kumulative = F)
# ���������� ��������� �� ��������� ��������
RMFM(filename = 'Lug_sum.xls', kumulative = F)
# ���������� ��������� �� ����� ������ 5 ��������
RMFM(filename = 'Lug_sum.xls', n = 5)
# ���������� ��������� �� ����� ���� ��������
RMFM(filename = 'Elnik_sum.xlsx')


# ������� ��� ���������� �������� ��������� ��� ���������� ������
# ������ �������� (files) - ������ � ������� ������
# ������ �� ������ ����� � ����� (�� �����, �� �� ��������)
# � ������ ������ - �������� �����, 
# � ������ ������� - �������� ��������
# ������ �������� (names) - ������ � ����������, ������� ����� ���������� �� ��������� (��������������)
# ���������� ���� �� ����� ���� �������� � �����
RMFMmany <- function(files, names = files) {
  
  # � ����� �������� ��� �������
  for (i in 1:length(files)) {
      # �������� �������
      data <- read.xlsx(files[i], sheetIndex = 1) # �������� ������� ������ �� �������
      if (!is.numeric(data[1,1])) # ���� � ������ ������� �������� �����
        data <- data[,-1] # ������ ������ �������
      
      # �������� ������������� �������
      df <- data[1,]
      for (j in 2:nrow(data)) {
        df[j,] <- df[j - 1,] + data[j,]
      }
      
      # �� ��������� ��������� ������, �� ���� ������ ����� ������, �� �� ���� ������
      v <- df[nrow(df),] %>% 
        as.numeric() %>% 
        (function(v) 100 * v / sum(v)) %>% # ������� � ���������
        sort(decreasing = T) # ����������
      
      df <- data.frame(fitomassa = v,
                       rang = 1:length(v),
                       name = names[i]) %>% # ������������ ����������
      filter(fitomassa != 0) # ������������ ������� ��������
      
      cat(files[i], ' ��������\n') # ����� ���������
      
      # ������ � ���������� ���������� ���������
      # fitomassa = intercept_exp * e ^ (slope_exp * rang) �������������
      stat <- summary(lm(log(fitomassa) ~ rang, df))
      cat('������������� ������') # ����� ���������
      print(stat) # ����� ����������� ������
      
      # fitomassa = intercept_step * rang ^ slope_step ���������
      stat2 <- summary(lm(log(fitomassa) ~ log(rang), df))
      cat('��������� ������') # ����� ���������
      print(stat2) # ����� ����������� ������
      
      # ����������� � ������ �������
      if (i == 1) DF = df else DF <- rbind(DF, df)
        
       
    }
  
  options(scipen = 3) # ��� ����������� ����������� ��������
  ggplot(DF, aes(x = rang, y = fitomassa, col = name)) +
    geom_line() + # ��������� �����
    geom_point() + # ���������� �����
    labs(x = "���� ����", y = "���������, %", col = '����') + # ������� ����
    theme_bw() + # ������� ����
    scale_y_log10() + # ��������������� �������
    geom_smooth(method = "lm", se = F)
  ggsave('���������.jpeg', width = 10, height = 8) # ���������� � ����
  cat('��������� ���������\n') # ����� ���������
}

# ������
files <- c('�������� ������� ���� 2.xlsx', 'Elnik_sum.xls', 'Lug_sum.xls', 
           'P7-KP_bereznyak_ne_polny.xlsx', 'Korennye_Sbr_yag.xlsx', 'Korennye_Srtr.xlsx')
names <- c('�������� ������� ����', '�������', '����', 
           '��������', '�������� ���', '�������� ����')
RMFMmany(files, names)


