# ����������� ����������� ���������
library(tidyverse)
library(xlsx)
library(microbenchmark)
library(stringr)

# ������� ��� ������� ������������ �������� �� �����, ������� � �.�. ��������
# ������������ �������� (df) - ��������� � ������� �� ���������.
# ���������� ��������� ����������� ���������.
SuperCum <- function(df) {
  # ������� ��� ������� ������������ ����
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
  # ������� ��� ������ ����
  combination <- function(x) {
    if (is.numeric(x)) combinationNUM(x) else
      if (is.character(x)) combinationSTR(x) else
        cat('������������ ������')
  }
  
  # ���������� ������� � �������� �����
  df$area <- 1
  df$name <- as.character(1:nrow(df))
  
  # ������ ���� ���� � ����������� ������� �������
  cat('������ ������������ �������� �����', 
      microbenchmark(df2 <- as.data.frame(lapply(df, FUN = combination)), times = 1)$time / 10^9,
      '������(�)\n')
  df2 # ����������� ��������
}

# ������
df <- read.xlsx('Lug_sum.xls', sheetIndex = 1)
df2 <- SuperCum(df)

# ������� ��� ���������� ����� ������������ �������� ��������
# ������ �������� (name) - ��� ����� � �������.
# ������ �� ������ ����� � ����� (�� �����, �� �� ��������)
# � ������ ������ - �������� �����, 
# � ������ ������� - �������� ��������
# ������ �������� (saveMD) - ��� True ������� ����� "�������� ��������� %��� �����%",
# � ������ ����� ��������� �������� ��������� ��� ������ ��������
# ��������! ��� saveMD = T ���������� ����� ������ ����� �������
# ������ �������� (mainDSave) - ��� True �������� ��������� ����������� intercept, slope � adjasted R-squared
# ��� ��������� � ������������� ������� �� ������� ��������
ManyRangDiagram <- function(filename, saveMD = F, mainDSave = T) {
  # �������� ������
  df <- read.xlsx(filename, sheetIndex = 1)
  cat('������ ���������\n') # ����� ���������
  
  df2 <- SuperCum(df) # ������ ������������ ��������
  
  # �������� ������� ��������� ��� ������������ ����������
  data <- data.frame(area = numeric(0), R_exp = numeric(0), intercept_exp = numeric(0), slope_exp = numeric(0),
                     R_step = numeric(0), intercept_step = numeric(0), slope_step = numeric(0))
  
  # ������� ��� ���������� �������� ��������
  plotRang <- function(v, saveDG){
    name <- v[length(v)] # ��������� ����� �������� �� �������
    area <- v[length(v) - 1] # ��������� ������� �� �������
    v <- v %>% 
      .[1:(length(v) - 3)] %>%  # ������� ����������� ��������: ���, ������� � ���������� �����
      as.numeric() %>% 
      (function(v) 100 * v / sum(v)) %>% # ������� � ���������
      sort(decreasing = T) # ����������
    
    # ������������ ���������� ��� ���������� ���������
    df <- data.frame(fitomassa = v,
                     rang = 1:length(v)) %>%  # ������������ ����������
      filter(fitomassa != 0) # ������������ ������� ��������
    
    if (nrow(df) > 1) {  
      options(scipen = 3) # ��� ����������� ����������� ��������
      
      # ������ � ���������� ���������� ���������
      # fitomassa = intercept_exp * e ^ (slope_exp * rang) �������������
      stat <- summary(lm(log(fitomassa) ~ rang, df))
      
      # fitomassa = intercept_step * rang ^ slope_step ���������
      stat2 <- summary(lm(log(fitomassa) ~ log(rang), df))
      
      data <<- add_row(data, area = area, R_exp = stat[["adj.r.squared"]], 
                       intercept_exp = exp(stat[["coefficients"]][1, 1]), 
                       slope_exp = stat[["coefficients"]][2, 1],
                       R_step = stat2[["adj.r.squared"]], 
                       intercept_step = exp(stat2[["coefficients"]][1, 1]), 
                       slope_step = stat2[["coefficients"]][2, 1])
      
      
      if (saveDG) {
        # ������ ������������ ������������� ��������� �� ����� ��������
        ggplot(df, aes(x = rang, y = fitomassa)) +
          geom_line() + # ��������� �����
          geom_point() + # ���������� �����
          labs(x = "���� ����", y = "���������, %", title = name) + # ������� ����
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
        
        ggsave(paste('���������', name, '.jpeg'), width = 10, height = 8, 
               path = paste('�������� ���������', str_remove(filename, '.xlsx|.xls'))) # ���������� � ����
        cat(name, '��������� ���������\n') # ����� ���������
      }
    }
  }
  
  # �������� ����� ��� ���������� ��������, ���� ���������
  if (saveMD) {
    # �������� ������� �����. ���� ����, ��������
    if (dir.exists(paste('�������� ���������', str_remove(filename, '.xlsx|.xls')))) {
      unlink(paste('�������� ���������', str_remove(filename, '.xlsx|.xls')), recursive = TRUE)
    }
    dir.create(paste('�������� ���������', str_remove(filename, '.xlsx|.xls'))) # �������� ����� ��� ���������� ��������
  }
 
  
  # ���������� �������� �������� � ��������� ������� ����������
  cat('���������� �������� ������', 
      microbenchmark(apply(df2, 1, plotRang, saveDG = saveMD), times = 1)$time / 10^9, 
      '������(�)\n')
  
  # ���������� ������
  write.xlsx(data, paste0(str_remove(filename, '.xlsx|.xls'), '_rangs.xlsx'), sheetName = '���� 1')
  cat('����', paste0(str_remove(filename, '.xlsx|.xls'), '_rangs.xlsx'), '��������\n')

  
  # ���������� ��������
  if (mainDSave) {
    # ��� ���������
    # ����������� R-squared �� ������� ��������
    ggplot(data, aes(x = area, y = R_step)) +  
      # geom_line() + # ��������� �����
      geom_point() + # ���������� �����
      labs(x = "������� ��������", y = "R-squared", title = '���������') + # ������� ����
      theme_bw() # ������� ����
    
    ggsave(paste0(str_remove(filename, '.xlsx|.xls'), '_R_step.jpeg'), width = 10, height = 8) # ���������� � ����
    cat('������ ����������� R-squared �� ������� �������� ��� ��������� ��������\n')
    
    
    # ����������� Intercept �� ������� ��������
    ggplot(data, aes(x = area, y = intercept_step)) +  
      # geom_line() + # ��������� �����
      geom_point() + # ���������� �����
      labs(x = "������� ��������", y = "Intercept", title = '���������') + # ������� ����
      theme_bw() # ������� ����
    
    ggsave(paste0(str_remove(filename, '.xlsx|.xls'), '_I_step.jpeg'), width = 10, height = 8) # ���������� � ����
    cat('������ ����������� Intercept �� ������� �������� ��� ��������� ��������\n')
    
    
    # ����������� Slope �� ������� ��������
    ggplot(data, aes(x = area, y = slope_step)) +  
      # geom_line() + # ��������� �����
      geom_point() + # ���������� �����
      labs(x = "������� ��������", y = "Slope", title = '���������') + # ������� ����
      theme_bw() # ������� ����
    
    ggsave(paste0(str_remove(filename, '.xlsx|.xls'), '_S_step.jpeg'), width = 10, height = 8) # ���������� � ����
    cat('������ ����������� Slope �� ������� �������� ��� ��������� ��������\n')
    
    
    #��� �������������
    # ����������� R-squared �� ������� ��������
    ggplot(data, aes(x = area, y = R_exp)) +  
      # geom_line() + # ��������� �����
      geom_point() + # ���������� �����
      labs(x = "������� ��������", y = "R-squared", title = '�������������') + # ������� ����
      theme_bw() # ������� ����
    
    ggsave(paste0(str_remove(filename, '.xlsx|.xls'), '_R_exp.jpeg'), width = 10, height = 8) # ���������� � ����
    cat('������ ����������� R-squared �� ������� �������� ��� ������������� ��������\n')
    
    
    # ����������� Intercept �� ������� ��������
    ggplot(data, aes(x = area, y = intercept_exp)) +  
      # geom_line() + # ��������� �����
      geom_point() + # ���������� �����
      labs(x = "������� ��������", y = "Intercept", title = '�������������') + # ������� ����
      theme_bw() # ������� ����
    
    ggsave(paste0(str_remove(filename, '.xlsx|.xls'), '_I_exp.jpeg'), width = 10, height = 8) # ���������� � ����
    cat('������ ����������� Intercept �� ������� �������� ��� ������������� ��������\n')
    
    
    # ����������� Slope �� ������� ��������
    ggplot(data, aes(x = area, y = slope_exp)) +  
      # geom_line() + # ��������� �����
      geom_point() + # ���������� �����
      labs(x = "������� ��������", y = "Slope", title = '�������������') + # ������� ����
      theme_bw() # ������� ����
    
    ggsave(paste0(str_remove(filename, '.xlsx|.xls'), '_S_exp.jpeg'), width = 10, height = 8) # ���������� � ����
    cat('������ ����������� Slope �� ������� �������� ��� ������������� ��������\n')
  }
  cat('\n���������� ���������!\n\n') # ����� ���������
}

# ������
# ����� ��������� �������� ��������� ��� ������ ��������, ������� ������������.
# �� �� ����� ��������� ��������� ����������� intercept, slope � adjasted R-squared
ManyRangDiagram('Elnik_2009-2010-.xlsx', T, F)

