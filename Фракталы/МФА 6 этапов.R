# ����������� ����������� ���������
library(xlsx)
library(ggplot2)
library(tidyverse)
library(stringr)

# ������� ��� ���������� 6 ��������:
# 1) ��������� ����������� ���������� �������� ��������� S �� ������ ������� N;
# 2) ��������� ����������� �������� ������������� ������ �� ����� Mq �� ������ ������� N;
# 3) ������������ ����������� ������������ ����������� t �� ������� ������� q;
# 4) ������������ ���������� ������������ ����� Dq �� ������� ������� q;
# 5) �������������� ���������� ������������ ������������ ����� ����� � ����������� ����������;
# 6) ����������������� ������.
# ��� ��������� ����������� � ������� ����������.
# ������ �������� (filename) - ��� ����� � �������.
# ������ �� ������ ����� � ����� (�� �����, �� �� ��������)
# � ������ ������ - �������� �����, 
# � ������ ������� - �������� ��������
# ������ �������� (Q) - ������ �������� q ��� 2 � 5 �������� (��������������)
MFAF <- function(filename, Q = seq(from = -2, to = 2, by = 1)){
  # �������� �������
  data <- read.xlsx(filename, sheetIndex = 1) # �������� ������� ������ �� �������
  if (!is.numeric(data[1,1])) # ���� � ������ ������� �������� �����
    data <- data[,-1] # ������ ������ �������
  cat('������ ���������\n') # ����� ���������
  
  # �������� ������������� �������
  df <- data[1,]
  for (i in 2:nrow(data)) {
    df[i,] <- df[i - 1,] + data[i,]
  }
  cat('������������ ������ ������\n') # ����� ���������
  
  filename <- str_remove(filename, '.xlsx|.xls') # ���������� ���������� ����� �� �����
 
  cat(rep('-', 80), sep = '') # ����������� �������
  cat('\n���� 1. ��������� ����������� ���������� �������� ��������� S �� ������ ������� N\n')
  # ���� 1 (S �� N) ----
  df1 <- data.frame(N = apply(df, 1, sum), # ������� ��������� �� ���������
                    S = apply(df, 1, function(x) length(x[x > 0]))) # ������� ���������� ����� �� ���������
  
  ggplot(df1, aes(x = N, y = S)) +  
    # geom_line() + # ��������� �����
    geom_point() + # ���������� �����
    labs(x = "N", y = "S", title = '��������� ����������� ���������� �������� ��������� S �� ������ ������� N') + # ������� ����
    theme_bw() + # ������� ����
    scale_x_log10() + # ����������������� �������
    scale_y_log10() 
  ggsave(paste0('���� 1 ', filename, '.jpeg'), width = 10, height = 8) # ���������� � ����
  cat('������ ������� ����� ��������\n') # ����� ���������
  
  # ����� ���������� ���������
  stat <- summary(lm(log(S) ~ log(N), df1))
  cat('����������� �������� ��������� S �� ������ ������� N\n',
      'S = ', exp(stat[["coefficients"]][1, 1]), ' * N ^ ', stat[["coefficients"]][2, 1],
      '; Adjusted R-squared:  ', stat[["adj.r.squared"]], '\n\n', sep = '')
  
  
  cat(rep('-', 80), sep = '') # ����������� �������
  cat('\n���� 2. ��������� ����������� �������� ������������� ������ �� ����� Mq �� ������ ������� N\n')
  # ���� 2 (Mq �� N) ----
  # ������� �������������� �������
  graph2 <- function(v){
    v <- v[v != 0]
    p <- v/sum(v) # ��������� � ������������� ��������
    M <- sapply(Q, function(i) sum(p^i)) # ������� �������
  }
  
  df2 <- as.data.frame(cbind(t(apply(df, 1, graph2)), apply(df, 1, sum))) # �������� ������� � ���������
  colnames(df2) <- c(Q, 'N') # ���������������� ��������
  
  # ������� ���������� �� �������� � �������
  df2  <-  df2 %>% 
    pivot_longer(cols = colnames(df2)[1]:colnames(df2)[length(Q)], 
                 names_to = 'q', 
                 values_to = 'Mq') %>% 
    filter(q != 1)
  
  ggplot(df2, aes(x = N, y = Mq, col = q)) +
    # geom_line() + # ��������� �����
    geom_point() + # ���������� �����
    labs(x = "N", y = "Mq", title = '��������� ����������� �������� ������������� ������ �� ����� Mq �� ������ ������� N') + # ������� ����
    theme_bw() + # ������� ����
    scale_x_log10() + # ����������������� �������
    scale_y_log10() 
  ggsave(paste0('���� 2 ', filename, '.jpeg'), width = 10, height = 8) # ���������� � ����
  cat('������ ������� ����� ��������\n') # ����� ���������
  
  # ����� ���������� ���������
  cat('����������� �������� Mq �� ������ ������� N\n')
  for (i in 1:length(Q)) {
    try({
      stat <- summary(lm(log(Mq) ~ log(N), df2, q == Q[i]))
      cat('q = ', Q[i], '; ',
          'Mq = ', exp(stat[["coefficients"]][1, 1]), ' * N ^ ', stat[["coefficients"]][2, 1],
          '; Adjusted R-squared: ', stat[["adj.r.squared"]], '\n', sep = '')
    }, silent = T)
  } 
  cat('\n')
  
  
  cat(rep('-', 80), sep = '') # ����������� �������
  cat('\n���� 3. ������������ ����������� ������������ ����������� t �� ������� ������� q\n')
  # ���� 3 (t �� q) ----
  # ������� ��� ���
  mfa <- function(v, Qmin = -7, Qmax = 5, step = 0.01){ 
    v <- v[v != 0]
    p <- v/sum(v) # ��������� � ������������� ��������
    q <- seq(Qmin, Qmax, step) # ������� ������ �������� �������� ������������� ������ (���������) �� �����
    
    M <- sapply(q, function(i) sum(p^i)) # ������� �������
    t <- log(M)/log(sum(v)) # ������������ ������������ ����������
    D <- t/(1 - q) # ������������ ���������� ����������� �����
    
    a <- (t[1:(length(t) - 1)] - t[2:length(t)])/step # ��������� ������� ����������� �� ���������� ����� (������� �������������)
    f <- q[-1]*a + t[-1] # ������� ������ ��������������
    
    return(list(vars = data.frame(q = q, M = M, t = t, D = D), mfs = data.frame(a = a, f = f))) 
  }
  # �������� ���������� �� ������������� ������������
  df3 <- mfa(df[nrow(df),])$vars
  
  ggplot(df3, aes(x = q, y = t)) +
    geom_line() + # ��������� �����
    labs(x = "q", y = "t", title = '������������ ����������� ������������ ����������� t �� ������� ������� q') + # ������� ����
    theme_bw() # ������� ����
  ggsave(paste0('���� 3 ', filename, '.jpeg'), width = 10, height = 8) # ���������� � ����
  cat('������ �������� ����� ��������\n\n') # ����� ���������
  
  
  cat(rep('-', 80), sep = '') # ����������� �������
  cat('\n���� 4. ������������ ����������� ���������� ������������ ����� Dq �� ������� ������� q\n')
  # ���� 4 (Dq �� q) ----
  # ������������ ������ �� ����������, ����������� �� ���������� ����
  ggplot(df3[df3$q != 1,], aes(x = q, y = D)) +
    geom_line() + # ��������� �����
    labs(x = "q", y = "Dq", title = '������������ ����������� ���������� ������������ ����� Dq �� ������� ������� q') + # ������� ����
    theme_bw() # ������� ����
  ggsave(paste0('���� 4 ', filename, '.jpeg'), width = 10, height = 8) # ���������� � ����
  cat('������ ���������� ����� ��������\n\n') # ����� ���������
  
  
  cat(rep('-', 80), sep = '') # ����������� �������
  cat('\n���� 5. �������������� ���������� ������������ ������������ ����� ����� � ����������� ����������\n')
  # ���� 5 (Dq �� N) ----
  # ������� �������������� �����������
  graph5 <- function(v){
    v <- v[v != 0]
    p <- v/sum(v) # ��������� � ������������� ��������
    D <- sapply(Q, function(i) log(sum(p^i))/((1 - i) * log(sum(v)))) # ������� �����������
  }
  
  df5 <- as.data.frame(cbind(t(apply(df, 1, graph5)), apply(df, 1, sum))) # �������� ������� � �������������
  colnames(df5) <- c(Q, 'N') # ���������������� ��������
  
  # ������� ���������� �� �������� � �������
  df5  <-  df5 %>% 
    pivot_longer(cols = colnames(df5)[1]:colnames(df5)[length(Q)], 
                 names_to = 'q', 
                 values_to = 'Dq')
  
  ggplot(df5, aes(x = N, y = Dq, col = q)) +
    # geom_line() + # ��������� �����
    geom_point() + # ���������� �����
    labs(x = "N", y = "Dq", title = '�������������� ���������� ������������ ������������ ����� ����� � ����������� ����������') + # ������� ����
    theme_bw() + # ������� ����
    scale_x_log10() + # ����������������� �������
    scale_y_log10() 
  ggsave(paste0('���� 5 ', filename, '.jpeg'), width = 10, height = 8) # ���������� � ����
  cat('������ ������ ����� ��������\n') # ����� ���������
  
  # ����� ���������� ���������
  cat('�������������� ���������� ������������ Dq\n')
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
  
  
  cat(rep('-', 80), sep = '') # ����������� �������
  cat('\n���� 6. ����������������� ������\n')
  # ���� 6 (����������������� ������) ----
  v <- data[nrow(data),] # ������� ������� � ���������� ���������� ��������
  
  mf <- mfa(v, step = 0.01) # ������ ������������������ �������
  
  ggplot(mf$mfs, aes(x = a, y = f)) +
    geom_line() + # ��������� �����
    labs(x = "\u03B1", y = "f(\u03B1)", title = '����������������� ������') + # ������� ����
    theme_bw() # ������� ����
  ggsave(paste0('���� 6 ', filename, '.jpeg'), width = 10, height = 8) # ���������� � ����
  cat('������ ������� ����� ��������\n\n') # ����� ���������
  cat('���������� ���������!\n\n') # ����� ���������
}

# ������
MFAF('Elnik_2009-2010.xlsx')


# ������� ��� ���������� ��������� 
# "��������� ����������� ���������� �������� ��������� S �� ������ ������� N"
# ��� ���������� �������� ������ ��� ���������
# ������ �������� (file) - ������ � ������� ������
# ������ �������� (names) - ������ � ����������, ������� ����� ���������� �� ��������� (��������������)
SNmany <- function(files, names = files) {
  
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
    
    if (i == 1) 
      df1 <- data.frame(N = apply(df, 1, sum),
                        S = apply(df, 1, function(x) length(x[x > 0])),
                        sp = names[i]) else
      df1 <- rbind(df1, data.frame(N = apply(df, 1, sum),
                                   S = apply(df, 1, function(x) length(x[x > 0])),
                                   sp = names[i]))
    cat(files[i], ' ��������\n') # ����� ���������
  }
  
  ggplot(df1, aes(x = N, y = S, col = sp)) +
    geom_line() + # ��������� �����
    geom_point() + # ���������� �����
    labs(x = "N", y = "S", col = '����', title = '��������� ����������� ���������� �������� ��������� S �� ������ ������� N') + # ������� ����
    theme_bw() + # ������� ����
    scale_x_log10() + # ����������������� �������
    scale_y_log10() 
  ggsave('���������.jpeg', width = 10, height = 8) # ���������� � ����
  cat('��������� ���������\n') # ����� ���������
}

# ������
files <- c('�������� ������� ���� 2.xlsx', 'Elnik_sum.xls', 'Lug_sum.xls', 
          'P7-KP_bereznyak_ne_polny.xlsx', 'Korennye_Sbr_yag.xlsx', 'Korennye_Srtr.xlsx')
names <- c('�������� ������� ����', '�������', '����', 
           '��������', '�������� ���', '�������� ����')
SNmany(files, names)

