# ���������� ������ (����� - ���������� ��������. ������ ���� ��������.)
options(java.parameters = '-Xmx8g')

# ����������� ����������� ���������
library(xlsx)
library(tidyverse)
library(microbenchmark)


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
# ������ �������� (savefile) - ��� True �������� ���������� � ���� Excel (��������������)
# ��������! ��� savefile = T ����� ������������� ����� ����������� ������ 
# � ������ ����� �������� ����������� ������ �������
MFAFallCum <- function(filename, Q = seq(from = -2, to = 2, by = 1), savefile = F){
  
  cat('�������� ������\n') # ����� ���������
  # �������� �������
  data <- read.xlsx(filename, sheetIndex = 1) # �������� ������� ������ �� �������
  if (!is.numeric(data[1,1])) # ���� � ������ ������� �������� �����
    data <- data[,-1] # ������ ������ �������
  cat('������ ���������\n') # ����� ���������
  filename <- str_remove(filename, '.xlsx|.xls') # ���������� ���������� ����� �� �����
  filename2 <- paste0(filename, '_analyse.xlsx')
  
  # �������� ������������� �������
  df <- SuperCum(data)
  df$sum <- apply(df[, 1:(ncol(df) - 2)], 1, sum) # ������ ���� �� �������
  df <- arrange(df, sum) # ���������� �� �����
  cat('������������ ������ ������\n') # ����� ���������
  
  if (savefile) {
    # ���������� ������ � ����
    write.xlsx(df, filename2, sheetName = '������������')
    cat('����', filename2, '��������\n\n') # ����� ���������
  }

  df <- select(df, -sum, -name, -area) # ���������� ��������
  
  
  cat(rep('-', 80), sep = '') # ����������� �������
  cat('\n���� 1. ��������� ����������� ���������� �������� ��������� S �� ������ ������� N\n')
  # ���� 1 (S �� N) ----
  df1 <- data.frame(N = apply(df, 1, sum), # ������� ��������� �� ���������
                    S = apply(df, 1, function(x) length(x[x > 0]))) # ������� ���������� ����� �� ���������
  
  if (savefile) {
    # ���������� ������ � ����
    write.xlsx(df1, filename2, sheetName = '���� � ���������', append = T)
    cat('����', filename2, '��������\n\n') # ����� ���������
  }
  
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
  
  if (savefile) {
    # ���������� ������ � ����
    write.xlsx(df2, filename2, sheetName = '�������', append = T)
    cat('����', filename2, '��������\n') # ����� ���������
  }
  
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
    
    M <- sapply(q, FUN = function(i) sum(p^i)) # ������� �������
    t <- log(M)/log(sum(v)) # ������������ ������������ ����������
    D <- t/(1 - q) # ������������ ���������� ����������� �����
    
    a <- (t[1:(length(t) - 1)] - t[2:length(t)])/step # ��������� ������� ����������� �� ���������� ����� (������� �������������)
    f <- q[-1]*a + t[-1] # ������� ������ ��������������
    
    return(list(vars = data.frame(q = q, M = M, t = t, D = D), mfs = data.frame(a = a, f = f))) 
  }
  # �������� ���������� �� ������������� ������������
  df3 <- mfa(df[nrow(df),])$vars
 
  if (savefile) {
    # ���������� ������ � ����
    write.xlsx(df3, filename2, sheetName = '������������ ����������', append = T)
    cat('����', filename2, '��������\n') # ����� ���������
  }
  
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
  
  if (savefile) {
    # ���������� ������ � ����
    write.xlsx(df5, filename2, sheetName = '�����������', append = T)
    cat('����', filename2, '��������\n') # ����� ���������
  }
  
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
  
  if (savefile) {
    # ���������� ������ � ����
    write.xlsx(mf$mfs, filename2, sheetName = '����������������� ������', append = T)
    cat('����', filename2, '��������\n') # ����� ���������
  }
  
  ggplot(mf$mfs, aes(x = a, y = f)) +
    geom_line() + # ��������� �����
    labs(x = "\u03B1", y = "f(\u03B1)", title = '����������������� ������') + # ������� ����
    theme_bw() # ������� ����
  ggsave(paste0('���� 6 ', filename, '.jpeg'), width = 10, height = 8) # ���������� � ����
  cat('������ ������� ����� ��������\n\n') # ����� ���������
  
  cat(rep('-', 80), sep = '') # ����������� �������
  cat('\n������ �������� ������������\n')
  # ������ �������� ������������ ----
  
  # ������� ��� ������� �������� ������������
  indexes <- function(df){
    library(xlsx) # ���������� ����� ��� ������ � ���������
    library(vegan) # ���������� ����� ��� �������� ��������� �������������
    diversity.zhiv <- function(x, MARGIN = 1) # ������� ��� ���������� �������� ������������, 
      # ������������ �� �������� � �������� diversity ������ vegan.
    {
      x <- drop(as.matrix(x)) # �������������� �� ���������� � �������
      if (!is.numeric(x)) # �������� ������������ ��������
        stop("input data must be numeric")
      if (any(x < 0, na.rm = TRUE)) # �������� ������������ ��������
        stop("input data must be non-negative")
      if (length(dim(x)) > 1) { # ���� ��������� �����
        total <- apply(x, MARGIN, sum) # ������ ����
        x <- sweep(x, MARGIN, total, "/") # ������� �� ���������� �������� � �������������
      }
      else { # ���� ���� ������
        x <- x/(total <- sum(x)) # ������� �� ���������� �������� � �������������
      }
      x <- sqrt(x) # ������ �������� �� ���������� �����
      if (length(dim(x)) > 1) # ���� ��������� �����
        H <- apply(x, MARGIN, sum, na.rm = TRUE) # ������ ����� ������
      else H <- sum(x, na.rm = TRUE)
      H <- H * H # ���������� � �������
      if (any(NAS <- is.na(total)))  # �������� ������������ ��������
        H[NAS] <- NA
      H # ����������� ����������
    }
    
    result <- data.frame( # �������� ��������� � ������������
      N = apply(df, FUN = sum, na.rm = TRUE, MARGIN = 1), # ����� �����������, ��� ����� ���� �������� �� �������
      S = specnumber(df)) # ������� ���������, ������� �� ������ vegan
    result$k <- log(result$S)/log(result$N) # ���������� ���������, ������ �� �������
    result$d <- (result$S - 1)/log(result$N) # ������ �������� ��������� ���������
    result$H <- diversity(df, index = "shannon") # ������ �������, ������� �� ������ vegan
    result$e <- result$H/log(result$S) # ������ ������������� �����, ������ �� �������
    result$� <- 1 - diversity(df, index = "simpson") # ������ ������������� ��������, ������� �� ������ vegan
    result$m <- diversity.zhiv(df) # ������ ������������
    result$h <- 1 - result$m / result$S # ���� ������ ����� ������������, ������ �� �������
    
    result # ���������� ����������
  }
  
  if (savefile) {
  # ���������� ������ � ����
  write.xlsx(indexes(df), filename2, sheetName = '�������', append = T)
  cat('����', filename2, '��������\n\n') # ����� ���������
  }
  cat('���������� ���������!\n\n') # ����� ���������
}

# ������
# ����� ���������� ������� ����������� �����
microbenchmark(MFAFallCum('Elnik_2009-2010.xlsx', savefile = T), times = 1)
