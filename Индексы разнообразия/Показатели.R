library(xlsx) # ���������� ����� ��� ������ � ���������
library(vegan) # ���������� ����� ��� �������� ��������� �������������

indexes <- function(name, sheetindex){
  diversity.zhiv <- function(x, MARGIN = 1) # ������� ��� ���������� �������� ������������, 
    # ���������� �� �������� � �������� diversity ������ vegan.
  {
    x <- drop(as.matrix(x)) # �������������� �� ���������� � �������
    if (!is.numeric(x)) # �������� ������������ ��������
      stop("input data must be numeric")
    if (any(x < 0, na.rm = TRUE)) # �������� ������������ ��������
      stop("input data must be non-negative")
    if (length(dim(x)) > 1) { # ���� ��������� �����
      total <- apply(x, MARGIN, sum) # ������� ����
      x <- sweep(x, MARGIN, total, "/") # ������� �� ���������� �������� � �������������
    }
    else { # ���� ���� ������
      x <- x/(total <- sum(x)) # ������� �� ���������� �������� � �������������
    }
    x <- sqrt(x) # ������ �������� �� ���������� �����
    if (length(dim(x)) > 1) # ���� ��������� �����
      H <- apply(x, MARGIN, sum, na.rm = TRUE) # ������� ����� ������
    else H <- sum(x, na.rm = TRUE)
    H <- H * H # ���������� � �������
    if (any(NAS <- is.na(total)))  # �������� ������������ ��������
      H[NAS] <- NA
    H # ����������� ����������
  }
  
  df <- read.xlsx(name, sheetIndex = sheetindex) # �������� ������� ������ �� �������
  if (!is.numeric(df[1,1])) # ���� � ������ ������� �������� �����
    df <- df[-1] # ������ ������ �������
  
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
  
  write.xlsx(result, file = paste("result", name)) # ���������� ���������� � ����
  
  result # ���������� ����������
}

indexes('data.xlsx', 2) # �������� ������/������


