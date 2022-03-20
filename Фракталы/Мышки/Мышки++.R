#---------- подготовка данных
dfr <- data.frame(num = 1:10,
                  bt = 1:10,
                  loc = 1:10,
                  ls = rep(10, 10),
                  o1 = 1:10,
                  o2 = 2:11,
                  o3 = 3:12,
                  o4 = 4:13,
                  o5 = 10:1,
                  o6 = 11:2,
                  o7 = 1:10,
                  o8 = 2:11,
                  o9 = 5:14,
                  o10 = 15:6,
                  o11 = 1:10)
dfr$sum = sum(dfr$o1, dfr$o2, dfr$o3, dfr$o4, dfr$o5, dfr$o6, dfr$o7, dfr$o8, dfr$o9, dfr$o10, dfr$o11)


dfr <- read.csv('d:/Kireeva/R/fr/dfr.csv') 
#dfr[,1] - номер выборки 
#dfr[,2] - номер биотопа
#dfr[,3] - номер локалитета
#dfr[,4] - число ловушко-суток
#dfr[,5:15] - число особей 11 видов в выборке
#dfr[,16] - общее число особей в выборке 
pfr <- dfr[,5:15] / dfr[,16]    # расчет доли особей вида в выборке
N <- nrow(dfr)                # общее число выборок
nq <- 13                      # количество значений q
q <- seq(-3, 3, length.out = nq)  # значения q
h <- (3 - -3)/(nq - 1)		# количество диапазонов в интервале -3:3
mq <- array(1, dim = c(N, nq)) # массив для моментов
p.na.fr <- pfr


#---------- ликвидация нулей, чтобы затем исключить из расчетов
for (j in 1:11) {for (i in 1:N) {if (p.na.fr[i, j] == 0) {p.na.fr[i, j] <- NA}}}    


#---------- расчет матрицы моментов
for (i in 1:nq)
  {mq[,i] <- apply(p.na.fr^q[i], 1, sum, na.rm = TRUE)}
lmq <- log(mq)               # логарифмы моментов
lnum <- log(dfr$num)         # логарифмы масштабов
plot(lnum, lmq[, 5])           # диаграммы скейлинга моментов
cd <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)
matplot(lnum, lmq, xlab = 'LogN', main = "", ylab = 'LogM', pch = cd, col = 'black', cex = 0.7, bty = 'l', xlim = c(0, 10))
legend(9, 20, legend = c(1:13), pch = cd, text.font = 3)
s <- summary(lm(lmq ~ 0 + lnum))   # уравнения линии


#---------- расчет тау
t <- c(1:nq)                 # массив для тау
p <- c(1:nq) 
for (i in 1:nq)
{
  p[i] <- s[[i]][8] # 4 уровень значимости для линии
  #t[i] <- lmq[N,i]/lnum[N]
  t[i] = s[[i]]$coefficients[1]
}
plot(q, t, type = 'l', pch = 16, xlab = 'q', ylab = 't', lwd = 2, adj = 1, font.axis = 1, cex.axis = 1.2, cex.lab = 1.4)
print(cbind(q, p))            # вывод значимости линий для q 


#---------- расчет фрактальной размерности
aq <- rep(0, times = nq) 
fa <- rep(0, times = nq) 
dq <- rep(0, times = nq) 
for (i in 2:(nq - 1 ))
{
  aq[i] <- -(t[i + 1] - t[i - 1]) / (2 * h)
  fa[i] <- q[i] * aq[i] + t[i]
  dq[i] <- t[i] / (1 - q[i])
}
dq[1] = t[1] / (1 - q[1])
dq[9] = -sum(pfr[69,] * log(pfr[69,])) / lnum[69] 
matplot(q, dq, xlab = 'q', ylab = 'Dq', type = 'b', pch = 16, bty = 'l')


#---------- расчет показателей биоразнообразия
dq[7] <- t[7] / (1 - q[7])                      # 0 фрактальная размерность
dq[8] <- t[8] / (1 - q[8])                      # 0.5 Животовского
dq[9] <- -sum(pfr[N,] * log(pfr[N,])) / lnum[N] # (1) Шеннона
dq[11] <- t[11] / (1 - q[11])                   # 2   Симпсона


#---------- диаграмма фрактального спектра
plot(aq, fa, lab = c(4, 4, 5), pch = 16, type = 'p', adj = 1, cex.axis = 1.4, cex.lab = 1.4)

