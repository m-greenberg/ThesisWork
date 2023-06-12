if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, markovchain)

# IMPORT STATEMENTS ----

path1  <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search4\\mean_wealth_s1.csv"
path2  <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search4\\mean_wealth_s2.csv"
path3  <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search4\\mean_wealth_w.csv"
path4  <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search4\\wealth_gini.csv"
path5  <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search4\\income_gini.csv"

path6  <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search5\\mean_wealth_s1.csv"
path7  <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search5\\mean_wealth_s2.csv"
path8  <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search5\\mean_wealth_w.csv"
path9  <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search5\\wealth_gini.csv"
path10  <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search5\\income_gini.csv"

path11  <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search6\\mean_wealth_s1.csv"
path12  <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search6\\mean_wealth_s2.csv"
path13  <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search6\\mean_wealth_w.csv"
path14  <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search6\\wealth_gini.csv"
path15  <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search6\\income_gini.csv"

path16  <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search7\\mean_wealth_s1.csv"
path17  <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search7\\mean_wealth_s2.csv"
path18  <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search7\\mean_wealth_w.csv"
path19  <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search7\\wealth_gini.csv"
path20  <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search7\\income_gini.csv"

path21  <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search8\\mean_wealth_s1.csv"
path22  <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search8\\mean_wealth_s2.csv"
path23  <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search8\\mean_wealth_w.csv"
path24  <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search8\\wealth_gini.csv"
path25  <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search8\\income_gini.csv"

wealth_sector1_wtr <- colMeans(t(read.csv(path1, header=TRUE)[-1][,c(6:100)])) / 1000
wealth_sector2_wtr <- colMeans(t(read.csv(path2, header=TRUE)[-1][,c(6:100)])) / 1000
wealth_workers_wtr <- colMeans(t(read.csv(path3, header=TRUE)[-1][,c(6:100)])) * 8 / 1000
wealth_gini_wtr <- colMeans(t(read.csv(path4, header=TRUE)[-1][,c(6:100)]))
income_gini_wtr <- colMeans(t(read.csv(path5, header=TRUE)[-1][,c(6:100)]))

wealth_sector1_itr <- colMeans(t(read.csv(path6, header=TRUE)[-1][,c(6:100)])) / 1000
wealth_sector2_itr <- colMeans(t(read.csv(path7, header=TRUE)[-1][,c(6:100)])) / 1000
wealth_workers_itr <- colMeans(t(read.csv(path8, header=TRUE)[-1][,c(6:100)])) * 8 / 1000
wealth_gini_itr <- colMeans(t(read.csv(path9, header=TRUE)[-1][,c(6:100)]))
income_gini_itr <- colMeans(t(read.csv(path10, header=TRUE)[-1][,c(6:100)]))

wealth_sector1_str <- colMeans(t(read.csv(path11, header=TRUE)[-1][,c(6:100)])) / 1000
wealth_sector2_str <- colMeans(t(read.csv(path12, header=TRUE)[-1][,c(6:100)])) / 1000
wealth_workers_str <- colMeans(t(read.csv(path13, header=TRUE)[-1][,c(6:100)])) * 8 / 1000
wealth_gini_str <- colMeans(t(read.csv(path14, header=TRUE)[-1][,c(6:100)]))
income_gini_str <- colMeans(t(read.csv(path15, header=TRUE)[-1][,c(6:100)]))

wealth_sector1_ptr <- colMeans(t(read.csv(path16, header=TRUE)[-1][,c(6:100)])) / 1000
wealth_sector2_ptr <- colMeans(t(read.csv(path17, header=TRUE)[-1][,c(6:100)])) / 1000
wealth_workers_ptr <- colMeans(t(read.csv(path18, header=TRUE)[-1][,c(6:100)])) * 8 / 1000
wealth_gini_ptr <- colMeans(t(read.csv(path19, header=TRUE)[-1][,c(6:100)]))
income_gini_ptr <- colMeans(t(read.csv(path20, header=TRUE)[-1][,c(6:100)]))

wealth_sector1_ttr <- colMeans(t(read.csv(path21, header=TRUE)[-1][,c(6:100)])) / 1000
wealth_sector2_ttr <- colMeans(t(read.csv(path22, header=TRUE)[-1][,c(6:100)])) / 1000
wealth_workers_ttr <- colMeans(t(read.csv(path23, header=TRUE)[-1][,c(6:100)])) * 8 / 1000
wealth_gini_ttr <- colMeans(t(read.csv(path24, header=TRUE)[-1][,c(6:100)]))
income_gini_ttr <- colMeans(t(read.csv(path25, header=TRUE)[-1][,c(6:100)]))

# THEORETICAL PREDICTIONS ----
gen_X <- function (gam=0.5, wtr=0, itr=0, str=0, ptr=0, ttr=0) {
  X <- c(1/(4*(1 + str)), gam/(4*(1 + ttr)), (1-gam)/(4*(1 + ptr)),
         1/(4*(1 + str)), gam/(4*(1 + ttr)), (1-gam)/(4*(1 + ptr)),
         1/(2*(1+str)),   0,                 0)
  dim(X) <- c(3,3)
  return (t(X))
}

gen_T_W <- function(wtr=0) {
  return (wtr * diag(3))
}

gen_R <- function(N=5000, q_cap=0.2, q_S1=0.5) {
  N_C <- ceiling(N * q_cap)
  N_S1 <- ceiling(N * q_cap * q_S1)
  N_S2 <- N_C - N_S1
  N_W <- N - N_C
  RN <- rep(c(N_S1, N_S2, N_W), 3)
  dim(RN) <- c(3,3)
  return (t(RN)/N)
}

gen_P <- function(N=5000, q_cap=0.2, q_S1=0.5, gam=0.5, wtr=0, itr=0, str=0, ptr=0, ttr=0) {
  I <- diag(3)
  X <- gen_X(gam, wtr, itr, str, ptr, ttr)
  T_W <- gen_T_W(wtr)
  R <- gen_R(N, q_cap, q_S1)
  return ((I/2 + (1 - itr) * X) %*% (I - T_W) %*% (I - R) + R)
}

prediction <- function(N=5000, q_cap=0.2, q_S1=0.5, gam=0.5, wtr=0, itr=0, str=0, ptr=0, ttr=0) {
  P <- gen_P(N, q_cap, q_S1, gam, wtr, itr, str, ptr, ttr)
  chain <- new("markovchain", states = c('S_1', 'S_2', 'W'), transitionMatrix = P)
  return (steadyStates(chain))
}

prediction_wtr <- prediction_itr <- prediction_str <- prediction_ptr <- prediction_ttr <- matrix(NA, 101, 3)

for (i in c(1:101)) {
  tax_rate <- (i - 1)/100
  prediction_wtr[i,] <- prediction(wtr=tax_rate)
  prediction_itr[i,] <- prediction(itr=tax_rate)
  prediction_str[i,] <- prediction(str=tax_rate)
  prediction_ptr[i,] <- prediction(ptr=tax_rate)
  prediction_ttr[i,] <- prediction(ttr=tax_rate)
}

# CLASS DIVISION OF WEALTH PLOTS ----
x <- seq(0, 1, 0.01)

par(mfrow=c(2,1), mar=c(4, 4, 2, 11), xpd=TRUE)

plot(x, wealth_sector1_wtr, xlim=c(0, 1), ylim=c(0, 1), xlab='Tax Rate', ylab='Fraction of System Wealth', main='Effect of Wealth and Income Taxes on Class Division of Wealth', col='red')
points(x, wealth_sector1_itr, pch=2, col='red')
points(x, wealth_sector2_wtr, col='dodgerblue')
points(x, wealth_sector2_itr, pch=2, col='dodgerblue')
points(x, wealth_workers_wtr, col='green')
points(x, wealth_workers_itr, pch=2, col='green')
lines(x, prediction_wtr[,1], lty=1, col='darkred')
lines(x, prediction_itr[,1], lty=5, col='darkred')
lines(x, prediction_wtr[,2], lty=1, col='darkblue')
lines(x, prediction_itr[,2], lty=5, col='darkblue')
lines(x, prediction_wtr[,3], lty=1, col='forestgreen')
lines(x, prediction_itr[,3], lty=5, col='forestgreen')
legend("right", inset=c(-0.29, 0),
       legend=c('Sector 1 (wealth tax)', 'Sector 1 (income tax)',
                'Sector 2 (wealth tax)', 'Sector 2 (income tax)',
                'Workers (wealth tax)', 'Workers (income tax)'), 
       col=c('red', 'red', 'dodgerblue', 'dodgerblue', 'green', 'green'), 
       pch=c(1, 2, 1, 2, 1, 2), lty=c(1, 5, 1, 5, 1, 5), cex=0.8, title=expression(bold('Legend')))

plot(x, wealth_sector1_str, xlim=c(0, 1), ylim=c(0, 0.7), xlab='Tax Rate', ylab='Fraction of System Wealth', main='Effect of Sales, Payroll, and Turnover Taxes on Class Division of Wealth', pch=0, col='red')
points(x, wealth_sector1_ptr, pch=1, col='red')
points(x, wealth_sector1_ttr, pch=2, col='red')
points(x, wealth_sector2_str, pch=0, col='dodgerblue')
points(x, wealth_sector2_ptr, pch=1, col='dodgerblue')
points(x, wealth_sector2_ttr, pch=2, col='dodgerblue')
points(x, wealth_workers_str, pch=0, col='green')
points(x, wealth_workers_ptr, pch=1, col='green')
points(x, wealth_workers_ttr, pch=2, col='green')
lines(x, prediction_str[,1], lty=1, col='darkred')
lines(x, prediction_ptr[,1], lty=5, col='darkred')
lines(x, prediction_ttr[,1], lty=6, col='darkred')
lines(x, prediction_str[,2], lty=1, col='darkblue')
lines(x, prediction_ptr[,2], lty=5, col='darkblue')
lines(x, prediction_ttr[,2], lty=6, col='darkblue')
lines(x, prediction_str[,3], lty=1, col='forestgreen')
lines(x, prediction_ptr[,3], lty=5, col='forestgreen')
lines(x, prediction_ttr[,3], lty=6, col='forestgreen')
legend("right", inset=c(-0.29, 0),
       legend=c('Sector 1 (sales tax)', 'Sector 1 (payroll tax)', 'Sector 1 (turnover tax)',
                'Sector 2 (sales tax)', 'Sector 2 (payroll tax)', 'Sector 2 (turnover tax)',
                'Workers (sales tax)', 'Workers (payroll tax)', 'Workers (turnover tax)'), 
       col=c('red', 'red', 'red', 'dodgerblue', 'dodgerblue', 'dodgerblue', 'green', 'green', 'green'), 
       pch=c(0,1,2,0,1,2,0,1,2), lty=c(1,5,6,1,5,6,1,5,6), cex=0.8, title=expression(bold('Legend')))


# GINI PLOTS ----
par(mfrow=c(2,1), mar=c(4,4, 2, 9), xpd=TRUE)

plot(x, wealth_gini_wtr, xlim=c(0, 1), ylim=c(0, 1), xlab='Tax Rate', ylab='Gini coefficient', main='Effect of Wealth and Income Taxes on Gini Coefficients', col='red')
points(x, wealth_gini_itr, pch=0, col='red')
points(x, income_gini_wtr, col='dodgerblue')
points(head(x, -1), head(income_gini_itr, -1), pch=0, col='dodgerblue')
legend('right', inset=c(-0.225, 0),
       legend=c(expression(G[W]~'(wealth tax)'), expression(G[W]~'(income tax)'), 
                expression(G[I]~'(wealth tax)'), expression(G[I]~'(income tax)')),
       col=c('red','red','dodgerblue','dodgerblue'), pch = c(1,0,1,0), 
       cex=0.8, title=expression(bold('Legend')))

plot(x, wealth_gini_str, pch=0, xlim=c(0, 1), ylim=c(0.45, 0.85), xlab='Tax Rate', ylab='Gini Index', main='Effect of Sales, Payroll, and Turnover Taxes on Gini Indices', col='red')
points(x, wealth_gini_ptr, pch=1, col='red')
points(x, wealth_gini_ttr, pch=2, col='red')
points(x, income_gini_str, pch=0, col='dodgerblue')
points(x, income_gini_ptr, pch=1, col='dodgerblue')
points(x, income_gini_ttr, pch=2, col='dodgerblue')
legend('right', inset=c(-0.225, 0),
       legend=c(expression(G[W]~'(sales tax)'), expression(G[W]~'(payroll tax)'), expression(G[W]~'(turnover tax)'),
                expression(G[I]~'(sales tax)'), expression(G[I]~'(payroll tax)'), expression(G[I]~'(turnover tax)')),
       col=c('red','red', 'red', 'dodgerblue', 'dodgerblue','dodgerblue'), pch = c(0, 1, 2, 0, 1, 2), 
       cex=0.8, title=expression(bold('Legend')))
# CLEANUP ----
rm(list = ls()) 
p_unload(all) 
cat("\014")