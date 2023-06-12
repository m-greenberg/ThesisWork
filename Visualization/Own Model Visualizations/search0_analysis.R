if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, fitdistrplus, rlang, tidyverse)

# IMPORT STATEMENTS ----

path1  <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search0\\capital_expenditures.csv"
path2  <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search0\\consumption_expenditures.csv"
path3  <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search0\\final_income_dist.csv"
path4  <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search0\\final_wealth_dist.csv"
path5  <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search0\\income_deciles.csv"
path6  <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search0\\income_deciles_s1.csv"
path7  <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search0\\income_deciles_s2.csv"
path8  <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search0\\income_deciles_w.csv"
path9  <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search0\\income_gini.csv"
path10 <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search0\\labor_expenditures.csv"
path11 <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search0\\mean_income.csv"
path12 <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search0\\mean_income_s1.csv"
path13 <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search0\\mean_income_s2.csv"
path14 <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search0\\mean_income_w.csv"
path15 <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search0\\mean_wealth.csv"
path16 <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search0\\mean_wealth_s1.csv"
path17 <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search0\\mean_wealth_s2.csv"
path18 <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search0\\mean_wealth_w.csv"
path19 <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search0\\unemployment_rate.csv"
path20 <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search0\\wealth_deciles.csv"
path21 <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search0\\wealth_deciles_s1.csv"
path22 <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search0\\wealth_deciles_s2.csv"
path23 <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search0\\wealth_deciles_w.csv"
path24 <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search0\\wealth_gini.csv"

capital_expenditures     <- t(read.csv(path1, header=TRUE)[-1])
consumption_expenditures <- t(read.csv(path2, header=TRUE)[-1])
final_income_dist        <- t(read.csv(path3, header=TRUE)[-1])
final_wealth_dist        <- t(read.csv(path4, header=TRUE)[-1])
income_deciles           <- t(read.csv(path5, header=TRUE)[-1][-1])
income_deciles_s1        <- t(read.csv(path6, header=TRUE)[-1][-1])
income_deciles_s2        <- t(read.csv(path7, header=TRUE)[-1][-1])
income_deciles_w         <- t(read.csv(path8, header=TRUE)[-1][-1])
income_gini              <- t(read.csv(path9, header=TRUE)[-1])
labor_expenditures       <- t(read.csv(path10, header=TRUE)[-1])
mean_income              <- t(read.csv(path11, header=TRUE)[-1])
mean_income_s1           <- t(read.csv(path12, header=TRUE)[-1])
mean_income_s2           <- t(read.csv(path13, header=TRUE)[-1])
mean_income_w            <- t(read.csv(path14, header=TRUE)[-1])
mean_wealth              <- t(read.csv(path15, header=TRUE)[-1])
mean_wealth_s1           <- t(read.csv(path16, header=TRUE)[-1])
mean_wealth_s2           <- t(read.csv(path17, header=TRUE)[-1])
mean_wealth_w            <- t(read.csv(path18, header=TRUE)[-1])
unemployment_rate        <- t(read.csv(path19, header=TRUE)[-1])
wealth_deciles           <- t(read.csv(path20, header=TRUE)[-1][-1])
wealth_deciles_s1        <- t(read.csv(path21, header=TRUE)[-1][-1])
wealth_deciles_s2        <- t(read.csv(path22, header=TRUE)[-1][-1])
wealth_deciles_w         <- t(read.csv(path23, header=TRUE)[-1][-1])
wealth_gini              <- t(read.csv(path24, header=TRUE)[-1])


# CLASS WEALTH AND INCOME DISTRIBUTIONS ----
w_wealth <- final_wealth_dist[1001:5000]
w_income <- final_income_dist[1001:5000]
s1_wealth <- final_wealth_dist[1:500]
s1_income <- final_income_dist[1:500]
s2_wealth <- final_wealth_dist[501:1000]
s2_income <- final_income_dist[501:1000]

worker_wealth_hist <- hist(w_wealth, breaks=100, plot=FALSE)
worker_income_hist <- hist(w_income, breaks=20, plot=FALSE)
s1_wealth_hist <- hist(s1_wealth, breaks=100, plot=FALSE)
s1_income_hist <- hist(s1_income, breaks=100, plot=FALSE)
s2_wealth_hist <- hist(s2_wealth, breaks=50, plot=FALSE)
s2_income_hist <- hist(s2_income, breaks=50, plot=FALSE)

wx_w <- worker_wealth_hist$breaks[-1]
wy_w <- worker_wealth_hist$density
wx_i <- worker_income_hist$breaks[-1]
wy_i <- worker_income_hist$density

w_wealth_fit <- fitdist(w_wealth, distr = "gamma", method = "mle")
w_wealth_shape <- w_wealth_fit$estimate[1]
w_wealth_rate <- w_wealth_fit$estimate[2]

w_income_fit <- fitdist(w_income[w_income > 0], distr = "gamma", method = "mle")
w_income_shape <- w_income_fit$estimate[1]
w_income_rate <- w_income_fit$estimate[2]

lwx_w <- log(wx_w[wy_w > 0])
lwy_w <- log(wy_w[wy_w > 0])
lwx_i <- log(wx_i[wy_i > 0])
lwy_i <- log(wy_i[wy_i > 0])

s1x_w <- s1_wealth_hist$breaks[-1]
s1y_w <- s1_wealth_hist$density
s1x_i <- s1_income_hist$breaks[-1]
s1y_i <- s1_income_hist$density

s1_wealth_fit <- fitdist(s1_wealth, distr = "gamma", method = "mle")
s1_wealth_shape <- s1_wealth_fit$estimate[1]
s1_wealth_rate <- s1_wealth_fit$estimate[2]

s1it <- c(na.omit(s1_income[s1_income > 0][s1_income < 1000]))
s1_income_fit <- fitdist(s1it, distr = "gamma", method = "mle")
s1_income_shape <- s1_income_fit$estimate[1]
s1_income_rate <- s1_income_fit$estimate[2]

ls1x_w <- log(s1x_w[s1y_w > 0])
ls1y_w <- log(s1y_w[s1y_w > 0])
ls1x_i <- log(s1x_i[s1y_i > 0])
ls1y_i <- log(s1y_i[s1y_i > 0])

s2x_w <- s2_wealth_hist$breaks[-1]
s2y_w <- s2_wealth_hist$density
s2x_i <- s2_income_hist$breaks[-1]
s2y_i <- s2_income_hist$density

s2_wealth_fit <- fitdist(s2_wealth[s2_wealth > 0], distr = "gamma", method = "mle")
s2_wealth_shape <- s2_wealth_fit$estimate[1]
s2_wealth_rate <- s2_wealth_fit$estimate[2]
summary(s2_wealth_fit)

s2_income_fit <- fitdist(c(s2_income[s2_income > 0]), distr = "gamma", method = "mle")
s2_income_shape <- s2_income_fit$estimate[1]
s2_income_rate <- s2_income_fit$estimate[2]

ls2x_w <- log(s2x_w[s2y_w > 0])
ls2y_w <- log(s2y_w[s2y_w > 0])
ls2x_i <- log(s2x_i[s2y_i > 0])
ls2y_i <- log(s2y_i[s2y_i > 0])

ls2x_w_tail <- ls2x_w[ls2x_w > 5]
ls2y_w_tail <- ls2y_w[ls2x_w > 5]

reg_ls2_w <- lm(ls2y_w_tail ~ ls2x_w_tail)
reg_ls2_i <- lm(ls2y_i ~ ls2x_i)

b_ls2_w <- round(reg_ls2_w$coefficients[1], 2)
b_ls2_i <- round(reg_ls2_i$coefficients[1], 2)
m_ls2_w <- round(reg_ls2_w$coefficients[2], 2)
m_ls2_i <- round(reg_ls2_i$coefficients[2], 2)

par(mfrow=c(3,2))

plot(wx_w, wy_w, xlab='Wealth level', ylab='Density', main='Worker Wealth Distribution', col='red')
lines(seq(0.01, 120, by=0.01), dgamma(seq(0.01, 120, by=0.01), shape = w_wealth_shape, rate=w_wealth_rate), col = "red", lty = "dotted")
legend=legend(x='topright', legend=expr(paste(Gamma, '(', !!round(w_wealth_shape, 3), ',', !!round(w_wealth_rate, 3), ') ')), lty=2, col='red', cex=1)

plot(lwx_w, lwy_w, xlab='Log wealth level', ylab='Log density', main='Log Worker Wealth Distribution', col='red')

plot(s1x_w, s1y_w, xlab='Wealth level', ylab='Density', main='Sector 1 Wealth Distribution', col='blue')
lines(seq(0.01, 3000, by=0.01), dgamma(seq(0.01, 3000, by=0.01), shape = s1_wealth_shape, rate=s1_wealth_rate), col = "blue", lty = "dotted")
legend=legend(x='topright', legend=expr(paste(Gamma, '(', !!round(s1_wealth_shape, 3), ',', !!round(s1_wealth_rate, 3), ') ')), lty=2, col='blue', cex=1)

plot(ls1x_w, ls1y_w, xlab='Log wealth level', ylab='Log density', main='Log Sector 1 Wealth Distribution', col='blue')

plot(s2x_w, s2y_w, xlab='Wealth level', ylab='Density', main='Sector 2 Wealth Distribution', col='forestgreen')
lines(seq(0.01, 1200, by=0.01), dgamma(seq(0.01, 1200, by=0.01), shape = s2_wealth_shape, rate=s2_wealth_rate), col = "forestgreen", lty = "dotted")
legend=legend(x='topright', legend=expr(paste(Gamma, '(', !!round(s2_wealth_shape, 3), ',', !!round(s2_wealth_rate, 3), ') ')), lty=2, col='forestgreen', cex=1)

plot(ls2x_w, ls2y_w, xlab='Log wealth level', ylab='Log density', main='Log Sector 2 Wealth Distribution', col='forestgreen')
abline(reg_ls2_w, lty=2, col='forestgreen')
legend=legend(x='topright', legend=paste('y = ', m_ls2_w, '*x +', b_ls2_w), lty=2, col='forestgreen', cex=0.8)

par(mfrow=c(3, 2))

plot(wx_i, wy_i, xlab='Income level', ylab='Density', main='Worker Income Distribution', col='red')
lines(seq(0.01, 100, by=0.01), dgamma(seq(0.01, 100, by=0.01), shape = w_income_shape, rate=w_income_rate), col = "red", lty = "dotted")
legend=legend(x='topright', legend=expr(paste(Gamma, '(', !!round(w_income_shape, 3), ',', !!round(w_income_rate, 3), ') ')), lty=2, col='red', cex=1)

plot(lwx_i, lwy_i, xlab='Log income level', ylab='Log density', main='Log Worker Income Distribution', col='red')

plot(s1x_i, s1y_i, xlab='Income level', ylab='Density', main='Sector 1 Income Distribution', col='blue')
lines(seq(0.01, 2500, by=0.01), dgamma(seq(0.01, 2500, by=0.01), shape = s1_income_shape, rate=s1_income_rate), col = "blue", lty = "dotted")
legend=legend(x='topright', legend=expr(paste(Gamma, '(', !!round(s1_income_shape, 3), ',', !!round(s1_income_rate, 3), ') ')), lty=2, col='blue', cex=1)

plot(ls1x_i, ls1y_i, xlab='Log income level', ylab='Log density', main='Log Sector 1 Income Distribution', col='blue')

plot(s2x_i, s2y_i, xlab='Income level', ylab='Density', main='Sector 2 Income Distribution', col='forestgreen')
lines(seq(0.01, 1200, by=0.01), dgamma(seq(0.01, 1200, by=0.01), shape = s2_income_shape, rate=s2_income_rate), col = "forestgreen", lty = "dotted")
legend=legend(x='topright', legend=expr(paste(Gamma, '(', !!round(s2_income_shape, 3), ',', !!round(s2_income_rate, 3), ') ')), lty=2, col='forestgreen', cex=1)

plot(ls2x_i, ls2y_i, xlab='Log income level', ylab='Log density', main='Log Sector 2 Income Distribution', col='forestgreen')
abline(reg_ls2_i, lty=2, col='forestgreen')
legend=legend(x='topright', legend=paste('y = ', m_ls2_i, '*x +', b_ls2_i), lty=2, col='forestgreen', cex=1)

# UNEMPLOYMENT RATE ----
unemployment_hist <- hist(unemployment_rate, breaks=50, plot=FALSE)
uhist_x <- unemployment_hist$breaks[-1]
uhist_y <- unemployment_hist$density

plot(c(1:100), unemployment_rate, ylim=c(0, 0.5), xlab='Simulation Step', ylab='Unemployment Rate', main='Unemployment Rate', type='b', col='blue', lty='dotted')
par(fig = c(0.4,0.95,0.4,0.95), new = T)  
plot(uhist_x, uhist_y, xlim=c(0, 0.2), ylim=c(0, 55), xlab='Unemployment Rate', ylab='Density', col='blue')

# Cleanup ----

rm(list = ls()) 
p_unload(all) 
cat("\014")
