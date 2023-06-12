if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, fitdistrplus, rlang, tidyverse)

wp_path <- "C:\\Users\\maxgr\\Desktop\\visualizations\\wright_pop.csv"
ws_path <- "C:\\Users\\maxgr\\Desktop\\visualizations\\wright_size.csv"

wp <- t(read.csv(wp_path, header=TRUE)[-1])
ws <- t(read.csv(ws_path, header=TRUE)[-1])

wealth <- wp[,1]
income <- wp[,2]

n_c <- ws[1,]/5000
n_w <- ws[2,]/5000
n_u <- ws[3,]/5000

wealth_hist <- hist(wealth, breaks=2000, plot=FALSE)
income_hist <- hist(income, breaks=2000, plot=FALSE)

x_w <- wealth_hist$breaks[-1]
y_w <- wealth_hist$density

x_i <- income_hist$breaks[-1]
y_i <- income_hist$density

lx_w <- log(x_w)
ly_w <- log(y_w)

lx_i <- log(x_i)
ly_i <- log(y_i)

nch <- hist(n_c, breaks=10, plot=FALSE)
nwh <- hist(n_w, breaks=10, plot=FALSE)
nuh <- hist(n_u, breaks=10, plot=FALSE)

wealth_fit <- fitdist(c(wealth[wealth > 0]), distr = "gamma", method = "mle")
wealth_shape <- wealth_fit$estimate[1]
wealth_rate <- wealth_fit$estimate[2]

income_fit <- fitdist(c(income[income > 1][income < 1000]), distr = "gamma", method = "mle")
income_shape <- income_fit$estimate[1]
income_rate <- income_fit$estimate[2]

w_tail_x <- lx_w[lx_w > 4.5][lx_w < 6]
w_tail_y <- ly_w[lx_w > 4.5][lx_w < 6]

i_tail_x <- lx_i[lx_i > 6.5][lx_i < 8]
i_tail_y <- ly_i[lx_i > 6.5][lx_i < 8]

reg1 <- lm(w_tail_y ~ w_tail_x)
reg2 <- lm(i_tail_y ~ i_tail_x)

intercept1 <- round(reg1$coefficients[1], 2)
intercept2 <- round(reg2$coefficients[1], 2)
slope1 <- round(reg1$coefficients[2], 2)
slope2 <- round(reg2$coefficients[2], 2)

par(mfrow=c(3,2))

plot(x_w, y_w, xlab='Wealth level', ylab='Density', xlim=c(0, 500), col='blue')
lines(seq(0.01, 600, by=0.01), dgamma(seq(0.01, 600, by=0.01), shape = wealth_shape, rate=wealth_rate), col = "forestgreen", lty = "dotted")
legend=legend(x='topright', legend=expr(paste(Gamma, '(', !!round(wealth_shape, 3), ',', !!round(wealth_rate, 3), ') ')), lty=2, col='forestgreen', cex=1)

plot(lx_w, ly_w, xlab='Log wealth level', ylab='Log density', xlim=c(2, 7), col='blue')
abline(reg1, lty=2, col='red')
legend=legend(x='topright', legend=paste('y = ', slope1, '*x +', intercept1), lty=2, col='red', cex=1)

plot(x_i, y_i, xlab='Income level', ylab='Density', xlim=c(0, 2500), ylim=c(0, 0.0025), col='blue')
lines(seq(0.01, 3000, by=0.01), dgamma(seq(0.01, 3000, by=0.01), shape = income_shape, rate=income_rate), col = "forestgreen", lty = "dotted")
legend=legend(x='topright', legend=expr(paste(Gamma, '(', !!round(income_shape, 3), ',', !!round(income_rate, 3), ') ')), lty=2, col='forestgreen', cex=1)

plot(lx_i, ly_i, xlab='Log income level', ylab='Log density', xlim=c(3.5, 9), col='blue')
abline(reg2, lty=2, col='red')
legend=legend(x='topright', legend=paste('y = ', slope2, '*x +', intercept2), lty=2, col='red', cex=1)

plot(nch$breaks[-1], nch$density, type='b', xlab='Fraction of population', ylab='Density', xlim=c(0.1, 0.8), ylim=c(0, 45), col='blue')
points(nwh$breaks[-1], nwh$density, type='b', xlab='Income level', ylab='Density', col='forestgreen')
points(nuh$breaks[-1], nuh$density, type='b', col='red')
legend=legend(x='topright', legend=c(expression(N[capitalists]/N), expression(N[workers]/N), expression(N[unemployed]/N)), col=c('blue', 'forestgreen', 'red'), lty=1, pch=1, cex=1)

plot(0:100, c(0, n_c), type='l', xlab='Year rule iteration', ylab='Fraction of population', xlim=c(-1, 100), ylim=c(0, 1.15), col='blue')
lines(0:100, c(0, n_w), type='l', col='forestgreen')
lines(0:100, c(1, n_u), type='l', col='red')

mtext(substitute(paste(bold("Wealth, Income, and Class Size Density Distributions in SA Model"))), side = 3, line = - 2, outer = TRUE)

# Cleanup

rm(list = ls()) 
p_unload(all) 
cat("\014")
