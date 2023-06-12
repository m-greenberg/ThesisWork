if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, fitdistrplus, poweRlaw, tidyverse)

bdy_path <- "C:\\Users\\maxgr\\Desktop\\visualizations\\bdy.csv"
cc0_path <- "C:\\Users\\maxgr\\Desktop\\visualizations\\cc0.csv"
cc02_path <- "C:\\Users\\maxgr\\Desktop\\visualizations\\cc02.csv"
cc04_path <- "C:\\Users\\maxgr\\Desktop\\visualizations\\cc04.csv"
cc06_path <- "C:\\Users\\maxgr\\Desktop\\visualizations\\cc06.csv"
cc08_path <- "C:\\Users\\maxgr\\Desktop\\visualizations\\cc08.csv"
cc095_path <- "C:\\Users\\maxgr\\Desktop\\visualizations\\cc095.csv"
ccm_path <- "C:\\Users\\maxgr\\Desktop\\visualizations\\ccm.csv"

bdy_data <- read.csv(bdy_path, header=TRUE)
cc0_data <- read.csv(cc0_path, header=TRUE)
cc02_data <- read.csv(cc02_path, header=TRUE)
cc04_data <- read.csv(cc04_path, header=TRUE)
cc06_data <- read.csv(cc06_path, header=TRUE)
cc08_data <- read.csv(cc08_path, header=TRUE)
cc095_data <- read.csv(cc095_path, header=TRUE)
ccm_data <- read.csv(ccm_path, header=TRUE)

bdy_hist <- hist(bdy_data$X0, breaks=100, plot=FALSE)
bdy_fit <- fitdist(c(bdy_data$X0)[bdy_data$X0 > 0], distr = "gamma", method = "mle")
bdy_shape <- bdy_fit$estimate[1]
bdy_rate <- bdy_fit$estimate[2]

summary(bdy_fit)

cc0_hist <- hist(cc0_data$X0, breaks=50, plot=FALSE)
cc0_fit <- fitdist(c(cc0_data$X0)[cc0_data$X0 > 0], distr = "gamma", method = "mle")
cc0_shape <- cc0_fit$estimate[1]
cc0_rate <- cc0_fit$estimate[2]

cc02_hist <- hist(cc02_data$X0, breaks=50, plot=FALSE)
cc02_fit <- fitdist(c(cc02_data$X0)[cc02_data$X0 > 0], distr = "gamma", method = "mle")
cc02_shape <- cc02_fit$estimate[1]
cc02_rate <- cc02_fit$estimate[2]

cc04_hist <- hist(cc04_data$X0, breaks=50, plot=FALSE)
cc04_fit <- fitdist(c(cc04_data$X0)[cc04_data$X0 > 0], distr = "gamma", method = "mle")
cc04_shape <- cc04_fit$estimate[1]
cc04_rate <- cc04_fit$estimate[2]

cc06_hist <- hist(cc06_data$X0, breaks=50, plot=FALSE)
cc06_fit <- fitdist(c(cc06_data$X0)[cc06_data$X0 > 0], distr = "gamma", method = "mle")
cc06_shape <- cc06_fit$estimate[1]
cc06_rate <- cc06_fit$estimate[2]

cc08_hist <- hist(cc08_data$X0, breaks=50, plot=FALSE)
cc08_fit <- fitdist(c(cc08_data$X0)[cc08_data$X0 > 0], distr = "gamma", method = "mle")
cc08_shape <- cc08_fit$estimate[1]
cc08_rate <- cc08_fit$estimate[2]

ccm_hist <- hist(ccm_data$X0, breaks=250, plot=FALSE)
ccm_fit <- fitdist(c(ccm_data$X0)[ccm_data$X0 > 0][ccm_data$X0 < 150], distr = "gamma", method = "mle")
ccm_shape <- ccm_fit$estimate[1]
ccm_rate <- ccm_fit$estimate[2]

x <- log(ccm_hist$breaks[-1])
y <- log(ccm_hist$density)
x_prime <- x[log(ccm_hist$breaks[-1]) > 3.5][log(ccm_hist$breaks[-1]) < 6]
y_prime <- y[log(ccm_hist$breaks[-1]) > 3.5][log(ccm_hist$breaks[-1]) < 6]
reg <- lm(y_prime ~ x_prime)
intercept <- round(reg$coefficients[1], 2)
slope <- round(reg$coefficients[2], 2)

par(mfrow=c(3,2))

plot(bdy_hist$breaks[-1], bdy_hist$density, col='darkred', xlim=c(0, 300), xlab='Wealth level', ylab='Density')
lines(bdy_hist$breaks, dexp(bdy_hist$breaks, rate=bdy_rate), col = "darkred", lty = "dotted")
legend=legend(x='topright', legend=expr(paste(Gamma, '(', !!round(bdy_shape, 3), ',', !!round(bdy_rate, 3), ') ')), lty=2, col='darkred', cex=0.8)

plot(log(bdy_hist$breaks[-1]), log(bdy_hist$density), col='darkred', xlim=c(2, 7), ylim=c(-10.5,-3.5), xlab='Log wealth level', ylab='Log density')

plot(cc02_hist$breaks[-1], cc02_hist$density, col='red', xlim=c(0, 300), ylim=c(0, 0.02), xlab='Wealth level', ylab='Density')
lines(cc02_hist$breaks, dgamma(cc02_hist$breaks, shape = cc02_shape, rate=cc02_rate), col = "red", lty = "dotted")

points(cc04_hist$breaks[-1], cc04_hist$density, col='green')
lines(cc04_hist$breaks, dgamma(cc04_hist$breaks, shape = cc04_shape, rate=cc04_rate), col = "green", lty = "dotted")

points(cc06_hist$breaks[-1], cc06_hist$density, col='blue')
lines(cc06_hist$breaks, dgamma(cc06_hist$breaks, shape = cc06_shape, rate=cc06_rate), col = "blue", lty = "dotted")

points(cc08_hist$breaks[-1], cc08_hist$density, col='purple')
lines(cc08_hist$breaks, dgamma(cc08_hist$breaks, shape = cc08_shape, rate=cc08_rate), col = "purple", lty = "dotted")

legend(x='topright', legend=c(expression(paste(lambda, "=0.2 ")), expression(paste(lambda, "=0.4 ")), expression(paste(lambda, "=0.6 ")), expression(paste(lambda, "=0.8 "))),
       col=c("red", "green", "blue", "purple"), lty=2, pch=1, cex=1)

plot(log(cc02_hist$breaks[-1]), log(cc02_hist$density), col='red', xlim=c(2, 7), ylim=c(-10.5,-3.5), xlab='Log wealth level', ylab='Log density')
points(log(cc04_hist$breaks[-1]), log(cc04_hist$density), col='green')
points(log(cc06_hist$breaks[-1]),log(cc06_hist$density), col='blue')
points(log(cc08_hist$breaks[-1]), log(cc08_hist$density), col='purple')
legend(x='topright', legend=c(expression(paste(lambda, "=0.2 ")), expression(paste(lambda, "=0.4 ")), expression(paste(lambda, "=0.6 ")), expression(paste(lambda, "=0.8 "))),
       col=c("red", "green", "blue", "purple"), pch=1, cex=1)

plot(ccm_hist$breaks[-1], ccm_hist$density, col='violetred', xlim=c(0,300), xlab='Wealth level', ylab='Density')
lines(ccm_hist$breaks, dgamma(ccm_hist$breaks, shape = ccm_shape, rate=ccm_rate), col = "violetred", lty = "dotted")
legend=legend(x='topright', legend=expr(paste(Gamma, '(', !!round(ccm_shape, 3), ',', !!round(ccm_rate, 3), ') ')), lty=2, col='violetred', cex=0.8)

plot(x, y, col='violetred', xlim=c(2, 7), ylim=c(-10.5,-3.5), xlab='Log wealth level', ylab='Log density')
abline(reg, col='violetred', lty=2)
legend(x='topright', legend=paste0('y = ', slope, '*x + ', intercept), col='violetred', lty=2, cex=0.8)

mtext(substitute(paste(bold('Wealth Density Distributions in BDY, CC, and CCM Models'))), side = 3, line = - 2, outer = TRUE)

# Cleanup

rm(list = ls()) 
p_unload(all) 
cat("\014")
