library(pacman, fitdistrplus)

cemm_path <- "C:\\Users\\maxgr\\Desktop\\visualizations\\cemm.csv"

cemm_data <- read.csv(cemm_path, header=TRUE)

cemm_hist <- hist(cemm_data$X0, breaks=50, plot=FALSE)
cemm_fit <- fitdist(c(cemm_data$X0)[cemm_data$X0 > 0.5], distr = "gamma", method = "mle")
summary(cemm_fit)
cemm_shape <- cemm_fit$estimate[1]
cemm_rate <- cemm_fit$estimate[2]

par(mfrow=c(1,2))

x <- log(cemm_hist$breaks[-1][cemm_hist$density > 0])
y <- log(cemm_hist$density[cemm_hist$density > 0])
x_prime <- x[log(cemm_hist$breaks[-1]) > -0.6]
y_prime <- y[log(cemm_hist$breaks[-1]) > -0.6]
reg <- lm(y_prime ~ x_prime)
intercept <- round(reg$coefficients[1], 2)
slope <- round(reg$coefficients[2], 2)

plot(cemm_hist$breaks[-1], cemm_hist$density, col='blue', xlim=c(0, 1), xlab='Wealth Level', ylab='Density')
lines(cemm_hist$breaks, exp(intercept) * (cemm_hist$breaks ^ slope), col = "red", lty = "dotted")
legend(x='topright', legend=paste0('y = exp(', intercept, ') * x^(', slope, ')'), col='red', lty=2, cex=0.7)
plot(x, y, col='blue', xlab='Log wealth level', ylab='Log density')
abline(reg, col='red', lty=2)
legend(x='topright', legend=paste0('y = ', slope, '*x - ', abs(intercept)), col='red', lty=2, cex=0.7)

mtext(substitute(paste(bold("Wealth Density Distribution in CEM Model"))), side = 3, line = - 2, outer = TRUE)

# Cleanup

rm(list = ls()) 
p_unload(all) 
cat("\014")
