library(pacman, fitdistrplus)

dm01_path <- "C:\\Users\\maxgr\\Desktop\\visualizations\\dm01.csv"

dm01 <- t(read.csv(dm01_path, header=TRUE)[-1])

dm01_wealth <- dm01[,1]
dm01_neighbors <- dm01[,2]

dm01h <- hist(dm01_wealth, breaks=200, plot=FALSE)
x <- dm01h$breaks[-1]
y <- dm01h$density
lx <- log(x)[log(x) > 4][log(x) < 7][log(y) > -1000]
ly <- log(y)[log(x) > 4][log(x) < 7][log(y) > -1000]
reg1 <- lm(ly ~ lx)
intercept1 <- round(reg1$coefficients[1], 2)
slope1 <- round(reg1$coefficients[2], 2)

reg2 <- lm(dm01_wealth ~ dm01_neighbors)
intercept2 <- round(reg2$coefficients[1], 2)
slope2 <- round(reg2$coefficients[2], 2)

# par(mfrow=c(1,3))

par(fig = c(0,0.5,0,1))

plot(dm01h$breaks[-1], dm01h$density, xlim=c(0, 500), xlab='Wealth level', ylab='Density', col='blue')
lines(dm01h$breaks, exp(intercept1) * (dm01h$breaks ^ slope1), col = "red", lty = "dotted")
legend(x='bottomleft', legend=paste('y = exp(', intercept1, ') * x^(', slope1, ')'), col='red', lty=2, cex=1)

par(fig = c(0.2,0.5, 0.4, 1), new = T)  

plot(lx, ly, xlab='Log wealth level', ylab='Log density', col='blue')
abline(reg1, col='red', lty=2)
legend(x='topright', legend=paste('y = ', slope1, '*x + ', intercept1), col='red', lty=2, cex=1)

par(fig = c(0.5,1, 0, 1), new = T)  

plot(dm01_neighbors, dm01_wealth, xlab='Neighbors', ylab='Wealth level', col='blue')
abline(reg2, col='red', lty=2)
legend(x='topleft', legend=paste('y = ', slope2, '*x + ', intercept2), col='red', lty=2, cex=1)

mtext(substitute(paste(bold('Wealth Density Distribution in Discrete-Time BM Model'))), side = 3, line = - 2, outer = TRUE)

# Cleanup

rm(list = ls()) 
p_unload(all) 
cat("\014")