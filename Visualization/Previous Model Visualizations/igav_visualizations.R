if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, fitdistrplus, rlang, tidyverse)

path <- "C:\\Users\\maxgr\\Desktop\\visualizations\\igav.csv"

igav <- t(read.csv(path, header=TRUE)[-1])

igav1 <- igav[,1]
igav2 <- igav[,2]
igav3 <- igav[,3]
igav4 <- igav[,4]
igav5 <- igav[,5]

hist1 <- hist(igav1, breaks=50, plot=FALSE)
hist2 <- hist(igav2, breaks=50, plot=FALSE)
hist3 <- hist(igav3, breaks=50, plot=FALSE)
hist4 <- hist(igav4, breaks=50, plot=FALSE)
hist5 <- hist(igav5, breaks=50, plot=FALSE)

x1 <- hist1$breaks[-1]
x2 <- hist2$breaks[-1]
x3 <- hist3$breaks[-1]
x4 <- hist4$breaks[-1]
x5 <- hist5$breaks[-1]

y1 <- hist1$density
y2 <- hist2$density
y3 <- hist3$density
y4 <- hist4$density
y5 <- hist5$density

summary(y1)

lx <- log(x5)[x5 > 0][y5 > 0]
ly <- log(y5)[x5 > 0][y5 > 0]
lxt <- lx[lx > 4][lx < 7]
lyt <- ly[lx > 4][lx < 7]
reg <- lm(lyt ~ lxt)
intercept <- round(reg$coefficients[1], 2)
slope <- round(reg$coefficients[2], 2)

par(mfrow = c(1, 2))

plot(x5, y5, type='p', xlab='Wealth level', ylab='Density', xlim=c(0, 500), col='blue')
lines(seq(0, 1000, 0.01), exp(intercept) * (seq(0, 1000, 0.01) ^ slope), col = "red", lty = "dotted")
legend(x='topright', legend=paste('y = exp(', intercept, ') * x^(', slope, ')'), col='red', lty=2, cex=1)


plot(lx, ly, type='p', xlab='Log wealth level', ylab='Log density', col='blue')
abline(reg, lty=2, col='red')
legend(x='topright', legend=paste0('y = ', slope, '*x + ', intercept), col='red', lty=2, cex=1)

mtext(substitute(paste(bold('Wealth Density Distributions in IGAV Model'))), side = 3, line = - 2, outer = TRUE)

# Cleanup

rm(list = ls()) 
p_unload(all) 
cat("\014")