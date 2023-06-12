if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, fitdistrplus)

# IMPORT STATEMENTS ----

path1  <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search2\\mean_wealth_s1.csv"
path2  <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search2\\mean_wealth_s2.csv"
path3  <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search2\\mean_wealth_w.csv"

mean_wealth_s1 <- colMeans(t(read.csv(path1, header=TRUE)[c(533:551),c(6:100)])) * (0.2 * 0.5) / 100
mean_wealth_s2 <- colMeans(t(read.csv(path2, header=TRUE)[c(533:551),c(6:100)])) * (0.2 * 0.5) / 100
mean_wealth_w <- colMeans(t(read.csv(path3, header=TRUE)[c(533:551),c(6:100)])) * 0.8 / 100

mws1_prediction <- function(gam) {
  return ((2 - gam)/(3 - gam))
}

mws2_prediction <- function(gam) {
  return ((gam)/(3 - gam))
}

mww_prediction <- function(gam) {
  return ((1 - gam)/(3 - gam))
}

par(mar=c(4, 4, 2, 10), xpd=TRUE)

x <- seq(from=0.05, to=0.95, by=0.05)

plot(x, mean_wealth_s1, xlim=c(0, 1), ylim=c(0, 0.7), col='red', xlab=expression(paste(gamma)), ylab='Fraction of System Wealth', main='Class Division of Wealth')
points(x, mean_wealth_s2, col='dodgerblue')
points(x, mean_wealth_w, col='green')
lines(x, mws1_prediction(x), col='darkred')
lines(x, mws2_prediction(x), col='darkblue')
lines(x, mww_prediction(x), col='forestgreen')

legend=legend(x='right', inset=c(-0.275, 0), 
              legend=c('Sector 1 (simulated)', 'Sector 2 (simulated)', 'Workers (simulated)',
                       'Sector 1 (theoretical)', 'Sector 2 (theoretical)', 'Workers (theoretical)'), 
              pch=c(1,1,1,NA,NA,NA), lty=c(0,0,0,1,1,1), 
              col=c('red', 'dodgerblue', 'green', 'darkred', 'darkblue', 'forestgreen'), 
              title=expression(bold('Legend')), cex=0.8)

# Cleanup ----

rm(list = ls()) 
p_unload(all) 
cat("\014")
