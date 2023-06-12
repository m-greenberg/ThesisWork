if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, fitdistrplus, heatmaply)

# IMPORT STATEMENTS ----

path  <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search3\\mean_wealth_w.csv"

mean_worker_wealth <- colMeans(t(read.csv(path, header=TRUE)[-1][,c(91:100)])) * 4000 / (100 * 5000)

# MATRIX INITIALIZATION ----

mww_w1 <- matrix(NA, 10, 10)
mww_w4 <- matrix(NA, 10, 10)
mww_w7 <- matrix(NA, 10, 10)
mww_w10 <- matrix(NA, 10, 10)

for (i in c(1:10)) {
  for (j in c(1:10)) {
    mww_w1[i,j] <- mean_worker_wealth[10*(i-1) + j]
    mww_w4[i,j] <- mean_worker_wealth[300 + 10*(i-1) + j]
    mww_w7[i,j] <- mean_worker_wealth[600 + 10*(i-1) + j]
    mww_w10[i,j] <- mean_worker_wealth[900 + 10*(i-1) + j]
  }
}

# PLOTTING ----

heatmaply(mww_w1, xlab='Price of consumption good (p)', ylab='Price of capital (r)', main='w = 1', limits=c(0.19, 1), column_text_angle=0, dendrogram='none')
heatmaply(mww_w4, xlab='Price of consumption good (p)', ylab='Price of capital (r)', main='w = 4', limits=c(0.19, 1), column_text_angle=0, dendrogram='none')
heatmaply(mww_w7, xlab='Price of consumption good (p)', ylab='Price of capital (r)', main='w = 7', limits=c(0.19, 1), column_text_angle=0, dendrogram='none')
heatmaply(mww_w10, xlab='Price of consumption good (p)', ylab='Price of capital (r)', main='w = 10', limits=c(0.19, 1), column_text_angle=0, dendrogram='none')

# Cleanup ----

rm(list = ls()) 
p_unload(all) 
cat("\014")


