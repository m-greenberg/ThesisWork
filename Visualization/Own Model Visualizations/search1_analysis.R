if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, fitdistrplus, rlang, tidyverse)

# IMPORT STATEMENTS ----

path1  <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search1\\capital_expenditures.csv"
path2  <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search1\\consumption_expenditures.csv"
path3  <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search1\\final_income_dist.csv"
path4  <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search1\\final_wealth_dist.csv"
path5  <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search1\\income_deciles.csv"
path6  <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search1\\income_deciles_s1.csv"
path7  <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search1\\income_deciles_s2.csv"
path8  <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search1\\income_deciles_w.csv"
path9  <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search1\\income_gini.csv"
path10 <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search1\\labor_expenditures.csv"
path11 <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search1\\mean_income.csv"
path12 <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search1\\mean_income_s1.csv"
path13 <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search1\\mean_income_s2.csv"
path14 <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search1\\mean_income_w.csv"
path15 <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search1\\mean_wealth.csv"
path16 <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search1\\mean_wealth_s1.csv"
path17 <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search1\\mean_wealth_s2.csv"
path18 <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search1\\mean_wealth_w.csv"
path19 <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search1\\unemployment_rate.csv"
path20 <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search1\\wealth_deciles.csv"
path21 <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search1\\wealth_deciles_s1.csv"
path22 <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search1\\wealth_deciles_s2.csv"
path23 <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search1\\wealth_deciles_w.csv"
path24 <- "C:\\Users\\maxgr\\Desktop\\ThesisData\\Search1\\wealth_gini.csv"

capital_expenditures     <- read.csv(path1, header=TRUE)[-1]
consumption_expenditures <- read.csv(path2, header=TRUE)[-1]
final_income_dist        <- read.csv(path3, header=TRUE)[-1]
final_wealth_dist        <- read.csv(path4, header=TRUE)[-1]
income_deciles           <- read.csv(path5, header=TRUE)[-1][-1]
income_deciles_s1        <- read.csv(path6, header=TRUE)[-1][-1]
income_deciles_s2        <- read.csv(path7, header=TRUE)[-1][-1]
income_deciles_w         <- read.csv(path8, header=TRUE)[-1][-1]
income_gini              <- read.csv(path9, header=TRUE)[-1]
labor_expenditures       <- read.csv(path10, header=TRUE)[-1]
mean_income              <- read.csv(path11, header=TRUE)[-1]
mean_income_s1           <- read.csv(path12, header=TRUE)[-1]
mean_income_s2           <- read.csv(path13, header=TRUE)[-1]
mean_income_w            <- read.csv(path14, header=TRUE)[-1]
mean_wealth              <- read.csv(path15, header=TRUE)[-1]
mean_wealth_s1           <- read.csv(path16, header=TRUE)[-1]
mean_wealth_s2           <- read.csv(path17, header=TRUE)[-1]
mean_wealth_w            <- read.csv(path18, header=TRUE)[-1]
unemployment_rate        <- read.csv(path19, header=TRUE)[-1]
wealth_deciles           <- read.csv(path20, header=TRUE)[-1][-1]
wealth_deciles_s1        <- read.csv(path21, header=TRUE)[-1][-1]
wealth_deciles_s2        <- read.csv(path22, header=TRUE)[-1][-1]
wealth_deciles_w         <- read.csv(path23, header=TRUE)[-1][-1]
wealth_gini              <- read.csv(path24, header=TRUE)[-1]

wealth_dist_01_025 <- t(final_wealth_dist[12,])
wealth_dist_01_075 <- t(final_wealth_dist[22,])
wealth_dist_09_025 <- t(final_wealth_dist[188,])
wealth_dist_09_075 <- t(final_wealth_dist[198,])

income_dist_01_025 <- t(final_income_dist[12,])
income_dist_01_075 <- t(final_income_dist[22,])
income_dist_09_025 <- t(final_income_dist[188,])
income_dist_09_075 <- t(final_income_dist[198,])

wealth_dist_01_025_s1 <- wealth_dist_01_025[c(1:125)]
wealth_dist_01_075_s1 <- wealth_dist_01_075[c(1:375)]
wealth_dist_09_025_s1 <- wealth_dist_09_025[c(1:1125)]
wealth_dist_09_075_s1 <- wealth_dist_09_075[c(1:3375)]

income_dist_01_025_s1 <- income_dist_01_025[c(1:125)]
income_dist_01_075_s1 <- income_dist_01_075[c(1:375)]
income_dist_09_025_s1 <- income_dist_09_025[c(1:1125)]
income_dist_09_075_s1 <- income_dist_09_075[c(1:3375)]

wealth_dist_01_025_s2 <- wealth_dist_01_025[c(126:500)]
wealth_dist_01_075_s2 <- wealth_dist_01_075[c(376:500)]
wealth_dist_09_025_s2 <- wealth_dist_09_025[c(1126:4500)]
wealth_dist_09_075_s2 <- wealth_dist_09_075[c(3376:4500)]

income_dist_01_025_s2 <- income_dist_01_025[c(126:500)]
income_dist_01_075_s2 <- income_dist_01_075[c(376:500)]
income_dist_09_025_s2 <- income_dist_09_025[c(1126:4500)]
income_dist_09_075_s2 <- income_dist_09_075[c(3376:4500)]

wealth_dist_01_025_s1_hist <- hist(wealth_dist_01_025_s1, breaks=25, plot=FALSE)
wealth_dist_01_025_s1_x <- wealth_dist_01_025_s1_hist$breaks[-1]
wealth_dist_01_025_s1_y <- wealth_dist_01_025_s1_hist$density
wealth_dist_01_075_s1_hist <- hist(wealth_dist_01_075_s1, breaks=25, plot=FALSE)
wealth_dist_01_075_s1_x <- wealth_dist_01_075_s1_hist$breaks[-1]
wealth_dist_01_075_s1_y <- wealth_dist_01_075_s1_hist$density
wealth_dist_09_025_s1_hist <- hist(wealth_dist_09_025_s1, breaks=100, plot=FALSE)
wealth_dist_09_025_s1_x <- wealth_dist_09_025_s1_hist$breaks[-1]
wealth_dist_09_025_s1_y <- wealth_dist_09_025_s1_hist$density
wealth_dist_09_075_s1_hist <- hist(wealth_dist_09_075_s1, breaks=100, plot=FALSE)
wealth_dist_09_075_s1_x <- wealth_dist_09_075_s1_hist$breaks[-1]
wealth_dist_09_075_s1_y <- wealth_dist_09_075_s1_hist$density

income_dist_01_025_s1_hist <- hist(income_dist_01_025_s1, breaks=25, plot=FALSE)
income_dist_01_025_s1_x <- income_dist_01_025_s1_hist$breaks[-1]
income_dist_01_025_s1_y <- income_dist_01_025_s1_hist$density
income_dist_01_075_s1_hist <- hist(income_dist_01_075_s1, breaks=25, plot=FALSE)
income_dist_01_075_s1_x <- income_dist_01_075_s1_hist$breaks[-1]
income_dist_01_075_s1_y <- income_dist_01_075_s1_hist$density
income_dist_09_025_s1_hist <- hist(income_dist_09_025_s1, breaks=100, plot=FALSE)
income_dist_09_025_s1_x <- income_dist_09_025_s1_hist$breaks[-1]
income_dist_09_025_s1_y <- income_dist_09_025_s1_hist$density
income_dist_09_075_s1_hist <- hist(income_dist_09_075_s1, breaks=100, plot=FALSE)
income_dist_09_075_s1_x <- income_dist_09_075_s1_hist$breaks[-1]
income_dist_09_075_s1_y <- income_dist_09_075_s1_hist$density

wealth_dist_01_025_s2_hist <- hist(wealth_dist_01_025_s2, breaks=25, plot=FALSE)
wealth_dist_01_025_s2_x <- wealth_dist_01_025_s2_hist$breaks[-1]
wealth_dist_01_025_s2_y <- wealth_dist_01_025_s2_hist$density
wealth_dist_01_075_s2_hist <- hist(wealth_dist_01_075_s2, breaks=25, plot=FALSE)
wealth_dist_01_075_s2_x <- wealth_dist_01_075_s2_hist$breaks[-1]
wealth_dist_01_075_s2_y <- wealth_dist_01_075_s2_hist$density
wealth_dist_09_025_s2_hist <- hist(wealth_dist_09_025_s2, breaks=100, plot=FALSE)
wealth_dist_09_025_s2_x <- wealth_dist_09_025_s2_hist$breaks[-1]
wealth_dist_09_025_s2_y <- wealth_dist_09_025_s2_hist$density
wealth_dist_09_075_s2_hist <- hist(wealth_dist_09_075_s2, breaks=100, plot=FALSE)
wealth_dist_09_075_s2_x <- wealth_dist_09_075_s2_hist$breaks[-1]
wealth_dist_09_075_s2_y <- wealth_dist_09_075_s2_hist$density

income_dist_01_025_s2_hist <- hist(income_dist_01_025_s2, breaks=25, plot=FALSE)
income_dist_01_025_s2_x <- income_dist_01_025_s2_hist$breaks[-1]
income_dist_01_025_s2_y <- income_dist_01_025_s2_hist$density
income_dist_01_075_s2_hist <- hist(income_dist_01_075_s2, breaks=25, plot=FALSE)
income_dist_01_075_s2_x <- income_dist_01_075_s2_hist$breaks[-1]
income_dist_01_075_s2_y <- income_dist_01_075_s2_hist$density
income_dist_09_025_s2_hist <- hist(income_dist_09_025_s2, breaks=100, plot=FALSE)
income_dist_09_025_s2_x <- income_dist_09_025_s2_hist$breaks[-1]
income_dist_09_025_s2_y <- income_dist_09_025_s2_hist$density
income_dist_09_075_s2_hist <- hist(income_dist_09_075_s2, breaks=100, plot=FALSE)
income_dist_09_075_s2_x <- income_dist_09_075_s2_hist$breaks[-1]
income_dist_09_075_s2_y <- income_dist_09_075_s2_hist$density

par(mfrow=c(4,2))

plot(wealth_dist_01_025_s1_x, wealth_dist_01_025_s1_y, xlab='Wealth Level', ylab='Density', main='%C = 0.1, %S1 = 0.25', xlim=c(0, 6000), ylim=c(0, 0.005), col='blue')
points(wealth_dist_01_025_s2_x, wealth_dist_01_025_s2_y, col='forestgreen')

plot(income_dist_01_025_s1_x, income_dist_01_025_s1_y, xlab='Income Level', ylab='Density', main='%C = 0.1, %S1 = 0.25', xlim=c(0, 6000), ylim=c(0, 0.003), col='blue')
points(income_dist_01_025_s2_x, income_dist_01_025_s2_y, col='forestgreen')

plot(wealth_dist_01_075_s1_x, wealth_dist_01_075_s1_y, xlab='Wealth Level', ylab='Density', main='%C = 0.1, %S1 = 0.75', ylim=c(0, 0.002), col='blue')
points(wealth_dist_01_075_s2_x, wealth_dist_01_075_s2_y, col='forestgreen')

plot(income_dist_01_075_s1_x, income_dist_01_075_s1_y, xlab='Income Level', ylab='Density', main='%C = 0.1, %S1 = 0.75', ylim=c(0, 0.004), col='blue')
points(income_dist_01_075_s2_x, income_dist_01_075_s2_y, col='forestgreen')

plot(wealth_dist_09_025_s1_x, wealth_dist_09_025_s1_y, xlab='Wealth Level', ylab='Density', main='%C = 0.9, %S1 = 0.25', xlim=c(0, 1000), ylim=c(0, 0.01), col='blue')
points(wealth_dist_09_025_s2_x, wealth_dist_09_025_s2_y, col='forestgreen')

plot(income_dist_09_025_s1_x, income_dist_09_025_s1_y, xlab='Income Level', ylab='Density', main='%C = 0.9, %S1 = 0.25', xlim=c(0, 500), ylim=c(0, 0.015), col='blue')
points(income_dist_09_025_s2_x, income_dist_09_025_s2_y, col='forestgreen')

plot(wealth_dist_09_075_s1_x, wealth_dist_09_075_s1_y, xlab='Wealth Level', ylab='Density', main='%C = 0.9, %S1 = 0.75', xlim=c(0, 400), ylim=c(0, 0.01), col='blue')
points(wealth_dist_09_075_s2_x, wealth_dist_09_075_s2_y, col='forestgreen')

plot(income_dist_09_075_s1_x, income_dist_09_075_s1_y, xlab='Income Level', ylab='Density', main='%C = 0.9, %S1 = 0.75', xlim=c(0, 200), ylim=c(0, 0.08), col='blue')
points(income_dist_09_075_s2_x, income_dist_09_075_s2_y, col='forestgreen')

# Cleanup ----

rm(list = ls()) 
p_unload(all) 
cat("\014")
