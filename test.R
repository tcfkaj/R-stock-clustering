source("kmeans_functions.R")
library(tidyverse)
library(kernlab)

## Build simple data set

grp <- c(rep("green",40)) %>%
	c(rep("red", 24))
x <- c(rep(1,10 )) %>%
	c(rep(-1,10)) %>%
	c(runif(10,min=-1, max=1)) %>%
	c(runif(10, min=-1, max=1)) %>%
	c(rep(-80,12)) %>%
	c(runif(12, min=-80, max=80))
y <- c(runif(10, min=-1, max=1)) %>%
	c(runif(10, min=-1, max=1)) %>%
	c(rep(1,10)) %>%
	c(rep(-1,10)) %>%
	c(runif(12,min=-80, max=80)) %>%
	c(rep(80, 12))
test.data <- data.frame(X=x, Y=y, grp=grp)
# plot(test.data$Y~test.data$X, pch=20, col=test.data$grp)

## kernel k-means

dd <- as.matrix(test.data[,-3])
dd
dd.kk <- kkmeans(dd, kernel=polydot(degree=3, offset=-5, scale=1), centers=2)
dd.kk

## playing around with kkmeans

# str(dd.kk)
size(dd.kk)[1]
size(dd.kk)[2]
withinss(dd.kk)[1]
withinss(dd.kk)[2]
disp <- withinss(dd.kk)/size(dd.kk)
disp
sum(disp)
compute_cost(dd.kk)


dd.kk.best <- best_run(dd, kernel=polydot(degree=3, offset=-5, scale=1), centers=2, nruns=100)
dd.kk.best
compute_cost(dd.kk.best)

best.scale <- best_scale_poly(dd, degree=3, centers=2, nruns=100, offset=-5, min.scale=1, max.scale=10)
best.scale
compute_cost(best.scale)
## optimizing
## optimizing cluster sizes, we want a ratio of 0.6


