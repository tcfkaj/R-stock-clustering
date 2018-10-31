source("kkmeans_functions.R")
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
compute_cost(dd.kk)
clusters <- dd.kk@.Data
kernelM <- kernelMatrix(dd, kernel=polydot(degree=3, offset=-5, scale=1))
cost_in_H(2, clusters, kernelM)
smat <- smat(nclusters=2, clusters=clusters, kernelMatrix=kernelM)
smat
best <- best_run_H(dd, kernel=polydot(degree=3, offset=-5, scale=1), ncenters=2, nruns=100)
best
cost_in_H(2, best@.Data, kernelM)
# kernelf(dd.kk)
# length(size(dd.kk))
## playing around with kkmeans

# str(dd.kk)
# size(dd.kk)[1]
# size(dd.kk)[2]
# withinss(dd.kk)[1]
# withinss(dd.kk)[2]
# disp <- withinss(dd.kk)/size(dd.kk)
# disp
# sum(disp)
# compute_cost(dd.kk)
#  dd.kk.best <- best_run(dd, kernel=polydot(degree=3, offset=-5, scale=1), centers=2, nruns=100)

# clusters <- dd.kk@.Data
# kernelM <- kernelMatrix(dd, kernel=rbfdot(1))
# distance_from_center(64, clusters, kernelM)
#
# current1 <- which(clusters==1)
# current2 <- which(clusters==2)
# distances1 <- sapply(current1, distance_from_center, clusters=clusters, kernelMatrix=kernelM)
# distances2 <- sapply(current2, distance_from_center, clusters=clusters, kernelMatrix=kernelM)
# distances1
# distances2
# unname(quantile(distances,.95))
# radii(2,clusters,kernelM)

# cluster <- which(dd.kk@.Data==2)
# cluster
# length(cluster)

# dd.kk.best
# compute_cost(dd.kk.best)
#
# best.offset <- best_offset_poly(dd, degree=3, centers=2, nruns=100, min.offset=0, max.offset=50)
# best.offset
# compute_cost(best.offset)
# best.scale <- best_scale_poly(dd, degree=3, centers=2, nruns=100, offset=-5, min.scale=1, max.scale=10)
# best.scale
# compute_cost(best.scale)
## optimizing
## optimizing cluster sizes, we want a ratio of 0.6


