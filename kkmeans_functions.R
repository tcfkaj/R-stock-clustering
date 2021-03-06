## Defines functions for optimizing kkmeans

library(kernlab)
library(tidyverse)

## Compute the following in Hilbert space

# Computes the Norm squared - distance squared - in Hilbert space

distance_from_center <- function(x, clusters, kernelMatrix){
		cluster=which(clusters==clusters[x]);
		r <- length(cluster);
		X <- kernelMatrix[x,x]
		Y <- 0;
		Z <- 0;
	for (i in cluster){
		for (j in cluster){
				Y <- Y+kernelMatrix[i,j]
			}
	Z <- Z+kernelMatrix[x,i]
	}
	Y <- Y/r^2;
	Z <- Z*(-2/r);
	distance <- X + Y + Z
	return(distance)
}


# Compute within-cluster sum of squares in Hilbert space

wcss <- function(cluster_number, clusters, kernelMatrix){
			cluster <- which(clusters==cluster_number);
			distances <- sapply(cluster,
						distance_from_center,
						clusters=clusters,
						kernelMatrix=kernelMatrix);
			wcss <- sum(distances);
			return(wcss)
}


# Compute total-wcss in Hilbert space

total_wcss <- function(nclusters, clusters, kernelMatrix){
			wcss <- c(1:nclusters) %>%
				sapply(disp_sq, clusters=clusters,
					kernelMatrix=kernelMatrix) %>%
				sum();
			return(wcss)
}
# Computes the radius of all clusters, by quantile

radii <- function(nclusters, clusters, kernelMatrix){
	radii <- c();
	for (i in 1:nclusters){
		current_cluster <- which(clusters==i)
		distances <- sapply(current_cluster,
						distance_from_center,
						clusters=clusters,
						kernelMatrix=kernelMatrix);
		radii <- c(radii, sqrt(unname(quantile(distances, 0.8))))
		}
	return(radii)
}

# Norm between cluster centers i,j  in Hilbert space

nbcc <- function(i, j, clusters, kernelMatrix){
		clusteri <- which(clusters==i);
		clusterj <- which(clusters==j);
		a <- length(clusteri);
		b <- length(clusterj);
		X = Y = Z = 0;
		for (m1 in clusteri){
			for (m2 in clusteri){
				X <- X+kernelMatrix[m1,m2]
				}
			for (n1 in clusterj){
				Y <- Y+kernelMatrix[m1,n1]
				}
		}
		for (n1 in clusterj){
			for(n2 in clusterj){
				Z <- Z+kernelMatrix[n1,n2]
			}
		}
		ns <- (X/(a^2))-(2*Y/(a*b))+(Z/(b^2));
		return(sqrt(ns))
}

# Compute the seperability matrix

smat <- function(nclusters, clusters, kernelMatrix){
			smat <- diag(nclusters);
			radii <- radii(nclusters, clusters, kernelMatrix);
			for (i in 1:nclusters){
				for (j in 1:nclusters){
					nbcc <- nbcc(i=i,j=j,
							clusters=clusters,
							kernelMatrix=kernelMatrix);
					sep <- (radii[i]+radii[j])/nbcc;
					smat[i,j] <- sep;
				}
			}
			return(smat)
}

# Compute dispersion of a cluster in Hilbert space

disp_sq <- function(cluster_number, clusters, kernelMatrix){
			cluster <- which(clusters==cluster_number);
			distances <- sapply(cluster,
						distance_from_center,
						clusters=clusters,
						kernelMatrix=kernelMatrix);
			disp.sq <- sum(distances)/length(cluster);
			return(disp.sq)
}

# Compute cost in Hilbert space

cost_in_H <- function(nclusters, clusters, kernelMatrix){
			disp.sq <- c(1:nclusters) %>%
				sapply(disp_sq, clusters=clusters,
					kernelMatrix=kernelMatrix) %>%
				sum();
			return(disp.sq)
}

# Compute best run based on cost in Hilbert space

best_run_H  <- function(x, kernel, ncenters, nruns){
		kernelM <- kernelMatrix(x, kernel=kernel);
		best <- kkmeans(x, kernel=kernel, centers=ncenters);
		cost_of_best <- cost_in_H(ncenters, best@.Data, kernelM);
		for (i in 1:(nruns-1)){
			current <- kkmeans(x, kernel=kernel,
					centers=ncenters);
			cost_of_current <- cost_in_H(ncenters, current@.Data, kernelM);
			if(cost_of_current < cost_of_best){
				best <- current;
				cost_of_best <- cost_of_current
			}
		}
		return(best)
}



## The following compute things in feature space and is more or less useless

# compute_cost <- function(kkmeans_results_){
#                 cost <- sum(withinss(kkmeans_results_)/size(kkmeans_results_));
#                 return(cost)
# }
#
# best_run  <- function(x, kernel, centers, nruns){
#                 best <- kkmeans(x, kernel=kernel, centers=centers);
#                 cost_of_best <- compute_cost(best);
#                 for (i in 1:(nruns-1)){
#                         current <- kkmeans(x, kernel=kernel,
#                                                 centers=centers);
#                         cost_of_current <- compute_cost(current);
#                         if(cost_of_current < cost_of_best){
#                                 best <- current;
#                                 cost_of_best <- cost_of_current
#                                 }
#                         }
#                         return(best)
#                 }
#
# best_offset_poly <- function(x, degree, centers, nruns, min.offset, max.offset){
#                         best <- best_run(x, kernel=polydot(degree=degree,
#                                                         offset=min.offset),
#                                         centers=centers,
#                                         nruns=nruns);
#                         cost_of_best <- compute_cost(best);
#                         min.offset <- min.offset+1;
#                         while (min.offset <= max.offset){
#                         current <- best_run(x, kernel=polydot(degree=degree,
#                                                         offset=min.offset),
#                                         centers=centers,
#                                         nruns=nruns);
#                         cost_of_current <- compute_cost(best);
#                         if(cost_of_current < cost_of_best){
#                                 best <- current;
#                                 cost_of_best <- cost_of_current
#                                 }
#                         min.offset <- min.offset+1
#                         }
#                         return(best)
#                 }
#
# best_scale_poly <- function(x, degree, centers, nruns, offset, min.scale, max.scale){
#                         best <- best_run(x, kernel=polydot(degree=degree,
#                                                         offset=offset,
#                                                         scale=min.scale),
#                                         centers=centers,
#                                         nruns=nruns);
#                         cost_of_best <- compute_cost(best);
#                         min.scale <- min.scale+1;
#                         while (min.scale <= max.scale){
#                         current <- best_run(x, kernel=polydot(degree=degree,
#                                                         offset=offset,
#                                                         scale=min.scale),
#                                         centers=centers,
#                                         nruns=nruns);
#                         cost_of_current <- compute_cost(best);
#                         if(cost_of_current < cost_of_best){
#                                 best <- current;
#                                 cost_of_best <- cost_of_current
#                                 }
#                         min.scale <- min.scale+1
#                         }
#                         return(best)
#                 }
#
# best_sigma_rfb <- function(x, centers, nruns, min.sigma, max.sigma){
#                         best <- best_run(x, kernel=rfbdot(sigma=min.sigma),
#                                         centers=centers,
#                                         nruns=nruns);
#                         cost_of_best <- compute_cost(best);
#                         min.sigma <- min.sigma+1;
#                         while (min.sigma <= max.sigma){
#                         current <- best_run(x, kernel=rfbdot(sigma=max.sigma),
#                                         centers=centers,
#                                         nruns=nruns);
#                         cost_of_current <- compute_cost(best);
#                         if(cost_of_current < cost_of_best){
#                                 best <- current;
#                                 cost_of_best <- cost_of_current
#                                 }
#                         min.sigma <- min.sigma+1
#                         }
#                         return(best)
#                 }
#
