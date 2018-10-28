# Defines functions for optimizing kkmeans

library(kernlab)

compute_cost <- function(kkmeans_results_){
		cost <- sum(withinss(kkmeans_results_)/size(kkmeans_results_));
		return(cost)
}

best_run  <- function(x, kernel, centers, nruns){
		best <- kkmeans(x, kernel=kernel, centers=centers);
		cost_of_best <- compute_cost(best);
		for (i in 1:(nruns-1)){
			current <- kkmeans(x, kernel=kernel,
						centers=centers);
			cost_of_current <- compute_cost(current);
			if(cost_of_current < cost_of_best){
				best <- current;
				cost_of_best <- cost_of_current
				}
			}
			return(best)
		}

best_offset_poly <- function(x, degree, centers, nruns, min.offset, max.offset){
			best <- best_run(x, kernel=polydot(degree=degree,
							offset=min.offset),
					centers=centers,
					nruns=nruns);
			cost_of_best <- compute_cost(best);
			min.offset <- min.offset+1;
			while (min.offset <= max.offset){
			current <- best_run(x, kernel=polydot(degree=degree,
							offset=min.offset),
					centers=centers,
					nruns=nruns);
			cost_of_current <- compute_cost(best);
			if(cost_of_current < cost_of_best){
				best <- current;
				cost_of_best <- cost_of_current
				}
			min.offset <- min.offset+1
			}
			return(best)
		}

best_scale_poly <- function(x, degree, centers, nruns, offset, min.scale, max.scale){
			best <- best_run(x, kernel=polydot(degree=degree,
							offset=offset,
							scale=min.scale),
					centers=centers,
					nruns=nruns);
			cost_of_best <- compute_cost(best);
			min.scale <- min.scale+1;
			while (min.scale <= max.scale){
			current <- best_run(x, kernel=polydot(degree=degree,
							offset=offset,
							scale=min.scale),
					centers=centers,
					nruns=nruns);
			cost_of_current <- compute_cost(best);
			if(cost_of_current < cost_of_best){
				best <- current;
				cost_of_best <- cost_of_current
				}
			min.scale <- min.scale+1
			}
			return(best)
		}

best_sigma_rfb <- function(x, centers, nruns, min.sigma, max.sigma){
			best <- best_run(x, kernel=rfbdot(sigma=min.sigma),
					centers=centers,
					nruns=nruns);
			cost_of_best <- compute_cost(best);
			min.sigma <- min.sigma+1;
			while (min.sigma <= max.sigma){
			current <- best_run(x, kernel=rfbdot(sigma=max.sigma),
					centers=centers,
					nruns=nruns);
			cost_of_current <- compute_cost(best);
			if(cost_of_current < cost_of_best){
				best <- current;
				cost_of_best <- cost_of_current
				}
			min.sigma <- min.sigma+1
			}
			return(best)
		}
