# Contains functions for operations on data

library(imputeTS)

# Turn time frame into indices
initialize_time <- function(timeframe_, test_stock_){
	x <- as.Date(timeframe_[1]);
	y <- as.Date(timeframe_[2]);
	while (!(x %in% test_stock_$Date)){
		x <- x+1;
	};
	while (!(y %in% test_stock_$Date)){
		y <- y+1;
	};
	return(c(which(test_stock_$Date == x),
			which(test_stock_$Date == y)));
}

# Take data from csv files and put into 1 df
# Also perform necessary shifts while data is open
# Also fill in NA data will data is available
build_master <- function(tikz_, indices_, shifts_,
				column_, csv_folder=csvloc){
	collen <- (indices[2] - indices[1] + 1);
	master <- data.frame()[1:collen,];
	for (tik in tikz_){
		print(paste("Extracting", tik, "..."));
		stock <- read.table(paste(csv_folder, tik,
					".csv", sep=""),sep="\t",
					header=TRUE);
		master[tik] <- stock[indices[1]:indices[2],column_];
		na.locf(master[tik], na.remaining="rev");
		for (i in shifts_){
			master[paste(tik,".s",i,sep="")] <-
				stock[(indices[1]+i):(indices[2]+i),column_];
			}
		rm(stock);
	}
	rownames(master) <- c(1:collen);
	return(master)
}

# Standardize Data

# Standardize by error from mean
standardize <- function(master_){
	print("Standardizing data as error from mean divided by sd...");
	for (i in colnames(master_)){
		master_[,i] <- (master_[,i] - mean(master_[,i]))/sd(master_[,i])
	};
	return(master_);
}

# Standardize as percent change and discretize
standardize1 <- function(master_, threshold_){
	print("Standardizing data as percent daily change...");
	for (tik in colnames(master_)){
		print(paste("Standardizing ", tik, "..."))
		repl <- c();
		for (i in 1:(length(master_[,tik])-1)){
			repl[i] <- (master_[i+1,tik]-master_[i,tik])/master_[i,tik]
		};
		print("Discretizing data...")
		ifelse(abs(repl) < threshold_, 0, 1);
		repl[length(repl)+1] <- 0;
		master_[tik] <- repl;
		rm(repl)
	};
	master_ <- head(master_, -1);
	return(master_)
}

# Add inverses
inv_master <- function(master_){
	print("Calculating inverses...");
	for (i in colnames(master_)){
		master_[paste(i,".INV",sep="")] <- -1*master_[,i];
	};
	return(master_);
}

# Transpose master, make tidy
transp <- function(master_){
	transp <- t(master_);
	rownames(transp) <- colnames(master_);
	colnames(transp) <- rownames(master_);
	return(transp)
}
