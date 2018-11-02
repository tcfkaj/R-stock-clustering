source("DataPrep.R")

# Specify the location of needed files
list_of_tickers = "sp500.csv"
csvloc = "../../stock-scraper/data/sp500/csv-2017/"
test_stock = "../../stock-scraper/data/sp500/csv-2017/ADS.csv"
folder_to_save = "../../data/SP500/2017/"

# Change time frame here
starttime = "2017-01-01"
endtime = "2017-12-31"

# Standardize options-
# 0 is error from mean / sd
# 1 is percent daily change and discretized with threshold
# set as 2 or larger for no standardize
stand.opt = 2
threshold = 0.2
# To explore quantiles to determine threshold,
# set to TRUE
explore=FALSE
quant=0.8

# Other parameters
type_of_data = "Close"
shifts = c()
inverses=FALSE
save_to_csv=TRUE
save_to_rda=TRUE
tidy=FALSE

# Now run and relax...

# Initalize timeframe
print("Initializing timeframe...")
timeframe <- c(starttime, endtime)
print("Reading dates from test stock...");
dates <- read.table(test_stock, header=TRUE, sep="\t")
dates$Date <- as.Date(dates$Date)
dates$Date
indices <- initialize_time(timeframe, dates)
indices

# Load tikz and build master
print("Reading tickers...")
readtikz <- read.table(list_of_tickers, header=FALSE)
tikz  <- readtikz[,1]
master <- build_master(tikz, indices, shifts, type_of_data)

# Standardize data
if (stand.opt == 0){
	master <- standardize(master)
}
if (stand.opt == 1){
	master <- standardize1(master, threshold)
}

# Explore quantiles to determine threshold to discretize
if(explore){
	quantile(sapply(abs(master),
		function(x)
		quantile(x,probs=quant,
		na.rm=TRUE)),
		probs=c(0.3,0.5,0.7,0.9),
		na.rm=TRUE)
}

# Transform master
if (inverses){master <- inv_master(master)}
if (tidy){master <-  transp(master)}

# Save to csv and R data
if (save_to_csv){
	print("Saving data...");
	write.table(master,
		file=paste(folder_to_save,"master.csv",sep=""),sep="\t")
}
if (save_to_rda){
	print("Saving data...");
	save(master, file=paste(folder_to_save,"master.Rda",sep=""))
}

if (tidy){
	print(rownames(master));
	master[,1:5]
}
if (! tidy){
	print(colnames(master));
	master[1:12, 1:4]
}


