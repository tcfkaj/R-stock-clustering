source("DataPrep.R")

# Specify the location of needed files
list_of_tickers = "tikz.csv"
csvloc = "../../complete_csv/"
test_stock = "test_stock.csv"
folder_to_save = "../../data/2018-09-14/"

# Change time frame here
starttime = "2015-01-01"
endtime = "2017-12-31"

# Standardize options-
# 0 is error from mean / sd
# 1 is percent daily change and discretized with threshold
stand.opt = 1
threshold = 0

# Other parameters
type_of_data = "Close"
shifts = c()
inverses=FALSE
save_to_csv=FALSE
save_to_rda=FALSE
tidy=TRUE

# Now run and relax...

# Initalize timeframe
timeframe <- c(starttime, endtime)
dates <- read.table(test_stock, header=TRUE, sep="\t")
dates$Date <- as.Date(dates$Date)
indices <- initialize_time(timeframe, dates)

# Load tikz and build master
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

