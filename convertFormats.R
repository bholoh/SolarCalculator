#usage instructions:
#move the usage data to a new excel workbook and save it as "targdata.csv"
#run the function, with a numerical value for number of minutes in the interval
#run 15 for 15 minute data, 30 for 30 minute data, etc.
#it will output a file called "hourlydata.csv" that should be compatible with calcExport

convertFormats <- function(intTime) {

	origData <- read.csv("targdata.csv")
	outData <- origData[,1:27]
	outData[,4:27] <- 0
	sumSpan <- 60/intTime
	startPoint <- 4	

	for (i in 1:24) {
		for (w in 1:nrow(origData)) {
			outData[w,(i+3)] <- sum(origData[w,startPoint:(startPoint+sumSpan-1)])
		}
		startPoint <- startPoint + sumSpan
	}
	write.csv(outData,file="hourlydata.csv",row.names=FALSE)

}