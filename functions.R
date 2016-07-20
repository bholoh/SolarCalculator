#steps for usage:
#Paste the hourly usage data into a new workbook, save it as "hourlydata", with the file type "CSV (MS-DOS)".  This is important.
#Paste the pvwatts hourly data without the metadata passed to pvwatts into a new workbook, save it as "pvwatts" with the same CSV file type.

calcExport <- function (systemSize) {

hourdat <- read.csv("hourlydata.csv")
pvwatts <- read.csv("pvwatts.csv")

if (ncol(hourdat) == 4) {
	splitstr <- strsplit(as.character(hourdat[,1]),split="/")
	hourdat[,2] <- as.character(hourdat[,2])
	hourSplit <- strsplit(as.character(hourdat[,2]),split=":")
	for (i in 1:nrow(hourdat)) {
		tempDat <- as.character(hourSplit[[i]])
		if (tempDat[1] == "12") {
			tempDat[1] <- 0
		}

		if (tempDat[2] == "00 PM") {
			tempDat[1] <- as.character(as.numeric(tempDat[1]) + 12)
			hourdat[i,2] <- tempDat[1]			
		} else {  	
			hourdat[i,2] <- tempDat[1]
		}
	}
} else {
	splitstr <- strsplit(as.character(hourdat[,3]),split="/")
}

hourdat[,(ncol(hourdat)+1)] <- 0
names(hourdat)[ncol(hourdat)] <- "month"
hourdat[,(ncol(hourdat)+1)] <- 0
names(hourdat)[ncol(hourdat)] <- "day"

for (i in 1:length(splitstr)) {
	hourdat[i,"month"] <- splitstr[[i]][1]
	hourdat[i,"day"] <- splitstr[[i]][2]
}

pvwatts[,(ncol(pvwatts)+1)] <- 0
names(pvwatts)[ncol(pvwatts)] <- "rowmatch"

pvwatts[,(ncol(pvwatts)+1)] <- 0
names(pvwatts)[ncol(pvwatts)] <- "usage"

#find matching row in hourly data for usage data lookup
if (ncol(hourdat) == 6) {
	for (i in 1:nrow(pvwatts)) {
		matchrow <- which(hourdat[,"month"] == pvwatts[i,"Month"] & hourdat[,"day"] == pvwatts[i,"Day"] & hourdat[,"Hour"] == pvwatts[i,"Hour"])
		if (length(matchrow) > 0) {
			pvwatts[i,"rowmatch"] <- matchrow[1]
			pvwatts[i,"usage"] <- hourdat[pvwatts[i,"rowmatch"],"kWh"]
		}
	}
} else {
	for (i in 1:nrow(pvwatts)) {
		matchrow <- which(hourdat[,"month"] == pvwatts[i,"Month"] & hourdat[,"day"] == pvwatts[i,"Day"])
		if (length(matchrow) > 0) {
			pvwatts[i,"rowmatch"] <- matchrow[1]
			pvwatts[i,"usage"] <- hourdat[pvwatts[i,"rowmatch"],(4+pvwatts[i,"Hour"])]
		}
	}
}

pvwatts[,(ncol(pvwatts)+1)] <- (pvwatts[,"AC.System.Output..W."]/1000) * systemSize
names(pvwatts)[ncol(pvwatts)] <- "kwhGen"


pvwatts[,(ncol(pvwatts)+1)] <- pvwatts[,"kwhGen"] - pvwatts[,"usage"]
names(pvwatts)[ncol(pvwatts)] <- "difference"

zeroUsage <- which(pvwatts[,"usage"] == 0)
toPlot <- 1:(nrow(pvwatts)-1)
toPlot <- toPlot[-zeroUsage]

pdf("SurplusGraph.pdf")
plot(x=seq(1,length(toPlot)),y=pvwatts[toPlot,"difference"],pch=16,cex=0.2,
xlab="Hour",
ylab="Surplus production (kwh)",
main="Surplus production over the time period specified \n(excluding periods without usage data)"
)
dev.off()

monthdat <- as.data.frame(matrix(0,ncol=10,nrow=13))
names(monthdat) <- c("month","perGen","perExp","kwhGen","kwhExp","perAnnGenExp","kwhUsed","perUsedSolar","perUsedExported","intervalsNoUsage")
monthdat[1:13,1] <- c("January","February","March","April","May","June","July","August","September","October","November","December","Totals")

for (i in 1:12) {

	thisMonth <- which(pvwatts[,"Month"] == i)
	noUsage <- which(pvwatts[thisMonth,"usage"] == 0)
	monthdat[i,"intervalsNoUsage"] <- length(noUsage)
	monthdat[i,"kwhGen"] <- sum(pvwatts[thisMonth,"kwhGen"])
	monthdat[i,"kwhUsed"] <- sum(pvwatts[thisMonth,"usage"])
	if (length(noUsage) > 0 & length(noUsage) < length(thisMonth)) {
		thisMonth <- thisMonth[-noUsage]
		exportInt <- which(pvwatts[thisMonth,"difference"] > 0)
		monthdat[i,"kwhExp"] <- sum(pvwatts[thisMonth[exportInt],"difference"])
	} else if (length(noUsage) == 0) {
		exportInt <- which(pvwatts[thisMonth,"difference"] > 0)
		monthdat[i,"kwhExp"] <- sum(pvwatts[thisMonth[exportInt],"difference"])
	}
}

monthdat[13,"perGen"]  <- 100
monthdat[13,"perExp"] <- 100
monthdat[13,"kwhGen"] <- sum(monthdat[1:12,"kwhGen"])
monthdat[13,"kwhUsed"] <- sum(monthdat[1:12,"kwhUsed"])
monthdat[13,"kwhExp"] <- sum(monthdat[1:12,"kwhExp"])

monthdat[1:13,"perGen"] <- (monthdat[1:13,"kwhGen"] / sum(monthdat[1:12,"kwhGen"])) * 100

monthdat[1:13,"perExp"] <- (monthdat[1:13,"kwhExp"] / sum(monthdat[1:12,"kwhExp"])) * 100
monthdat[,"perAnnGenExp"] <- monthdat[,"kwhExp"] / monthdat[13,"kwhGen"] * 100

monthdat[,"perUsedSolar"] <- monthdat[,"kwhGen"] / monthdat[,"kwhUsed"] *100
monthdat[,"perUsedExported"] <- monthdat[,"kwhExp"] / monthdat[,"kwhUsed"] *100

write.csv(monthdat,file="MonthlySummaryData.csv",quote=FALSE)
write.csv(pvwatts,file="hourlyUsageAndProductionData.csv",quote=FALSE)

}




