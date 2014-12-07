## Exploratory Data Analysis Coursera Week 1 Project
##
## Packages to load
require(data.table)

## Create data directory if none exists
if(!file.exists("data")) {
        dir.create("data")
}

## Get data
fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
myFile <- "./data/household_power_consumption.zip"
download.file(fileUrl, destfile=myFile)
dateDownloaded <- date()

## Only unzip file if data file has not already been loaded in before
if(!file.exists("./data/household_power_consumption.txt")) {
        myUnzippedFile <- unzip(myFile, exdir = "./data")
}
myUnzippedFile <- "./data/household_power_consumption.txt"
## get col classes from first 20 rows
firstFewDT <- as.data.table(read.table(myUnzippedFile, sep=";", quote="", 
                                       header = TRUE, nrows=20, na.strings="?",
                                       stringsAsFactors = FALSE))
classes <- sapply(firstFewDT, class)
## sample 1/10 of the data to estimate total size
sampleDT <- as.data.table(read.table(myUnzippedFile, sep=";", quote="", 
                                     header = TRUE, nrows=207500, 
                                     na.strings="?", colClasses = classes,
                                     stringsAsFactors = FALSE))
## check table memory and inspect answer in console
tables() ## answer = 15 MB, so full file ~ 150MB or less
## Now run full table
DT <- as.data.table(read.table(myUnzippedFile, sep=";", quote="", 
                                  header = TRUE, nrows=2100000, 
                                  na.strings="?", colClasses = classes,
                                        stringsAsFactors = FALSE))
## Some optional basic inspection details
DT_classes <- sapply(DT, class)
## Check for NA counts by column
naCount <- sapply(DT, function(x) sum(is.na(x)))

## Subset the right data
## First check date formatting
a <- unique(DT$Date) ## find unique values
b <- grep("2/2007",a) ## index of regex matches in unique dates for dates wanted
a[b] ## return matching entries oin console so you can see date formatting
targetDates <- c("1/2/2007", "2/2/2007")
myDT <- DT[DT$Date %in% targetDates,]
## Convert date & time to correct format
## First merge date and time columns into 1
myDT$tmpDateTime <- paste(myDT$Date, myDT$Time)
## then convert making sure to use POSIXct because POSIXlt is a set of lists
newDateTime <- as.POSIXct(strptime(myDT$tmpDateTime, 
                                   format = "%d/%m/%Y %H:%M:%S"))
myDT[,datetime:=newDateTime] ## Add the new variable to the data table
myDT$tmpDateTime <- NULL ## remove temp variable

## Plot4 ##
png("data/plot4.png") 
par(mfcol=c(2,2), bg = "transparent") ## needs to be set after device selection
z <- range(myDT$datetime) # used for recreating tickmarks
with(myDT, {
        ## Top-left plot ##
        ylab <- "Global Active Power (kilowatts)"
        plot(datetime, Global_active_power, 
                type = "l", xlab = "", ylab = ylab, axes = F)
        ## fixing grey box / black axes issue
        box(col="grey31")
        axis(1, at=c(z[1], median(z)+30, z[2]+60), 
             lab=c("Thu","Fri","Sat"), col = "black")
        axis(2, col="black")
        
        ## Bottom-left plot ##
        ylab <- "Energy sub metering"
        plot(datetime, Sub_metering_1, 
                type = "n", xlab = "", ylab = ylab, axes = F)
        ## fixing grey box / black axes issue
        box(col="grey31")
        axis(1, at=c(z[1], median(z)+30, z[2]+60), 
             lab=c("Thu","Fri","Sat"), col = "black")
        axis(2, col="black")
        ## back to rest of the plot
        lines(datetime, Sub_metering_1, col = "black")
        lines(datetime, Sub_metering_2, col = "red")
        lines(datetime, Sub_metering_3, col = "blue")
        legend("topright", lwd = 1,legend = 
                       c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
                        col = c("black", "blue", "red"), bty="n")
        
        ## Top-right plot ##
        plot(datetime, Voltage, type ="l", axes = F)
        ## fixing grey box / black axes issue
        box(col="grey31")
        axis(1, at=c(z[1], median(z)+30, z[2]+60), 
             lab=c("Thu","Fri","Sat"), col = "black")
        axis(2, col="black")
        
        ##bottom-right plot
        plot(datetime, Global_reactive_power, type="l", axes = F)
        ## fixing grey box / black axes issue
        box(col="grey31")
        axis(1, at=c(z[1], median(z)+30, z[2]+60), 
             lab=c("Thu","Fri","Sat"), col = "black")
        axis(2, col="black")
})
dev.off()

## defaults for mfcol, mar
par(mfcol=c(1,1), mar = c(5.1,4.1,4.1,2.1), bg = "white")