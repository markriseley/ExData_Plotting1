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

## Plot2 ##
png("data/plot2.png")
par(bg = "transparent")
ylab <- "Global Active Power (kilowatts)"
z <- range(myDT$datetime) # used for recreating tickmarks
with(myDT, {
        plot(datetime, Global_active_power, 
                type = "l", xlab = "", ylab = ylab, axes = F)
        box(col="grey31")
        ## Hacky way to recreate black axes on grey plot border as per original
        ## No luck finding default changing for 
        ## plot border colours in ?par, or searching online eg "change default 
        ## plot border colour r". Instead added a grey box after plotting
        ## and added fresh axes (but default axis adds different labels &
        ## tick mark locations than original plot so have had to fix this)
        axis(1, at=c(z[1], median(z)+30, z[2]+60), ## lazy way to get midnight values
             lab=c("Thu","Fri","Sat"), col = "black")
        axis(2, col="black")
})
dev.off()

