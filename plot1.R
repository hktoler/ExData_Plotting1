plot1 <- function() {

    ## read chunks of data from file in batches of 45k lines (about a month) with 5k lines overlap
    ## stop when desired time frame is found
        
    tChunk <- read.delim("household_power_consumption.txt", nrows = 1, skip =1, header = FALSE, sep = ";", dec=".")

    dEnd = as.Date(tChunk[1,1], format = "%d/%m/%Y")
    
    i <- 1
    while (dEnd <= as.Date("02/02/2007", format= "%d/%m/%Y")){
        tChunk <- read.delim("household_power_consumption.txt", nrows = 45000, skip =i, header = FALSE, sep = ";")
        tChunk[,1] <- as.Date(tChunk[,1],"%d/%m/%Y")
        dEnd <- tChunk[45000,1]
        i <- i + 40000
    }
    colnames(tChunk) <- c("Date", "Time", "Global_active_power", 
                     "Global_reactive_power", "Voltage", "Global_intensity",
                     "Sub_metering_1","Sub_metering_2","Sub_metering_3")
    
    plotData <- tChunk[tChunk$Date >= as.Date("01/02/2007","%d/%m/%Y") & tChunk$Date <= as.Date("02/02/2007","%d/%m/%Y") ,]
    rm(tChunk)

    plotData [,3:9] <- sapply(plotData [,3:9], FUN = function(p){as.numeric(paste(p))}) 
    
    ############# End of generic data reading and cleaning code ############
    
    
    png(filename="plot1.png")
    hist(plotData$Global_active_power, col = "red", main = "Global Active Power", 
         xlab = "Global Active Power (kilowatts)", ylab = "Frequency")
    z <- dev.off()
}