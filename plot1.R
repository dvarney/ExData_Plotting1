#
# Get the remote zipped power consumption file, unzip and select only
# those dates/columns of interest for 'Global_active_power'. Create
# a histogram and sent a copy to a PNG file.
#
# NOTE [boilerplate-like]:
#   I have left a number of print statements, as they are useful if 
#   anything in the code needs changing later, plus I might not
#   remember how to do a time to DOW.
#
# OWNER:
#   dvarney     08/09/2014

plot1 <- function()
    {
        #set inital file paths, this computer and remote 
        destDir <- "C:/Users/Doug/My Documents/GitHub/courses/ExData_Plotting1"            
        directory <- setwd(destDir)
        pathToZip <- paste(directory, '/ihec_pwr.zip', sep='')
        
        unZipFile <- "household_power_consumption.txt"
        zipdFile <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
        pathToSavedZip <- paste(destDir, pathToZip, sep='')    # point to where to store file

        #check if already grabbed remote file
        if (!file.exists(pathToZip))  
        {
            message("PULLING ziped file")
            download.file(zipdFile, pathToZip)
        }
        else
            message("ZIP file already available")
        
        #unzip the remote data file to my computer at 'unZipFile'
        message("UNZIPPING  ", pathToZip)
        unZip <- unz(pathToZip, unZipFile)
        zipTable <- read.table(unZip, header=TRUE, sep=';', as.is=TRUE)
        #head(zipTable)

        #extract only requested data for date/times and 'Global_active_power'
        message("SELECT the datafile rows/columns")
        subTable <- subset(zipTable, (Date=="1/2/2007"|Date=="2/2/2007"), 
                           select=c(Date,Time,Global_active_power))

        #create 1 histogram for 'Global_active_power'
        #write.table(subTable, "pwrCon.out")
        message("BUILD a histogram")
        hist(as.numeric(subTable$Global_active_power), freq=TRUE, 
             main="Global Active Power", xlab="Global Active Power (kilowatts)", col="Red")

        #open file graphics device, write PNG formatted histogram 
        #plot to the local dir and close device
        message("MAKE a PNG plot: ", "plot1.png")
        dev.copy(png,'plot1.png'); dev.off()
    }

