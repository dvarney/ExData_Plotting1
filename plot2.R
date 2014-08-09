#
# Get the remote zipped power consumption file, unzip and select only
# those dates/columns of interest for 'Global_active_power'. Create
# a line plot and add a legend. Since I removed the annotation and 
# axes, they have to be rebuilt. Put the day-of-week for the x-axis, 
# add the y-axis and labels for both. Sent a copy to a PNG file.
#
# NOTE [boilerplate-like]:
#   I have left a number of print statements, as they are useful if 
#   anything in the code needs changing later, plus I might not
#   remember how to do a time to DOW.
#
# OWNER:
#   dvarney     08/09/2014

plot2 <- function()
    {
        #set inital file paths, this computer and remote
        destDir <- "C:/Users/Doug/My Documents/GitHub/courses/ExData_Plotting1"    
        directory <- setwd(destDir)
        
        pathToZip <- paste(directory, '/ihec_pwr.zip', sep='')
        
        unZipdFile <- "household_power_consumption.txt"
        zipdFile <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
        pathToSavedZip <- paste(destDir, unZipdFile, sep='')    # point to where to store file, could use destFile
        print(pathToSavedZip)
        
        #check if already grabbed remote file
        if (!file.exists(pathToZip))  
        {
            message("PULLING ziped file")
            download.file(zipdFile,pathToZip)
        }
        else
            message("ZIP file available")
        
        #unzip the remote data file to my computer at 'unZipdFile'
        message("UNZIPPING  ", pathToZip)
        unZip <- unz(pathToZip, unZipdFile)
        
        #read into a .table struct and extract only requested data for 
        #date/times and 'Global_active_power'
        message("SELECT the datafile rows/columns")
        zipTable <- read.table(unZip, header=TRUE, sep=';', as.is=TRUE)

        #print(head(zipTable))
        subTable <- subset(zipTable, (Date=="1/2/2007"|Date=="2/2/2007"), select=c(Date,Time,Global_active_power))
        #print(nrow(subTable))
        
        #create 1 line plot for 'Global_active_power', add DOW to x-axis
        message("BUILD a line plot")
        
        x_axis <- 1
        y_axis <- 2
        xm <- nrow(subTable)
        plot(as.numeric(subTable$Global_active_power),type="l", axes=FALSE, ann=FALSE)
        
        #since both axes were removed to eliminate the 'index' subtitle, need to 
        #rebuild both and the appropriate text
        axis(y_axis, at=seq(0, 8, 2))
        axis(x_axis, at=c(1, xm/2, xm), labels=c("Thu","Fri","Sat"))
        mtext(text="Global Active Power", side=y_axis, line=2)   
        box()
        
        #open file graphics device, write PNG formatted line plot 
        #to the local dir and close device
        #wdayData <- weekdays(strptime(subTable$Date, "%d/%m/%Y"))
        message("MAKE a PNG plot: ", "plot2.png")
        dev.copy(png,'plot2.png'); dev.off()
    }

