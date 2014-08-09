#
# Get the remote zipped power consumption file, unzip and select only
# those dates/columns of interest for 'Energy Sub Metering'. Create
# a line plot, overlay the 3 parameters and add a legend. Since I 
# removed the annotation and axes, they have to be rebuilt. Put the 
# day-of-week for the x-axis, add the y-axis and labels for both. Sent 
# a copy to a PNG file.
#
# NOTE [boilerplate-like]:
#   I have left a number of print statements, as they are useful if 
#   anything in the code needs changing later, plus I might not
#   remember how to do a time to DOW.
#
# OWNER:
#   dvarney     08/09/2014

plot3 <- function()
    {
        #set inital file paths, this computer and remote
        destDir <- "C:/Users/Doug/My Documents/GitHub/courses/ExData_Plotting1"    
        directory <- setwd(destDir)
        
        pathToZip <- paste(directory, '/ihec_pwr.zip', sep='')
        
        unZipFile <- "household_power_consumption.txt"
        zipdFile <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
        pathToSavedZip <- paste(destDir, unZipFile, sep='')    # point to where to store file, could use destFile
        print(pathToSavedZip)
        
        #check if already grabbed remote file
        if (!file.exists(pathToZip))  
        {
            message("PULLING ziped file")
            download.file(zipdFile,pathToZip)
        }
        else
            message("ZIP file already available")

        #unzip the remote data file to my computer at 'unZipdFile'
        message("UNZIPPING  ", pathToZip)
        unZip <- unz(pathToZip, "household_power_consumption.txt")
        
        #read into a .table struct and extract only requested data for 
        #date/times and 'Energy Sub Metering'
        #print(unZip)
        message("SELECT the datafile rows/columns")
        zipTable <- read.table(unZip, header=TRUE, sep=';', as.is=TRUE)

        #print(head(zipTable))
        subTable <- subset(zipTable, (Date=="1/2/2007"|Date=="2/2/2007"), 
                           select=c(Date,Time,Sub_metering_1, Sub_metering_2, Sub_metering_3))
        #print(nrow(subTable))
        #print(subTable)
        
        #create 1 line plot for 'Energy Sub Metering', add DOW to x-axis and
        #overlay 3 sub metering meaasurements
        message("BUILD a plot")
        with (subTable,
        {
            plot(as.numeric(Sub_metering_1), type="l", axes=FALSE, ann=FALSE)
            lines(c(1:nrow(subTable)), as.numeric(Sub_metering_2), col="red")
            lines(c(1:nrow(subTable)), as.numeric(Sub_metering_3), col="blue")
        })
        
        #build a legend for 3 sub_meter readings and add an enclosing box
        #since the PNG copy changes the aspect ratio (I think), the legend
        #box needs to be shifted left to 1800 (x0axis user-coordinates)
        #instead of 2250. Also, do not add the box, will need to add later.
        
        #needed to adj legend box w/rect for PNG, grumble, grumble
        legend(1800, 40, c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
               col=c("black", "red", "blue"),  bty="n", lty=1, lwd=2)
        rect(1800, 40, 3400, 31.5) 
        
        #since both axes were removed to eliminate the 'index' subtitle, need to 
        #rebuild both and the appropriate text
        x_axis <- 1
        y_axis <- 2
        xm <- nrow(subTable)
        
        axis(y_axis, at=seq(0, 40, 10))
        axis(x_axis, at=c(1, xm/2, xm), labels=c("Thu","Fri","Sat"))
        mtext(text="Energy sub metering", side=y_axis, line=2)
        box()

        #open file graphics device, write PNG formatted line plot 
        #to the local dir and close device
        #wdayData <- weekdays(strptime(subTable$Date, "%d/%m/%Y"))
        message("MAKE a PNG plot: ", "plot3.png")
        dev.copy(png,'plot3.png'); dev.off()
    }


