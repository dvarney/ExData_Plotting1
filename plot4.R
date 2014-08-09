#
# Get the remote zipped power consumption file, unzip and select only
# those rows of interest for the dates required. Split the graphics 
# device into 4 segments
#   where,
#           top left:       1, Global Acvice Power
#           top right:      2, Voltage
#           bottom left:    3, Energy sub metering
#           bottom right:   4, Global reactive power
#
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

plot4 <- function()
    {
        #set inital file paths, this computer and remote
        destDir <- "C:/Users/Doug/My Documents/GitHub/courses/ExData_Plotting1/"    
        directory <- setwd(destDir)
        
        pathToZip <- paste(directory, '/ihec_pwr.zip', sep='')
        
        unZipFile <- "household_power_consumption.txt"
        zipdFile <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
        pathToSavedZip <- paste(destDir, unZipFile, sep='')    # point to where to store file, could use destFile
        #print(pathToSavedZip)
        #print(unZipFile)
        
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
        message("to ", pathToSavedZip)
        unZip <- unz(pathToZip, "household_power_consumption.txt")

        #read into a .table struct and extract only requested data for a block of date/times
        print(unZip)
        scan()
        message("SELECT the datafile rows/columns")
        zipTable <- read.table(unZip, header=TRUE, sep=';', as.is=TRUE)

        #print(head(zipTable))
        subTable <- subset(zipTable, (Date=="1/2/2007"|Date=="2/2/2007"))
        
        #print(nrow(subTable))
        #print(subTable)
        
        #create 4 line plots in a conceptual matix[2,2], which is imposed 
        #on the screen. To work on any plot, the screen number is given
        #first. 

        message("BUILD a plot")
        
        x_axis <- 1
        y_axis <- 2
        line <- "l"
        xm <- nrow(subTable)
        layout(rbind(c(1,2),c(3,4)))    #screen layout
        
        with (subTable,
        {   
            #create a line plot for each of following from the .Table column data,
            #       'Global_active_power', 
            #       'Voltage', 
            #       'Sub_metering', 1:3
            #       'Global_reactive_power'
            # 
            #The axes and annotations are turned off, so both are rebuilt. Y-axes
            #text needs adjustment a bit, 0.5 inward, towards the graph and reduced
            #in size to 0.7.The x-axes are date related, but DOW are shown. As there
            #is not border in the plots since the axes were removed, a box is placed
            #where the rectangle should be.  
            
            screen(1)
            plot(as.numeric(Global_active_power), type=line, axes=FALSE, ann=FALSE)
            axis(y_axis, at=seq(0, 8, 2), cex.axis=0.8)
            axis(x_axis, at=c(1, xm/2, xm), labels=c("Thu","Fri","Sat"), cex.axis=0.9)
            
            par(mar = c(5,3.5,4,2))     #shrink left margin a bit
            mtext(text="Global Active Power", side=y_axis, line=2, cex=0.7)   
            par(mar=c(5,4,4,2))         #reset to default
            box()

            screen(2)
            nVolts <- as.numeric(Voltage)
            xrmin <- round(min(nVolts)) 
            xrmax <- round(max(nVolts))
            
            plot(as.numeric(Voltage), type=line, axes=FALSE, ann=FALSE)
            axis(y_axis, at=seq(xrmin+1, xrmax-1, 4), cex.axis=0.8)
            axis(x_axis, c(1, xm/2, xm), labels=c("Thu","Fri","Sat"), cex.axis=0.9)
            
            par(mar=c(6,4,4,2))         #expand bottom margins, brings text up 1
            title(sub="datetime", cex.sub=0.8)                        
            par(mar = c(5,3.5,4,2))     #shrink left margin a bit
            mtext(text="Voltage", side=y_axis, line=2, cex=0.7)   
            par(mar=c(5,4,4,2))
            box()
            
            screen(3)
            plot(as.numeric(Sub_metering_1), type=line, axes=FALSE, ann=FALSE)
            lines(c(1:nrow(subTable)), as.numeric(Sub_metering_2), col="red")
            lines(c(1:nrow(subTable)), as.numeric(Sub_metering_3), col="blue")
            legend(1400, 40, c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
                   col=c("black", "red", "blue"), cex=0.7, bty="n", lty=1, lwd=2)
            axis(y_axis, at=seq(0, 40, 10), cex.axis=0.8)
            axis(x_axis, at=c(1, xm/2, xm), labels=c("Thu","Fri","Sat"), cex.axis=0.9)
            
            par(mar = c(5,3.5,4,2)) 
            mtext(text="Energy sub metering", side=y_axis, line=2, cex=0.7)   
            par(mar=c(5,4,4,2))
            box()
                        
            screen(4)
            plot(as.numeric(Global_reactive_power), type=line, main="", axes=FALSE, ann=FALSE)
            axis(y_axis, at=seq(0.0,0.5,0.1), labels=c(0.0,0.1,0.2,0.3,0.4,0.5), cex.axis=0.8)
            axis(x_axis, at=c(1, xm/2, xm), labels=c("Thu","Fri","Sat"), cex.axis=0.9)
            
            par(mar=c(6,4,4,2))         #expand bottom margins, brings text up 1
            title(sub="datetime", cex.sub=0.8)            
            par(mar = c(5,3.5,4,2))     #shrink left margin a bit
            mtext(text="Global_reactive_power", side=y_axis, line=2, cex=0.7)   
            par(mar=c(5,4,4,2))     
            box()
        })
                
        #wdayData <- weekdays(strptime(subTable$Date, "%d/%m/%Y"))
        message("MAKE a PNG file: ", "plot4.png")
        dev.copy(png,'plot4.png'); dev.off()
    }


