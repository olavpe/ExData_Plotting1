## This function runs without input to the function itself. For the data to be 
## loaded it the "household_power_consumption.txt" file must be in the working
## directory. The function extracts the specific data and creates 4 various
## plots and saves them onto one image as "plot4.png" file in the working
## directory

plot4 <- function(){
    #Loading specific data from 01/02/2007 to 02/02/2007 in datafile
    allLines <- readLines("household_power_consumption.txt")
    lines <- grep("^[12]/2/2007",substr(allLines,1,8))
    dat <- read.table(text = allLines[lines],
                       header = TRUE,sep = ";",
                       col.names = c("Date", "Time", "Global_active_power",
                                     "Global_reactive_power","Voltage",
                                     "Global_intensity", "Sub_metering_1",
                                     "Sub_metering_2",
                                     "Sub_metering_3"))
    
    ## Preparing dateime and other input for plotting functions
    dat$datetime <- with(dat, paste(Date, Time, sep = " "))
    dat$datetime <- strptime(dat$datetime, format='%d/%m/%Y %T')
    
    GActPower <- dat$Global_active_power
    xpositions <- c(1,grep("2007-02-02 00:00:00",dat$datetime),
                    length(dat$datetime))
    xlabels <- c("Thr","Fri","Sat")
    
    # Opening graphic device
    png("plot4.png", width = 480, height = 480)
    
    # Setting up 4 figure image that the plots will be loaded into
    par(mfrow = c(2,2))
    
    #### The plotting functions themselves
    ## 1st plot
    plot(GActPower, xaxt = "n", type = "l" , ylab = "Global Active Power (kW)",
         xlab = "")
        axis(1, at = xpositions ,labels = xlabels)
        
    ## 2nd plot
    plot(dat$Voltage, xaxt = "n", type = "l" , ylab = "Voltage",
         xlab = "datetime")
        axis(1, at = xpositions ,labels = xlabels)
        
    ## 3rd plot
    plot(GActPower, xaxt = "n", type = "n" ,
         ylim = c(0,40) ,ylab = "Energy sub meeting", xlab = "")
        
        axis(1, at = xpositions ,labels = xlabels)
        legend('topright',colnames(dat)[7:9], lty = 1,
                 col = c("black","red","blue"))
        lines(dat$Sub_metering_1, col = "black")
        lines(dat$Sub_metering_2, col = "red")
        lines(dat$Sub_metering_3, col = "blue")
    
    ## 4th plot
    plot(dat$Global_reactive_power, xaxt = "n", type = "l" ,
         ylab = "Global Reactive Power", xlab = "datetime")
        axis(1, at = xpositions ,labels = xlabels)
    
    # Closes graphic device and saves png
    dev.off()
}