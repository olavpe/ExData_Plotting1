## This function runs without input to the function itself. For the data to be 
## loaded it the "household_power_consumption.txt" file must be in the working
## directory. The function extracts the specific data and creates a lineplot
## of sub metering measures over both days. It then saves it as "plot3.png"
## in the working directory

plot3 <- function(){
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
    
    #Opening graphic device
    png("plot3.png", width = 480, height = 480)
    
    # Variables used in the plotting functions
    GActPower <- dat$Global_active_power
    xpositions <- c(1,grep("00:00:00",dat$Time),length(dat$Time))
    xlabels <- c("Thr","Fri","Sat")
    
    # The plotting functions
    plot(GActPower, xaxt = "n", type = "n" ,
         ylim = c(0,40) ,ylab = "Energy sub meeting", xlab = "")
        axis(1, at = xpositions ,labels = xlabels)
        legend('topright',colnames(dat)[7:9], lty = 1,
                col = c("black","red","blue"))
    
        lines(dat$Sub_metering_1, col = "black")
        lines(dat$Sub_metering_2, col = "red")
        lines(dat$Sub_metering_3, col = "blue")
        
    # Closes graphic device and saves png
    dev.off()
}