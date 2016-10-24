## This function runs without input to the function itself. For the data to be 
## loaded it the "household_power_consumption.txt" file must be in the working
## directory. The function extracts the specific data and creates a lineplot
## of global active power (kW) over both days. It then saves it as "plot2.png"
## in the working directory

plot2 <- function(){
    #Loading specific data from 01/02/2007 to 02/02/2007 in datafile
    allLines <- readLines("household_power_consumption.txt")
    lines <- grep("^[12]/2/2007",substr(allLines,1,8))
    data <- read.table(text = allLines[lines],
                       header = TRUE,sep = ";",
                       col.names = c("Date", "Time", "Global_active_power",
                                     "Global_reactive_power","Voltage",
                                     "Global_intensity", "Sub_metering_1",
                                     "Sub_metering_2",
                                     "Sub_metering_3"))
    
    #Opening graphic device
    png("plot2.png", width = 480, height = 480)
    
    # The plots themselves
    GActPower <- data$Global_active_power
    xpositions <- c(1,grep("00:00:00",data$Time),length(data$Time))
    xlabels <- c("Thr","Fri","Sat")
    
    plot(GActPower, xaxt = "n", type = "l" , ylab = "Global Active Power (kW)",
         xlab = "")
    axis(1, at = xpositions ,labels = xlabels)
    
    # Closes graphic device and saves png
    dev.off()
}