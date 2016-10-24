## This function runs without input to the function itself. For the data to be 
## loaded it the "household_power_consumption.txt" file must be in the working
## directory. The function extracts the specific data and creates a barplot
## of frequency of instances of global active power in kW. It thensaves it
## as "plot1.png" in the working directory

plot1 <- function(){
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
    
    # Calculating data for plotting
    GActPower <- dat$Global_active_power
    frequency <- table((GActPower%/%0.5+1) * 0.5)
    
    #Opening graphic device
    png("plot1.png", width = 480, height = 480)
    
    # The plots themselves
    atXvalues <- seq(0,16,4)
    Xvalues <- seq(0,8,2)
    w <- rep(1,length(frequency))
    barplot(frequency, col = "red", space = 0, xaxt = "n",
            ylab = "Frequency", xlab = "Global Active Power (kW)",
            main = "Global Active Power")
    axis(1, at = atXvalues, labels = Xvalues)
    # Closes graphic device and saves png
    dev.off()
}