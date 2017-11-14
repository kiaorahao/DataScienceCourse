getwd()
setwd("Data Science/Label56/")
library(readr)
Dataset <- read_delim("~/Data Science/Label56/Dataset.txt", 
                      ";", escape_double = FALSE, trim_ws = TRUE)

png("plot2.png")
boxplot(subset(Dataset, Voltage == "237")$Global_active_power, 
        main = "Global Active Power(kilowatts) for Voltage 237", 
        col = "red",  
        xlab = "Voltage 237" )

dev.off()