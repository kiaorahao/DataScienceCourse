getwd()
setwd("Data Science/Label56/")
library(readr)
Dataset <- read_delim("~/Data Science/Label56/Dataset.txt", 
                      ";", escape_double = FALSE, trim_ws = TRUE)

png("plot4.png")

par(mfrow = c(3,1))
hist(subset(Dataset, Sub_metering_2 == 0)$Global_active_power, 
     main = "Global Active Power for Sub_metering_2 =0.000 ", col = "red", 
     xlab = "Global Active Power(kilowatts)")
hist(subset(Dataset, Sub_metering_2 == 1)$Global_active_power, 
     main = "Global Active Power for Sub_metering_2 =1.000 ", col = "red", 
     xlab = "Global Active Power(kilowatts)")
hist(subset(Dataset, Sub_metering_2 == 2)$Global_active_power, 
     main = "Global Active Power for Sub_metering_2 =2.000 ", col = "red", 
     xlab = "Global Active Power(kilowatts)")
dev.off()
