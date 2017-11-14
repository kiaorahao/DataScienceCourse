getwd()
setwd("Data Science/Label56/")
library(readr)
Dataset <- read_delim("~/Data Science/Label56/Dataset.txt", 
                      ";", escape_double = FALSE, trim_ws = TRUE)

png("plot3.png")

with(subset(Dataset, Voltage >= "249"),
     plot(Voltage, Global_active_power, main = "Global Active Power(kilowatts) for Voltage >=249", type = "n"))

with(subset(Dataset,Voltage >= 249, Voltage < 250), 
     points(Voltage, Global_active_power)
     )

with(subset(Dataset,Voltage >= 250), 
     points(Voltage, Global_active_power, col = "red"))

legend("topright", pch = 1, col = c("black","red"), 
       legend = c("Voltage >=249 & <250", "Voltage >=250"))
dev.off()