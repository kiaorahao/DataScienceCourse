# Part 1 - Data Cleaning and Transformation
# A) Write a data cleaning function that makes the data set ready for further analysis. 
# This function may perform various data cleaning tasks including but not limited to 
# - Correcting possible typos
# - Removing irrelevant data (only houses in Auckland and Wellington are considered)
# - Removing outliers, e.g. negative area, negative power consumptions, very high areas, very high power consumptions
# 
# Note: You should not clean the data set manually. All the data cleaning tasks should be carried
# out by the data cleaning function automatically.

dataclean <- function(DataSet){

# 0.check column type 
  lapply(DataSet,class)
  # $Area
  # [1] "numeric"
  # 
  # $City
  # [1] "character"
  # 
  # $`P Winter`
  # [1] "numeric"
  # 
  # $`P Summer`
  # [1] "numeric"
  
# 1. get unique value of "City"  
  unique(DataSet[,"City"])
  # # A tibble: 4 x 1
  # City
  # <chr>
  # 1   Auckland
  # 2    Ackland
  # 3 Wellington
  # 4     Sydney
  
# 2.- Correcting possible typos
# 2.1. check
  DataSet$City[DataSet$City == "Ackland"]
# 2.2. remove
  DataSet$City[DataSet$City == "Ackland"] = "Auckland"
# 2.3. double-check
  DataSet$City[DataSet$City == "Ackland"]
  
# 3.- Removing irrelevant data
# 3.1. check
  subset(DataSet, City != 'Auckland' & City != 'Wellington')
# 3.2. remove
  df = subset(DataSet, City == 'Auckland' | City == 'Wellington')
# 3.3. double-check  
  subset(df, City != 'Auckland' & City != 'Wellington')
  
# 4.- Removing outliers  
# 4.1 Remove negative area, negative power consumptions 
# 4.1.1 check 
  subset(df, Area < 0 | `P Winter` < 0 | `P Summer` < 0)
# 4.1.2. remove
  df2 = subset(df, Area >= 0 & `P Winter` >= 0 & `P Summer` >= 0)
# 4.1.3. double-check 
  subset(df2, Area < 0 | `P Winter` < 0 | `P Summer` < 0)
# 4.2 Remove very high areas, very high power consumptions
# Reference: https://www.r-statistics.com/2011/01/how-to-label-all-the-outliers-in-a-boxplot/  
  # 4.2.1 check 
  subset(df2, `Area` %in% boxplot.stats(df$`Area`)$out |
             `P Winter` %in% boxplot.stats(df$`P Winter`)$out |
             `P Summer` %in% boxplot.stats(df$`P Summer`)$out) 
  # 4.2.2. remove
  df3 = subset(df2,!`Area` %in% boxplot.stats(df$`Area`)$out &
                   !`P Winter` %in% boxplot.stats(df$`P Winter`)$out &
                   !`P Summer` %in% boxplot.stats(df$`P Summer`)$out) 
  # 4.2.3. double-check   
  subset(df3, `Area` %in% boxplot.stats(df$`Area`)$out |
           `P Winter` %in% boxplot.stats(df$`P Winter`)$out |
           `P Summer` %in% boxplot.stats(df$`P Summer`)$out)  
  
# 5.- add 'P Annual' column
  df4 = transform(df3, 'P Annual' = (`P Winter` + `P Summer`)/2) 

# 6.- order by 'Area' to refine the plot   
  df4 = df4[order(df4$Area),]
  return(df4)
}


# 
# B) Write a function that calculates the annual average power consumption given "P.Winter" and
# "P.Summer". (you just need to add "P.Winter" and "P.Summer" and divide the result by two).
# By using this function, create a new variable named "P.Annual" and add it to the dataset.

cal_avepower <- function(df){
  average_power = mean(df$P.Annual) 
  return(average_power)
}

# Part 2 - Univariate Analysis
# A) Write R codes that calculate the mean and standard deviation of the annual, 
#    winter and summer power consumption. Show the results in your report by using a table.
cal_stat <- function(df){

  Type = c('Annual','Winter','Summer') 
  Mean = c(mean(df$P.Annual),mean(df$P.Winter),mean(df$P.Summer))
  SD = c(sd(df$P.Annual),sd(df$P.Winter),sd(df$P.Summer))
  
  output = data.frame(Type,Mean,SD)
  
}

# B) Write R codes that plots the density function of the annual, winter and summer power consumption.
#    Use appropriate labels for the plots. Use same scale for the plots. Add the plots to your report.

  plot_density <- function(df) {
    plot(density(df$P.Winter), main =  'Density of the power consumption', col = terrain.colors(3))
    # lines(density(df$P.Annual), col = colors()[654])
    # lines(density(df$P.Summer), col = colors()[36])
    lines(density(df$P.Annual), col = 'yellow')
    lines(density(df$P.Summer), col = 'brown')
    legend("topleft",c('P.Winter','P.Annual','P.Summer'),fill= terrain.colors(4))
}

# C) Write R codes that creates the boxplots for the annual, winter and summer power consumption. 
#    Use appropriate labels for the plots. Use same scale for the plots. Add the plots to your report.

boxplot <- function(df){
  library(plotly)
  bp = plot_ly(y = ~df$"P.Annual", type = "box", name = 'P.Annual') %>%
    add_trace(y = ~df$"P.Winter", name = 'P.Winter') %>%
    add_trace(y = ~df$"P.Summer", name = 'P.Summer')
}

# D) Write R codes that divide the data set into two subsets based on the values of "City" variable.
# df = df3
divide <- function(df,city) {
#  split(df3,df3$City)
#  df_Auckland = subset(df, City = 'Auckland')
#  df_Wellington = subset(df, City = 'Wellington')
  df_divide = subset(df, City == city)
}

# E) Write R codes that repeat tasks A, B, C for the two subsets.


# F) Compare the results obtained from the above tasks and make comments on the power
#    consumptions of Auckland and Wellington residential houses during winter and summer.

# Part 3 - Bivariate Analysis
# A) Write R codes that create a scatterplot from "P.Annual" and "Area" variables. 
# Use appropriate labels for the plots. Use same scale for the plots. Add the plots to your report.
scatterplot <- function(df,time){
  plot(x=df$Area,y=df[,time],xlab = "Area",ylab = time)
}

# B) Write R codes that calculate a linear regression model for "P.Annual" and "Area" variables. 
# Show the linear model in the scatterplot. Calculate the MSE (mean square error) of the model.

# C) Write R codes that calculate a second order polynomial regression model for "P.Annual" and "Area" variables. 
# Show the linear model in the scatterplot. Calculate the MSE of the model.

# D) Write R codes that calculate a third order polynomial regression model for "P.Annual" and "Area" variables. 
# Show the linear model in the scatterplot. Calculate the MSE of the model.

# E) Make comments on the three MSE values obtained in the previous tasks. 
# Which regression model has the highest accuracy?


# F) Repeat tasks A-E for "P.Winter" and "P.Summer".

# G) Repeat task A-F for Auckland and Wellington sub data sets.
