# 0.Initialization
# load data and source code
library(readr)
DataSet <- read_csv("D:/onedrive/PGD/Data Science/Assign B/Data Set 6.csv")

source('D:/onedrive/PGD/Data Science/Assign B/assignB_script.R')

# Part 1 - Data Cleaning and Transformation
# A) Write a data cleaning function that makes the data set ready for further analysis. 
# This function may perform various data cleaning tasks including but not limited to 
# - Correcting possible typos
# - Removing irrelevant data (only houses in Auckland and Wellington are considered)
# - Removing outliers, e.g. negative area, negative power consumptions, very high areas, very high power consumptions
# 
# Note: You should not clean the data set manually. All the data cleaning tasks should be carried
# out by the data cleaning function automatically.

df = dataclean(DataSet)

# B) Write a function that calculates the annual average power consumption given "P.Winter" and
# "P.Summer". (you just need to add "P.Winter" and "P.Summer" and divide the result by two).
# By using this function, create a new variable named "P.Annual" and add it to the dataset.

avepower = cal_avepower(df)
#[1] 1616.925
#
# Part 2 - Univariate Analysis
# A) Write R codes that calculate the mean and standard deviation of the annual, 
#    winter and summer power consumption. Show the results in your report by using a table.

stat = cal_stat(df)

# B) Write R codes that plots the density function of the annual, winter and summer power consumption.
#    Use appropriate labels for the plots. Use same scale for the plots. Add the plots to your report.

dplot = plot_density(df) 

# C) Write R codes that creates the boxplots for the annual, winter and summer power consumption. 
#    Use appropriate labels for the plots. Use same scale for the plots. Add the plots to your report.

bplot = boxplot(df)

# D) Write R codes that divide the data set into two subsets based on the values of "City" variable.

df_Auckland = divide(df,'Auckland')
df_Wellington = divide(df,'Wellington')

# E) Write R codes that repeat tasks A, B, C for the two subsets.
# 1.subset of df_Auckland

# A 
stat_Auckland = cal_stat(df_Auckland)
# B 
dplot_Auckland = plot_density(df_Auckland)
# C 
bplot_Auckland = boxplot(df_Auckland)

# 2.subset of Wellington

# A 
stat_Wellington =cal_stat(df_Wellington)
# B 
dplot_Wellington = plot_density(df_Wellington)
# C 
bplot_Wellington = boxplot(df_Wellington)


# F) Compare the results obtained from the above tasks and make comments on the power
#    consumptions of Auckland and Wellington residential houses during winter and summer.

# Part 3 - Bivariate Analysis
# A) Write R codes that create a scatterplot from "P.Annual" and "Area" variables. 
# Use appropriate labels for the plots. Use same scale for the plots. Add the plots to your report.
splot_annual = scatterplot(df,'P.Annual')

# B) Write R codes that calculate a linear regression model for "P.Annual" and "Area" variables. 
# Show the linear model in the scatterplot. Calculate the MSE (mean square error) of the model.

  model_annual = lm(df$P.Annual~df$Area)
  lines(df$Area,model_annual$fitted.values,col='orange')
  error = (model_annual$fitted.values - df$P.Annual)
  MSE_annual = mean(error^2)
# [1] 11577.44

# C) Write R codes that calculate a second order polynomial regression model for "P.Annual" and "Area" variables. 
# Show the linear model in the scatterplot. Calculate the MSE of the model.

  model2_annual = lm(df$P.Annual~poly(df$Area,2))
  lines(df$Area,model2_annual$fitted.values,col='red')
  error = model2_annual$fitted.values - df$P.Annual
  MSE2 = mean(error^2)  
# [1] 8718.943

# D) Write R codes that calculate a third order polynomial regression model for "P.Annual" and "Area" variables. 
# Show the linear model in the scatterplot. Calculate the MSE of the model.

  model3_annual = lm(df$P.Annual~poly(df$Area,3))
  lines(df$Area,model3_annual$fitted.values,col='blue')
  legend('bottomright',c('LineReg','2nd polynomialReg','3rd polynomialReg'),
         col = c('orange','red','blue'),lwd=3)
  error = model3_annual$fitted.values - df$P.Annual
  MSE3 = mean(error^2)  
#   [1] 8343.488
# E) Make comments on the three MSE values obtained in the previous tasks. 
# Which regression model has the highest accuracy?
# B.Third order polynomial regression model has the highest accuracy.
  
# F) Repeat tasks A-E for "P.Winter" and "P.Summer".
# 1."P.Winter"
  # A)
  splot_Winter = scatterplot(df,'P.Winter')
  # B)
  model_Winter = lm(df$P.Winter~df$Area)
  lines(df$Area,model_Winter$fitted.values,col='orange')
  error = model_Winter$fitted.values - df$P.Winter
  MSE_Winter = mean(error^2)
  # [1] 15579.95
  # C)
  model2_Winter = lm(df$P.Winter~poly(df$Area,2))
  lines(df$Area,model2_Winter$fitted.values,col='red')
  error = model2_Winter$fitted.values - df$P.Winter
  MSE2_Winter = mean(error^2)  
  # [1] 8948.185
  # D)
  model3_Winter = lm(df$P.Winter~poly(df$Area,3))
  lines(df$Area,model3_Winter$fitted.values,col='blue')
  legend('bottomright',c('LineReg','2nd polynomialReg','3rd polynomialReg'),
         col = c('orange','red','blue'),lwd=3)
  error = model3_Winter$fitted.values - df$P.Winter
  MSE3_Winter = mean(error^2)  
  # [1] 8874.323  
  # E)
  # B.Third order polynomial regression model has the highest accuracy.
  
# 2."P.Summer"
  # A)
  splot_Summer = scatterplot(df,'P.Summer')
  # B)
  model_Summer = lm(df$P.Summer~df$Area)
  lines(df$Area,model_Summer$fitted.values,col='orange')
  error = model_Summer$fitted.values - df$P.Summer
  MSE_Summer = mean(error^2)
  # [1] 19964.4
  # C)
  model2_Summer = lm(df$P.Summer~poly(df$Area,2))
  lines(df$Area,model2_Summer$fitted.values,col='red')
  error = model2_Summer$fitted.values - df$P.Summer
  MSE2_Summer = mean(error^2)  
  # [1] 19314.44
  # D)
  model3_Summer = lm(df$P.Summer~poly(df$Area,3))
  lines(df$Area,model3_Summer$fitted.values,col='blue')
  legend('bottomright',c('LineReg','2nd polynomialReg','3rd polynomialReg'),
         col = c('orange','red','blue'),lwd=3)
  error = model3_Summer$fitted.values - df$P.Summer
  MSE3_Summer = mean(error^2)  
  # [1] 17072.64    
  # E)
  # B.Third order polynomial regression model has the highest accuracy.
  
# G) Repeat task A-F for Auckland and Wellington sub data sets.
#  1.Auckland
  # 1.1 "P.Annual"
  # A)
  splot_Annual = scatterplot(df_Auckland,'P.Annual')
  # B)
  model_Annual = lm(df_Auckland$P.Annual~df_Auckland$Area)
  lines(df_Auckland$Area,model_Annual$fitted.values,col='orange')
  error = model_Annual$fitted.values - df_Auckland$P.Annual
  MSE_Annual = mean(error^2)
  # [1] 5782.934
  # C)
  model2_Annual = lm(df_Auckland$P.Annual~poly(df_Auckland$Area,2))
  lines(df_Auckland$Area,model2_Annual$fitted.values,col='red')
  error = model2_Annual$fitted.values - df_Auckland$P.Annual
  MSE2_Annual = mean(error^2)  
  # [1] 4266.965
  # D)
  model3_Annual = lm(df_Auckland$P.Annual~poly(df_Auckland$Area,3))
  lines(df_Auckland$Area,model3_Annual$fitted.values,col='blue')
  legend('bottomright',c('LineReg','2nd polynomialReg','3rd polynomialReg'),
         col = c('orange','red','blue'),lwd=3)
  error = model3_Annual$fitted.values - df_Auckland$P.Annual
  MSE3_Annual = mean(error^2)  
  # [1] 3661.838  
  # E)
  # B.Third order polynomial regression model has the highest accuracy.
  
  # 1.2 "P.Winter"
  # A)
  splot_Winter = scatterplot(df_Auckland,'P.Winter')
  # B)
  model_Winter = lm(df_Auckland$P.Winter~df_Auckland$Area)
  lines(df_Auckland$Area,model_Winter$fitted.values,col='orange')
  error = model_Winter$fitted.values - df_Auckland$P.Winter
  MSE_Winter = mean(error^2)
  # [1] 14532.9
  # C)
  model2_Winter = lm(df_Auckland$P.Winter~poly(df_Auckland$Area,2))
  lines(df_Auckland$Area,model2_Winter$fitted.values,col='red')
  error = model2_Winter$fitted.values - df_Auckland$P.Winter
  MSE2_Winter = mean(error^2)  
  # [1] 7951.114
  # D)
  model3_Winter = lm(df_Auckland$P.Winter~poly(df_Auckland$Area,3))
  lines(df_Auckland$Area,model3_Winter$fitted.values,col='blue')
  legend('bottomright',c('LineReg','2nd polynomialReg','3rd polynomialReg'),
         col = c('orange','red','blue'),lwd=3)
  error = model3_Winter$fitted.values - df_Auckland$P.Winter
  MSE3_Winter = mean(error^2)  
  # [1] 7949.242  
  # E)
  # B.Third order polynomial regression model has the highest accuracy.
  
  # 1.3."P.Summer"
  # A)
  splot_Summer = scatterplot(df_Auckland,'P.Summer')
  # B)
  model_Summer = lm(df_Auckland$P.Summer~df_Auckland$Area)
  lines(df_Auckland$Area,model_Summer$fitted.values,col='orange')
  error = model_Summer$fitted.values - df_Auckland$P.Summer
  MSE_Summer = mean(error^2)
  # [1] 8682.97
  # C)
  model2_Summer = lm(df_Auckland$P.Summer~poly(df_Auckland$Area,2))
  lines(df_Auckland$Area,model2_Summer$fitted.values,col='red')
  error = model2_Summer$fitted.values - df_Auckland$P.Summer
  MSE2_Summer = mean(error^2)  
  # [1] 8672.36
  # D)
  model3_Summer = lm(df_Auckland$P.Summer~poly(df_Auckland$Area,3))
  lines(df_Auckland$Area,model3_Summer$fitted.values,col='blue')
  legend('bottomright',c('LineReg','2nd polynomialReg','3rd polynomialReg'),
         col = c('orange','red','blue'),lwd=3)
  error = model3_Summer$fitted.values - df_Auckland$P.Summer
  MSE3_Summer = mean(error^2)  
  # [1] 6115.346    
  # E)
  # B.Third order polynomial regression model has the highest accuracy.
  
  #  2.Wellington
  # 2.1 "P.Annual"
  # A)
  splot_Annual = scatterplot(df_Wellington,'P.Annual')
  # B)
  model_Annual = lm(df_Wellington$P.Annual~df_Wellington$Area)
  lines(df_Wellington$Area,model_Annual$fitted.values,col='orange')
  error = model_Annual$fitted.values - df_Wellington$P.Annual
  MSE_Annual = mean(error^2)
  # [1] 7218.791
  # C)
  model2_Annual = lm(df_Wellington$P.Annual~poly(df_Wellington$Area,2))
  lines(df_Wellington$Area,model2_Annual$fitted.values,col='red')
  error = model2_Annual$fitted.values - df_Wellington$P.Annual
  MSE2_Annual = mean(error^2)  
  # [1] 2442.235
  # D)
  model3_Annual = lm(df_Wellington$P.Annual~poly(df_Wellington$Area,3))
  lines(df_Wellington$Area,model3_Annual$fitted.values,col='blue')
  legend('bottomright',c('LineReg','2nd polynomialReg','3rd polynomialReg'),
         col = c('orange','red','blue'),lwd=3)
  error = model3_Annual$fitted.values - df_Wellington$P.Annual
  MSE3_Annual = mean(error^2)  
  # [1] 2394.252  
  # E)
  # B.Third order polynomial regression model has the highest accuracy.
  
  # 2.2. "P.Winter"
  # A)
  splot_Winter = scatterplot(df_Wellington,'P.Winter')
  # B)
  model_Winter = lm(df_Wellington$P.Winter~df_Wellington$Area)
  lines(df_Wellington$Area,model_Winter$fitted.values,col='orange')
  error = model_Winter$fitted.values - df_Wellington$P.Winter
  MSE_Winter = mean(error^2)
  # [1] 14287.97
  # C)
  model2_Winter = lm(df_Wellington$P.Winter~poly(df_Wellington$Area,2))
  lines(df_Wellington$Area,model2_Winter$fitted.values,col='red')
  error = model2_Winter$fitted.values - df_Wellington$P.Winter
  MSE2_Winter = mean(error^2)  
  # [1] 7663.161
  # D)
  model3_Winter = lm(df_Wellington$P.Winter~poly(df_Wellington$Area,3))
  lines(df_Wellington$Area,model3_Winter$fitted.values,col='blue')
  legend('bottomright',c('LineReg','2nd polynomialReg','3rd polynomialReg'),
         col = c('orange','red','blue'),lwd=3)
  error = model3_Winter$fitted.values - df_Wellington$P.Winter
  MSE3_Winter = mean(error^2)  
  # [1] 7331.925  
  # E)
  # B.Third order polynomial regression model has the highest accuracy.
  
  # 2.3 "P.Summer"
  # A)
  splot_Summer = scatterplot(df_Wellington,'P.Summer')
  # B)
  model_Summer = lm(df_Wellington$P.Summer~df_Wellington$Area)
  lines(df_Wellington$Area,model_Summer$fitted.values,col='orange')
  error = model_Summer$fitted.values - df_Wellington$P.Summer
  MSE_Summer = mean(error^2)
  # [1] 6489.563
  # C)
  model2_Summer = lm(df_Wellington$P.Summer~poly(df_Wellington$Area,2))
  lines(df_Wellington$Area,model2_Summer$fitted.values,col='red')
  error = model2_Summer$fitted.values - df_Wellington$P.Summer
  MSE2_Summer = mean(error^2)  
  # [1] 3259.647
  # D)
  model3_Summer = lm(df_Wellington$P.Summer~poly(df_Wellington$Area,3))
  lines(df_Wellington$Area,model3_Summer$fitted.values,col='blue')
  legend('bottomright',c('LineReg','2nd polynomialReg','3rd polynomialReg'),
         col = c('orange','red','blue'),lwd=3) 
  error = model3_Summer$fitted.values - df_Wellington$P.Summer
  MSE3_Summer = mean(error^2)  
  # [1] 2232.194    
  # E)
  # B.Third order polynomial regression model has the highest accuracy.