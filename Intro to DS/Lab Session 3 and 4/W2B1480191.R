# 1. Plot the histograms of L, H, N, B in one page.
rm(list=ls())
cat("\14")
library(readr)
table1 <- read_csv("~/Documents/OneDrive/PGD/Data Science/Lab Session 3 and 4/Datasets/Data Set 3.csv")
library(plotly)
PL = plot_ly(x = ~table1$L, type = 'histogram', name = 'Histograms of L')
PH = plot_ly(x = ~table1$H, type = 'histogram', name = 'Histograms of H')
PN = plot_ly(x = ~table1$N, type = 'histogram', name = 'Histograms of N')
PB = plot_ly(x = ~table1$B, type = 'histogram', name = 'Histograms of B')
p_histograms <- subplot(PL, PH,PN, PB,nrows = 2)

# 2. Plot the density functions of L, H, N, B in one page.
fit = density(table1$L)
#PL = plot_ly(x = ~table1$L, type = 'histogram', name = 'Density Function of L')%>%
#  add_trace(x = fit$x, y = fit$y, type = 'scatter', mode = 'lines', fill = 'tozeroy',
#            yaxis = 'y2', name = 'Density')%>%
#  layout(yaxis2 = list(overlaying = 'y', side = 'right'))

PL = plot_ly(x = fit$x, y = fit$y, type = 'scatter', mode = 'lines', fill = 'tozeroy',
          name = 'Density Function of L')

fit = density(table1$H)
# PH = plot_ly(x = ~table1$H, type = 'histogram', name = 'Density Function of H')%>%
#   add_trace(x = fit$x, y = fit$y, type = 'scatter', mode = 'lines', fill = 'tozeroy',
#             yaxis = 'y2', name = 'Density')%>%
#   layout(yaxis2 = list(overlaying = 'y', side = 'right'))

PH = plot_ly(x = fit$x, y = fit$y, type = 'scatter', mode = 'lines', fill = 'tozeroy',
            name = 'Density Function of H')
  
fit = density(table1$N)
# PN = plot_ly(x = ~table1$N, type = 'histogram', name = 'Density Function of N')%>%
#   add_trace(x = fit$x, y = fit$y, type = 'scatter', mode = 'lines', fill = 'tozeroy',
#             yaxis = 'y2', name = 'Density')%>%
#   layout(yaxis2 = list(overlaying = 'y', side = 'right'))

PN = plot_ly(x = fit$x, y = fit$y, type = 'scatter', mode = 'lines', fill = 'tozeroy',
           name = 'Density Function of N')
  
fit = density(table1$B)

# PB = plot_ly(x = ~table1$B, type = 'histogram', name = 'Density Function of B')%>%
#   add_trace(x = fit$x, y = fit$y, type = 'scatter', mode = 'lines', fill = 'tozeroy',
#             yaxis = 'y2', name = 'Density')%>%
#   layout(yaxis2 = list(overlaying = 'y', side = 'right'))

PB = plot_ly(x = fit$x, y = fit$y, type = 'scatter', mode = 'lines', fill = 'tozeroy',
            name = 'Density Function of B')
  
p_df <- subplot(PL, PH,PN, PB,nrows = 2)

# 3. Compare the density functions against a normal density function.
library(moments)

skewness(table1$L)
skewness(table1$H)
skewness(table1$N)
skewness(table1$B)

kurtosis(table1$L)
kurtosis(table1$H)
kurtosis(table1$N)
kurtosis(table1$B)

# 4. Create the boxplots of L, H, N and B using a similar scale. 
p <- plot_ly(y = ~table1$L, type = "box", name = 'L') %>%
  add_trace(y = ~table1$H, name = 'H') %>%
  add_trace(y = ~table1$N, name = 'N') %>%
  add_trace(y = ~table1$B, name = 'B')

# 5. Calculate the man, variance and standard deviation of L, H, N and B and complete the following table.

sd(table1$L)  #[1] 5.377844
sd(table1$H)  #[1] 4.939346
sd(table1$N)  #[1] 3.207932
sd(table1$B)  #[1] 4.89068

mean(table1$L) #[1] 96.46
mean(table1$H) #[1] 132.5467
mean(table1$N) #[1] 50.93333
mean(table1$B) #[1] 133.9733

var(table1$L) #[1] 28.92121
var(table1$H) #[1] 24.39714
var(table1$N) #[1] 10.29083
var(table1$B) #[1] 23.91875

