# 1. Omitting all the transactions made before 1 Jan 2010
library(readr)
table1 <- read_csv("~/Documents/OneDrive/PGD/Data Science/Lab Session 3 and 4/Datasets/Data Set 1a.csv")
table2 <- read_csv("~/Documents/OneDrive/PGD/Data Science/Lab Session 3 and 4/Datasets/Data Set 1b.csv")
table3 = merge(table1,table2,"Product")
table4 = subset(table3,table3$Date>'2017-01-01')

# 2. Calculating monthly total sale
library(zoo)
table4$YearMon=as.yearmon(table4$Date)
GroupColumns = c('YearMon')
DataColumns=c('Quantity','Unit.Price.NZD')
res=ddply(table4,GroupColumns,function(x) colSums(x[DataColumns]))
table5=head(res)

# 3. Visualizing the results (monthly total sale) by using appropriate charts/plots.
install.packages('plotly')
library(plotly)
packageVersion('plotly')
table6=data.frame(table5)
p_Quantity = plot_ly(
  x = table6$YearMon,
  y = table6$Quantity,
  name = 'Quantity',
  type = 'bar'
)

p_Prize = plot_ly(
  x = table6$YearMon,
  y = table6$Unit.Price.NZD,
  name = 'Prize',
  type = 'bar'
)