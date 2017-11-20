# 0. raw data cleaning
rm(list=ls())
cat("\14")
library(readr)
table1 <- read_csv("~/Data Science/Datasets/Data Set 1a.csv")
table2 <- read_csv("~/Data Science/Datasets/Data Set 1b.csv")

table1$Quantity = as.numeric(table1$Quantity)

levels(table1$Product)	<- c(levels(table1$Product),	"Galaxy Edge S8")
table1$Product[table1$Product	== "Galaxi Edge S8"]	<- "Galaxy Edge S8"

table1$Product[table1$Product	=="ALice"]<-"Alice"
names(table1)[names(table1)=='Name']<-'Customer'

table1$Quantity[table1$Quantity=="one"]<-"1"
table1$Quantity[table1$Quantity=="two"]<-"2"

library(lubridate)
table1$Date = dmy(table1$Date)

table3 = merge(table1,table2,"Product")

Calculate.Rate <- function(arg1){	
  if	(arg1=='NZD')	{out<-1}	
  if	(arg1=='AUD')	{out<-1.2}		
  return(out)
}
table3["Exchange.Rate"]<-sapply(table3$Currency,Calculate.Rate)
table3$Unit.Price.NZD<-table3$`Unit Price`*table3$Exchange.Rate
#  table3$Unit.Price.NZD = as.numeric(table3$Unit.Price.NZD)

# 1. Omitting all the transactions made before 1 Jan 2010


table4 = subset(table3,table3$Date>'2010-01-01')

# 2. Calculating monthly total sale
library(zoo)
library(plyr)

table4$YearMon=as.yearmon(table4$Date)
GroupColumns = c('YearMon')
DataColumns = c('Quantity','Unit.Price.NZD')
#DataColumns=c('Unit.Price.NZD')
#res = ddply(table4,GroupColumns,function(x) colSums(x[DataColumns]))
res = ddply(table4,GroupColumns,function(x) colSums(x[DataColumns]))
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

