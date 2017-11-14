# 1. Create a scatter plot for petal length and width variables.
library(readr)
table1 <- read_csv("~/Documents/OneDrive/PGD/Data Science/Lab Session 3 and 4/Datasets/Data Set 4.csv")

p = plot_ly(data = table1, type = 'scatter', mode = 'markers', x = ~Petal.Length, y = ~Petal.Width, name = 'Scatter')

# 2. Calculate a liner model between petal length and width and show it in the scatter plot.

fit_all = lm(data = table1, Petal.Width ~ Petal.Length)
p = plot_ly(data = table1, type = 'scatter', mode = 'markers', x = ~Petal.Length, y = ~Petal.Width, name = 'Scatter') %>%
  add_trace(data = table1, type = 'scatter', mode = 'lines', x = ~Petal.Length, y = fitted(fit_all), name = 'Linear Model')

# 3. Based on the Species data, subdivide the iris dataset into three separate subsets (for each species).
unique(table1$Species)
table_se = subset(table1,table1$Species =='setosa')
table_ve = subset(table1,table1$Species =='versicolor')
table_vi = subset(table1,table1$Species =='virginica')

# 4. Repeat steps 1 and 2 for each subsets.
# 4.1. setosa
p_se_1 = plot_ly(data = table_se, type = 'scatter', mode = 'markers', x = ~Petal.Length, y = ~Petal.Width, name = 'Scatter')

fit_1 = lm(data = table_se, Petal.Width ~ Petal.Length)
p_se_2 = plot_ly(data = table_se, type = 'scatter', mode = 'markers', x = ~Petal.Length, y = ~Petal.Width, name = 'Scatter') %>%
  add_trace(data = table_se, type = 'scatter', mode = 'lines', x = ~Petal.Length, y = fitted(fit_1), name = 'Linear Model')

# 4.2. versicolor
p_ve_1 = plot_ly(data = table_ve, type = 'scatter', mode = 'markers', x = ~Petal.Length, y = ~Petal.Width, name = 'Scatter')

fit_2 = lm(data = table_ve, Petal.Width ~ Petal.Length)
p_ve_2 = plot_ly(data = table_ve, type = 'scatter', mode = 'markers', x = ~Petal.Length, y = ~Petal.Width, name = 'Scatter') %>%
  add_trace(data = table_ve, type = 'scatter', mode = 'lines', x = ~Petal.Length, y = fitted(fit_2), name = 'Linear Model')

# 4.3. virginica
p_vi_1 = plot_ly(data = table_vi, type = 'scatter', mode = 'markers', x = ~Petal.Length, y = ~Petal.Width, name = 'Scatter')

fit_3 = lm(data = table_vi, Petal.Width ~ Petal.Length)
p_vi_2 = plot_ly(data = table_vi, type = 'scatter', mode = 'markers', x = ~Petal.Length, y = ~Petal.Width, name = 'Scatter') %>%
  add_trace(data = table_vi, type = 'scatter', mode = 'lines', x = ~Petal.Length, y = fitted(fit_3), name = 'Linear Model')


# 5. Plot all the results including scatter plots and linear models in one plot. 

p_all <- subplot(p_se_2, p_ve_2,p_vi_2, shareX = T, shareY = T, titleX = F, titleY = F)

# 6. Show that the linear models obtained for the three subsets can describe the relationship 
#    between petal length and width more accurately.
error_all = fit_all$fitted.value-table1$Petal.Width
MSE_all = mean(error_all^2)
# [1] 0.04206731

error_1 = fit_1$fitted.value-table1$Petal.Width
MSE_1 = mean(error_1^2)
# [1] 1.487158

error_2 = fit_2$fitted.value-table1$Petal.Width
MSE_2 = mean(error_2^2)
# [1] 0.6041109

error_3 = fit_3$fitted.value-table1$Petal.Width
MSE_3 = mean(error_3^2)
# [1] 1.264536

# Conclusion: MSE_1, MSE_2 and MSE_3 shows much more higher value than MSE_all presents more accurate relationship indication
#             between petal length and width.


# 7. Repeat the above steps for sepal length and sepal width data.

# step1: Create a scatter plot
p = plot_ly(data = table1, type = 'scatter', mode = 'markers', x = ~Sepal.Length, y = ~Sepal.Width, name = 'Scatter')

# step2: Calculate a liner model between Sepal length and width and show it in the scatter plot.

fit_all = lm(data = table1, Sepal.Width ~ Sepal.Length)
p = plot_ly(data = table1, type = 'scatter', mode = 'markers', x = ~Sepal.Length, y = ~Sepal.Width, name = 'Scatter') %>%
  add_trace(data = table1, type = 'scatter', mode = 'lines', x = ~Sepal.Length, y = fitted(fit_all), name = 'Linear Model')

# step3: Based on the Species data, subdivide the iris dataset into three separate subsets (for each species).
unique(table1$Species)
table_se = subset(table1,table1$Species =='setosa')
table_ve = subset(table1,table1$Species =='versicolor')
table_vi = subset(table1,table1$Species =='virginica')

# step4:. Repeat steps 1 and 2 for each subsets.
# 4.1. setosa
p_se_1 = plot_ly(data = table_se, type = 'scatter', mode = 'markers', x = ~Sepal.Length, y = ~Sepal.Width, name = 'Scatter')

fit_1 = lm(data = table_se, Sepal.Width ~ Sepal.Length)
p_se_2 = plot_ly(data = table_se, type = 'scatter', mode = 'markers', x = ~Sepal.Length, y = ~Sepal.Width, name = 'Scatter') %>%
  add_trace(data = table_se, type = 'scatter', mode = 'lines', x = ~Sepal.Length, y = fitted(fit_1), name = 'Linear Model')

# 4.2. versicolor
p_ve_1 = plot_ly(data = table_ve, type = 'scatter', mode = 'markers', x = ~Sepal.Length, y = ~Sepal.Width, name = 'Scatter')

fit_2 = lm(data = table_ve, Sepal.Width ~ Sepal.Length)
p_ve_2 = plot_ly(data = table_ve, type = 'scatter', mode = 'markers', x = ~Sepal.Length, y = ~Sepal.Width, name = 'Scatter') %>%
  add_trace(data = table_ve, type = 'scatter', mode = 'lines', x = ~Sepal.Length, y = fitted(fit_2), name = 'Linear Model')

# 4.3. virginica
p_vi_1 = plot_ly(data = table_vi, type = 'scatter', mode = 'markers', x = ~Sepal.Length, y = ~Sepal.Width, name = 'Scatter')

fit_3 = lm(data = table_vi, Sepal.Width ~ Sepal.Length)
p_vi_2 = plot_ly(data = table_vi, type = 'scatter', mode = 'markers', x = ~Sepal.Length, y = ~Sepal.Width, name = 'Scatter') %>%
  add_trace(data = table_vi, type = 'scatter', mode = 'lines', x = ~Sepal.Length, y = fitted(fit_3), name = 'Linear Model')


# step5: Plot all the results including scatter plots and linear models in one plot. 

p_all <- subplot(p_se_2, p_ve_2,p_vi_2, shareX = T, shareY = T, titleX = F, titleY = F)

# step6: Show that the linear models obtained for the three subsets can describe the relationship 
#    between Sepal length and width more accurately.
error_all = fit_all$fitted.value-table1$Sepal.Width
MSE_all = mean(error_all^2)
# [1] 0.1861044

error_1 = fit_1$fitted.value-table1$Sepal.Width
MSE_1 = mean(error_1^2)
# [1] 0.3411616

error_2 = fit_2$fitted.value-table1$Sepal.Width
MSE_2 = mean(error_2^2)
# [1] 0.2861961

error_3 = fit_3$fitted.value-table1$Sepal.Width
MSE_3 = mean(error_3^2)
# [1] 0.2142345

# Conclusion: MSE_1, MSE_2 and MSE_3 shows sligtly higher values than MSE_all presents little more accurate relationship indication
#             between sepal length and width.