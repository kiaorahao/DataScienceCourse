# 1. Create a scatter plot for the two variables.
library(readr)
table1 <- read_csv("~/Documents/OneDrive/PGD/Data Science/Lab Session 3 and 4/Datasets/Data Set 5.csv")

p = plot_ly(data = table1, type = 'scatter', mode = 'markers', x = ~Size, y = ~KW, name = 'Scatter')

# 2. Calculate a linear regression model.

fit = lm(data = table1, KW ~ Size)
p_l = plot_ly(data = table1, type = 'scatter', mode = 'markers', x = ~Size, y = ~KW, name = 'Scatter') %>%
  add_trace(data = table1, type = 'scatter', mode = 'lines', x = ~Size, y = fitted(fit), name = 'Linear Model')

# 3. Calculate polynomial regression models of order 2 and 3.

model_ploy2 = lm(table1$KW~poly(table1$Size,2))
model_ploy3 = lm(table1$KW~poly(table1$Size,3))

# 4. Plot the regression models
p_2 = plot_ly(data = table1, type = 'scatter', mode = 'markers', x = ~Size, y = ~KW, name = 'Scatter') %>%
  add_trace(data = table1, type = 'scatter', mode = 'lines', x = ~Size, y = fitted(model_ploy2), name = 'polynomial 2')
p_3 = plot_ly(data = table1, type = 'scatter', mode = 'markers', x = ~Size, y = ~KW, name = 'Scatter') %>%
  add_trace(data = table1, type = 'scatter', mode = 'lines', x = ~Size, y = fitted(model_ploy3), name = 'polynomial 3')

p_all <- subplot(p_l, p_2,p_3)


# 5. Compare the accuracy of the three model.

error_l = fit$fitted.value-table1$KW
MSE_l = mean(error_l^2)
# [1] 14320.65

error_2 = fit_2$fitted.value-table1$KW
MSE_2 = mean(error_2^2)
# [1] 2618886

error_3 = fit_3$fitted.value-table1$KW
MSE_3 = mean(error_3^2)
# [1] 2618234