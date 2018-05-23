library(ggplot2)
library(dplyr)

orders = read.csv('./Machine_Learning_Lab/data/Orders.csv', stringsAsFactors = F)
returns = read.csv('./Machine_Learning_Lab/data/Returns.csv', stringsAsFactors = F)


##### Problem 1 ####
orders$Profit = gsub("[\\$,]", "", orders$Profit)
orders$Sales = gsub("[\\$,]", "", orders$Sales)

orders$Profit = as.numeric(orders$Profit)
orders$Sales = as.numeric(orders$Sales)
View(orders)
View(returns)

summary(orders)
str(orders)

#### Problem 2 ####

orders$Ship.Date = as.Date(orders$Ship.Date, format = "%m/%d/%y")
orders$Order.Date = as.Date(orders$Order.Date, format = "%m/%d/%y")

# density_plot = orders %>%
#   group_by(Order.Date, Category)
# 
# ggplot(density_plot, aes(Order.Date)) +
#   geom_density(aes(color = Category))

seasonality = orders %>%
  group_by(Order.Date, Category) %>%
  summarise(count = sum(Quantity))

g = ggplot(data = seasonality, aes(x = Order.Date, y = count)) +
  geom_smooth(aes(color = Category))
g

#### Problem 3 ####




