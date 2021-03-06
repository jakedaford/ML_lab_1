library(ggplot2)
library(dplyr)
library(data.table)

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
returned_orders = inner_join(orders, returns, by = 'Order.ID')

#1

returned_orders_year = returned_orders %>%
  group_by(year = year(Order.Date)) %>%
  summarise(profits_lost = sum(Profit))

g = ggplot(data = returned_orders_year, aes(x = year, y = profits_lost)) +
  geom_bar(stat = 'identity')
g

#2

more_than_once_returning_customers = returned_orders %>%
  group_by(Customer.ID, Order.ID) %>%
  group_by(Customer.ID) %>%
  summarise(count = n()) %>%
  filter(count > 1)
  
nrow(more_than_once_returning_customers)


more_than_five_returning_customers = returned_orders %>%
  group_by(Customer.ID, Order.ID) %>%
  group_by(Customer.ID) %>%
  summarise(count = n()) %>%
  filter(count > 5)

nrow(more_than_five_returning_customers)

#3



