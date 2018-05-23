library(ggplot2)
library(dplyr)
library(data.table)

orders <- fread("./Machine_Learning_Lab/data/Orders.csv", stringsAsFactors = F)
returns <- fread("./Machine_Learning_Lab/data/Returns.csv", stringsAsFactors = F)

orders$Profit = gsub("[\\$,]", "", orders$Profit)
orders$Sales = gsub("[\\$,]", "", orders$Sales)

orders$Profit = as.numeric(orders$Profit)
orders$Sales = as.numeric(orders$Sales)

orders$Ship.Date = as.Date(orders$Ship.Date, format = "%m/%d/%y")
orders$Order.Date = as.Date(orders$Order.Date, format = "%m/%d/%y")

qty_group = orders %>%
  group_by(Order.Date) %>%
  summarise(count = sum(Quantity)) %>%
  arrange(Order.Date)

group_cat = orders %>% 
  group_by(Order.Date, Category) %>% 
  summarise(count = sum(Quantity))

# Part 2.1
ggplot(data = qty_group, aes(x = Order.Date, y = count)) + geom_smooth()

# Part 2.2
ggplot(data = group_cat, aes(x = Order.Date, y = count)) + geom_smooth(aes(color = Category))

# Part 3.1
returns$Order.ID = returns$`Order ID`
returns = returns[,-2]
returned_orders = inner_join(orders, returns, by = 'Order.ID')

yearly_loss = group_by(returned_orders, year = year(Order.Date)) %>%
  summarise(profit_loss = sum(Profit))

ggplot(data = yearly_loss, aes(x = year, y = profit_loss)) + geom_bar(stat = 'identity')

# Part 3.2
num_returns = returned_orders %>%
  group_by(Order.ID, Customer.ID) %>%
  group_by(Customer.ID) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  filter(count > 1)

nrow(num_returns)

nrow(filter(num_returns, count > 5)) 
