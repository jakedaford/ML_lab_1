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
  group_by(Ship.Date) %>%
  summarise(count = sum(Quantity)) %>%
  arrange(Ship.Date)

group_cat = orders %>% 
  group_by(Ship.Date, Category) %>% 
  summarise(count = sum(Quantity))

# Part 2.1
ggplot(data = qty_group, aes(x = Ship.Date, y = count)) + geom_line()
# Part 2.2
ggplot(data = group_cat, aes(x = Ship.Date, y = count)) + geom_line(aes(color = Category))

# Alternatively
density_plot = orders %>% 
  group_by(Order.Date, Category)

ggplot(density_plot, aes(Order.Date)) + geom_density(aes(color = Category))
