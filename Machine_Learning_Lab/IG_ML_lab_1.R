orders <- read.csv("./data/Orders.csv",  stringsAsFactors = FALSE)
returns <- read.csv("./data/Returns.csv",  stringsAsFactors = FALSE)

## Part I: Preprocessing and EDA
library(tidyverse)
library(dplyr)
library(data.table)
str(orders)
summary(orders)
orders$Profit = as.numeric(parse_number(gsub("\\$","",orders$Profit)))
orders$Sales= as.numeric(parse_number(gsub("\\$","",orders$Sales)))

### Problem 2: Inventory Management
orders$Ship.Date = as.Date(orders$Ship.Date, format = "%m/%d/%y")
orders$Order.Date = as.Date(orders$Order.Date, format = "%m/%d/%y")

qty_group <- orders %>% select (Order.Date, Quantity) %>% group_by(Order.Date) %>% 
  summarise(count = sum(Quantity)) %>% arrange(Order.Date)

ggplot(data = qty_group, aes(x = Order.Date, y = count)) + geom_line(aes(color='identity'))


group_cat <- orders %>% select (Order.Date, Category, Quantity) %>% group_by(Order.Date, Category) %>% 
  summarise(count = sum(Quantity)) %>% arrange(Order.Date)

ggplot(data = group_cat, aes(x = Order.Date, y = count)) + geom_line(aes(color = Category))

### Problem 3: Why did customers make returns?
#Merge orders with the returns

returned_orders <- inner_join(orders, returns, by = 'Order.ID') 

#1. How much profit did we lose due to returns each year?
group_by(returned_orders, year(Order.Date)) %>% summarise(profit_loss = sum(Profit))

#2. How many customer returned more than once? more than 5 times?
nrow(group_by(returned_orders, Order.ID, Customer.ID) %>% group_by(Customer.ID) %>%
       summarise(count=n()) %>% filter(count >1))
nrow(group_by(returned_orders, Order.ID, Customer.ID) %>% group_by(Customer.ID) %>% 
       summarise(count=n()) %>% filter(count >5))   

#3. Which regions are more likely to return orders?
returned_orders %>% group_by(Region.y) %>% summarise(count=n()) %>% arrange(desc(count)) %>% head((20))

#4. Which categories (sub-categories) of products are more likely to be returned?
returned_orders %>% group_by(Category) %>% summarise(count=n()) %>% arrange(desc(count)) %>% head((20))
returned_orders %>% group_by(Sub.Category) %>% summarise(count=n()) %>% arrange(desc(count)) %>% head((20))


## Part II: Machine Learning and Business Use Case

### Problem 4: Feature Engineering
all_data <- left_join(orders, returns, by = 'Order.ID')








