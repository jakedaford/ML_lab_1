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
ggplot(data = qty_group, aes(x = Order.Date, y = count)) +
  geom_smooth()

# Part 2.2
ggplot(data = group_cat, aes(x = Order.Date, y = count)) +
  geom_smooth(aes(color = Category))

# Part 3.1
returns$Order.ID = returns$`Order ID`
returns = returns[,-2]
returned_orders = inner_join(orders, returns, by = 'Order.ID')

yearly_loss = group_by(returned_orders, year = year(Order.Date)) %>%
  summarise(profit_loss = sum(Profit))

ggplot(data = yearly_loss, aes(x = year, y = profit_loss)) +
  geom_bar(stat = 'identity')

# Part 3.2
num_returns = returned_orders %>%
  group_by(Order.ID, Customer.ID) %>%
  group_by(Customer.ID) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  filter(count > 1)

nrow(num_returns)

nrow(filter(num_returns, count > 5)) 

# Visually determining the amount of people who returned x amount of items
ggplot(data = num_returns, aes(count)) + geom_bar() +
  xlab("number of customers returned") +
  scale_x_continuous(breaks = 2:13)

# Part 3.3 Which regions are more likely to return orders?
returned_orders %>%
  group_by(Region.y) %>%
  summarise(count=n()) %>%
  arrange(desc(count)) %>%
  head((20))

# Part 3.4 Which categories (sub-categories) of products are more likely to be returned?
# by general category
returned_orders %>% 
  group_by(Category) %>%
  summarise(count=n()) %>%
  arrange(desc(count)) %>%
  head((20))

# by subcategory
returned_orders %>%
  group_by(Sub.Category) %>%
  summarise(count=n()) %>%
  arrange(desc(count)) %>% 
  head((20))

#### Machine Learning Section

all_data <- left_join(orders, returns, by = 'Order.ID')

#Step 1
all_data$Returned <- as.factor(ifelse(is.na(all_data$Returned), 0, 1))

#Step 2
all_data$Process.Time <- all_data$Ship.Date - all_data$Order.Date

#Step 3
prod_group <- all_data %>%
  select(Product.ID, Returned) %>%
  filter(Returned==1) %>%
  group_by(Product.ID) %>%
  summarise(return_cnt = n())

all_data <- left_join(all_data, prod_group, by = 'Product.ID')

# Fixing the categorical variable Returned
all_data[all_data$Returned==0, ]$return_cnt <- 0 

# Calculating another variable, the percentage of items returned for the total order
prod_count_group = all_data %>%
  select(Product.ID) %>%
  group_by(Product.ID) %>%
  summarise(total_cnt = n())

all_data = left_join(all_data, prod_count_group, by = 'Product.ID')
all_data$return_pct = all_data$return_cnt / all_data$total_cnt

# Part 5

# Selecting what we personally think may be important
model_data = all_data %>%
  mutate(Month = month(Order.Date)) %>% 
  select(Month, Ship.Mode, Segment, Region.x, Category, Sub.Category, Sales, 
         Quantity, Discount, Shipping.Cost, Order.Priority, Returned,
         Process.Time, return_cnt)

# Getting rid of the date class, turning it into a numeric
model_data$Process.Time = as.numeric(model_data$Process.Time)
model_data$Returned = as.numeric(as.character(model_data$Returned))

# Grabbing the variables we we ant to set as factors -
cols_to_factor = c('Month', 'Ship.Mode', 'Segment', 'Region.x', 
                   'Sub.Category', 'Category', 'Order.Priority')

model_data[cols_to_factor] = lapply(model_data[cols_to_factor], factor)
str(model_data)

library(caret)

# Stratified data partitioning for training and testing via caret function
set.seed(123)
folds = createDataPartition(model_data$Returned, p=0.8) # 80-20% split
train = model_data[folds[[1]], ]
test = model_data[-folds[[1]], ]

# Checking the percentage of returned orders in testing and train
nrow(test[test$Returned==1,]) # 440
nrow(train[train$Returned==1,]) # 1776
# 444 + 1776 = 2220, so it was distributed well through

# This is weird? Algorithm does not converge, values seem off..
logit.returned = glm(Returned ~ .,
                     family = "binomial",
                     data = train)
library(randomForest)

# This randomForest also seems weird, accuracy is 100% on train test?
rf = randomForest(Returned ~ ., data = train, importance = TRUE, split="gini")
rf_pred = predict(rf, test, type = "class")
table(Predicted = rf_pred, Test = test$Returned)
