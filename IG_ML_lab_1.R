orders <- read.csv("./data/Orders.csv",  stringsAsFactors = FALSE)
returns <- read.csv("./data/Returns.csv",  stringsAsFactors = FALSE)

## Part I: Preprocessing and EDA
library(tidyverse)
str(orders)
summary(orders)
orders$Profit = parse_number(gsub("\\$","",orders$Profit))
orders$Sales= parse_number(gsub("\\$","",orders$Sales))

### Problem 2: Inventory Management
