library(ggplot2)
library(dplyr)
library(data.table)

orders <- fread("./Machine_Learning_Lab/data/Orders.csv", stringsAsFactors = F)
returns <- fread("./Machine_Learning_Lab/data/Returns.csv", stringsAsFactors = F)

orders$Profit = gsub("[\\$,]", "", orders$Profit)
orders$Sales = gsub("[\\$,]", "", orders$Sales)

orders$Profit = as.numeric(orders$Profit)
orders$Sales = as.numeric(orders$Sales)