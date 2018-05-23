orders = read.csv('./Machine_Learning_Lab/data/Orders.csv', stringsAsFactors = F)
returns = read.csv('./Machine_Learning_Lab/data/Returns.csv', stringsAsFactors = F)

orders$Profit = gsub("[\\$,]", "", orders$Profit)
orders$Sales = gsub("[\\$,]", "", orders$Sales)

orders$Profit = as.numeric(orders$Profit)
orders$Sales = as.numeric(orders$Sales)
View(orders)
View(returns)
