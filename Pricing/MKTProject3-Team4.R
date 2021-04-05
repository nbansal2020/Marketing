library(lubridate)
library(tidyverse)
library(data.table)
library(dplyr)
library(arules)
library(plyr)

# IMPORTING DATA -----------------------------------------------------------
product <- fread("product_table.csv")
transaction <- fread("transaction_clean.csv")

# DATA CLEANING & PREPARATION ----------------------------------------------
#Same as last project
# Remove transactions that contain "bags" as these are the bags purchased during check out and do not really help our marketing strategy 
#transaction <- subset(transaction, prod_id != 999231999) 

# Look for transaction amount paid that has negative values. It looks like the discount amount is larger than the purchase amount. This could be a return that was discounted to return the initial money back. There are a total of 8 transactions that satisfy this. We need to match them back to their original purchase transactions and remove those as well so there are no discrepancies. 
#neg_trans_return <- transaction[transaction$tran_prod_paid_amt < 0,]
#transaction <- subset(transaction, tran_prod_paid_amt > 0) 

# Removing all of the original transactions to avoid double counting transactions and projecting higher sales than actual. 
#transaction[!(transaction$cust_id==93409897 & transaction$prod_id == 357541011 & transaction$prod_unit_price == 0.55 & transaction$store_id == 340),]
#transaction[!(transaction$cust_id == 93409897 & transaction$prod_id == 357541011 & transaction$prod_unit_price==0.55 & transaction$store_id == 340),]
#transaction[!(transaction$cust_id == 73479594 & transaction$prod_id == 999241421 & transaction$prod_unit_price==16.90),]
#transaction[!(transaction$cust_id == 40099908 & transaction$prod_id == 999250092 & transaction$prod_unit_price==1.59 & transaction$store_id == 344),]
#transaction[!(transaction$cust_id == 51749812 & transaction$prod_id == 999264989 & transaction$prod_unit_price==0.30 & transaction$store_id == 325),]
#transaction[!(transaction$cust_id == 42509966 & transaction$prod_id == 999295518 & transaction$prod_unit_price==3.59 & transaction$store_id == 984 & transaction$tran_dt == 2016-03-26),]
#transaction[!(transaction$cust_id == 16339676 & transaction$prod_id == 999436833 & transaction$prod_unit_price==5.49 & transaction$store_id == 576 & transaction$tran_dt == 2016-03-26),]
#transaction[!(transaction$cust_id == 7869780 & transaction$prod_id == 999476721 & transaction$prod_unit_price==3.29 & transaction$store_id == 988),]

# The transaction ID in the dataset does not uniquely identify each transaction due to its large number format. So we will create a new column that will concatenate the customer id, store id, and transaction date columns. We will do this under the assumption that each customer only visits a store once during the day and this will allow us to group together all the products purchased into a single transaction. 
#transaction$new_tran_id <- paste(transaction$cust_id, transaction$store_id, transaction$tran_dt, sep="-")

# Ensuring all the dates are datetime variables
#transaction$tran_dt <- as.Date(transaction$tran_dt)

# Extract day of the week from to see what days are most popular 
#transaction$day <- weekdays(as.Date(transaction$tran_dt))
#transaction$month <- month(as.Date(transaction$tran_dt))

# Removing transactions that do not map to a specific product in the product table
#transaction <- subset(transaction, prod_id %in% product$prod_id)

# Calculating profit - per our research grocery stores have a 2.2% profit margin so we're using this as a baseline profit for all products 
#transaction$profit <- transaction$tran_prod_paid_amt * 0.022

# Converting discount to discount percentage 
#transaction$discount_percent <- -(transaction$tran_prod_discount_amt)/transaction$tran_prod_sale_amt

# Creating a binary for whether a product is considered "fresh produce" or not. We are not removing them from the dataset because they could be complimentary/substitute goods and so creating a binary helps see this later on
##Finding unique categories 
unique_categories <- product %>%
  group_by(category_desc_eng) %>%
  count() %>%
  arrange(desc(n))

##Entering each product manually as there are other products with the word "fresh" in it that are not actually fresh produce and so to avoid misrepresenting the data we did it manually
product <- product %>%
  mutate(fresh = ifelse(category_desc_eng == "ZUCCHINI" | category_desc_eng == "RADISHES" | category_desc_eng == "POMEGRANATE" | category_desc_eng == "PEAS" | category_desc_eng == "FRESH ORGANIC FRUITS" | category_desc_eng == "FRESH COW MEAT" | category_desc_eng == "CHERRY" | category_desc_eng == "ASPARAGUS" | category_desc_eng == "APRICOT" | category_desc_eng == "TURNIP" | category_desc_eng == "MEAT SMALL FRESH ANIMALS" | category_desc_eng == "CUCUMBER" | category_desc_eng == "CHESTNUT" | category_desc_eng == "BEET" | category_desc_eng == "STRAWBERRY" | category_desc_eng == "SPINACH" | category_desc_eng == "PINEAPPLE" | category_desc_eng == "GREENS" | category_desc_eng == "VEGETABLES" | category_desc_eng == "PLUM" | category_desc_eng == "MELON" | category_desc_eng == "GREEN BEAN" | category_desc_eng == "BANANA" | category_desc_eng == "PORK" | category_desc_eng == "KIWI" | category_desc_eng == "GARLIC" | category_desc_eng == "CARROT" | category_desc_eng == "WATERMELON" | category_desc_eng == "PUMPKIN" | category_desc_eng == "MUSHROOMS" | category_desc_eng == "SMALL FRUITS" | category_desc_eng == "RELATED COD" | category_desc_eng == "PEPPERS" | category_desc_eng == "LETTUCE" | category_desc_eng == "PEACH" | category_desc_eng == "ONION" | category_desc_eng == "CHICKEN" | category_desc_eng == "FRUITS" | category_desc_eng == "FISH CUT" | category_desc_eng == "GRAPE" | category_desc_eng == "FRESH SEAFOOD" | category_desc_eng == "TOMATO" | category_desc_eng == "POTATO" | category_desc_eng == "FISH SPECIALTIES" | category_desc_eng == "CITRUS" | category_desc_eng == "REGULAR EGGS" | category_desc_eng == "CABBAGE" | category_desc_eng == "FISH SPECIAL CUTS" | category_desc_eng == "APPLE" | category_desc_eng == "FRESH PORK" | category_desc_eng == "FRESH BEEF" | category_desc_eng == "	FRESH POULTRY MEAT" | category_desc_eng == "WILD FRESH FISH" | category_desc_eng == "FRESH POULTRY MEAT" | category_desc_eng == "FRESH ORGANIC VEGETABLES" | category_desc_eng == "OTHER FRESH VEGETABLES" | category_desc_eng == "FRESH SHEEPMEAT / GOATMEAT" | category_desc_eng == "FRESH POULTRY MEAT", 0, 1))

# Merging both dataframes into one 
data <- merge(transaction, product, by='prod_id', all=TRUE)

fwrite(data, "data.csv")
data <- fread("data.csv")

# Finding the top categories sold by sales for all non-fresh products
no_fresh_sales <- data %>%
  filter(fresh==1) %>%
  group_by(category_desc_eng) %>%
  summarize(revenue = sum(tran_prod_sale_amt), num_prod = uniqueN(prod_id)) %>%
  arrange(desc(revenue))
no_fresh_sales <- no_fresh_sales[1:5,]#Top 5 categories include fine wine, dry salted cod, beer with alcohol, washing machine detergents, coffee and roasted mixtures

# Adding in a binary variable of whether a category will be targeted or not based on the top 5 categories by sales
data <- data %>%
  mutate(target_cat = ifelse(category_desc_eng %in% no_fresh_sales$category_desc_eng, 1, 0 ))

  # Looking at price variance on a weekly basis because we don't have daily transaction information so here we are creating an index to calculate different values (sales, discounts, unit price etc) by week
#https://stackoverflow.com/questions/22439540/how-to-get-week-numbers-from-dates
# Creating week index 
data <- data %>%
  mutate(tran_week = as.integer(strftime(tran_dt, format = "%Y%V")))
data <- data %>%
  mutate(week_index = as.integer(strftime(tran_dt, format = "%V")))

# Analyzing data for the 15th week of 2016 and 2017 as April 12-18 is the 15th week of 2021
data_april_2017 <- subset(data, tran_week == "201715") #~310k transactions
data_april_2016 <- subset(data, tran_week == "201615") #~250k transactions

##Understand what kinds of purchases are made during those weeks 
# What are the discounts being given?
unique(data_april_2017$tran_prod_discount_amt) #no discounts given during these dates 
unique(data_april_2016$tran_prod_discount_amt) #a lot more variance in discounts 

# What brands are being sold?
sort(unique(data_april_2017$brand_desc))
sort(unique(data_april_2016$brand_desc))

# What is the average unit price of products being sold?
View(data_april_2016 %>%
  group_by(prod_id, store_id) %>%
  summarise(avg = mean(prod_unit_price))) #The unit price fluctuated for some items from store to store but for the most part was uniform across the stores
View(data_april_2017 %>%
       group_by(prod_id, store_id) %>%
       summarise(avg = mean(prod_unit_price))) #Similar to 2016, little fluctuation

# Create a list of all holidays during the two year period for use later
holidays <- c('2016-01-01', '2016-03-25', '2016-03-27', '2016-04-25','2016-05-1', '2016-05-26', '2016-06-10', '2016-08-15', '2016-10-05', '2016-11-01', '2016-12-01', '2016-12-08', '2016-12-25', '2017-01-01', '2017-04-14', '2017-04-16', '2017-04-25', '2017-05-01', '2017-05-12', '2017-06-10', '2017-06-15', '2017-08-15', '2017-10-05', '2017-11-01', '2017-12-01', '2017-12-08', '2017-12-25')
holidays <- as.Date(holidays)

# Removing transactions where there is no price variance on a weekly-basis as we cannot estimate demand change and therefore the elasticity
data <- data %>%
  group_by(store_id, prod_id, tran_week) %>%
  mutate(weekly_price = sum(tran_prod_sale_amt)/sum(tran_prod_sale_qty)) %>%
  ungroup() %>%
  group_by(store_id, prod_id) %>%
  mutate(weekly_change_price = sd(weekly_price)) %>%
  filter(weekly_change_price != 0 & (!is.na(weekly_change_price)))
##left with 21mi transactions 

# STORE + PRODUCT SELECTION ------------------------------------------------------------
#We want to change the price for the same 100 products across 10 stores. We are going to find the stores that sell the most number of our target categories and have the most number of target products in common with each other. We will short-list the top 15 stores as possible options and narrow it down further as the analysis proceeds
# Which store is sell the most of our target categories? 
target <- subset(data, target_product == 1)
target_stores <- target %>%
  group_by(store_id, prod_id, tran_week) %>%
  mutate(weekly_price = sum(tran_prod_sale_amt)/sum(tran_prod_sale_qty))

target_stores %>%
  group_by(store_id) %>%
  count(uniqueN(prod_id)) %>%
  arrange(desc(n)) #store 342 has the most number of target products (393 unique target products)

# What number of products is in each category?
target_stores_prod <- target %>%
  filter(store_id == 342) %>%
  group_by(category_desc_eng) %>%
  count(uniqueN(prod_id)) %>%
  arrange(desc(n))

# What products is store 342 selling?
target_stores_prod <- target %>%
  filter(store_id == 342) %>%
  count(uniqueN(prod_id)) %>%
  arrange(desc(n))

# What stores are selling similar products to 342?
target_stores_final <- subset(target_stores, prod_id %in% target_stores_prod$prod_id)

# Final store selection
final_stores <- target_stores_final %>% 
  group_by(store_id) %>%
  count(uniqueN(prod_id)) %>%
  arrange(desc(n))

# Only selecting the top 15 stores because we have to target 10 and we want to have some buffer for options 
final_stores <- final_stores[1:15,]
# Final selection of stores is: 342, 349, 347, 346, 345, 343, 588, 344, 335, 341, 395, 320, 525, 157, and 398

# EDA for those stores 
data <- subset(data, store_id %in% final_stores$store_id)
##left with 2mi transactions 

##Average quantity Sold
data %>%
  group_by(store_id) %>%
  summarize_at("tran_prod_sale_qty", mean)

##Averag sales for each store
data %>%
  group_by(store_id) %>% 
  mutate(revenue = sum(tran_prod_sale_amt)) %>%
  summarise_at("revenue", mean)

##Average discounts offered
data %>%
  group_by(store_id) %>% 
  mutate(discount = sum(discount_percent)) %>%
  summarise_at("discount", mean)
##working with 62,206 store-product combinations (720 products in target categories, 15 stores)

# What are the top products being sold? In order to do this we will look at elasticity 
#http://www.salemmarafi.com/code/price-elasticity-with-r/, https://github.com/MatCyt/Price_Elasticity
#Creating lists of stores and products to use in the for loop
stores <- unique(data$store_id)
products <- unique(data$prod_id)

#Initializing empty lists to calculate elasticity and alpha values 
elasticity <-list()
alpha <- c()

#Build an iterative process over each product, looking at any different price points for each one. Using this difference in price we can then calculate change in demand and the slope+alpha values for them (this information gives us the change in quantity demanded/change in price). Here we are using slope as a measure for price sensitivity to calculate point elasticity. Lastly, we combine this information with the mean price+demand to calculate elasticity 
for(i in 1:length(products)) {
  tmp <- subset(data, data$prod_id == products[i])
  tmp_new <- unique(tmp$prod_unit_price)
  price <- c()
  demand <- c()
  for(j in 1:length(tmp_new)) {
    demand_new <- sum(tmp$tran_prod_sale_qty[tmp$prod_unit_price == tmp_new[j]])
    price <- c(price, tmp_new[j])
    demand <- c(demand, demand_new)
  }
  tmp_data <- as.data.frame(cbind(price, demand))
  slope <- lm(demand ~ price, data = tmp_data)$coefficients[[2]]
  alpha <- lm(demand ~ price, data = tmp_data)$coefficients[[1]]
  mean_price <- mean(price)
  mean_demand <- mean(demand)
  elasticity[i] <- mean_price*slope/mean_demand
}

elasticity_product <- data.frame(matrix(unlist(elasticity), nrow=length(elasticity), byrow=T))
colnames(elasticity_product) <- "elasticities"

# Add the elasticity information back into the data 
product_info$elasticity <- elasticity_product$elasticities
product_info <- subset(product_info, (!is.na(elasticity)))
product_info <- product_info[order(product_info$elasticity),]

data_final <- merge(data, product_info, by.x = 'prod_id',by.y = 'products')

# Creating a subset of just target category transactions
target_category_trans <- subset(data_final, target_product == 1) #105k transactions

# Count of products in each target category
target_category_trans %>%
  group_by(category_desc_eng) %>%
  count() %>%
  arrange(desc(n))

# Calculating weekly prices, discounts, and sales volume
data_final <- data_final %>%
  group_by(store_id, prod_id, tran_week) %>%
  mutate(weekly_price = sum(tran_prod_sale_amt)/sum(tran_prod_sale_qty),
         weekly_amount = sum(tran_prod_sale_qty),
         weekly_discount = sum(discount_percent))

# ACCOUNTING FOR SEASONALITY & HOLIDAYS  -----------------------------------------------------
##Here we want to account for seasonal trends like whether or not a holiday has taken place and so there are heavy discounts because of that
##https://www.hindawi.com/journals/jat/2019/3847837/
data_final <- data_final %>%
  mutate(holiday = ifelse(tran_dt %in% holidays, 1, 0))

##Creating a binary for whether there was a holiday during the week of the transaction 
data_final <- data_final %>%
  group_by(tran_week) %>%
  mutate(holiday_index = ifelse(max(holiday), 1, 0))

##Looking at year over year sales to see if there is seasonality in the data 
data$year <- year(data$tran_dt)
seasonality <- data[,c(27,29,30)]
seasonality$weekyear <- paste(seasonality$year, seasonality$week_index, sep="-")
seasonality2 <- seasonality %>%
  group_by(weekyear) %>%
  mutate(Avg_change = mean(weekly_change_price))

seasonality2 <- seasonality2[-2]
seasonality2 <- seasonality2[!duplicated(seasonality2$Avg_change),]
seasonality2 <- subset(seasonality2, !weekyear == "2016-53")

seasonality2016 <- subset(seasonality2, year=="2016")
seasonality2017 <- subset(seasonality2, year=="2017")
cor(seasonality2016, seasonality2017)

# COMPLEMENTS AND SUBSTITUTES -----------------------------------------------------------------
#https://www.datacamp.com/community/tutorials/market-basket-analysis-r
# Similar to HW2 we will use Association Rules to figure out what goods are complements and substitutes. Complements will be items frequently purchased together and substitutes will be items in the same product category with a different brand type or different sub-category type

##COMPLEMENTS 
complements = ddply(data_final,c("cust_id","tran_dt"),
                        function(x)paste(x$prod_id,
                                           collapse = ","))
complements$cust_id = NULL
complements$tran_dt = NULL
colnames(complements) = c("complements")

fwrite(complements,"complements.csv")

# Reading in complement data as a transaction type to be able to use Association Rules on it
complements_ar = as(complements, "transactions")

# Calculate the Association Rules 
rules = apriori(complements_ar, parameter=list(supp=0,conf=0.20, minlen=2, maxlen=2))

# Get the dataframe out
output = capture.output(inspect(rules))

# Get the dataframes in the right format 
rhs = gsub("[^{}.*", "\\2", output)[-1]
lhs = gsub("[^{].*", "\\1", output)[-1]

complements_df = as.data.frame(cbind(lhs, rhs))
colnames(complements_df) = c("product","complement")

# We only want to see the complements for our target categories so here we are subsetting to see the complements of those 
products_interest <- subset(data_final, target_product == 1)

##SUBSTITUTES 
#https://stackoverflow.com/questions/38645377/inverse-association-rules
# Finding all the subcategories 
subcategories <- unique(data_final$sub_category_desc)

# Initializing an empty list for all subcategories in the dataset
subcat_list = list()

# Similar to HW2, we are going to use Association Rules to see what products were bought together and not. For that we need to prepare the data in order to be able to extract the necessary information. In this for loop we iterate over every product_id to get all the products that can be substitutes based off of the subcategory
for (i in c(1:length(subcategories))){
  subcat = subset(data_final, sub_category_desc == subcategories[i])
  
  transactions = ddply(subcat,c("cust_id","tran_dt"),
                          function(x)paste(x$prod_id,
                                             collapse = ","))
  transactions$cust_id = NULL
  transactions$tran_dt = NULL
  colnames(transactions) = c("products")
  
  transactions = data.frame(lapply(transactions,as.factor))
  products = strsplit(as.character(transactions$products), ",")
  test = as(products,"transactions")
  
  # Using association rules to find out which products have been purchased together
  rules = apriori(test, parameter = list(supp=0, conf=0,minlen=2, maxlen=2))
  output = capture.output(inspect(rules))
  rhs = gsub("[^{]}.*", "\\2", output)[-1]
  lhs = gsub("[^{]}.*", "\\1", output)[-1]
  lift = vector()
  for (j in (2:length(output)) ){
    lift[j-1] = substr(output[j],61,64)
  }
lift_value = as.data.frame(lift)
  
  co_buy = as.data.frame(cbind(lhs, rhs, lift))
  sub_list = as.data.table(co_buy[lift<1,])
  sub_list[,list(rhs),by=lhs]
  colnames(sub_list) = c("product","substitute","lift")
  subcat_list[[i]] = sub_list
}

subcategory_data = do.call(rbind, subcat_list)
subcategory_data$lift = NULL

# Adding the complements and substitutes into the data 
data_final$complements <- complements$complement[match(data_final$prod_id,complements$product)]
data_final$substitutes <- subcategory_data$substitute[match(data_final$prod_id,subcategory_data$product)]

fwrite(data_final, "data_final.csv")
data_final <- fread("data_final.csv")

test <- subset(data_final, prod_id %in% complements)
test <- test %>% 
  group_by(tran_week) %>%
  mutate(com_price = tran_prod_sale_amt/tran_prod_sale_qty)
data_final$com_price = test$com_price[match(data_final$complements, test$prod_id)]  

test <- subset(data_final, prod_id %in% substitutes)
test <- test %>% 
  group_by(tran_week) %>%
  mutate(sub_price = tran_prod_sale_amt/tran_prod_sale_qty)
data_final$sub_price = test$sub_price[match(data_final$substitutes, test$prod_id)]  

# REGRESSION ----------------------------------------------------------------------------------
# Use substitutes, compliments, our chosen goods, price, discount, seasonality, and holidays to run a regression 

# Looking at target categories only 
# Since we're using a logit response function, we need to transform demand in order for it to match the function 
target <- subset(data_final, target_product == 1)
target[, unique_id := do.call(paste, c(.SD, sep = "_")), .SDcols = c('store_id', 'prod_id')]
target[, max_volume := max(weekly_amount), by = .(store_id, prod_id)]
target[, new_volume := log(weekly_amount/(max_volume - weekly_amount))]

# Separating out which store-product pairs have complements or substitutes and which don't 
##From our output we can see that more goods don't have a complement or substitute
no_compl_subs <- unique(target[is.na(target$complements)|is.na(target$substitutes), c("store_id", "prod_id")])
no_compl_subs[, unique_id := do.call(paste, c(.SD, sep = "_")), .SDcols = c('store_id', 'prod_id')]
compl_subs <- unique(target[(!is.na(target$complements))|(!is.na(target$substitutes)), c("store_id", "prod_id")])
compl_subs[, unique_id := do.call(paste, c(.SD, sep = "_")), .SDcols = c('store_id', 'prod_id')]


# Adjusting the price (we are looking at a max increase of 25% and a max decrease of 25% going up 1% at a time)
most_recent_tran = data_final[, list(tran_dt = max(tran_dt)), by = .(store_id, prod_id)]
recent = merge(data_final, most_recent_tran, by = c('store_id', 'prod_id', 'tran_dt'))
recent_price = recent[, list(prod_unit_price = min(prod_unit_price)), by = .(store_id, prod_id)]
data_new = data_final[tran_week == '201715', .(store_id, prod_id, weekly_price, weekly_discount)]
data_new[, ratio := weekly_discount/weekly_price]
recent_price = merge(recent_price, data_new, all.x = TRUE, by = c('store_id', 'prod_id'))
recent_price[is.na(ratio), ratio := 0]
recent_price[is.na(weekly_discount), weekly_discount := 0]
recent_price[, weekly_price := NULL]
recent_price[, unique_id := do.call(paste, c(.SD, sep = "_")), .SDcols = c('store_id', 'prod_id')]
recent_price_target = recent_price[unique_id %in% target$unique_id]

# As said earlier we are looking at a price change of 25% with 1% at a time
price_change = data.table(prod_unit_price = 1, suggested_price = 1)
for (i in unique(recent_price_target$prod_unit_price)) {
  for (j in seq(-0.25, 0.25, 0.1)){
    price_change = rbind(price_change, list(i, round((1+j)*i, 2)))
  }
}

# What are the unique price changes that have been made?
price_change = unique(price_change)
price_change = merge(price_change, recent_price_target[, .(store_id, prod_id, prod_unit_price, ratio, unique_id)], by = 'prod_unit_price', allow.cartesian = TRUE)

# Using the same discount ratio as 2017 
price_change[, weekly_discount := suggested_price * ratio] 
colnames(price_change)[2] <- 'weekly_price'

# We are looking at the 15th week of 2021
price_change[, week_index := 15]
price_change = merge(price_change, unique(target[, .(unique_id, max_volume)]), by = 'unique_id')

com_sub <- data_final[,c("store_id", "prod_id", "unique_id", "tran_week", "complements", "substitutes", "com_price", "sub_price")]
com_sub[, max_wk_comp := max(tran_week), by = .(store_id, complements)]
com_sub[, max_wk_sub := max(tran_week), by = .(store_id, substitutes)]
recent_com_price = com_sub[tran_week == max_wk_comp][, .(unique_id, com_price)]
recent_sub_price = com_sub[tran_week == max_wk_sub][, .(unique_id, sub_price)]
price_change = merge(price_change, recent_com_price, by = 'unique_id', all.x = TRUE)
price_change = merge(price_change, recent_sub_price, by = 'unique_id', all.x = TRUE)

comprice <- data_final$com_price
subprice <- data_final$sub_price

for (id in unique(target$unique_id)){
  model <- target[unique_id == id]
  if (id %in% no_compl_subs) {
    formula = new_volume ~ weekly_price + weekly_discount + week_index + holiday_index
  } else {
    formula = new_volume ~ weekly_price + weekly_discount + week_index + holiday_index + comprice + subs_price
  }
  lm <- lm(formula, data = model)
  price_change[unique_id == id, c('new_volume')] <- predict(lm, newdata = price_change[unique_id == id])
}

price_change[, estimated_volume := max_volume * exp(new_volume) / (exp(new_volume))]
price_change[, revenue := (weekly_price - weekly_discount) * estimated_volume]
final <- price_change[revenue == max_rev]

final <- final %>%
  mutate(new_profit = profit - (original_revenue - original_amount))

# The weekly changes in sales quantities, revenue and profits respectively  
changes_sales <- final %>%
  group_by(store_id) %>%
  mutate(incremental_quantity = sum(estimated_quantity)-sum(original_amount), incremental_profit = sum(profit))

