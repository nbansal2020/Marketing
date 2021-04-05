library(readr)
library(dplyr)
library(plotly)
library(lubridate)
library(tidyverse)
library(cluster) #need for knn
library(factoextra) #need for knn
library(GGally)
library(plotly)

# IMPORTING DATA ----------------------------------------------------------
product <- read_csv("product_table.csv")
transaction <- read_csv("transaction_table.csv")

# No NAs in the data files
is.na(product)
is.na(transaction$tran_prod_sale_amt)
is.na(transaction)

# DATA CLEANING -----------------------------------------------------------
# Examining the structure of data - all the variable types
str(product)
# convert IDs into factors from double/integer
product$prod_id <- as.factor(product$prod_id)
product$subcategory_id <- as.factor(product$subcategory_id)
product$category_id <- as.factor(product$category_id)

str(transaction)
transaction$cust_id <- as.factor(transaction$cust_id)
transaction$tran_id <- as.factor(transaction$tran_id)
transaction$store_id <- as.factor(transaction$store_id)
transaction$prod_id <- as.factor(transaction$prod_id)

# Remove transactions that contain "bags" as these are the bags purchased during check out and do not really help our marketing strategy 
transaction <- subset(transaction, prod_id != 999231999) 

# Look for transaction amount paid that has negative values. It looks like the discount amount is larger than the purchase amount. This could be a return that was discounted to return the initial money back. There are a total of 8 transactions that satisfy this. We need to match them back to their original purchase transactions and remove those as well so there are no discrepancies. 
neg_trans_return <- transaction[transaction$tran_prod_paid_amt < 0,]
transaction <- subset(transaction, tran_prod_paid_amt > 0) 

# Removing all of the original transactions to avoid double counting transactions and projecting higher sales than actual. 
transaction[!(transaction$cust_id==93409897 & transaction$prod_id == 357541011 & transaction$prod_unit_price == 0.55 & transaction$store_id == 340),]
transaction[!(transaction$cust_id == 93409897 & transaction$prod_id == 357541011 & transaction$prod_unit_price==0.55 & transaction$store_id == 340),]
transaction[!(transaction$cust_id == 73479594 & transaction$prod_id == 999241421 & transaction$prod_unit_price==16.90),]
transaction[!(transaction$cust_id == 40099908 & transaction$prod_id == 999250092 & transaction$prod_unit_price==1.59 & transaction$store_id == 344),]
transaction[!(transaction$cust_id == 51749812 & transaction$prod_id == 999264989 & transaction$prod_unit_price==0.30 & transaction$store_id == 325),]
transaction[!(transaction$cust_id == 42509966 & transaction$prod_id == 999295518 & transaction$prod_unit_price==3.59 & transaction$store_id == 984 & transaction$tran_dt == 2016-03-26),]
transaction[!(transaction$cust_id == 16339676 & transaction$prod_id == 999436833 & transaction$prod_unit_price==5.49 & transaction$store_id == 576 & transaction$tran_dt == 2016-03-26),]
transaction[!(transaction$cust_id == 7869780 & transaction$prod_id == 999476721 & transaction$prod_unit_price==3.29 & transaction$store_id == 988),]

# The transaction ID in the dataset does not uniquely identify each transaction due to its large number format. So we will create a new column that will concatenate the customer id, store id, and transaction date columns. We will do this under the assumption that each customer only visits a store once during the day and this will allow us to group together all the products purchased into a single transaction. 
transaction$new_tran_id <- paste(transaction$cust_id, transaction$store_id, transaction$tran_dt, sep="-")
View(transaction)

# We need to check whether each transaction amount checks out, We can do this by subtracting the discount amount from the total amount and verifying if this value equals the amount paid.
transaction$verify_total_amt <- transaction$tran_prod_sale_amt + transaction$tran_prod_discount_amt
transaction$verify_total <- identical(transaction[['verify_total_amt']],transaction[['tran_prod_paid_amt']])
subset(transaction, verify_total == TRUE) # All values add up 

# Extract day of the week from to see what days are most popular 
transaction$day <- weekdays(as.Date(transaction$tran_dt))
pop_day <- transaction %>%
  group_by(day) %>% 
  count() %>%
  arrange(desc(n))
pop_day #Saturday is the busiest day followed by friday, mondays are the lowest 
ggplot(transaction, aes(day)) + geom_bar()

# Amount of money spent on any given day of the week (aggregate)
amount_per_day <- transaction %>%
  group_by(day) %>%
  summarise(total = sum(tran_prod_paid_amt))
amount_per_day #Mondays are the slowest days so this is useful as we can create some incentive for people on Mondays

# DATA EXPLORATION --------------------------------------------------------
## Customers 

# Which customer has the most transactions?
transaction %>%
  group_by(cust_id, store_id) %>%
  count(new_tran_id) %>%
  arrange(desc(n)) #customer 92619600 shopped at store 543 - 334 times 

# Which customer has spent the most?
transaction %>%
  group_by(cust_id) %>%
  summarize_at("tran_prod_paid_amt", sum) %>%
  arrange(desc(tran_prod_paid_amt)) #customer 96879682 has spent the most on transactions - 14020 euros

# Which customer has spent the most per store?
transaction %>%
  group_by(cust_id, store_id) %>%
  summarize_at("tran_prod_paid_amt", sum) %>%
  arrange(desc(tran_prod_paid_amt)) #customer 13489682 has spent the most (13339 euros) at store 695.

# Which customer has bought the most products?
transaction %>%
  group_by(cust_id, prod_id) %>%
  count() %>%
  arrange(desc(n)) #customer 72999968 and 72999968 bought the most products - 688 and 674 units of bread respectively 
# Other popular products among customers with high transactions are - beverage mixers, beer, bread, frozen bread, coffee, and milk.
popular_prods <- subset(product, prod_id == 999251927 | prod_id == 999951864 | prod_id == 999746519 | prod_id == 999478576 | prod_id == 999192126 | prod_id == 999742491 | prod_id == 999305477)

# Which customers buy the most using coupons/offers?  
transaction %>%
  group_by(cust_id) %>%
  summarize_at("tran_prod_offer_cts", sum) %>%
  arrange(desc(tran_prod_offer_cts)) # we can look at these customers and see what they usually buy since they use the most offers 

cust_most_offers <- subset(transaction, cust_id == 73979986 | cust_id == 18239665 | cust_id == 80579664)

## Store

# Which store has made the highest revenue? 
transaction %>%
  group_by(store_id) %>%
  summarize_at("tran_prod_paid_amt", sum) %>%
  arrange(desc(tran_prod_paid_amt)) #store 342 has made the highest revenue (784655 euros) combined of all customers 

# What is the average discount per store?
discount <- transaction %>%
  group_by(store_id) %>%
  summarize_at("tran_prod_discount_amt", sum) %>%
  arrange(desc(tran_prod_discount_amt))

median(discount$tran_prod_discount_amt) # Median discount per store of $23092

discount_per_prod <- transaction %>%
  group_by(store_id, prod_id) %>%
  summarize_at("tran_prod_discount_amt", sum) %>%
  arrange(desc(tran_prod_discount_amt))

median(discount_per_prod$tran_prod_discount_amt) # Median discount per product in store is 0.79 

# Which store gives out the most offers?
transaction %>%
  group_by(store_id) %>%
  summarize_at("tran_prod_offer_cts", sum) %>%
  arrange(desc(tran_prod_offer_cts)) #store 349, 342, 345, 344 have given the most offers

store_most_offers_prods <- subset(transaction, store_id %in% c(349, 342, 345, 344, 343, 346, 341, 347, 588, 157, 994, 335, 321, 331, 525, 572, 307, 315, 332, 988, 395, 996, 627, 348, 673))

store_most_offers_prods %>%
  group_by(prod_id) %>%
  count(prod_id) %>%
  arrange(desc(n)) # This will give us the most popular products sold at the stores that give out the most offers 

# carrots, banana, milk, onion, citrus fruit, mineral water, onion, zucchini
pop_prods_offers <- subset(product, prod_id %in% c(999956795, 999361204, 999953571, 999680491, 999712725, 999401500, 999951863, 999957158))

## Products 

# What are the products that are most bought? 
transaction %>%
  group_by(prod_id) %>%
  count() %>%
  arrange(desc(n)) 
pop_prod <- subset(product, prod_id == 999956795 | prod_id == 999361204 | prod_id == 999951863 | prod_id == 999746519 |prod_id == 999401500 |prod_id == 999712725 |prod_id == 999749894 |prod_id == 999953571 |prod_id == 999356553 | prod_id == 999957158)
View(pop_prod) # Most bought  products are - sugar, carrots, mineral water, onion, drinks, fresh pork, milk, citrus, banana, and zucchini

# Which is the most expensive product and what is the distribution of product prices in the dataset? Histogram
transaction %>%
  group_by(prod_id) %>%
  print(max(unit_prod_price))
ggplot(transaction, aes(prod_unit_price)) + 
  geom_histogram(binwidth=1) +
  xlim(0,25) #No item sold for $0 so nothing is completely free

# What are the products that are most bought using offers/coupons?
transaction %>% 
  group_by(prod_id) %>%
  summarize_at("tran_prod_offer_cts", sum) %>%
  arrange(desc(tran_prod_offer_cts)) 
#Carrot, coffee, banana, oil, tomato, onion, milk, green bean, zucchini are most bought using coupons
prod_most_coupons <- subset(product, prod_id == 999956795 | prod_id == 999361204 | prod_id == 999746519 | prod_id == 999951863 | prod_id == 999712725  | prod_id == 999967197 | prod_id == 999957158 | prod_id == 999957157 | prod_id == 999421692 | prod_id == 999626930)
View(prod_most_coupons)

# Which products generate the highest revenue?
transaction %>%
  group_by(prod_id) %>%
  summarise(high_rev = sum(tran_prod_paid_amt)) %>%
  arrange(desc(high_rev))
high_rev_prod = subset(product, prod_id == 999749469 | prod_id == 999956795 | prod_id == 999749894 | prod_id == 999455829 | prod_id == 999649801 | prod_id == 999747259 | prod_id == 999557956 | prod_id == 999955966 | prod_id == 999749460 | prod_id == 999696393)
View(high_rev_prod)

# KMEANS CLUSTERING -------------------------------------
# Customer 
# Variable 1: How many units of a product did a person purchase
prod_purch <- transaction %>%
  group_by(cust_id, prod_unit) %>%
  summarise(num_prod = sum(tran_prod_sale_qty)) %>% 
  arrange(desc(num_prod))
View(prod_purch)
# Pivoting the table so we have counts separated from KG 
prod_purch <- prod_purch %>%
  pivot_wider(names_from = prod_unit, values_from = num_prod)
View(prod_purch)

# Variable 2: How much did each person spend
amount_spent <- transaction %>%
  group_by(cust_id) %>%
  summarise(amt_spent = sum(tran_prod_paid_amt)) %>%
  arrange(desc(amt_spent))
View(amount_spent)

# Variable 3: How many items did they buy in 1 transaction 
num_products <- transaction %>% 
  group_by(cust_id) %>%
  count() %>%
  arrange(desc(n))
View(num_products)

# Variable 4: How many coupons did they use
num_coupons <- transaction %>%
  group_by(cust_id) %>%
  summarize_at("tran_prod_offer_cts", sum) %>%
  arrange(desc(tran_prod_offer_cts))
View(num_coupons)

# Variable 5: How much of a discount are they getting
discount_total <- transaction %>%
  group_by(cust_id) %>%
  summarise(discount = sum(tran_prod_discount_amt)) %>% 
  arrange(desc(discount))
View(discount_total)

# Making a dataframe of all the variables we want to include in our Customer Clustering
knn_df <- data.frame(prod_purch, amount_spent, num_products, num_coupons, discount_total)
knn_df <- subset(knn_df, select = c("cust_id", "CT", "KG",  "amt_spent", "n", "tran_prod_offer_cts", "discount"))
View(knn_df)

# Scaling the data as knn is sensitive to this 
knn_df_scaled <- scale(knn_df[,2:7])
View(knn_df_scaled)


# First iteration of the clustering
k1 <- kmeans(knn_df_scaled, centers = 2, nstart = 25)
str(k1)
fviz_cluster(k1, data= knn_df_scaled)
k1

# Finding the ideal number of clusters we should have
##Elbow Method
wss <- function(k) {
  kmeans(knn_df_scaled, k, nstart = 10 )$tot.withinss
}

k.values <- 1:15

wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

fviz_nbclust(knn_df_scaled, kmeans, method = "wss")

##Average Silhouette Method
avg_sil <- function(k) {
  km.res <- kmeans(knn_df_scaled, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(knn_df_scaled))
  mean(ss[, 3])
}

k.values <- 2:15

avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")

fviz_nbclust(knn_df_scaled, kmeans, method = "silhouette")

# Final KMeans Clustering
final <- kmeans(knn_df_scaled, centers = 4, nstart = 25)
str(final)
final

# Plotting the final clusters for customer segments
customer_kmeans_plot <- fviz_cluster(final, geom="point", data=knn_df_scaled) + ggtitle("Customer Clusters, k=4")
customer_kmeans_plot

# Mapping clusters back to original data 
knn_df$clustering <- final$cluster
View(knn_df)

# Creating individual datasets for each cluster to further understand their needs
cluster1 <- subset(knn_df, clustering==1)
cluster2 <- subset(knn_df, clustering==2)
cluster3 <- subset(knn_df, clustering==3)
cluster4 <- subset(knn_df, clustering==4)
##Did a sanity check to make sure every customer was only segmented once 

# Mapping back to transactions table
c1 <- subset(transaction, cust_id %in% cluster1$cust_id)
c2 <- subset(transaction, cust_id %in% cluster2$cust_id)
c3 <- subset(transaction, cust_id %in% cluster3$cust_id)
c4 <- subset(transaction, cust_id %in% cluster4$cust_id)

# Summary of clusters 
summary(cluster1) #buying fewer things in count but more amount in KG with high discount amounts 
summary(cluster2) #more quantity but with a lot of offers but total discount amount is low 
summary(cluster3) # very average purchasing behavior 
summary(cluster4) # High amounts purchased but rest of the behavior is average

# Interpreting clusterss 
knn_df$clustering <- as.factor(knn_df$clustering)
(p <- ggparcoord(data=knn_df, columns=c(2:7), groupColumn="clustering", scale="std"))

# KMEANS CLUSTERING ----------------------------------
# Store 
# Variable 1: Number of transactions
num_shopping <- transaction %>%
  group_by(store_id) %>%
  count() %>%
  arrange(desc(n))
View(num_shopping)

# Variable 2: How many units are they selling
units <- transaction %>%
  group_by(store_id, prod_unit) %>%
  summarise(num_prod = sum(tran_prod_sale_qty)) %>% 
  arrange(desc(num_prod))
View(units)
# Pivoting the table so we have counts separated from KG 
units <- units %>%
  pivot_wider(names_from = prod_unit, values_from = num_prod)
View(units)

# Variable 3: How much revenue are they generating
revenue <- transaction %>%
  group_by(store_id) %>%
  summarise(high_rev = sum(tran_prod_paid_amt)) %>%
  arrange(desc(high_rev))
View(revenue)

# Variable 4: How many discounts do they offer 
discount <- transaction %>%
  group_by(store_id) %>%
  summarise(total_disc = sum(tran_prod_offer_cts)) %>%
  arrange(desc(total_disc))
View(discount)

# Making a dataframe of all the variables we want to include in our Customer Clustering
knn_df_store <- data.frame(num_shopping, units, revenue, discount)
knn_df_store <- subset(knn_df_store, select = c("store_id", "n", "CT", "KG",  "high_rev", "total_disc"))
View(knn_df_store)

# Scaling the data as knn is sensitive to this 
knn_df_scaled_store <- scale(knn_df_store[,2:6])
View(knn_df_scaled_store)

# Remove any 0/NA Values
knn_df_scaled_store <- knn_df_scaled_store[1:419,]

# First iteration of the clustering
k1_store <- kmeans(knn_df_scaled_store, centers = 2, nstart = 25)
str(k1_store)
fviz_cluster(k1_store, data= knn_df_scaled_store)
k1_store


# Finding the ideal number of clusters we should have
##Elbow Method
wss <- function(k) {
  kmeans(knn_df_scaled_store, k, nstart = 10 )$tot.withinss
}

k.values <- 1:15

wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

fviz_nbclust(knn_df_scaled_store, kmeans, method = "wss")

##Average Silhouette Method
avg_sil <- function(k) {
  km.res <- kmeans(knn_df_scaled_store, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(knn_df_scaled_store))
  mean(ss[, 3])
}

k.values <- 2:15

avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")

fviz_nbclust(knn_df_scaled_store, kmeans, method = "silhouette")


# Final KMeans Clustering
final_store <- kmeans(knn_df_scaled_store, centers = 3, nstart = 25)
str(final_store)
final_store

# Plotting the final clusters for store segments
store_kmeans_plot <- fviz_cluster(final_store, geom="point", data=knn_df_scaled_store) + ggtitle("Store Clusters, k=5")
store_kmeans_plot

# Mapping clusters back to original data 
knn_df_store <- knn_df_store[1:419,]
knn_df_store$clustering <- final_store$cluster
View(knn_df_store)

# Creating individual datasets for each cluster to further understand their needs
store1 <- subset(knn_df_store, clustering==1)
store2 <- subset(knn_df_store, clustering==2)
store3 <- subset(knn_df_store, clustering==3)
store4 <- subset(knn_df_store, clustering==4)
store5 <- subset(knn_df_store, clustering==5)
##Similar to above - did a sanity check to make sure every store was only segmented once 

# Mapping back to transactions table
s1 <- subset(transaction, store_id %in% store1$store_id)
s2 <- subset(transaction, store_id %in% store2$store_id)
s3 <- subset(transaction, store_id %in% store3$store_id)
s4 <- subset(transaction, store_id %in% store4$store_id)
s5 <- subset(transaction, store_id %in% store5$store_id)
View(s2)

# Interpreting clusters 
knn_df_store$clustering <- as.factor(knn_df_store$clustering)
(p <- ggparcoord(data=knn_df_store, columns=c(2:6), groupColumn="clustering"))
