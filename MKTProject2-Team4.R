library(readr)
library(dplyr)
library(lubridate)
library(tidyverse)
library(scales)
library(lsa)


# IMPORTING DATA ----------------------------------------------------------
product <- read_csv("product_table.csv")
transaction <- read_csv("transaction_table.csv")


# DATA CLEANING -----------------------------------------------------------
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

# Extract day of the week from to see what days are most popular 
transaction$day <- weekdays(as.Date(transaction$tran_dt))
transaction$month <- month(as.Date(transaction$tran_dt))

# Removing transactions that do not map to a specific product in the product table
transaction <- subset(transaction, prod_id %in% product$prod_id)

# Calculating profit - per our research grocery stores have a 2.2% profit margin so we're using this as a baseline profit for all products 
transaction$profit <- transaction$tran_prod_paid_amt * 0.022

# Converting discount to discount percentage 
transaction$discount_percent <- -(transaction$tran_prod_discount_amt)/transaction$tran_prod_sale_amt


# DATA EXPLORATION ------------------------------------------------------
# What are the chocolate products being sold?
patterns <- c("CHOCOLATE", "BONBONS")
chocolate <- filter(product, grepl(paste(patterns, collapse="|"), category_desc_eng))
View(chocolate)

# Removing any chocolate drinks from the subset as this project is only looking at chocolate candy products
chocolate <- subset(chocolate, category_desc != "ACHOCOLATADOS")

# Creating a new brand indexing that combines all sub-brands for each company and maps some to category "other" if they're not one of the main competitors 
chocolate <- chocolate %>%
  mutate(bigbrand = case_when(brand_desc == "NESTLE" | brand_desc == "KIT KAT" | brand_desc == "SMARTIES" | brand_desc == "AFTER EIGHT" | brand_desc == "LION"  ~ "NESTLE",
                              brand_desc == "LACASA" | brand_desc == "LACASITOS" ~ "LACASA",
                              brand_desc == "PINTAROLAS" | brand_desc == "REGINA" | brand_desc == "JUBILEU" | brand_desc == "IMPERIAL" ~ "IMPERIAL",
                              brand_desc == "TOBLERONE" | brand_desc == "MILKA" | brand_desc == "CADBURY" | brand_desc == "COTE D'OR" ~ "MONDELEZ",
                              brand_desc == "KINDER" | brand_desc == "MON CHERI" | brand_desc == "FERRERO ROCHER" | brand_desc == "FERRERO" | brand_desc == "RAFAELLO" ~ "FERRERO GROUP",
                              brand_desc == "LUTTI" | brand_desc == "JACQUOT" ~ "FRENCH",
                              brand_desc == "M&M'S" | brand_desc == "MALTESERS" | brand_desc == "MARS" | brand_desc == "TWIX" | brand_desc == "SNICKERS" | brand_desc == "BOUNTY" ~ "MARS",
                              brand_desc == "RITTER" | brand_desc == "FRIEDEL" | brand_desc == "MERCI" | brand_desc == "SCHOGETTEN" | brand_desc == "WERTHER" ~ "GERMAN",
                              brand_desc == "ESP BEIJOS" | brand_desc == "FRUTOP" | brand_desc == "GUYLIAN" | brand_desc == "SOCADO" | brand_desc == "CANDYCAT" | brand_desc == "BON-O-BON" | brand_desc == "DISNEY" | brand_desc == "HAMLET" | brand_desc == "DEKORA" | brand_desc == "WITORS" | brand_desc == "INTERDOCES" | brand_desc == "MCVITIES" ~ "OTHER",
                              brand_desc == "PRIVATE LABEL" ~ "PRIVATE LABEL",
                              brand_desc == "LINDT" | brand_desc == "LINDOR" ~ "LINDT"))

# Sanity check that every brand was accounted for 
#sum(is.na(chocolate$bigbrand))

# How many items is Nestle selling 
chocolate %>% 
  group_by(bigbrand) %>%
  count() %>%
  arrange(desc(n)) #Mondelez is selling the most number of items, followed by Nestle

# Nestle produces products other than chocolate, however what is Nestle's marketshare in the chocolate industry specifically?
##Calculating total revenue for each product 
revenue <- transaction %>%
  group_by(prod_id) %>%
  summarize_at("tran_prod_paid_amt", sum) %>%
  arrange(desc(tran_prod_paid_amt))
##Subsetting chocolate products
market_share <- subset(revenue, prod_id %in% chocolate$prod_id)
##Adding in brand information
market_share <- market_share %>%
  mutate(brand = case_when(prod_id %in% chocolate$prod_id ~ chocolate$bigbrand))
##Getting total revenue by brand
ms <- market_share %>%
  group_by(brand) %>%
  summarize_at("tran_prod_paid_amt", sum) %>%
  arrange(desc(tran_prod_paid_amt))
View(ms) # Nestle has 14% of the marketshare with Mondelez being their biggest competitor

# Visualizing the market share in Tableau
fwrite(mkt_share_by_brand, "Market Share.csv")

# Who is buying chocolate?
choc_pop <- subset(transaction, prod_id %in% chocolate$prod_id)
View(choc_pop) # ~323k transactions  for chocolates, very small proportion of total transactions (1% of total transactions made are on chocolate products - might be due to high taxation)

# Who is buying Nestle items?
product2 <- product %>%
  mutate(bigbrand = case_when(brand_desc == "NESTLE" | brand_desc == "KIT KAT" | brand_desc == "SMARTIES" | brand_desc == "AFTER EIGHT" | brand_desc == "LION"  ~ "NESTLE"))
product2 <- subset(product2, bigbrand == "NESTLE")
nestle_prod <- transaction[transaction$prod_id %in% product2$prod_id,]
nestle_customers <- nestle_prod$cust_id #134k transactions

# Who is buying Nestle chocolate?
nestle <- subset(chocolate, bigbrand == "NESTLE")
nestle_choc <- subset(transaction, prod_id %in% nestle$prod_id ) # ~53k transactions are on Nestle chocolate products

# Who is buying any other brands of chocolate?
other_brands <- subset(chocolate, !(bigbrand == "NESTLE"))
other_brands_transaction <- subset(transaction, prod_id %in% other_brands$prod_id ) # ~269k transactions

fwrite(other_brands_transaction, "OtherChocolateBuyers.csv")

# Who is buying Nestle itmes but not Nestle chocolate?
nestle_not_choc <- subset(nestle_prod, !(cust_id %in% nestle_choc$cust_id))

# Who is buying Nestle items and other brands of chocolate?
chocolate_other_brands <- subset(chocolate, !(bigbrand == "NESTLE"))
chocolate_other_brands_tran <- subset(transaction, prod_id %in% chocolate_other_brands$prod_id)

cust_nestle_not_choc <- unique(nestle_not_choc$cust_id)
cust_other_brand_choc <- unique(chocolate_other_brands_tran$cust_id)
common <- intersect(cust_nestle_not_choc, cust_other_brand_choc)
common <- subset(transaction, cust_id %in% common)
length(unique(common$cust_id)) #1020 customers

common %>%
  group_by(prod_id.x) %>%
  count() %>%
  arrange(desc(n)) # Top 10 most popular items are not any chocolate items, they are yogurts, animal food, baking products, and breakfast cereals
#subset(product, prod_id == 999180282 | prod_id == 999328402 | prod_id == 999333548 | prod_id == 999958115 | prod_id == 999334005 | prod_id == 180611010 | prod_id == 999370503 |prod_id == 999249522 | prod_id == 999457945 | prod_id == 999529178)

fwrite(common, "NestleAndOtherChocolates.csv")


# NESTLE DATA EXPLORATION -------------------------------------------
## Customer
str(nestle_choc)
summary(nestle_choc)
#UNITS: 1-48 units - avg 1.3
#SALE AMOUNT: 0.020 - 64.60 
#DISCOUNT: 64.5-0 - avg -0.8567, max 4 discounts offered 
#UNIT PRICE: 0.390-12.990

# Which customer has the most transactions
nestle_choc %>%
  group_by(cust_id) %>%
  count(new_tran_id) %>%
  arrange(desc(n)) #customer 49579835 & 80309883 have purchased chocolate 6 times 

# Number of customers 
length(unique(nestle_choc$cust_id)) #6642 customers 

# Highest spending 
highest_spending <- nestle_choc %>%
  group_by(cust_id) %>%
  mutate(sum=sum(tran_prod_paid_amt)) %>%
  arrange(desc(sum))
median(highest_spending$sum)

# Visualizing the distribution of spending
ggplot(mm, aes(tran_prod_paid_amt))+
  geom_histogram()
mean(mm$tran_prod_paid_amt)

# Which customer has bought the most products?
nestle_choc %>%
  group_by(cust_id, prod_id) %>%
  count() %>%
  arrange(desc(n)) #customer 31489706 bought 69 units of product 999178814

# We can study these customers in more detail based on transaction. 
# How many items per transactions? 
item_per_trans <- nestle_choc %>%
  group_by(new_tran_id) %>%
  mutate(sum = sum(tran_prod_sale_qty)) %>%
  arrange(desc(sum))

# Mean/median Nestle chocolates bought per trans -- 1 to 1.5
mean(item_per_trans$sum)
hist(item_per_trans$sum)

#Customers - 339627, 50399606, 749556 have bought more than 40 Nestle choc products in one transaction and have also made frequent transactions of such high amounts from the stores

# The customers have made 6, 8, and 19 transactions of large count of products from the stores. We could call these customers as heavy volume purchasers who probably buy Nestle chocolates in wholesale/bulk to either sell them or for events. 
high_vol_nestle_choc <- subset(nestle_choc, cust_id == 339627 | cust_id ==  50399606 | cust_id == 749556)
high_vol_nestle_choc %>%
  group_by(cust_id) %>%
  count()

# Which customers buy the most using coupons/offers?  
nestle_choc %>%
  group_by(cust_id) %>%
  summarize_at("tran_prod_offer_cts", sum) %>%
  arrange(desc(tran_prod_offer_cts)) #customer 49579835 uses the most discounts (91)

# Which customers are the most profitable?
nestle_choc %>%
  group_by(cust_id) %>%
  summarize_at("profit", sum) %>%
  arrange(desc(profit))

# What % of customers act above average discount
above_avg_disc <- subset(nestle_choc, discount_percent > 0.106)
  
## Products 

# What are the products that are most bought? 
nestle_choc %>%
  group_by(prod_id) %>%
  count() %>%
  arrange(desc(n)) 
pop_prod <- subset(product, prod_id == 999994226 | prod_id == 999983963 | prod_id == 999178814 | prod_id == 999180322 |prod_id == 999179830 |prod_id == 999196506 |prod_id == 999182987 |prod_id == 999349767 |prod_id == 999197983 | prod_id == 999983158)
View(pop_prod) # Nestle's seasonal chocolates are not popular, kitkat, lion, after eight doing really well 

# Which is the most expensive product and what is the distribution of product prices in the dataset? Histogram
nestle_choc %>%
  group_by(prod_id) %>%
  print(max(unit_prod_price))
ggplot(nestle_choc, aes(prod_unit_price)) + 
  geom_histogram(binwidth=1) +
  xlim(0,25) #No item sold for 0 so nothing is completely free, one chocolate being sold for more than 10 euros

# What are the products that are most bought using offers/coupons?
nestle_choc %>% 
  group_by(prod_id) %>%
  summarize_at("tran_prod_offer_cts", sum) %>%
  arrange(desc(tran_prod_offer_cts)) 

# Which products generate the highest revenue?
nestle_choc %>%
  group_by(prod_id) %>%
  summarise(high_rev = sum(tran_prod_paid_amt)) %>%
  arrange(desc(high_rev))
high_rev_prod = subset(product, prod_id == 999983158 | prod_id == 999983963 | prod_id == 999994226 | prod_id == 999398484 | prod_id == 999179830 | prod_id == 999180322 | prod_id == 999196506 | prod_id == 999178814 | prod_id == 999254968 | prod_id == 999197983)
View(high_rev_prod) #Kit kat, lion, and other generic Nestle chocolates are doing well

# Other products bought by bulk buyers
high_vol_nestle_choc_others <- subset(transaction, cust_id %in% high_vol_nestle_choc$cust_id)
high_vol_nestle_choc_others %>%
  group_by(prod_id) %>%
  count() %>%
  arrange(desc(n))

# Discounts in Nestle chocolate transactions
discount_nestle_choc_trans <- nestle_choc %>%
  group_by(new_tran_id) %>%
  mutate(total_discount = sum(tran_prod_discount_amt)) %>%
  arrange(total_discount)

median(discount_nestle_choc_trans$total_discount) #66 cents discount per trans

# Discounts used by Nestle choc customers 
discount_nestle_choc_cust <- nestle_choc %>%
  group_by(cust_id) %>%
  mutate(total_discount = sum(tran_prod_discount_amt)) %>%
  arrange(total_discount)

# Customers with large amounts of discounts (more than 150E in total) - 6139769, 77329638, 18359609, 2969695 but they do not have too many transactions (so they don't buy too much). They also have the largest discounts per transaction 

# These could be the bargain hunters of the Nestle choc customers

median(discount_nestle_choc_cust$total_discount) #About 10 euros discount per customer
summary(discount_nestle_choc_cust)

# Calculate a discount rate for all the Nestle choc customers 
nestle_choc <- nestle_choc %>%
  group_by(cust_id) %>%
  mutate(avg_discount_rate = sum(tran_prod_discount_amt)/sum(tran_prod_sale_amt))

# KMEANS CLUSTERING (NESTLE CHOC CUSTOMERS) -----------------------------------------
View(nestle_choc)

# Nestle chocolate customers clustering 
# Variable 1: How many units of a product did a person purchase
choc_qty <- nestle_choc %>%
  group_by(cust_id, prod_unit) %>%
  summarise(num_prod = sum(tran_prod_sale_qty)) %>% 
  arrange(desc(num_prod))
View(choc_qty)
# Pivoting the table so we have counts separated from KG 
choc_qty <- choc_qty %>%
  pivot_wider(names_from = prod_unit, values_from = num_prod)
View(choc_qty)

# Variable 2: How much did each person spend
amount_spent <- nestle_choc %>%
  group_by(cust_id) %>%
  summarise(amt_spent = sum(tran_prod_paid_amt)) %>%
  arrange(desc(amt_spent))
View(amount_spent)

# Variable 3: How many items did they buy in 1 transaction 
num_products <- nestle_choc %>% 
  group_by(cust_id) %>%
  count() %>%
  arrange(desc(n))
View(num_products)

# Variable 4: How many coupons did they use
num_coupons <- nestle_choc %>%
  group_by(cust_id) %>%
  summarize_at("tran_prod_offer_cts", sum) %>%
  arrange(desc(tran_prod_offer_cts))
View(num_coupons)

# Variable 5: How much of a discount are they getting
discount_total <- nestle_choc %>%
  group_by(cust_id) %>%
  summarise(discount = sum(tran_prod_discount_amt)) %>% 
  arrange(desc(discount))
View(discount_total)

# Making a dataframe of all the variables we want to include in our Customer Clustering
knn_df <- data.frame(choc_qty, amount_spent, num_products, num_coupons, discount_total)
knn_df <- subset(knn_df, select = c("cust_id", "CT", "amt_spent", "n", "tran_prod_offer_cts", "discount"))
View(knn_df)

# Scaling the data as knn is sensitive to this 
knn_df_scaled <- scale(knn_df[,2:6])
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
final <- kmeans(knn_df_scaled, centers = 3, nstart = 25)
str(final)
final

# Plotting the final clusters for customer segments
nestle_customer_kmeans_plot <- fviz_cluster(final, geom="point", data=knn_df_scaled) + ggtitle("Customer Clusters, k=3")
nestle_customer_kmeans_plot

# Mapping clusters back to original data 
knn_df$clustering <- final$cluster
View(knn_df)

# Creating individual datasets for each cluster to further understand their needs
cluster1 <- subset(knn_df, clustering==1)
cluster2 <- subset(knn_df, clustering==2)
cluster3 <- subset(knn_df, clustering==3)
##Did a sanity check to make sure every customer was only segmented once 

# Mapping back to transactions table
c1 <- subset(transaction, cust_id %in% cluster1$cust_id)
c2 <- subset(transaction, cust_id %in% cluster2$cust_id)
c3 <- subset(transaction, cust_id %in% cluster3$cust_id)

# Summary of clusters 
summary(cluster1) #buying large quantities in every transaction (discount does not really matter but bulk buying)
summary(cluster2) # average buyer
summary(cluster3) #average quantity but large discounts - bargain hunters

# Interpreting clusters 
knn_df$clustering <- as.factor(knn_df$clustering)
(p <- ggparcoord(data=knn_df, columns=c(2:6), groupColumn="clustering", scale="std"))

NestleCluster1 <- subset(nestle_choc, cust_id %in% cluster1$cust_id)
NestleCluster2 <- subset(nestle_choc, cust_id %in% cluster2$cust_id)
NestleCluster3 <- subset(nestle_choc, cust_id %in% cluster3$cust_id)

fwrite(NestleCluster1, "NestleCluster1.csv")
fwrite(NestleCluster2, "NestleCluster2.csv")
fwrite(NestleCluster3, "NestleCluster3.csv")

# Exporting clusters and creating recommender system using IBCF (cosine) in Python 

# Identified potential target customers from Python
# Reimporting data to explore purchasing behavior of these customers
data <- merge(transaction, product, by="prod_id")

c1 <- read_csv("C1.csv")
c2 <- read_csv("C2.csv")
c3 <- read_csv("C3.csv")

# Recommended cluster 1
cust_c1 <- c1$cust_id
c1 <- subset(data, cust_id == cust_c1)
c1_prods <- c1 %>%
  group_by(prod_id) %>%
  count() %>%
  arrange(desc(n))

# Recommended cluster 2
cust_c2 <- c2$cust_id
c2 <- subset(data, cust_id == cust_c2)
c2_prods <- c2 %>%
  group_by(prod_id) %>%
  count() %>%
  arrange(desc(n))

# Recommended cluster 3
cust_c3 <- c3$cust_id 
c3 <- subset(data, cust_id == cust_c3)
c3_prods <- c3 %>%
  group_by(prod_id) %>%
  count() %>%
  arrange(desc(n))

# ASSOCIATION RULES -------------------------------------------------------
#In order to see what items are freqently purchased together and which customers buy nestle just one time, we are using association rules. 
library(arules)
library(reshape2)
library(tm)

# Creating a list of items people have purchased  
cherry_pickers <- transaction %>%
  select(new_tran_id, prod_id) 

# Finding all the unique product ids
cherry_pickers <- unique(cherry_pickers)

# Splitting the product ids by the new transaction id we created
cherry_pickers <- split(cherry_pickers$prod_id, cherry_pickers$new_tran_id)

# Making dataframe into a transactions table
rules <- as(cherry_pickers, "transactions")

# Execute association rules
nestle_rules <- apriori(data=rules, parameter=list(supp=0.0015,conf = 0.003))
summary(nestle_rules) # Using 10 of these rules 

df_basket <- as(nestle_rules,"data.frame")

# Split lhs and rhs into two columns
df_basket <- transform(df_basket, rules = colsplit(rules, pattern = "=>", names = c("lhs","rhs")))

# Remove curly brackets around rules
df_basket$rules$lhs <- gsub("[{}]", "", df_basket$rules$lhs)
df_basket$rules$rhs <- gsub("[[:punct:]]", "", df_basket$rules$rhs)
df_basket$lhs$p1 <- as.factor(df_basket$lhs$p1)
df_basket$lhs$p2 <- as.factor(df_basket$lhs$p2)
df_basket$rules$rhs <- as.factor(df_basket$rules$rhs)

# Split the LHS into two product variables
df_basket <- transform(df_basket, lhs = colsplit(rules$lhs, pattern = ",", names = c("p1","p2")))

# Recommendations
#Find how many customers are buying Activia yogurt
yogurt <- subset(transaction, prod_id == 153721011|prod_id == 999167246|prod_id == 999333548)
target_pop <- unique(yogurt$cust_id)
target_pop <- intersect(nestle_customers, target_pop)

length(unique(yogurt$cust_id))

# EXPECTED TOTAL REDEMPTION COST & VOLUME ------------------------------------------
# What month is the busiest?
ggplot(transaction, aes(month)) + geom_bar()

# Finding the average price of Nestle products
count(nestle_choc$prod_unit_price)
count(nestle_choc$prod_unit_price) %>%
  summarize(sump = sum(x*freq)) 
sum(count(nestle_choc$prod_unit_price))
#divide the two values and it gives you 2.13 unit price

# Average quantity sold 
nestle_choc %>%
  summarize(avg = mean(tran_prod_sale_qty))

# Average discount 
nestle_choc %>%
  summarize(avg=mean(discount_percent))
