library(lubridate)
library(tidyverse)
library(data.table)
library(dplyr)
library(zoo)
library(dummies)
library(corrplot)
library(car)
library(MLmetrics)

# IMPORTING DATA -----------------------------------------------------------
product_sup <- fread("product_table_supp.csv", header = TRUE)
transaction_sup <- fread("transaction_table_supp.csv")
ad_promotion <- fread('promo_ad.csv', header = TRUE)
holiday <- fread('holiday.csv', header = TRUE)
seasonality <- fread('seasonality.csv', header = TRUE)
#product <- fread("product_table.csv", header = TRUE)
#transaction <- fread("transaction_clean.csv", header = TRUE)

# DATA EXPLORATION ---------------------------------------------------------
# Understanding San Miguel's Market Share
# The transaction ID in the dataset does not uniquely identify each transaction due to its large number format. So we will create a new column that will concatenate the customer id, store id, and transaction date columns. We will do this under the assumption that each customer only visits a store once during the day and this will allow us to group together all the products purchased into a single transaction. 
transaction_sup$new_tran_id <- paste(transaction_sup$cust_id, transaction_sup$store_id, transaction_sup$tran_dt, sep="-")

# Extract day of the week from to see what days are most popular 
transaction_sup$day <- weekdays(as.Date(transaction_sup$tran_dt))
transaction_sup$month <- month(as.Date(transaction_sup$tran_dt))

# Calculating profit - per our research grocery stores have a 2.2% profit margin so we're using this as a baseline profit for all products 
transaction_sup$profit <- transaction_sup$tran_prod_paid_amt * 0.022

# Converting discount to discount percentage 
transaction_sup$discount_percent <- -(transaction_sup$tran_prod_discount_amt)/transaction_sup$tran_prod_sale_amt

# Combining new supplement data with old
data <- rbind(transaction, transaction_sup, fill = TRUE)
products <- rbind(product, product_sup)

# Merging the two together 
data <- merge(data, products, by='prod_id', all=TRUE)
fwrite(data, "data.csv")

# Visualized market share through tableau 

# EDA for San Miguel Products
##The products we are working with are 138936951, 138936952, 138936953
## Working with 8745 transactions for all 3 products 
summary(transaction_sup)
##Sunday is the most popular day to purchase 
##July is the most popular month to purchase 
##Usually people buy 4 products 
##Usually no discounts offered but a max of 30% is offered because max discount offered is 1 

## Product
prod1 <- subset(transaction_sup, prod_id == 138936951)
prod2 <- subset(transaction_sup, prod_id == 138936952)
prod3 <- subset(transaction_sup, prod_id == 138936953)

median(prod1$prod_unit_price)
median(prod2$prod_unit_price)
median(prod3$prod_unit_price)

sum(prod1$tran_prod_sale_amt)
sum(prod2$tran_prod_sale_amt)
sum(prod3$tran_prod_sale_amt)

sum(prod1$tran_prod_sale_qty)
sum(prod2$tran_prod_sale_qty)
sum(prod3$tran_prod_sale_qty)

# What are the products that are most bought? 
transaction_sup %>%
  group_by(prod_id) %>%
  count() %>%
  arrange(desc(n)) #138936951 is the most purchased 

# What products are offering the most discounts
transaction_sup %>% 
  group_by(prod_id) %>%
  summarize_at("tran_prod_offer_cts", sum) %>%
  arrange(desc(tran_prod_offer_cts)) #138936951 offers the most (1379) 138936953 offers the least by a lot (290) 

# Which products generate the highest revenue?
transaction_sup %>%
  group_by(prod_id) %>%
  summarise(high_rev = sum(tran_prod_paid_amt)) 

## Store
# Which store has made the highest revenue? 
transaction_sup %>%
  group_by(store_id) %>%
  summarize_at("tran_prod_paid_amt", sum) %>%
  arrange(desc(tran_prod_paid_amt)) #store 342 has made the highest revenue (963 euros) combined of all customers 

# Which store gives out the most offers?
transaction_sup %>%
  group_by(store_id) %>%
  summarize_at("tran_prod_offer_cts", sum) %>%
  arrange(desc(tran_prod_offer_cts)) #store 342, 347, 349, 157 have given the most offers

# DATA PREPARATION ------------------------------------------------------
##Since we are looking at everything on a weekly basis we're extracting a week_index here that can be used for all the causals
week_index <- as.data.frame(as.Date(unique(ad_promotion$tran_wk)))
colnames(week_index) <- "tran_wk"
week_index$tran_wk <- as.Date(week_index$tran_wk)
  
# CAUSAL 1: TV & RADIO PROMOTION ----------------------------------------
# We want to convert GRPs to Reach as that is  a more ideal form for modeling. To do this, we will first convert GRP to AdStocked GRP by calculating the decay value (alpha value) using a half life of 4 for Radio and 8 for TV as given in the prompt. We will then convert AdStocked GRP to reach for TV and Radio respectively.

# What products are the TV and Radio ads for?
ad_promotion %>%
  filter(vehicle == "TV" | vehicle == "Radio") #promoting all 3 products 

# Subsetting and getting all the GRP for TV and Radio
TV <- subset(ad_promotion, vehicle == "TV")
Radio <- subset(ad_promotion, vehicle == "Radio")

# Calculating alpha values for 4 and 8 week half life
alphaTV = 1 - 0.5 ** (1 / 8) # Around 0.0829
alphaRadio = 1 - 0.5 ** (1 / 4) # Around 0.15910

##TV --------------------------------------------------------------------
# We are making the assumption that the first ad was shown on '2016-06-05' as that is the first date we have GRPs for. Additionally there is no carry over effect 
TV <- merge(week_index, TV, by = 'tran_wk', all.x = TRUE)
TV <- subset(TV, tran_wk >= '2016-06-05')

# Ensuring all the variables are datetime objects
TV$tran_wk <- as.Date(TV$tran_wk)

# After trying to calculate the AdStocked GRP using the NA's we realized that it was throwing an error so for converted them to zeros to be able to perform the calculation 
unique(TV$amount)
TV <- TV %>%
  mutate(amount = case_when(is.na(amount) ~ 0,
                            amount == 500 ~ 500,
                            amount == 300 ~ 300,
                            amount == 200 ~ 200))

# Initializing an empty column called adstock to allow for adstocked values to be input 
TV$adstock <- 0

# Calculating Adstocked GRP
for (week in as.character(TV$tran_wk)) {
  if (week == '2016-06-05') {
    TV_new <- TV %>%
      filter(tran_wk == week) %>%
      mutate(adstock = alphaTV*amount)
    adstock_previous_period <- TV_new %>%
      filter(tran_wk == week) %>%
      select(adstock)
  } else {
    GRP <- TV %>%
      filter(tran_wk == week) %>%
      select(amount)
    adstocked_grp <- (alphaTV*GRP + (1 - alphaTV)*adstock_previous_period) 
    TV[TV$tran_wk == week, 'adstock'] <- adstocked_grp
    adstock_previous_period <- TV %>%
      filter(tran_wk == week) %>%
      select(adstock)
  }
}

# Adding in the value for the first GRP 
TV$adstock[which(TV$tran_wk == "2016-06-05")] <- 41.49798

# Converting adstock to reach using the formula provided in the prompt
TV <- TV %>%
  mutate(reach = 0.95*(1-exp(-0.02*adstock)))

# Changing the name of the columns for easier merge later 
colnames(TV)[6] <- "adstockTV"
colnames(TV)[7] <- "reachTV"

##RADIO -----------------------------------------------------------------
# Repeating the same steps for Radio Promotions 
# We are making the assumption that the first ad was shown on '2016-06-05' as that is the first date we have GRPs for. Additionally there is no carry over effect 
Radio <- merge(week_index, Radio, by = 'tran_wk', all.x = TRUE)
Radio <- subset(Radio, tran_wk >= '2016-06-05')

# Ensuring all the variables are datetime objects
Radio$tran_wk <- as.Date(Radio$tran_wk)

# After trying to calculate the AdStocked GRP using the NA's we realized that it was throwing an error so for converted them to zeros to be able to perform the calculation 
unique(Radio$amount)
Radio <- Radio %>%
  mutate(amount = case_when(is.na(amount) ~ 0,
                            amount == 400 ~ 400,
                            amount == 300 ~ 300,
                            amount == 200 ~ 200,
                            amount == 100 ~ 100))

# Initializing an empty column called adstock to allow for adstocked values to be input 
Radio$adstock <- 0

# Calculating Adstocked GRP
for (week in as.character(Radio$tran_wk)) {
  if (week == '2016-06-05') {
    Radio_new <- Radio %>%
      filter(tran_wk == week) %>%
      mutate(adstock = alphaRadio*amount)
    adstock_previous_period <- Radio_new %>%
      filter(tran_wk == week) %>%
      select(adstock)
  } else {
    GRP <- Radio %>%
      filter(tran_wk == week) %>%
      select(amount)
    adstocked_grp <- (alphaRadio*GRP + (1 - alphaRadio)*adstock_previous_period) 
    Radio[Radio$tran_wk == week, 'adstock'] <- adstocked_grp
    adstock_previous_period <- Radio %>%
      filter(tran_wk == week) %>%
      select(adstock)
  }
}

# Adding in the value for the first GRP 
Radio$adstock[which(Radio$tran_wk == "2016-06-05")] <- 31.82072

# Converting adstock to reach using the formula provided in the prompt
Radio <- Radio %>%
  mutate(reach = 0.90*(1-exp(-0.025*adstock)))

# Changing the name of the columns for easier merge later 
colnames(Radio)[6] <- "adstockRadio"
colnames(Radio)[7] <- "reachRadio"

# Checking distribution of radio and TV reach
hist(Radio$reachRadio)
radio_copy <- copy(Radio)
radio_copy$logreachRadio <- sqrt(radio_copy$reachRadio) #sqrt looks good
hist(radio_copy$logreachRadio)

hist(TV$reachTV) # Looks fine as is 
TV_copy <- copy(TV)
TV_copy$logreachTV <- sqrt(TV_copy$reachTV)
hist(TV_copy$logreachTV)

# CAUSAL 2: PROMOTION & DISCOUNT ----------------------------------------
# We want to account for list price and how much discount was offered in our model. Here we make sure to get a weekly price and discount. 
##Making sure all the dates are actually date objects in r
transaction_sup$tran_dt <- as.Date(transaction_sup$tran_dt)
transaction_sup <- merge(transaction_sup, week_index, by.x="tran_dt", by.y="tran_wk", all.x = TRUE, all.y = TRUE)

##Upon viewing the transaction table now we can see we have a row for 2015 but no values, we are going to go ahead and remove this assuming it's a data entry error
transaction_sup <- subset(transaction_sup, tran_dt > "2015-12-27")

##The way R looks at weeks is that it has to start on a monday have 4+ days to be considered the first week. Because January 1 2016 does not start on a monday the dates prior to the 2nd are considered a part of the previous week - https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/strptime
transaction_sup$tran_wk = as.Date(cut(transaction_sup$tran_dt+1,"week"))-1

##Calculating weekly list price and discount percentage 
transaction_sup <- transaction_sup %>% 
  group_by(tran_wk, prod_id) %>%
  mutate(weekly_price = sum(tran_prod_sale_amt)/sum(tran_prod_sale_qty), weekly_discount = sum(tran_prod_discount_amt)/sum(tran_prod_sale_qty))

# CAUSAL 3: SEASONALITY & HOLIDAYS --------------------------------------
# SEASONALITY -----------------------------------------------------------
##We have been given the seasonality index so we will add this to our data and use it as a variable in the regression. However, we need to also account for the effect of holidays - looking specifically at what are the major holidays and how long do their effects last?
seasonality$tran_wk <- as.Date(seasonality$tran_wk)

##Adding seasonality information into the dataset
transaction_sup <- merge(transaction_sup, seasonality, by="tran_wk")

hist(transaction_sup$seas_index) #either log or leave as is
transaction_sup_copy$logseas_index <- log(transaction_sup_copy$seas_index)
hist(transaction_sup_copy$logseas_index)

# Holidays --------------------------------------------------------------
##We are trying to see which holidays are actually impacting sales so as to only include those variables in our model 

##Making copy of the transaction data 
transaction_sup_copy <- transaction_sup

##Creating dummy variables for each holiday 
holiday_dummies <- cbind(holiday, dummy(holiday$holiday, sep = "_"))

##Merging this back into our transaction information 
transaction_sup_copy <- merge(transaction_sup_copy, holiday_dummies, by="tran_wk")

##Calculating weekly sales quantities
transaction_sup_copy <- transaction_sup_copy %>%
  group_by(tran_wk, prod_id) %>%
  mutate(sales = sum(tran_prod_sale_qty))

##Calculating change in weekly sales quantities
transaction_sup_copy <- transaction_sup_copy %>%
  group_by(prod_id) %>%
  mutate(weekly_change_sales = sd(sales))

##Removing any NA values in the data frame
transaction_sup_copy <- transaction_sup_copy[complete.cases(transaction_sup_copy), ]

##Visualizing the correlation matrix to see which holidays positively/negatively impact sales
t <- cor(transaction_sup_copy)
corrplot(t, method="color")
col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = t, col=col, symm = TRUE) ##From this we can see that Allsaints, Immaculate, PreEaster, Corpus, and Liberty have negative impacts on sales and Portugal has a positive impact on sales

##To get the correlation values for each holiday
transaction_sup_copy <- transaction_sup_copy %>% 
    mutate(corr_allsaints = cor(holiday_ALLSAINTS, sales), corr_newyear = cor(holiday_NEWYEAR, sales), cor(holiday_PrEASTER, sales),cor(holiday_EASTER, sales),cor(holiday_PrLIBERTY, sales),cor(holiday_LIBERTY, sales),cor(holiday_LABOR, sales),cor(holiday_CORPUS, sales),cor(holiday_PORTUGAL, sales),cor(holiday_PrASSUMPTION, sales),cor(holiday_ASSUMPTION, sales),cor(holiday_REPUBLIC, sales),cor(holiday_RESTORATION, sales),cor(holiday_IMMACULATE, sales),cor(holiday_PrXMAS, sales),cor(holiday_XMAS, sales),cor(holiday_CARNIVAL, sales),cor(holiday_POPEVISIT, sales))

##Removing holidays we don't want 
holiday_dummies <- subset(holiday_dummies, select = -c(holiday_NEWYEAR, holiday_PrASSUMPTION, holiday_RESTORATION, holiday_REPUBLIC, holiday_LABOR))

holiday_dummies$tran_wk <- as.character(holiday_dummies$tran_wk)
transaction_sup$tran_wk <- as.character(transaction_sup$tran_wk)

##Adding in holiday information to the transaction dataset
transaction_sup <- transaction_sup %>%
  left_join(holiday_dummies)

##Converting any NA values to 0 
transaction_sup[is.na(transaction_sup)] = 0

##Removing unnecessary columns
transaction_sup <- subset(transaction_sup, select = -c(tran_id, prod_unit, holiday))

# CAUSAL 4: PAID SEARCH, WEB DISPLAY, EMAIL, STORE DISPLAY, FLYERS ------
##Because all the other types of promotions are on a product basis we are going to divide the data into the 3 different products and add all the data to them for modeling 
data_1 <- subset(transaction_sup, prod_id == 138936951)
data_2 <- subset(transaction_sup, prod_id == 138936952)
data_3 <- subset(transaction_sup, prod_id == 138936953)

# adding in TV & Radio promotion information to each sub-dataset
data_1$tran_wk <- as.Date(data_1$tran_wk)
data_1 <- merge(data_1, TV, by ="tran_wk", all.x = TRUE)
data_1 <- merge(data_1, Radio, by="tran_wk", all.x = TRUE )
data_1 <- subset(data_1, select = -c(vehicle.x, vehicle.y, amount.x, amount.y, unit.x, unit.y, prod_assoc.x, prod_assoc.y))
data_1[is.na(data_1)] = 0

data_2$tran_wk <- as.Date(data_2$tran_wk)
data_2 <- merge(data_2, TV, by="tran_wk", all.x = TRUE )
data_2 <- merge(data_2, Radio, by="tran_wk", all.x = TRUE )
data_2 <- subset(data_2, select = -c(vehicle.x, vehicle.y, amount.x, amount.y, unit.x, unit.y, prod_assoc.x, prod_assoc.y))
data_2[is.na(data_2)] = 0

data_3$tran_wk <- as.Date(data_3$tran_wk)
data_3 <- merge(data_3, TV, by="tran_wk", all.x = TRUE )
data_3 <- merge(data_3, Radio, by="tran_wk", all.x = TRUE )
data_3 <- subset(data_3, select = -c(vehicle.x, vehicle.y, amount.x, amount.y, unit.x, unit.y, prod_assoc.x, prod_assoc.y))
data_3[is.na(data_3)] = 0

##Adding in all the other data for flyers/paid search etc. 
ad_promotion <- subset(ad_promotion, (!(vehicle %in% c('TV', 'Radio'))))
ad_promotion$tran_wk <- as.character(ad_promotion$tran_wk)

# Breaking it out by product 
prod1 <- subset(ad_promotion, prod_assoc == 138936951)
prod1 <- dcast(prod1, form = tran_wk ~ vehicle, value.var = 'amount')
prod1$tran_wk <- as.Date(prod1$tran_wk)

prod2 <- subset(ad_promotion, prod_assoc == 138936952)
prod2 <- dcast(prod2, form = tran_wk ~ vehicle, value.var = 'amount')
prod2$tran_wk <- as.Date(prod2$tran_wk)

prod3 <- subset(ad_promotion, prod_assoc == 138936953)
prod3 <- dcast(prod3, form = tran_wk ~ vehicle, value.var = 'amount')
prod3$tran_wk <- as.Date(prod3$tran_wk)

prodall <- subset(ad_promotion, prod_assoc == 'ALL')
prodall$prod_assoc[prodall$prod_assoc == "ALL"] <- 123
prodall <- dcast(prodall, tran_wk ~ vehicle, value.var = 'amount')
prodall$tran_wk <- as.Date(prodall$tran_wk)

# Adding it back in for each product
data_1 <- merge(data_1, prod1, by="tran_wk", all.x = TRUE)
data_1 <- merge(data_1, prodall, by="tran_wk", all.x = TRUE)

data_2 <- merge(data_2, prod2, by="tran_wk", all.x = TRUE)
data_2 <- merge(data_2, prodall, by="tran_wk", all.x = TRUE)

data_3 <- merge(data_3, prod3, by="tran_wk", all.x = TRUE)
data_3 <- merge(data_3, prodall, by="tran_wk", all.x = TRUE)

# MODELING --------------------------------------------------------------
##Prepping the variables as we have to transform the y variable for both models. Multiplicative models look at the log transformation. For logit models we have a transformed y variable but we have to set a theoretical maximum to bound the variables. We assumed the theoretical maximum is 10% more than the historical maximum
data_1 <- data_1 %>%
  group_by(tran_wk) %>%
  mutate(sales = sum(tran_prod_sale_qty), logsales = log(sales), theo_max = max(sales)*1.1, transformedsales = log(sales)/(theo_max-sales))

data_2 <- data_2 %>%
  group_by(tran_wk) %>%
  mutate(sales = sum(tran_prod_sale_qty), logsales = log(sales), theo_max = max(sales)*1.1, transformedsales = log(sales)/(theo_max-sales))

data_3 <- data_3 %>%
  group_by(tran_wk) %>%
  mutate(sales = sum(tran_prod_sale_qty), logsales = log(sales), theo_max = max(sales)*1.1, transformedsales = log(sales)/(theo_max-sales))

##Getting a base price for each product 
data_1$baseprice <- 0.69
data_2$baseprice <- 3.99
data_3$baseprice <- 14.99

##Clean up the data a little before modeling - selecting only necessary columns 
data_1 <- subset(data_1, select = -c(cust_id, store_id, prod_id, tran_prod_sale_amt, tran_prod_sale_qty, tran_prod_discount_amt, tran_prod_offer_cts, tran_prod_paid_amt, prod_unit_price))
data_2 <- subset(data_2, select = -c(cust_id, store_id, prod_id, tran_prod_sale_amt, tran_prod_sale_qty, tran_prod_discount_amt, tran_prod_offer_cts, tran_prod_paid_amt, prod_unit_price))
data_3 <- subset(data_3, select = -c(cust_id, store_id, prod_id, tran_prod_sale_amt, tran_prod_sale_qty, tran_prod_discount_amt, tran_prod_offer_cts, tran_prod_paid_amt, prod_unit_price))

data_1[is.na(data_1)] = 0
data_2[is.na(data_2)] = 0
data_3[is.na(data_3)] = 0

##Changing the column names 
colnames(data_1)[colnames(data_1) == 'Paid Search'] <- 'paid_search'
colnames(data_1)[colnames(data_1) == 'Web Display'] <- 'web_display'
colnames(data_2)[colnames(data_2) == 'Paid Search'] <- 'paid_search'
colnames(data_2)[colnames(data_2) == 'Web Display'] <- 'web_display'
colnames(data_2)[colnames(data_2) == 'Store Display'] <- 'store_display'
colnames(data_3)[colnames(data_3) == 'Paid Search'] <- 'paid_search'
colnames(data_3)[colnames(data_3) == 'Web Display'] <- 'web_display'
colnames(data_3)[colnames(data_3) == 'Store Display'] <- 'store_display'

##Running test model with some transformations to independent variables
data_1$reachRadiosqrt <- sqrt(data_1$reachRadio)
data_2$reachTVsqrt <- sqrt(data_2$reachTV)
data_3$reachTVsqrt <- sqrt(data_3$reachTV)

prod1_logit <- lm(logsales ~ weekly_price + weekly_discount + seas_index + holiday_ALLSAINTS + holiday_ASSUMPTION +  holiday_CARNIVAL + holiday_EASTER + holiday_IMMACULATE + holiday_LIBERTY + holiday_PrEASTER + holiday_PrLIBERTY + holiday_XMAS + holiday_PrXMAS + reachTV + reachRadiosqrt + Flyer + Email + paid_search + web_display, data= data_1)
summary(prod1_logit)

#Here we can see that some of the coefficients are negative, which is contradictory to what we learned in class about Marketing Vehicles - that marketing vehicles should have non-negative effects on sales. We're going to see if there is multicolinearity and if there are any outliers that may be causing this.  

# FIXING NEGATIVE COEFFICIENTS ------------------------------------------------
##Visualizing the correlation matrix to see which causals impact sales
##To get the correlation values for each holiday
data1_mc <- data_1[, -c(1,2,3,4, 19, 21, 27, 29, 31 ,32, 33, 34 ,35)]
data1_mc <- sapply(data1_mc, as.numeric )
t <- cor(data1_mc)
corrplot(t, method="color")
t <- t[4:32, 4:32]

data2_mc <- data_2[, -c(1,2,3,4, 19, 21, 27, 28, 29, 30, 31 ,32)]
data2_mc <- sapply(data2_mc, as.numeric )
t2 <- cor(data2_mc)
corrplot(t2, method="color")

data3_mc <- sapply(data_3, as.numeric )
t3 <- cor(data3_mc)
corrplot(t3, method="color")

# FINAL MODELS ----------------------------------------------------------------
prod1_logit <- lm(transformedsales ~ weekly_price + weekly_discount + seas_index + holiday_ALLSAINTS + holiday_ASSUMPTION + holiday_CARNIVAL + holiday_IMMACULATE + holiday_PrLIBERTY + holiday_PORTUGAL + holiday_POPEVISIT + reachTV + reachRadio + Flyer + Email + paid_search + web_display, data= data_1)
prod1_mult <- lm(logsales ~ weekly_price + weekly_discount + seas_index + holiday_ALLSAINTS + holiday_ASSUMPTION +  holiday_CARNIVAL + holiday_EASTER + holiday_IMMACULATE + holiday_LIBERTY + holiday_PrEASTER + holiday_PrLIBERTY + holiday_XMAS + holiday_PrXMAS + reachTV + reachRadiosqrt + Flyer + Email + paid_search + web_display, data= data_1)

prod2_logit <- lm(transformedsales ~ weekly_price + weekly_discount + seas_index + holiday_ALLSAINTS + holiday_ASSUMPTION + holiday_IMMACULATE + holiday_PrLIBERTY + holiday_PORTUGAL + holiday_POPEVISIT + holiday_XMAS + reachTV + reachRadio + Flyer + Email + paid_search + web_display, data= data_2)
prod2_mult <- lm(logsales ~ weekly_price + weekly_discount + seas_index + holiday_ASSUMPTION + holiday_EASTER + holiday_PrEASTER + holiday_PrLIBERTY + reachTVsqrt + reachRadio + Flyer + Email + paid_search + web_display + store_display, data= data_2)

prod3_logit <- lm(transformedsales ~ weekly_price + weekly_discount + seas_index + holiday_ALLSAINTS + holiday_ASSUMPTION + holiday_CORPUS + holiday_EASTER + holiday_IMMACULATE + holiday_LIBERTY+ holiday_PrLIBERTY + holiday_PORTUGAL + holiday_POPEVISIT + holiday_XMAS + holiday_PrXMAS + reachTV + reachRadio + Flyer + Email + paid_search + web_display, data= data_3)
prod3_mult <- lm(logsales ~ weekly_price + weekly_discount + seas_index + holiday_ALLSAINTS + holiday_ASSUMPTION + holiday_CARNIVAL + holiday_EASTER + holiday_IMMACULATE + holiday_PrEASTER + holiday_PrLIBERTY + holiday_PORTUGAL + holiday_PrXMAS + reachTVsqrt + reachRadio + Flyer + Email + paid_search + web_display + store_display, data= data_3)

# MODEL DIAGNOSTICS  ----------------------------------------------------------
##PRODUCT 1
yhatm = exp(fitted(prod1_mult))
y = data_1$sales
p = 16
n = nrow(data_1)
theoretical_max = unique(data_1$theo_max)
yhatl = theoretical_max*exp(fitted(prod1_logit)) / (exp(fitted(prod1_logit)) + 1)

rss1_m <- sum((yhatm - y) ^ 2) ## residual sum of squares
rss1_l <- sum((yhatl - y) ^ 2)
tss <- sum((y - mean(y)) ^ 2) ## total sum of squares
residuals1_m <- as.vector(y-yhatm) ##calculating residuals 
residuals1_l <- as.vector(y-yhatl)
RMSE1_logit <- RMSE(y_pred=yhatl, y_true=y) ##RMSE
RMSE1_mult <- RMSE(y_pred=yhatm, y_true=y)
MAPE1_logit <- MAPE(y_pred=yhatl, y_true=y) ##MAPE
MAPE1_mult <- MAPE(y_pred=yhatm, y_true=y)
R21_logit <- 1 - rss1_l/tss ##rsquared
R21_mult <- 1 - rss1_m/tss
ftsat1_logit <- ((tss-rss1_l)/p ) / (rss1_l/(n-p-1)) ##F-stat
ftsat1_mult <- ((tss-rss1_m)/p ) / (rss1_m/(n-p-1))
dw1_logit <- durbinWatsonTest(residuals1_l) ##Durbin-Watson stat
dw1_mult <- durbinWatsonTest(residuals1_m)

## PRODUCT 2
#data_2 <- data_2 %>%
  #mutate(yhatm = exp(fitted(prod2_mult)), y = sales, p = predictors, n = nrow(data_2), theoretical_max = unique(theo_max), yhatl = theoretical_max*exp(fitted(prod2_logit)) / (exp(fitted(prod2_logit)) + 1)) 

yhatm = exp(fitted(prod2_mult))
y = data_2$sales
p = 18
n = nrow(data_2)
theoretical_max = unique(data_2$theo_max)
yhatl = theoretical_max*exp(fitted(prod2_logit)) / (exp(fitted(prod2_logit)) + 1)

rss2_m <- sum((yhatm - y) ^ 2) ## residual sum of squares
rss2_l <- sum((yhatl - y) ^ 2)
tss <- sum((y - mean(y)) ^ 2) ## total sum of squares
residuals2_m <- as.vector(y-yhatm) ##calculating residuals 
residuals2_l <- as.vector(y-yhatl)
RMSE2_logit <- RMSE(y_pred=yhatl, y_true=y) ##RMSE
RMSE2_mult <- RMSE(y_pred=yhatm, y_true=y)
MAPE2_logit <- MAPE(y_pred=yhatl, y_true=y) #MAPE
MAPE2_mult <- MAPE(y_pred=yhatm, y_true=y)
R22_logit <- 1 - rss2_l/tss # rsquared
R22_mult <- 1 - rss2_m/tss
ftsat2_logit <- ((tss-rss2_l)/p ) / (rss2_l/(n-p-1)) # F-stat
ftsat2_mult <- ((tss-rss2_m)/p ) / (rss2_m/(n-p-1))
dw2_logit <- durbinWatsonTest(residuals2_l) ##Durbin-Watson stat
dw2_mult <- durbinWatsonTest(residuals2_m)

## PRODUCT 3
#data_3 <- data_3 %>%
  #mutate(yhatm = exp(fitted(prod3_mult)), y = sales, p = predictors, n = nrow(data_3), theoretical_max = unique(theo_max), yhatl = theoretical_max*exp(fitted(prod3_logit)) / (exp(fitted(prod3_logit)) + 1))  

yhatm = exp(fitted(prod3_mult))
y = data_3$sales
p = 18
n = nrow(data_3)
theoretical_max = unique(data_3$theo_max)
yhatl = theoretical_max*exp(fitted(prod3_logit)) / (exp(fitted(prod3_logit)) + 1)

rss3_m <- sum((yhatm - y) ^ 2) ## residual sum of squares
rss3_l <- sum((yhatl - y) ^ 2)
tss <- sum((y - mean(y)) ^ 2) ## total sum of squares
residuals3_m <- as.vector(y-yhatm) ##calculating residuals 
residuals3_l <- as.vector(y-yhatl)
RMSE3_logit <- RMSE(y_pred=yhatl, y_true=y) ##RMSE
RMSE3_mult <- RMSE(y_pred=yhatm, y_true=y)
MAPE3_logit <- MAPE(y_pred=yhatl, y_true=y) #MAPE
MAPE3_mult <- MAPE(y_pred=yhatm, y_true=y)
R23_logit <- 1 - rss3_l/tss # rsquared
R23_mult <- 1 - rss3_m/tss
ftsat3_logit <- ((tss-rss3_l)/p ) / (rss3_l/(n-p-1)) # F-stat
ftsat3_mult <- ((tss-rss3_m)/p ) / (rss3_m/(n-p-1))
dw3_logit <- durbinWatsonTest(residuals3_l) ##Durbin-Watson stat
dw3_mult <- durbinWatsonTest(residuals3_m)

# IF WE WANT TO CALCULATE VIF HERE IS THE CODE 
##Calculate the VIF for each predictor variable in the model
#vif(prod1_mult)
#create vector of VIF values
##vif_values <- vif(prod1_logit)

# MODEL DECOMPOSITION --------------------------------------------------------
# PRODUCT 1
data_1$pred <- predict(prod1_logit, newdata = data_1)
data_1 <- data_1 %>%
  mutate(pred = theo_max*exp(pred)/(exp(pred)+1))

##Getting the base price 
data_1 <- data.table(data_1)
base <- copy(data_1)
base <- base [, weekly_price := baseprice] 
base[, c('weekly_discount', 'Email', 'Flyer', 'paid_search', 'web_display', 'store_display', 'reachTV', 'reachRadio')] <- 0
data_1$base <- predict(prod1_mult, newdata = base)
data_1 <- data_1 %>%
  mutate(base = theo_max * exp(base) / (exp(base) + 1))

##Price
price_dt <- copy(data_1)
price_dt <- price_dt[, weekly_price := baseprice]
data_1$price_dt <- predict(prod1_mult, newdata = price_dt)
data_1 <- data_1 %>%
  mutate(baseprice_dt = theo_max * exp(price_dt) / (exp(price_dt) + 1), price_dt = pred-baseprice_dt)
data_1$baseprice_dt <- NULL

#Promotion
promotion_dt <- copy(data_1)
promotion_dt <- promotion_dt %>%
  mutate(weekly_discount = 0)
data_1$promo_dt <- predict(prod1_mult, newdata = promotion_dt)
data_1 <- data_1 %>%
  mutate(promo_dt = theo_max * exp(promo_dt) / (exp(promo_dt) + 1),
         promo_dt = pred-promo_dt)

##Media (flyers, email, web search, paid search, TV, radio) 
for (media in c('Flyer', 'Email', 'web_display', 'store_display', 'paid_search', 'reachTV', 'reachRadio')) {
  media_dt <- copy(data_1)
  media_dt[, c(media) := 0]
  col_name = paste('due_to', media, sep = '_')
  data_1[, c(col_name) := predict(prod1_mult, newdata = media_dt)]
  data_1[, c(col_name)] <- data_1$theo_max * exp(data_1[, .SD, .SDcols = c(col_name)]) / (exp(data_1[, .SD, .SDcols = c(col_name)]) + 1)
  data_1[, c(col_name)] <- data_1$pred - data_1[, .SD, .SDcols = c(col_name)]
}


for (col in c('base', 'price_dt', 'promo_dt', 'due_to_Flyer', 'due_to_Email', 'due_to_web_display', 'due_to_paid_search', 'due_to_reachTV', 'due_to_reachRadio')) {
  data_1[, c(col)] <- data_1[, .SD, .SDcols = c(col)] / data_1$pred * data_1$sales
}

for (col in c('theo_max', 'logsales','transformedsales', 'pred')) {
  data_1[, c(col)] <- NULL
}

fwrite(data_1, "data_1.csv")

# PRODUCT 2
data_2$pred <- predict(prod2_mult, newdata = data_2)
data_2 <- data_2 %>%
  mutate(pred = theo_max*exp(pred)/(exp(pred)+1))

##Getting the base price 
data_2 <- data.table(data_2)
base2 <- copy(data_2)
base2 <- base2[, weekly_price := baseprice] 
base2[, c('weekly_discount', 'Email', 'Flyer', 'paid_search', 'web_display', 'store_display', 'reachTV', 'reachRadio')] <- 0
data_2$base <- predict(prod2_mult, newdata = base2)
data_2 <- data_2 %>%
  mutate(base = theo_max * exp(base) / (exp(base) + 1))

##Price
price_dt <- copy(data_2)
price_dt <- price_dt[, weekly_price := baseprice]
data_2$price_dt <- predict(prod2_mult, newdata = price_dt)
data_2 <- data_2 %>%
  mutate(baseprice_dt = theo_max * exp(price_dt) / (exp(price_dt) + 1), price_dt = pred-baseprice_dt)
data_2$baseprice_dt <- NULL

#Promotion
promotion_dt <- copy(data_2)
promotion_dt <- promotion_dt %>%
  mutate(weekly_discount = 0)
data_2$promo_dt <- predict(prod2_mult, newdata = promotion_dt)
data_2 <- data_2 %>%
  mutate(promo_dt = theo_max * exp(promo_dt) / (exp(promo_dt) + 1),
         promo_dt = pred-promo_dt)

##Media (flyers, email, web search, paid search, TV, radio) 
for (media in c('Flyer', 'Email', 'web_display', 'store_display', 'paid_search', 'reachTV', 'reachRadio')) {
  media_dt <- copy(data_2)
  media_dt[, c(media) := 0]
  col_name = paste('due_to', media, sep = '_')
  data_2[, c(col_name) := predict(prod2_mult, newdata = media_dt)]
  data_2[, c(col_name)] <- data_2$theo_max * exp(data_2[, .SD, .SDcols = c(col_name)]) / (exp(data_2[, .SD, .SDcols = c(col_name)]) + 1)
  data_2[, c(col_name)] <- data_2$pred - data_2[, .SD, .SDcols = c(col_name)]
}

for (col in c('base', 'price_dt', 'promo_dt', 'due_to_Flyer', 'due_to_Email', 'due_to_web_display', 'due_to_paid_search', 'due_to_reachTV', 'due_to_reachRadio')) {
  data_2[, c(col)] <- data_2[, .SD, .SDcols = c(col)] / data_2$pred * data_2$sales
}

for (col in c('theo_max', 'logsales','transformedsales', 'pred')) {
  data_2[, c(col)] <- NULL
}

fwrite(data_2, "data_2.csv")

# PRODUCT 3
data_3$pred <- predict(prod3_mult, newdata = data_3)
data_3 <- data_3 %>%
  mutate(pred = theo_max*exp(pred)/(exp(pred)+1))

##Getting the base price 
data_3 <- data.table(data_3)
base <- copy(data_3)
predicbase <- base %>%
  mutate(weekly_price = baseprice)
base[, c('weekly_discount', 'Email', 'Flyer', 'paid_search', 'web_display', 'store_display', 'reachTV', 'reachRadio')] <- 0
data_3$base <- predict(prod3_mult, newdata = base)
data_3 <- data_3 %>%
  mutate(base = theo_max * exp(base) / (exp(base) + 1))

##Price
price_dt <- copy(data_3)
price_dt <- price_dt[, weekly_price := baseprice]
data_3$price_dt <- predict(prod3_mult, newdata = price_dt)
data_3 <- data_3 %>%
  mutate(baseprice_dt = theo_max * exp(price_dt) / (exp(price_dt) + 1), price_dt = pred-baseprice_dt)
data_3$baseprice_dt <- NULL

#Promotion
promotion_dt <- copy(data_3)
promotion_dt <- promotion_dt %>%
  mutate(weekly_discount = 0)
data_3$promo_dt <- predict(prod3_mult, newdata = promotion_dt)
data_3 <- data_3 %>%
  mutate(promo_dt = theo_max * exp(promo_dt) / (exp(promo_dt) + 1),
         promo_dt = pred-promo_dt)

##Media (flyers, email, web search, paid search, TV, radio) 
for (media in c('Flyer', 'Email', 'web_display', 'store_display', 'paid_search', 'reachTV', 'reachRadio')) {
  media_dt <- copy(data_3)
  media_dt[, c(media) := 0]
  col_name = paste('due_to', media, sep = '_')
  data_3[, c(col_name) := predict(prod3_mult, newdata = media_dt)]
  data_3[, c(col_name)] <- data_3$theo_max * exp(data_3[, .SD, .SDcols = c(col_name)]) / (exp(data_3[, .SD, .SDcols = c(col_name)]) + 1)
  data_3[, c(col_name)] <- data_3$pred - data_3[, .SD, .SDcols = c(col_name)]
}

for (col in c('base', 'price_dt', 'promo_dt', 'due_to_Flyer', 'due_to_Email', 'due_to_web_display', 'due_to_paid_search', 'due_to_reachTV', 'due_to_reachRadio')) {
  data_3[, c(col)] <- data_3[, .SD, .SDcols = c(col)] / data_3$pred * data_3$sales
}

for (col in c('theo_max', 'logsales','transformedsales', 'pred')) {
  data_3[, c(col)] <- NULL
}

fwrite(data_3, "data_3.csv")

# Visualizing Due-Tos in Tableau
