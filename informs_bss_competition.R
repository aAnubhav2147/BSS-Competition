cat("\f") # Clear Console
rm(list = ls()) # Clear the working environment



# Import the requisite libraries
options(show.message = FALSE) # Suppress package startup messages
library("readxl") # To read Excel files
library("tidyverse") # for tidy data 
library("car") # for regression
library("broom")
library("lubridate") # for working with date/time-stamp records
library("tseries") # for working with time-series data
library("forecast") # for working with time-series data
library("ggplot2") # for visualization
library("ggfortify") # For Visualization
library("lme4") # For building Hierarchical/Mixed-Effect Models
library("merTools")



url <- ""

bss <- read_excel(url(url),header = TRUE)


# Create a backup of the data frame
bss_bkp <- bss


# Visualize the metadata
str(bss) 
max(bss$salesdate)
min(bss$salesdate)
length(unique(bss$sku))


# Function to change all integer types to double
change_to_dbl <- function(df) {
df %>% mutate_at(vars(which(sapply(df, is.integer))), as.double)
}

bss <- change_to_dbl(bss)

head(bss) # Get a pivot of the first few rows
summary(bss) # Get a basic data summary

# Rename the requisite columns
bss <- bss %>% rename("volume" = "unitsordered")
bss <- bss %>% rename("revenue" = "sales")
bss <- bss %>% rename("stock" = "managed_fba_stock_level")

is_na_count <- bss %>% summarize_all(funs(sum(is.na(.)))) # Check the NA's in each field
is_na_count

bss_rc_missing <- bss %>% summarise(across(where(is.numeric), ~sum(is.na(.x)))) %>% t() %>% as.data.frame()
bss_rc_nomiss <- bss %>% summarise(across(where(is.numeric), ~sum(!is.na(.x)))) %>% t() %>% as.data.frame()

bss$prod_cat <- trimws(gsub("SKU \\d+", "",bss$sku)) # Extract the unique product categories

# Change the requisite columns to factors for the Hierarchical Model
bss <- bss %>% mutate(prod_cat = as.factor(prod_cat),
                                  sku = as.factor(sku))

# Get the number of competitors for each product code

# A vector of all competitor price columns
comp_price_cols_n <- grep("comp_[0-9]+_price", colnames(bss), value = TRUE)
comp_price_cols <- grep("_price",colnames(bss),value = TRUE)
price_cols <- c("price",comp_price_cols_n) # Create a vector of BSS & Competitor price columns

bss$competitor_count <- rowSums(!is.na(bss[, comp_price_cols_n])) # Get the number of competitors for each product
# rowSums() in the above command is counting the number of columns that doesn't have a NA value for a particular row

# Verify to see if the competitor counts match the problem document
comp_count_verification <- bss %>%
  group_by(sku) %>%
  summarise(unique_competitor_count = unique(competitor_count)) %>%
  group_by(unique_competitor_count) %>%
  summarise(num_of_items = n()) %>%
  arrange(unique_competitor_count)
comp_count_verification

# Visualize the competition
bss_temp <- bss %>% dplyr::select(prod_cat, price, all_of(comp_price_cols),competitor_count) %>% group_by(prod_cat)
total_competitors_by_product <- aggregate(bss_temp$competitor_count, by=list(product_name=bss_temp$prod_cat), FUN=max)
colnames(total_competitors_by_product)[2] <- "competitors" # Rename the second element of the list; 'x' by default
ggplot(total_competitors_by_product,aes(x=product_name, y=competitors)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  # geom_smooth(aes(group=1), method="loess", color="red", se=FALSE) +
  geom_text(aes(label = competitors), vjust = -0.5) +
  labs(title = "Max No. of Competitors by Product",
       x = "Product",
       y = "No. of Competitors") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1.2, size = 8)) # Rotate the axis labels to make them more legible


max_prices_by_product <- bss %>%
  dplyr::select(sku, all_of(price_cols)) %>%
  group_by(sku) %>%
  summarise(across(all_of(price_cols),
                   ~ ifelse(all(is.na(.)), NA, max(., na.rm = TRUE)),
                   .names = "max_{.col}"))



min_prices_by_product <- bss %>%
  dplyr::select(sku, all_of(price_cols)) %>%
  group_by(sku) %>%
  summarise(across(all_of(price_cols),
                   ~ ifelse(all(is.na(.)), NA, min(., na.rm = TRUE)),
                   .names = "min_{.col}"))


# Merge all the costs together
bss$cost <- 0.0
bss$cost <- bss$cogs + bss$fba + bss$reffee + bss$adspend


# Do the necessary date operations
bss$salesdate <- as.Date(bss$salesdate,"%Y-%m-%d") # Coerce the 'salesdate' into Date type
bss$quarter <- as.factor(paste0("Q",quarter(bss$salesdate))) # Extract the Quarter from the date
bss$month <- month(bss$salesdate, label = TRUE, abbr = TRUE) # Extract the month from the date
bss$weekday <- factor(wday(bss$salesdate, label = TRUE), ordered = FALSE) # Introduce a weekday instrumental variable
bss <- bss %>% group_by(sku) %>% mutate(days_sold = round(as.numeric(difftime(max(salesdate), min(salesdate), units = "days")))) # Calculate the duration for which a good has been sold
bss <- bss %>% group_by(sku) %>% mutate(first_sell = min(salesdate)) # First instance of the good sold
bss <- bss %>% group_by(sku) %>% mutate(last_sell = max(salesdate)) # Latest instance of the good sold
length(unique(bss$first_sell)) # See the different starting dates
length(unique(bss$last_sell)) # See the different latest sell dates for products 

# Create a histogram to see the spread of the 'price' field
ggplot(bss, aes(x = price)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 10, fill = "blue", color = "black", alpha = 0.7) +
  geom_density(color = "red", linewidth = 1) + # plot a superimposing density curve 
  theme_minimal() +
  labs(title = "Distribution of Price", x = "Price", y = "Density")

# Create a histogram to see the spread of the 'cost' field
ggplot(bss, aes(x = cost)) + 
  # geom_histogram(aes(y = after_stat(density)),binwidth = 10, fill = "blue", color = "black", alpha = 0.7) + 
  geom_histogram(binwidth = 10, fill = "blue", color = "black", alpha = 0.7) + 
  theme_minimal() + 
  labs(title = "Distribution of Cost", x = "Cost", y = "Count")

# Create a boxplot to see the outliers in the 'price' field
ggplot(bss, aes(y = price)) + 
  geom_boxplot(fill = "lightblue", color = "black") + 
  theme_minimal() + 
  labs(title = "Boxplot of Price", y = "Price")

# Attempt to visualize the sales function
plot(bss$price,bss$volume)

ggplot(bss, aes(x = price, y = volume)) +
  geom_line() +
  labs(title = "Sales Function", x = "Price", y = "Volume") +
  theme_minimal()


# Create a boxplot to see the outliers in the 'cost' field
ggplot(bss, aes(y = cost)) + 
  geom_boxplot(fill = "lightblue", color = "black") + 
  theme_minimal() + 
  labs(title = "Boxplot of Cost", y = "Price")

# Let's see the distribution of all the price variables 

# Convert data to long format
bss_long <- bss %>%
  pivot_longer(cols = contains("_price"), # A regex operation
               names_to = "Price_Variable",
               values_to = "Price_Value",
               values_drop_na = TRUE)

# Plot using ggplot2
ggplot(bss_long, aes(x = Price_Value)) +
  geom_histogram(fill = "blue", color = "black", alpha = 0.7, bins = 10) +
  facet_wrap(~Price_Variable, scales = "free_y") + # Customize each plot basis it's respective range
  theme_minimal() +
  labs(title = "Distribution of Price Variables", x = "Price", y = "Count")

# Check the distribution of the 'volume' field
weekday_totals <- aggregate(bss$volume, by=list(weekday=bss$weekday), FUN=sum)
colnames(weekday_totals)[2] <- "volume"
ggplot(weekday_totals,aes(x=weekday, y=volume)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_smooth(aes(group=1), method="loess", color="red", se=FALSE) +
  geom_text(aes(label = volume), vjust = -0.5) +
  labs(title = "Volume sold by Weekday",
       x = "Weekday",
       y = "Volume") +
  theme_minimal()

# Check the distribution of the volume sold by the Product
product_totals <- aggregate(bss$volume, by=list(product_name=bss$prod_cat), FUN=sum)
colnames(product_totals)[2] <- "volume" # Rename the second element of the list; 'x' by default
ggplot(product_totals,aes(x=product_name, y=volume)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  # geom_smooth(aes(group=1), method="loess", color="red", se=FALSE) +
  geom_text(aes(label = volume), vjust = -0.5) +
  labs(title = "Volume sold by Product",
       x = "Product",
       y = "Volume") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1.2, size = 8)) # Rotate the axis labels to make them more legible



bss_pivot <- bss %>% filter(salesdate >= "2022-01-01" & salesdate <= "2022-07-13") %>% dplyr::select(sku, price, volume, revenue, prod_cat) %>% distinct()

# product_name <- c("File Folders SKU 47","File Folders SKU 20")
product_name <- c("File Folders SKU 47")
bss_pivot %>% filter(sku %in% product_name) %>%
  ggplot(aes(x = price, y = volume, color = sku)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Price vs Volume for Selected Product",
       x = "Price",
       y = "Volume",
       color = "Product") +
  theme_minimal()

product_name <- c("File Folders SKU 47")
bss_pivot %>% filter(sku %in% product_name) %>%
ggplot(aes(x = price, y = revenue, color = sku)) +
geom_point() +
geom_smooth(method = "lm", se = TRUE) +
labs(title = "Price vs Revenue for Selected Product",
x = "Price",
y = "Revenue",
color = "Product") +
theme_minimal()

# product_category <- c("File Folders", "Classification Folders")
product_category <- c("File Folders")
bss_pivot %>% filter(prod_cat %in% product_category) %>%
  ggplot(aes(x = price, y = volume, color = prod_cat)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Price vs Volume for Folders",
       x = "Price",
       y = "Volume",
       color = "Product") +
  theme_minimal()


# Filter for data in 2022
recent_profit <- bss %>%
  filter(year(salesdate) == 2022) %>%
  # Group by quarter and summarize the profit
  group_by(Qtr = quarter) %>%
  summarise(Total_Profit = sum(profit, na.rm = TRUE)) %>%
  ungroup()
# Convert the data to a time series object
profit_ts <- ts(recent_profit$Total_Profit, start = c(2022,1), frequency = 4)
# Plot
autoplot(profit_ts) + labs(y ="Profit Earned", title = "Quarterly Profits for 2022")


recent_sales <- bss %>%
  filter(year(salesdate) == 2022) %>%
  # Group by quarter and summarize the sales
  group_by(Qtr = quarter) %>%
  summarise(Total_Sales = sum(volume, na.rm = TRUE)) %>%
  ungroup()
sales_ts <- ts(recent_sales$Total_Sales, start = c(2022,1), frequency = 4)
autoplot(sales_ts) + labs(y ="Volume Sold", title = "Quarterly Sales for 2022")

monthly_sales <- bss %>%
  filter(year(salesdate) == 2022) %>%
  # Group by quarter and summarize the sales
  group_by(Month = month) %>%
  summarise(Total_Sales_m = sum(volume, na.rm = TRUE)) %>%
  ungroup()
monthly_sales_ts <- ts(monthly_sales$Total_Sales_m, start = c(2022,1), frequency = 12)
autoplot(monthly_sales_ts) + labs(y ="Volume Sold", title = "Monthly Sales for 2022")


# # Scale all the price columns with the competitor & market thresholds
# bss <- bss %>%
#   mutate(
#     across(all_of(price_cols),
#            ~ ifelse(is.infinite(round((. - min_price) / (max_price - min_price),2)),
#                     NA,
#                     round((. - min_price) / (max_price - min_price),2)),
#            .names = "{.col}_scaled_m"),
#     
#     across(all_of(price_cols),
#            ~ ifelse(is.infinite(round((. - comp_data_min_price) / (comp_data_max_price - comp_data_min_price),2)),
#                     NA,
#                     round((. - comp_data_min_price) / (comp_data_max_price - comp_data_min_price),2)),
#            .names = "{.col}_scaled_c")
#   )


glimpse(bss)





seed <- 123
set.seed(seed = seed) # Set seed for reproducibility

# Function to calculate the Mean Absolute Percentage Error

mape <- function(actual,forecast){
  
  valid_rows <- which(actual != 0 & !is.na(actual) & !is.na(forecast))
  actual <- actual[valid_rows]
  forecast <- forecast[valid_rows]
  
  n <- length(actual)
  
  if(n==0){
    
    return (NA)
    
  }
  
  val <- sum(abs((actual - forecast)/actual))/n * 100
  
  return(round(val,2))
  
}

# Function to round the price to $0.05 or $0.09

round_to_constraint <- function(x){
  integer_part <- floor(x)
  decimal_part <- x - integer_part
  
  if(decimal_part < 0.05){
      return(integer_part + 0.05)
  }
  
  else if(decimal_part < 0.09){
    return(integer_part + 0.09)
  }
  
  # else if(decimal_part == 0.05 || decimal_part == 0.09){
  #   return(integer_part)
  # }
  
  # else {
  #   return(integer_part)
  # }
  
  else{
    return(integer_part + 0.05 + 1) # Best way to constraint for the decimal part > 0.09
    # Basically, we're rounding it off to the next dollar for a nickel increment
  }
  
}


bss_train <- bss %>% filter(salesdate <= "2023-05-31") # Create the training set
bss_val <- bss %>% filter(salesdate >= "2023-06-01" & sku == "File Folders SKU 47") # Create the validation set

glimpse(bss_train)

glimpse(bss_val)



######################## Hierarchical Modeling ##########################################


bss_hierachical_price_product_intercept <- lmer(volume ~ 1 + comp_1_price + comp_2_price + cost + I(price - cost) + salesdate + weekday + (1 + cost | sku), data = bss_train, REML = TRUE)
ranef(bss_hierachical_price_product_intercept)
plot(ranef(bss_hierachical_price_product_intercept))
summary(bss_hierachical_price_product_intercept)

predictions <- predict(bss_hierachical_price_product_intercept, newdata = bss_val)
bss_val$pred_vol <- round(predictions,0)




initial_guess <- median(bss_val$price, na.rm = TRUE)
lower_limit <- min(bss_val$price, na.rm = TRUE)
upper_limit <- max(bss_val$price, na.rm = TRUE)


profit_function <- function(price, model, df) {
predicted_volume <- predict(model, newdata = transform(df, price = price))
profit <- price * predicted_volume - df$cost
return(-sum(profit, na.rm = TRUE))
}

result <- optim(
par = initial_guess,
fn = profit_function,
model = bss_hierachical_price_product_intercept,
df = bss_val,
method = "L-BFGS-B",
lower = lower_limit,
upper = upper_limit
)



t <- result$par
t



temp <- bss_val %>% filter(salesdate > "2023-08-29" & salesdate <= "2023-09-09")

# Remove the last two rows 
if (nrow(temp) > 2) {
temp <- temp[-((nrow(temp) - 1):(nrow(temp))), ]
} else {
# Handle the case where you have two or fewer rows
warning("Data frame has two or fewer rows. Cannot remove last two rows.")
}

temp$salesdate <- seq(from = as.Date("2023-09-17"), to = as.Date("2023-09-25"), by = "day")
temp$weekday <- wday(temp$salesdate, label = TRUE) # Introduce a weekday instrumental variable
temp$price <- round_to_constraint(t)
temp$pred_vol <- round(predict(bss_hierachical_price_product_intercept, newdata = temp, re.form = NA),0)
temp$pred_profit <- (temp$price * temp$pred_vol) - temp$cost
p <- mape(temp$profit, temp$pred_profit)
print(paste0("Profit MAPE: ", p, "%"))
















############################## Linear Regression ################################################

# Run a linear regression with volume as the response variable and the scaled prices as the features
features <- grep("_scaled_m", colnames(bss_train), value = TRUE)
formula <- paste("volume ~", paste(features, collapse = "+"))
bss_lm <- lm(as.formula(formula),data = bss_train)
summary(bss_lm)
vif(bss_lm)
coef(bss_lm)
confint(bss_lm)
residuals <- data.frame("Residuals" = bss_lm$residuals)
res_hist <- ggplot(residuals, aes(x=Residuals)) + geom_histogram(color='black', fill='red') + ggtitle('Histogram of Residuals')
res_hist
par(mfrow = c(2,2))
plot(bss_lm)
glance(bss_lm)
glance(bss_lm)
bss_lm_mse <- mean(residuals(bss_lm) ^ 2)
bss_lm_mse
bss_lm_rmse <- sqrt(bss_lm_mse)
bss_lm_rmse
bss_lm_rss <- sum(residuals(bss_lm)^2)
bss_lm_rse <- summary(bss_lm)$sigma
bss_lm_rse
fit <- predict(bss_lm,bss_val)
summary(fit)
fit_ssl <- sum((bss_val$volume - fit)^2)
sprintf("SSL/SSR/SSE: %f", fit_ssl)
fit_mse <- fit_ssl/nrow(bss_val)
sprintf("MSE: %f", fit_mse)
fit_rmse <- sqrt(fit_mse)
sprintf("RMSE: %f", fit_mse)
bss_val$pred_vol <- fit

pred_plot <- bss_val %>% ggplot(aes(volume,pred_vol)) + geom_point(alpha = 0.75) + geom_smooth(method = "loess") + stat_smooth(aes(color = "Predicted Volume")) + xlab("Actual Volume") + ylab("Predicted Volume")
pred_plot




















































































##############################################################################################

# library("zoo") # for interpolation
# library("mice") # for imputation purposes
# library("naniar") # for determining the nature of the missing data

# setwd("C:/Users/anubh/Desktop/Anubhav Shankar/Additional Education/INFORMS/BSS-Competition")
# getwd()

# bss <- read.csv("competition_training_data.csv") # Ingest the data set

# change_to_dbl <- function(df){
#   if(is.integer(df$column)){
#     return(as.double(df$column))
#   } else {
#     return (df$column)
#   }
# }


# bss_refresh <- bss

# x <- bss %>% filter(first_sell == "2022-01-01")
# length(unique(x$sku))
# length(unique(x$prod_cat))
# u <- as.list(unique(x$sku))
# View(u)

# Calculate the volume sold
# bss$vol <- 0.0
# bss$vol <- (bss$sales)/(bss$price) 
# bss <- bss %>% mutate(vol = round(vol,1)) 

# comp_price_cols <- grep("comp_[0-9]+_price", colnames(bss_train), value = TRUE)
# nomiss_comp_prices <- rowSums(!is.na(bss_train[, comp_price_cols]))

# bss$prod_cat <- trimws(bss$prod_cat) # Remove the whitespace

# index <- sample(1:nrow(bss), 0.8*nrow(bss), replace = FALSE) # Create the data subset split

# blank_count <- bss_refresh %>% summarize_all(funs(sum(is.na(.)))) # Check the NA's in each field
# print(blank_count) # Just an added check, the counts here should match with the counts of
#                    # the 'is_na_count' object

# bss_rc__train_missing <- bss_train %>% summarise(across(where(is.numeric), ~sum(is.na(.x)))) %>% t() %>% as.data.frame()
# bss_rc_train_nomiss <- bss_train %>% summarise(across(where(is.numeric), ~sum(!is.na(.x)))) %>% t() %>% as.data.frame()

# n_distinct(bss_refresh$sku) # Check the number of distinct items
# sum(is.na(bss_refresh$comp_1_price))
# sum(is.na(bss_refresh$comp_2_price))
# sum(is.na(bss_refresh$comp_3_price))
# sum(is.na(bss_refresh$comp_4_price))
# sum(is.na(bss_refresh$comp_5_price))
# sum(is.na(bss_refresh$comp_data_max_price))
# sum(is.na(bss_refresh$comp_data_min_price))
# sum(is.na(bss_refresh$min_price))
# sum(is.na(bss_refresh$max_price))

# bss_temp$scaled_price_m <- round((bss_temp$price - bss_temp$min_price)/(bss_temp$max_price - bss_temp$min_price),2)
# bss_temp$scaled_price_c <- round((bss_temp$price - bss_temp$comp_data_min_price)/(bss_temp$comp_data_max_price - bss_temp$comp_data_min_price),2)

# # Identify all the columns that contain price information, including 'price'
# price_columns <- c("price", grep("_price$", colnames(bss), value = TRUE))

# To ascertain the nature of the missing data
# group1 <- bss_refresh[!is.na(bss_refresh$comp_1_price), 'max_price']
# group2 <- bss_refresh[is.na(bss_refresh$comp_1_price), 'max_price']
# # Conduct a T-test
# t_test_result <- t.test(group1, group2)
# # Print the result
# print(t_test_result)
# # Plot histograms
# hist(group1, main="Group 1 (No Missing in comp_1_price)", xlab="max_price")
# hist(group2, main="Group 2 (Missing in comp_1_price)", xlab="max_price")
# # Create a new variable to indicate the group
# bss_refresh$group <- ifelse(is.na(bss_refresh$comp_1_price), "Missing", "No Missing")
# # Create a boxplot
# boxplot(max_price ~ group, data=bss_refresh, main="Comparison of max_price", ylab="max_price")

# Do a basic time-series EDA of the sales over the entire period
# bss_temp <- bss_refresh %>% select(salesdate,sales) # Create a temporary subset
# bss_ts <- ts(bss_temp$sales,frequency = 365) # Capture the yearly seasonality
# # Visualize the inherent seasonality 
# plot(bss_ts)
# plot.ts(bss_ts)
# # Check stationarity
# adf.test(diff(bss_ts)) 
# acf(bss_ts)
# plot(diff(bss_ts)) # Plot the data after rendering it stationary
# # diff(bss_ts)
# fit <- stl(bss_ts,s.window = "period")
# plot(fit)
# additive_fit <- decompose(bss_ts,type = "additive")
# multiplicative_fit <- decompose(bss_ts,type = "multiplicative")
# plot(bss_ts,ylab = "Sales")

# # Summing up unitsordered for each weekday
# weekday_totals <- aggregate(bss_train$volume, by=list(weekday=bss_train$weekday), FUN=sum)
# # Sorting the data in descending order based on the total volume
# weekday_totals <- weekday_totals[order(weekday_totals$x), ]
# # Creating a barplot
# barplot(weekday_totals$x, names.arg = weekday_totals$weekday, main = "Total Units Ordered by Weekday",
# xlab = "Weekday", ylab = "Total Units Ordered", col = "steelblue")
# text(x=1:length(bss_train$weekday), y=bss_train$volume + 2, labels=bss_train$volume, cex=1, pos=3)

# #Quick Check
# bss_temp <- bss %>% select(sku,salesdate,revenue,cost,profit)
# bss_temp$diff <- bss_temp$revenue - bss_temp$cost # Get a new 'profit' column
# #View(bss_temp)
# bss_temp$check <- (bss_temp$profit != bss_temp$diff) # A logical field to check if the difference
# # in aggregated 'cost' field is different from the defined 'profit' field provided
# table(bss_temp$check) # If the pivot only has 'FALSE' entries our logic of combining the costs is not harmful/wrong
# rm(bss_temp) # remove the temporary data frame afterwards

# bss <- bss[,-c(6:9)] # Drop the separate costs
# 
# 
# # As the 'comp_x_price' columns have multiple NA's we'll impute median values to 
# # replace the NA's. Median is better suited for it's lack of volatility and takes into
# # account the inherent skewness in the 'comp_X_price' fields.
# 
# # Create a function to aid the imputation in multiple columns
# median_impute <- function(column) {
# if (is.numeric(column)) {
# return(ifelse(is.na(column), median(column, na.rm = TRUE), column))
# } else {
# return(column)  # return as-is for non-numeric columns
# }
# }
# 
# # A vector of column names that need to be imputed
# cols_to_impute <- c("comp_1_price", "comp_2_price", "comp_3_price", "comp_4_price", "comp_5_price", "comp_data_min_price","comp_data_max_price","managed_fba_stock_level")
# # Use vapply() to execute the created function on the requisite columns
# bss_refresh[cols_to_impute] <- vapply(bss_refresh[cols_to_impute],median_impute, FUN.VALUE = numeric(nrow(bss_refresh)))
# # Check to see the successful implementation
# blank_count <- bss_refresh %>% summarize_all(funs(sum(is.na(.))))
# blank_count

# Imputation using interpolation (FOR EXPERIMENT ATM)

# na_count_train <- bss_train %>% summarize_all(funs(sum(is.na(.)))) # Check the NA's in each field
# na_count_train
# 
# interpolate_impute <- function(col_name){
# if(is.numeric(col_name)){
# return(ifelse(is.na(col_name), na.approx(col_name), col_name))
# }
# else{
# return(col_name)
# }
# }
# summary(bss_pivot$price)
# summary(bss$price)
# bss_temp <- bss_temp %>% group_by(sku) %>% mutate(days_sold = round(as.numeric(difftime(max(salesdate), min(salesdate), units = "days"))))
# 
# View(bss_temp)
# length(unique(bss_temp$days_sold))
# max(bss_temp$days_sold)
# min(bss_temp$days_sold)
# summary(bss_temp$days_sold)
# bss_temp <- bss_temp %>% group_by(sku) %>% mutate(first_sell = min(salesdate))
# bss_temp <- bss_temp %>% group_by(sku) %>% mutate(last_sell = max(salesdate))
# length(unique(bss_temp$first_sell))
# length(unique(bss_temp$last_sell))
# unique(bss_temp$first_sell)
# 
# # Scale all the price columns with the competitor & market thresholds
# bss_temp <- bss_temp %>%
# mutate(
#   across(all_of(price_cols),
#        ~ ifelse(is.infinite(round((. - min_price) / (max_price - min_price),2)),
#                           NA,
#                           round((. - min_price) / (max_price - min_price),2)),
#           .names = "{.col}_scaled_m"),
# 
#   across(all_of(price_cols),
#     ~ ifelse(is.infinite(round((. - comp_data_min_price) / (comp_data_max_price - comp_data_min_price),2)),
#                      NA,
#                      round((. - min_price) / (max_price - min_price),2)),
#         .names = "{.col}_scaled_c")
#   )

# summary(bss$volume)
# summary(bss$stock)
# 
# 
# y <- bss_val %>% filter(volume == 0)
# View(y)
# unique(y$pred_vol)
# 
# bss_train[cols_to_impute] <- vapply(bss_train[cols_to_impute],interpolate_impute,FUN.VALUE = numeric(nrow(bss_train)))
# 
# na_count_train <- bss_train %>% summarize_all(funs(sum(is.na(.)))) # Check the NA's in each field
# na_count_train

# View(bss_train)
# View(bss)
# Extract the minimum value from each price column
# min_prices <- bss %>%
#   select(all_of(price_cols)) %>%
#   summarise(across(everything(), min, na.rm = TRUE))
# # Convert to named vector
# min_prices_vector <- unlist(min_prices)
# min_prices_vector
# trend_temp <- ts(bss_temp$sales,start = c(2022,1),end = c(2023,3),frequency = 12)
# plot(trend_temp,xlab = "Year", ylab = "Sales", ylim = range(bss_temp$sales), bty = "l")
# Extract the maximum value from each price column
# max_prices <- bss %>%
#   select(all_of(price_cols)) %>%
#   summarise(across(everything(), max, na.rm = TRUE))
# # Convert to named vector
# max_prices_vector <- unlist(max_prices)
# max_prices_vector

# # Compute Q1 and Q3
# Q1 <- quantile(bss_train$price, 0.25, na.rm = TRUE)
# Q3 <- quantile(bss_train$price, 0.75, na.rm = TRUE)
# # Filter rows based on the IQR
# bss_train_f <- bss_train %>%
# filter(price >= Q1 & price <= Q3)

# maxp <- bss_train$max_price
# maxp <- max(bss_train$max_price)
# minp <- min(bss_train$min_price)
# d_minmax <- bss_train %>% filter(price>minp & price<maxp)

# bss_temp_f <- bss_temp %>% filter(first_sell != "2022-01-01")
# length(unique(bss_temp_f$sku))
# bss_temp_f <- bss_temp %>% select(sku,days_sold,first_sell,last_sell) %>% filter(first_sell != "2022-01-01")%>% group_by(sku)

# max_prices_f <- bss_f %>% select(prod_cat,all_of(price_cols),competitor_count) %>%
# group_by(prod_cat) %>% summarise(across(everything(), ~ifelse(all(is.na(.)),NA,max(., na.rm = TRUE))))
# min_prices_f <- bss_f %>% select(prod_cat,all_of(price_cols),competitor_count) %>%
# group_by(prod_cat) %>% summarise(across(everything(), ~ifelse(all(is.na(.)),NA,min(., na.rm = TRUE))))

# bss_temp <- bss
# glimpse(bss_temp)

# seed <- 456
# set.seed(seed)

# sample_rows <- sample(1:nrow(bss_temp),5)
# bss_sample <- bss_temp[sample_rows, ]
# glimpse(bss_sample)

# bss_cf <- bss %>% filter(prod_cat == "Classification Folders" & competitor_count >= 3)
# length(unique(bss_cf$sku))
# unique(bss_cf$sku)

# latest_date <- as.Date(max(bss_bkp$salesdate))
# new_dates <- seq(from = latest_date + 1, by = "day", length.out = 7)
# test <- data.frame()
# for(date in new_dates){
# daily_sample <- sample_n(bss_bkp,size = 10, replace = TRUE)
# # daily_sample$sku <- "File Folders SKU 47"
# daily_sample$salesdate <- as.Date(date)
# test <- rbind(test,daily_sample)
# }
# 
# p <- predict(fit,test)
# test$pred_profit <- p

# bss_mixed <- lmer(volume ~ price_scaled_m + comp_1_price_scaled_m + comp_2_price_scaled_m + comp_3_price_scaled_m + comp_4_price_scaled_m + comp_5_price_scaled_m + (1 | prod_cat), data = bss_train, REML = TRUE )
# summary(bss_mixed)
# 
# bss_mixed_1 <- lmer(volume ~ price_scaled_m + comp_1_price_scaled_m + comp_2_price_scaled_m + comp_3_price_scaled_m + comp_4_price_scaled_m + comp_5_price_scaled_m + (1 | sku), data = bss_train, REML = TRUE )
# summary(bss_mixed_1)
# 
# bss_mixed_2 <- lmer(volume ~ salesdate + price_scaled_m + comp_1_price_scaled_m + comp_2_price_scaled_m + comp_3_price_scaled_m + comp_4_price_scaled_m + comp_5_price_scaled_m + (1 | sku), data = bss_train, REML = TRUE )
# summary(bss_mixed_2)
# 
# bss_mixed_3 <- lmer(volume ~ price_scaled_m + comp_1_price_scaled_m + comp_2_price_scaled_m + comp_3_price_scaled_m + comp_4_price_scaled_m + comp_5_price_scaled_m + (1 + salesdate | sku), data = bss_train, REML = TRUE )
# summary(bss_mixed_3)
# 
# bss_mixed_4 <- lmer(volume ~ price + (1 + salesdate | sku), data = bss_train, REML = TRUE )
# summary(bss_mixed_4)
# predict(bss_mixed_4, re.form = NA) %>% head()
# predict(bss_mixed_4) %>% head()
# 
# predictInterval(bss_mixed_4)
# plotREsim(REsim(bss_mixed_4))
# plotREsim(REsim(bss_mixed_3))
# 
# fit_me <- predict(bss_mixed_4,bss_val)
# bss_val$pred_vol <- fit_me
# temp_lm <- lm(price ~ pred_vol, data = temp)
# summary(temp_lm)
# temp$pred_profit <- (temp$pred_vol * temp$price) - temp$cost

# bss_hierachical_price_product <- lmer(volume ~ price + (1 + price | sku), data = bss_train, REML = TRUE )
# summary(bss_hierachical_price_product)
# ranef(bss_hierachical_price_product)

# bss_hierachical_volume <- lmer(price ~ 1 + volume + (1 + volume | sku) + (1 + volume | salesdate), data = bss_train, REML = TRUE)

# bss_hierachical_volume <- lmer(price ~ 1 + volume + (1 + volume | sku) + (1 + sku | salesdate), data = bss_train, REML = TRUE)

# bss_hierachical_volume <- lmer(price ~ 1 + volume + (1 + volume | sku), data = bss_train, REML = TRUE)
# summary(bss_hierachical_volume)
# ranef(bss_hierachical_volume)
# plot(ranef(bss_hierachical_volume))
# 
# pred_price <- predict(bss_hierachical_volume, bss_val)
# bss_val$pred_price <- round(pred_price, 2)
# bss_val$pred_price <- sapply(bss_val$pred_price, round_to_constraint)

# bss_hierachical_date <- lmer(volume ~ price + (1 + salesdate | sku), data = bss_train, REML = TRUE )
# summary(bss_hierachical_date)

# val_lm <- lm(price ~ pred_vol, data = bss_val)
# summary(val_lm)

# temp <- bss_val %>% filter(sku == "File Folders SKU 47")
# #temp$est_profit <- round(((temp$pred_vol * temp$pred_price) - temp$cost), 2)
# 
# fit <- lm(profit ~ pred_price, data = bss_val)
# summary(fit)
# fit_temp <- lm(profit ~ pred_price, data = temp)
# summary(fit_temp)
# x <- predict(fit_temp,temp)
# temp$est_profit <- x
# rmse <- sqrt(mean(residuals(fit_temp) ^ 2))
# rmse

#bss_hierachical_price_product_intercept <- lmer(volume ~ 1 + price + (1 + price | sku), data = bss_train, REML = TRUE)
# bss_hierachical_price_product_intercept <- lmer(volume ~ 1 + price + I(comp_1_price - price) + poly(salesdate,3) + (1 + poly(salesdate,3) + price | sku), data = bss_train, REML = TRUE)
# bss_hierachical_price_product_intercept <- lmer(volume ~ 1 + price + comp_1_price + comp_2_price + poly(salesdate,2) + weekday + (1 + poly(salesdate,2) + price | sku), data = bss_train, REML = TRUE)

# bss_hierachical_price_product_intercept <- lmer(volume ~ 1 + price + comp_1_price + comp_2_price + salesdate + (1 + price | sku), data = bss_train, REML = TRUE)

#bss_hierachical_price_product_intercept <- lmer(volume ~ 1 + price + comp_1_price + comp_2_price + salesdate + weekday + (1 + cost | sku), data = bss_train, REML = TRUE)

# profit_function <- function(price){
# price * predict(model, transform(df, price = price) - df$cost)
# }

# m <- mape(temp$price,temp$pred_price)
# p <- mape(temp$profit, temp$est_profit)
# print(paste0("Price MAPE: ", m, "%"))
# print(paste0("Profit MAPE: ", p, "%"))

# dev.off() # Clear residual plots, if any