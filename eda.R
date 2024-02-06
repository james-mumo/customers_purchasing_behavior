# R Script Header
# Author: Your Name
# Date: 2024-02-05
# Description: This script performs data analysis on the purchasing behavior of different customer segments based on gender and age in relation to various product categories (Electronics, Books, Home, Clothing).


# Load necessary packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(psych)
library(skimr)


# Read CSV file into a data frame
csv_path <- "dataset_for_project.csv"
customer_df <- read.csv(csv_path)


# Display the structure of the loaded data frame
str(customer_df)

# Total Number of Rows and Columns

# Print total number of rows
cat("\nTotal number of Rows is", nrow(customer_df), "\n")


# Print total number of columns
cat("Total number of Columns is", ncol(customer_df), "\n")


# Select numeric columns
numeric_cols <- sapply(customer_df[, sapply(customer_df, function(x) !is.character(x))], is.numeric)
numeric_cols <- names(numeric_cols)
cat("Numeric columns are: ", paste(numeric_cols, collapse = ", "), "\n")


# Select non-numeric columns
nonnumeric_cols <- setdiff(names(customer_df), numeric_cols)
cat("Non-Numeric columns are: ", paste(nonnumeric_cols, collapse = ", "), "\n")

# Remove specified columns (including 'Returns')
columns_to_remove <- c("Customer.Name", "Payment.Method", "Churn", "Returns")
customer_df <- customer_df[, !(names(customer_df) %in% columns_to_remove)]

# Display the structure of the updated data frame
str(customer_df)


##DATA CLEANING



# Check for duplicates
duplicates <- customer_df[duplicated(customer_df), ]

# Display duplicate rows
if (nrow(duplicates) > 0) {
  cat("Duplicate rows found. Displaying first few:\n")
  print(duplicates)
} else {
  cat("No duplicate rows found.\n")
}

# Check for missing values
missing_values <- sapply(customer_df, function(x) sum(is.na(x) | x == ""))
cols_with_missing <- names(missing_values[missing_values > 0])

if (length(cols_with_missing) > 0) {
  cat("Columns with missing values: ", paste(cols_with_missing, collapse = ", "), "\n")
  print(missing_values[missing_values > 0])
} else {
  cat("No missing values found.\n")
}


# Check for missing values
missing_values <- sapply(customer_df, function(x) sum(is.na(x) | x == ""))

# Display columns with missing values
cols_with_missing <- names(missing_values[missing_values > 0])
if (length(cols_with_missing) > 0) {
  cat("Columns with missing values: ", paste(cols_with_missing, collapse = ", "), "\n")
  print(missing_values[missing_values > 0])
} else {
  cat("No missing values found.\n")
}


# Check missing values in the "Customer.ID" column
missing_count_customer_id <- sum(is.na(customer_df$Customer.ID) | customer_df$Customer.ID == "")

if (missing_count_customer_id > 0) {
  cat("Column 'Customer.ID' has", missing_count_customer_id, "missing values.\n")
} else {
  cat("No missing values found in 'Customer.ID' column.\n")
}


# Check missing values in the "Purchase.Date" column
missing_count_purchase_date <- sum(is.na(customer_df$Purchase.Date) | customer_df$Purchase.Date == "")

if (missing_count_purchase_date > 0) {
  cat("Column 'Purchase.Date' has", missing_count_purchase_date, "missing values.\n")
} else {
  cat("No missing values found in 'Purchase.Date' column.\n")
}

# Check missing values in the "Product.Category" column
missing_count_product_category <- sum(is.na(customer_df$Product.Category) | customer_df$Product.Category == "")

if (missing_count_product_category > 0) {
  cat("Column 'Product.Category' has", missing_count_product_category, "missing values.\n")
} else {
  cat("No missing values found in 'Product.Category' column.\n")
}


# Check missing values in the "Quantity" column
missing_count_quantity <- sum(is.na(customer_df$Quantity) | customer_df$Quantity == "")

if (missing_count_quantity > 0) {
  cat("Column 'Quantity' has", missing_count_quantity, "missing values.\n")
} else {
  cat("No missing values found in 'Quantity' column.\n")
}



# Check missing values in the "Total.Purchase.Amount" column
missing_count_total_purchase <- sum(is.na(customer_df$Total.Purchase.Amount) | customer_df$Total.Purchase.Amount == "")

if (missing_count_total_purchase > 0) {
  cat("Column 'Total.Purchase.Amount' has", missing_count_total_purchase, "missing values.\n")
} else {
  cat("No missing values found in 'Total.Purchase.Amount' column.\n")
}



# Check missing values in the "Customer.Age" column
missing_count_customer_age <- sum(is.na(customer_df$Customer.Age) | customer_df$Customer.Age == "")

if (missing_count_customer_age > 0) {
  cat("Column 'Customer.Age' has", missing_count_customer_age, "missing values.\n")
} else {
  cat("No missing values found in 'Customer.Age' column.\n")
}



# Check missing values in the "Age" column
missing_count_age <- sum(is.na(customer_df$Age) | customer_df$Age == "")

if (missing_count_age > 0) {
  cat("Column 'Age' has", missing_count_age, "missing values.\n")
} else {
  cat("No missing values found in 'Age' column.\n")
}



# Check missing values in the "Gender" column
missing_count_gender <- sum(is.na(customer_df$Gender) | customer_df$Gender == "")

if (missing_count_gender > 0) {
  cat("Column 'Gender' has", missing_count_gender, "missing values.\n")
} else {
  cat("No missing values found in 'Gender' column.\n")
}




# Print the number of rows with null values in the "Returns" column
missing_count_returns <- sum(is.na(customer_df$Returns) | customer_df$Returns == "")
cat("Number of rows with null values in 'Returns' column:", missing_count_returns, "\n")


# Display the structure of the updated data frame
str(customer_df)




## DATA Preparation

## AGE Group Preparations

# Creating Age Groups
customer_df$Age_Group <- cut(customer_df$Customer.Age, breaks = c(17, 25, 35, 45, 60, Inf), labels = c("18-25", "26-35", "36-45", "46-60", "60+"))
str(customer_df)


# Group by Age_Group Count The Number of People in Each Group

unique_people_count_by_age <- customer_df %>%
  group_by(Age_Group) %>%
  summarise(Number_of_Unique_People = n_distinct(Customer.ID))

# Print the result
print(unique_people_count_by_age)



# Summing up the total number of people across all age groups
total_people_count <- sum(unique_people_count_by_age$Number_of_Unique_People)

# Print the total result
print(total_people_count)


# Group by Age_Group and calculate the total purchase amount for each group
total_purchase_by_age <- customer_df %>%
  group_by(Age_Group) %>%
  summarise(Total.Purchase.Amount = sum(Total.Purchase.Amount))

# Print the result
print(total_purchase_by_age)

## Get the Products Categories
unique_product_categories <- unique(customer_df$Product.Category)
# Print the unique values
print(unique_product_categories)


# Filter the data for the first category ("Home") and then group by Age_Group
sales_first_category_by_Home <- customer_df %>%
  filter(Product.Category == "Home") %>%
  group_by(Age_Group) %>%
  summarise(Total.Purchase.Amount_Home = sum(Total.Purchase.Amount))

# Print the result
print(sales_first_category_by_Home)




# Filter the data for the first category ("Electronics") and then group by Age_Group
sales_first_category_by_Electronics <- customer_df %>%
  filter(Product.Category == "Electronics") %>%
  group_by(Age_Group) %>%
  summarise(Total.Purchase.Amount_Electronics = sum(Total.Purchase.Amount))

# Print the result
print(sales_first_category_by_Electronics)






# Filter the data for the first category ("Books") and then group by Age_Group
sales_first_category_by_Books <- customer_df %>%
  filter(Product.Category == "Books") %>%
  group_by(Age_Group) %>%
  summarise(Total.Purchase.Amount_Books = sum(Total.Purchase.Amount))

# Print the result
print(sales_first_category_by_Books)


# Filter the data for the first category ("Clothing") and then group by Age_Group
sales_first_category_by_Clothing <- customer_df %>%
  filter(Product.Category == "Clothing") %>%
  group_by(Age_Group) %>%
  summarise(Total.Purchase.Amount_Clothing = sum(Total.Purchase.Amount))

# Print the result
print(sales_first_category_by_Clothing)



## GENDER PREPARATIONS

# Create Total Purchase Amount based on gender groups
total_purchase_gender <- aggregate(Total.Purchase.Amount ~ Gender, data = customer_df, sum)

# Display the result
total_purchase_gender

# Calculate the total sum of the "Total_Purchase" column
total_purchase_sum <- sum(customer_df$Total.Purchase.Amount)

# Display the result
total_purchase_sum


# Filter data for the "Home" category
home_data <- subset(customer_df, Product.Category == "Home")

# Create Total Purchase Amount based on gender groups in the "Home" category
total_purchase_home_based_on_gender <- aggregate(Total.Purchase.Amount ~ Gender, data = home_data, sum)

# Display the result
total_purchase_home_based_on_gender


# Filter data for the "Electronics" category
electronics_data <- subset(customer_df, Product.Category == "Electronics")

# Create Total Purchase Amount based on gender groups in the "Electronics" category
total_purchase_electronics_gender <- aggregate(Total.Purchase.Amount ~ Gender, data = electronics_data, sum)

# Display the result for Electronics
total_purchase_electronics_gender


# Filter data for the "Clothing" category
clothing_data <- subset(customer_df, Product.Category == "Clothing")

# Create Total Purchase Amount based on gender groups in the "Clothing" category
total_purchase_clothing_gender <- aggregate(Total.Purchase.Amount ~ Gender, data = clothing_data, sum)

# Display the result for Clothing
total_purchase_clothing_gender


# Filter data for the "Books" category
books_data <- subset(customer_df, Product.Category == "Books")

# Create Total Purchase Amount based on gender groups in the "Books" category
total_purchase_books_gender <- aggregate(Total.Purchase.Amount ~ Gender, data = books_data, sum)

# Display the result for Books
total_purchase_books_gender


