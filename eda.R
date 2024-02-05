# R Script Header
# Author: Your Name
# Date: 2024-02-05
# Description: This script performs data analysis on the purchasing behavior of different customer segments based on gender and age in relation to various product categories (Electronics, Books, Home, Clothing).


# Load necessary packages
library(dplyr)
library(tidyr)
library(ggplot2)


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

# Creating Age Groups
customer_df$Age_Group <- cut(customer_df$Customer.Age, breaks = c(17, 25, 35, 45, 60, Inf), labels = c("18-25", "26-35", "36-45", "46-60", "60+"))
str(customer_df)

