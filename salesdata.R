# ==============================
# 📦 INSTALL & LOAD LIBRARIES
# ==============================
install.packages("dplyr")
install.packages("ggplot2")
install.packages("lubridate")

library(dplyr)
library(ggplot2)
library(lubridate)

# ==============================
# 📂 CREATE BIG SALES DATASET
# ==============================
set.seed(123)

n <- 10000   # BIG DATA (10,000 rows)

sales <- data.frame(
  Order_ID = 1:n,
  Date = sample(seq(as.Date("2023-01-01"), as.Date("2024-12-31"), by="day"), n, replace = TRUE),
  Product = sample(c("Laptop", "Mobile", "Tablet", "Headphones", "Smartwatch"), n, replace = TRUE),
  Category = sample(c("Electronics", "Accessories"), n, replace = TRUE),
  Region = sample(c("North", "South", "East", "West"), n, replace = TRUE),
  Quantity = sample(1:10, n, replace = TRUE),
  Price = sample(seq(5000, 80000, by = 1000), n, replace = TRUE)
)

# Add Revenue column
sales <- sales %>%
  mutate(Total_Sales = Quantity * Price)

# View data
head(sales)

# ==============================
# 📊 BASIC ANALYSIS
# ==============================
total_revenue <- sum(sales$Total_Sales)
avg_sales <- mean(sales$Total_Sales)

print(paste("Total Revenue:", total_revenue))
print(paste("Average Sale Value:", avg_sales))

# ==============================
# 🏆 PRODUCT PERFORMANCE
# ==============================
product_sales <- sales %>%
  group_by(Product) %>%
  summarise(Total = sum(Total_Sales), .groups = 'drop')

print(product_sales)

ggplot(product_sales, aes(x = Product, y = Total, fill = Product)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Sales by Product") +
  theme_minimal()

# ==============================
# 🌍 REGION ANALYSIS
# ==============================
region_sales <- sales %>%
  group_by(Region) %>%
  summarise(Total = sum(Total_Sales), .groups = 'drop')

print(region_sales)

ggplot(region_sales, aes(x = Region, y = Total, fill = Region)) +
  geom_bar(stat = "identity") +
  labs(title = "Sales by Region") +
  theme_minimal()

# ==============================
# 📅 MONTHLY SALES TREND
# ==============================
sales$Month <- floor_date(sales$Date, "month")

monthly_sales <- sales %>%
  group_by(Month) %>%
  summarise(Total = sum(Total_Sales), .groups = 'drop')

ggplot(monthly_sales, aes(x = Month, y = Total)) +
  geom_line(size = 1) +
  labs(title = "Monthly Sales Trend") +
  theme_minimal()

# ==============================
# 📊 CATEGORY ANALYSIS
# ==============================
category_sales <- sales %>%
  group_by(Category) %>%
  summarise(Total = sum(Total_Sales), .groups = 'drop')

ggplot(category_sales, aes(x = Category, y = Total, fill = Category)) +
  geom_bar(stat = "identity") +
  labs(title = "Sales by Category") +
  theme_minimal()

# ==============================
# 💾 SAVE DATA
# ==============================
write.csv(sales, "big_sales_data.csv", row.names = FALSE)