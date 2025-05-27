# Load required packages
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)

# Read the data
superstore <- read_csv("C:/Users/user/Downloads/archive (1)/Sample - Superstore.csv")

# Convert dates and extract components - ADDED MonthNum
superstore <- superstore %>%
  mutate(`Order Date` = mdy(`Order Date`),
         Year = year(`Order Date`),
         Month = month(`Order Date`, label = TRUE),
         MonthNum = month(`Order Date`))  # Added numeric month component

# Aggregate data by month and year
monthly_sales <- superstore %>%
  group_by(Year, Month) %>%
  summarize(Total_Sales = sum(Sales),
            Total_Profit = sum(Profit),
            .groups = 'drop')

# 1. Time Series Plot (Sales Trend) - FIXED geom_line size
ggplot(monthly_sales, aes(x = Month, y = Total_Sales, group = Year, color = factor(Year))) +
  geom_line(size = 1) +  # Changed from linewidth to size
  geom_point(size = 2) +
  labs(title = "Monthly Sales Trend by Year",
       x = "Month",
       y = "Total Sales",
       color = "Year") +
  scale_y_continuous(labels = dollar) +
  theme_minimal()

# 2. Bar Plot (Monthly Sales Comparison) - Correct
ggplot(monthly_sales, aes(x = Month, y = Total_Sales, fill = factor(Year))) +
  geom_col(position = "dodge") +
  labs(title = "Monthly Sales Comparison",
       x = "Month",
       y = "Total Sales",
       fill = "Year") +
  scale_y_continuous(labels = dollar) +
  theme_minimal()

# 3. Profit Margin Analysis - FIXED geom_line size
ggplot(monthly_sales, aes(x = Month, y = Total_Profit, group = Year, color = factor(Year))) +
  geom_line(size = 1) +  # Changed from linewidth to size
  geom_point(size = 2) +
  labs(title = "Monthly Profit Trend by Year",
       x = "Month",
       y = "Total Profit",
       color = "Year") +
  scale_y_continuous(labels = dollar) +
  theme_minimal()

# 4. Boxplot for Monthly Sales Distribution by Year - Correct
ggplot(superstore, aes(x = Month, y = Sales, fill = factor(Year))) +
  geom_boxplot() +
  facet_wrap(~Year, ncol = 2) +
  labs(title = "Monthly Sales Distribution by Year",
       x = "Month",
       y = "Sales Amount",
       fill = "Year") +
  scale_y_continuous(labels = dollar) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 5. Seasonal Decomposition Plot - FIXED MonthNum reference
# Create a time series object
ts_data <- superstore %>%
  group_by(Year, MonthNum) %>%  # Now using existing MonthNum
  summarize(Total_Sales = sum(Sales), .groups = 'drop') %>%
  arrange(Year, MonthNum)

# Plot seasonal patterns
ggplot(ts_data, aes(x = MonthNum, y = Total_Sales, color = factor(Year))) +
  geom_line(size = 1) +  # Changed from linewidth to size
  geom_point(size = 2) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(title = "Seasonal Sales Patterns by Year",
       x = "Month",
       y = "Total Sales",
       color = "Year") +
  scale_y_continuous(labels = dollar) +
  theme_minimal()

# 6. Anomaly Detection (Negative Profits) - Correct
negative_profits <- superstore %>% 
  filter(Profit < 0) %>%
  select(`Order Date`, Sales, Profit, Category, `Product Name`)

# Plot anomalies
ggplot(superstore, aes(x = `Order Date`, y = Profit)) +
  geom_point(aes(color = Profit < 0), size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  scale_color_manual(values = c("black", "red ")) +
  labs(title = "Profit Anomalies (Negative Profits)",
       x = "Order Date",
       y = "Profit",
       color = "Negative Profit") +
  scale_y_continuous(labels = dollar) +
  theme_minimal()

# Print negative profit cases
cat("\nNegative Profit Cases:\n") 
print(negative_profits)

