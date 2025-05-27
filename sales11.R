# ----------------------------
# 📦 1. Install & Load Packages
# ----------------------------
# Install only if not already installed
if (!require("readxl")) install.packages("readxl")
if (!require("arules")) install.packages("arules")
if (!require("janitor")) install.packages("janitor")
if (!require("ggthemes")) install.packages("ggthemes")

# Load libraries
library(tidyverse)
library(readxl)
library(ggplot2)
library(ggthemes)
library(janitor)
library(lubridate)
library(arules)

# ----------------------------
# 📥 2. Load Data
# ----------------------------
sales <- read_excel("C:/Users/user/Downloads/online_retail_II.xlsx")
view(sales)

# ----------------------------
# 🧼 3. Data Cleaning & Structure Check
# ----------------------------
sales <- clean_names(sales)  # janitor: standardize column names

# Basic structure and summary
head(sales)
tail(sales)
str(sales)
summary(sales)

# ----------------------------
# 💰 4. Add Sales Column
# ----------------------------
sales <- sales %>%
  mutate(
    sales_value = quantity * price,
    invoice_date = as.Date(invoice_date)
  )

# ----------------------------
# 🌍 5. Total Sales by Country
# ----------------------------
country_sales <- sales %>%
  group_by(country) %>%
  summarise(total_sales = sum(sales_value, na.rm = TRUE)) %>%
  arrange(desc(total_sales))

# Plot
ggplot(country_sales, aes(x = reorder(country, total_sales), y = total_sales)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Total Sales by Country", x = "Country", y = "Total Sales") +
  scale_y_continuous(labels = scales::dollar_format()) +
  theme_minimal()

# ----------------------------
# 📈 6. Daily Sales Trend
# ----------------------------
daily_sales <- sales %>%
  group_by(invoice_date) %>%
  summarise(daily_total = sum(sales_value, na.rm = TRUE))

# Line Plot
ggplot(daily_sales, aes(x = invoice_date, y = daily_total)) +
  geom_line(color = "darkgreen") +
  labs(title = "Daily Sales Over Time", x = "Date", y = "Total Sales") +
  scale_y_continuous(labels = scales::dollar_format()) +
  theme_minimal()

# ----------------------------
# 📦 7. Top Products by Quantity
# ----------------------------
top_products <- sales %>%
  group_by(description) %>%
  summarise(total_qty = sum(quantity, na.rm = TRUE)) %>%
  arrange(desc(total_qty)) %>%
  slice(1:20)

# Bar Plot
ggplot(top_products, aes(x = reorder(description, total_qty), y = total_qty)) +
  geom_col(fill = "orange") +
  coord_flip() +
  labs(title = "Top 20 Products by Quantity Sold", x = "Product", y = "Total Quantity") +
  theme_minimal()

# ----------------------------
# 🤝 8. Prepare for Market Basket Analysis (Apriori)
# ----------------------------
# Filter for valid transactions
basket_data <- sales %>%
  filter(!is.na(invoice), quantity > 0) %>%
  group_by(invoice) %>%
  summarise(items = paste(unique(description), collapse = ",")) %>%
  pull(items)

# Write to temporary CSV for transaction format
writeLines(basket_data, "basket.csv")

# Read as transactions
trans <- read.transactions("basket.csv", format = "basket", sep = ",")

# ----------------------------
# ⚖️ 9. Run Apriori Algorithm
# ----------------------------
rules <- apriori(trans, parameter = list(supp = 0.01, conf = 0.5))

# Show top rules
inspect(head(sort(rules, by = "lift"), 10))
# ----------------------------
# 🧠 10. Customer Segmentation using RFM Analysis
# ----------------------------

# Step 1: Remove missing Customer IDs
rfm_data <- sales %>%
  filter(!is.na(customer_id), quantity > 0, price > 0)

# Step 2: Calculate Recency, Frequency, Monetary values
# Define the analysis date (e.g., one day after the last invoice date)
analysis_date <- max(rfm_data$invoice_date, na.rm = TRUE) + 1

rfm_table <- rfm_data %>%
  group_by(customer_id) %>%
  summarise(
    recency = as.numeric(analysis_date - max(invoice_date, na.rm = TRUE)),
    frequency = n_distinct(invoice),
    monetary = sum(sales_value, na.rm = TRUE)
  )

# Step 3: RFM Scoring (1 = worst, 5 = best)
rfm_table <- rfm_table %>%
  mutate(
    r_score = ntile(-recency, 5),
    f_score = ntile(frequency, 5),
    m_score = ntile(monetary, 5),
    rfm_segment = paste0(r_score, f_score, m_score),
    rfm_score = r_score + f_score + m_score
  )

# Step 4: Segment Summary
segment_summary <- rfm_table %>%
  group_by(rfm_segment) %>%
  summarise(
    count = n(),
    avg_monetary = mean(monetary),
    avg_frequency = mean(frequency),
    avg_recency = mean(recency)
  ) %>%
  arrange(desc(count))

# View top segments
print(head(segment_summary))

# Optional: Visualize RFM Segments
ggplot(rfm_table, aes(x = r_score, y = f_score, size = monetary, color = m_score)) +
  geom_point(alpha = 0.7) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(
    title = "RFM Segmentation Map",
    x = "Recency Score",
    y = "Frequency Score",
    size = "Monetary Value",
    color = "Monetary Score"
  ) +
  theme_minimal()
