# ITA Yuva - Virtual R Data Analyst Internship
# Week 2: Data Visualization and Insight Communication using R
# Dataset: UCI Online Retail II
# Source: https://archive.ics.uci.edu/dataset/502/online+retail+ii

# Packages
packages <- c("readxl","dplyr","ggplot2","lubridate","scales","tidyr")
new <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new)) install.packages(new)
lapply(packages, library, character.only=TRUE)

# Import the cleaned sales file produced in Week 1.
# If using the original workbook instead, run the Week 1 cleaning script first.
sales <- read.csv("online_retail_II_clean_sales.csv", stringsAsFactors = FALSE)

sales$InvoiceDate <- as.POSIXct(sales$InvoiceDate)
sales$Country <- as.factor(sales$Country)
sales$YearMonth <- floor_date(sales$InvoiceDate, "month")
sales$Month <- floor_date(sales$InvoiceDate, "month")
sales$DayOfWeek <- weekdays(sales$InvoiceDate)
sales$Hour <- hour(sales$InvoiceDate)

# 1. Monthly revenue trend
monthly_sales <- sales %>%
  group_by(YearMonth) %>%
  summarise(Revenue = sum(Revenue, na.rm=TRUE),
            Orders = n_distinct(InvoiceNo), .groups="drop")

p_month <- ggplot(monthly_sales, aes(YearMonth, Revenue)) +
  geom_line(linewidth=1) +
  geom_point() +
  scale_y_continuous(labels=scales::comma) +
  labs(title="Monthly Revenue Trend",
       subtitle="Positive-sales transactions",
       x="Month", y="Revenue (£)") +
  theme_minimal()
print(p_month)
ggsave("01_monthly_revenue_trend.png", p_month, width=10, height=6, dpi=300)

# 2. Top countries by revenue
country_sales <- sales %>%
  group_by(Country) %>%
  summarise(Revenue=sum(Revenue, na.rm=TRUE), .groups="drop") %>%
  arrange(desc(Revenue)) %>%
  slice_head(n=10)

p_country <- ggplot(country_sales,
                    aes(x=reorder(Country, Revenue), y=Revenue)) +
  geom_col() + coord_flip() +
  scale_y_continuous(labels=scales::comma) +
  labs(title="Top 10 Countries by Revenue",
       x="Country", y="Revenue (£)") +
  theme_minimal()
print(p_country)
ggsave("02_top_countries_revenue.png", p_country, width=10, height=6, dpi=300)

# 3. Distribution of order-line quantity
p_quantity <- ggplot(sales, aes(Quantity)) +
  geom_histogram(bins=60) +
  coord_cartesian(xlim=c(0, quantile(sales$Quantity, .99, na.rm=TRUE))) +
  labs(title="Distribution of Purchase Quantity",
       subtitle="View limited to the 99th percentile for readability",
       x="Quantity", y="Number of records") +
  theme_minimal()
print(p_quantity)
ggsave("03_quantity_distribution.png", p_quantity, width=10, height=6, dpi=300)

# 4. Unit price distribution
p_price <- ggplot(sales, aes(UnitPrice)) +
  geom_histogram(bins=60) +
  coord_cartesian(xlim=c(0, quantile(sales$UnitPrice, .99, na.rm=TRUE))) +
  labs(title="Distribution of Unit Price",
       subtitle="View limited to the 99th percentile for readability",
       x="Unit price (£)", y="Number of records") +
  theme_minimal()
print(p_price)
ggsave("04_unit_price_distribution.png", p_price, width=10, height=6, dpi=300)

# 5. Quantity vs unit price
scatter_data <- sales %>%
  filter(Quantity <= quantile(Quantity, .99, na.rm=TRUE),
         UnitPrice <= quantile(UnitPrice, .99, na.rm=TRUE))

p_scatter <- ggplot(scatter_data, aes(UnitPrice, Quantity)) +
  geom_point(alpha=.25) +
  labs(title="Quantity and Unit Price Relationship",
       subtitle="Extreme upper-tail observations excluded from display",
       x="Unit price (£)", y="Quantity") +
  theme_minimal()
print(p_scatter)
ggsave("05_quantity_vs_price.png", p_scatter, width=10, height=6, dpi=300)

# 6. Monthly orders
p_orders <- ggplot(monthly_sales, aes(YearMonth, Orders)) +
  geom_line(linewidth=1) + geom_point() +
  scale_y_continuous(labels=scales::comma) +
  labs(title="Monthly Order Activity",
       x="Month", y="Unique invoices") +
  theme_minimal()
print(p_orders)
ggsave("06_monthly_orders.png", p_orders, width=10, height=6, dpi=300)

# 7. Top products by revenue
product_sales <- sales %>%
  group_by(StockCode, Description) %>%
  summarise(Revenue=sum(Revenue, na.rm=TRUE), .groups="drop") %>%
  arrange(desc(Revenue)) %>%
  slice_head(n=10)

p_products <- ggplot(product_sales,
                     aes(x=reorder(Description, Revenue), y=Revenue)) +
  geom_col() + coord_flip() +
  scale_y_continuous(labels=scales::comma) +
  labs(title="Top 10 Products by Revenue",
       x="Product", y="Revenue (£)") +
  theme_minimal()
print(p_products)
ggsave("07_top_products_revenue.png", p_products, width=10, height=7, dpi=300)

# 8. Weekday order pattern
weekday_sales <- sales %>%
  mutate(DayOfWeek=factor(DayOfWeek,
          levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))) %>%
  group_by(DayOfWeek) %>%
  summarise(Revenue=sum(Revenue, na.rm=TRUE),
            Orders=n_distinct(InvoiceNo), .groups="drop")

p_weekday <- ggplot(weekday_sales, aes(DayOfWeek, Revenue)) +
  geom_col() +
  scale_y_continuous(labels=scales::comma) +
  labs(title="Revenue by Day of the Week",
       x="Day", y="Revenue (£)") +
  theme_minimal()
print(p_weekday)
ggsave("08_weekday_revenue.png", p_weekday, width=10, height=6, dpi=300)

# Export summary tables
write.csv(monthly_sales, "monthly_sales_summary.csv", row.names=FALSE)
write.csv(country_sales, "country_sales_summary.csv", row.names=FALSE)
write.csv(product_sales, "product_sales_summary.csv", row.names=FALSE)
write.csv(weekday_sales, "weekday_sales_summary.csv", row.names=FALSE)
