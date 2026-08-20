# ITA Yuva - Virtual R Data Analyst Internship
# Week 1: Data Cleaning and Preliminary Analysis with R
# Dataset: UCI Online Retail II
# Source: https://archive.ics.uci.edu/dataset/502/online+retail+ii

# -----------------------------
# 1. Packages
# -----------------------------
packages <- c("readxl", "dplyr", "ggplot2", "tidyr", "stringr", "scales")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
lapply(packages, library, character.only = TRUE)

# -----------------------------
# 2. Import
# -----------------------------
# Download online_retail_II.xlsx from the official UCI page.
# The workbook contains two sheets: Year 2009-2010 and Year 2010-2011.

file_path <- "online_retail_II.xlsx"

retail_2009 <- read_excel(file_path, sheet = "Year 2009-2010")
retail_2010 <- read_excel(file_path, sheet = "Year 2010-2011")

retail <- bind_rows(retail_2009, retail_2010)

# Standardise names
names(retail) <- c("InvoiceNo", "StockCode", "Description", "Quantity",
                   "InvoiceDate", "UnitPrice", "CustomerID", "Country")

# -----------------------------
# 3. Initial inspection
# -----------------------------
dim(retail)
str(retail)
summary(retail)

# Missing values
missing_summary <- data.frame(
  Variable = names(retail),
  Missing = sapply(retail, function(x) sum(is.na(x))),
  Missing_Percent = round(sapply(retail, function(x) mean(is.na(x))) * 100, 2)
)
print(missing_summary)

# Duplicate records
sum(duplicated(retail))

# -----------------------------
# 4. Data-type cleaning
# -----------------------------
retail <- retail %>%
  mutate(
    InvoiceNo = as.character(InvoiceNo),
    StockCode = as.character(StockCode),
    Description = as.character(Description),
    InvoiceDate = as.POSIXct(InvoiceDate),
    CustomerID = as.character(CustomerID),
    Country = as.factor(Country)
  )

# Remove leading/trailing spaces in text
retail <- retail %>%
  mutate(
    Description = str_squish(Description),
    Country = factor(str_squish(as.character(Country)))
  )

# -----------------------------
# 5. Missing-value treatment
# -----------------------------
# Description: remove records with missing/blank product descriptions.
# CustomerID: retain missing IDs for transaction-level analysis but
# exclude them from customer-level analysis rather than inventing IDs.

retail <- retail %>%
  filter(!is.na(Description), Description != "")

# -----------------------------
# 6. Duplicate handling
# -----------------------------
retail <- retail %>% distinct()

# -----------------------------
# 7. Cancellation and invalid transaction handling
# -----------------------------
retail <- retail %>%
  mutate(
    IsCancellation = str_starts(InvoiceNo, "C"),
    Revenue = Quantity * UnitPrice
  )

# Keep cancellations identifiable for audit purposes.
# For a positive-sales analysis, create a separate sales table.
sales <- retail %>%
  filter(!IsCancellation, Quantity > 0, UnitPrice > 0)

returns <- retail %>%
  filter(IsCancellation | Quantity <= 0 | UnitPrice <= 0)

# -----------------------------
# 8. Outlier detection
# -----------------------------
# IQR method for Quantity and UnitPrice
iqr_bounds <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  c(lower = q1 - 1.5 * iqr, upper = q3 + 1.5 * iqr)
}

quantity_bounds <- iqr_bounds(sales$Quantity)
price_bounds <- iqr_bounds(sales$UnitPrice)

quantity_outliers <- sales %>%
  filter(Quantity < quantity_bounds["lower"] | Quantity > quantity_bounds["upper"])

price_outliers <- sales %>%
  filter(UnitPrice < price_bounds["lower"] | UnitPrice > price_bounds["upper"])

nrow(quantity_outliers)
nrow(price_outliers)

# Outliers are not automatically deleted: legitimate bulk orders may be
# genuine business activity. They are flagged and reviewed.

# -----------------------------
# 9. Normalization
# -----------------------------
# Min-max normalization for numeric variables used in exploratory work.
minmax <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

sales <- sales %>%
  mutate(
    Quantity_Normalized = minmax(Quantity),
    UnitPrice_Normalized = minmax(UnitPrice),
    Revenue_Normalized = minmax(Revenue)
  )

# -----------------------------
# 10. Categorical encoding
# -----------------------------
# Factor encoding for Country and one-hot encoding example for Country.
sales <- sales %>%
  mutate(Country_Factor = factor(Country))

country_dummies <- model.matrix(~ Country_Factor - 1, data = sales)
country_dummies <- as.data.frame(country_dummies)

# -----------------------------
# 11. Descriptive statistics
# -----------------------------
sales_summary <- sales %>%
  summarise(
    Transactions = n(),
    Unique_Invoices = n_distinct(InvoiceNo),
    Unique_Products = n_distinct(StockCode),
    Unique_Customers = n_distinct(CustomerID, na.rm = TRUE),
    Mean_Quantity = mean(Quantity, na.rm = TRUE),
    Median_Quantity = median(Quantity, na.rm = TRUE),
    Mean_UnitPrice = mean(UnitPrice, na.rm = TRUE),
    Median_UnitPrice = median(UnitPrice, na.rm = TRUE),
    Total_Revenue = sum(Revenue, na.rm = TRUE)
  )
print(sales_summary)

# -----------------------------
# 12. Correlation analysis
# -----------------------------
cor_data <- sales %>%
  select(Quantity, UnitPrice, Revenue) %>%
  cor(use = "complete.obs", method = "pearson")
print(round(cor_data, 3))

# -----------------------------
# 13. Visualizations
# -----------------------------
p1 <- ggplot(sales, aes(x = Quantity)) +
  geom_histogram(bins = 50) +
  labs(title = "Distribution of Quantity",
       x = "Quantity", y = "Frequency") +
  theme_minimal()
print(p1)

p2 <- ggplot(sales, aes(x = UnitPrice)) +
  geom_histogram(bins = 50) +
  coord_cartesian(xlim = c(0, quantile(sales$UnitPrice, .99, na.rm = TRUE))) +
  labs(title = "Distribution of Unit Price (99th Percentile View)",
       x = "Unit Price (£)", y = "Frequency") +
  theme_minimal()
print(p2)

country_sales <- sales %>%
  group_by(Country) %>%
  summarise(Revenue = sum(Revenue, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(Revenue)) %>%
  slice_head(n = 10)

p3 <- ggplot(country_sales,
             aes(x = reorder(Country, Revenue), y = Revenue)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Top 10 Countries by Revenue",
       x = "Country", y = "Revenue (£)") +
  theme_minimal()
print(p3)

# -----------------------------
# 14. Save cleaned data
# -----------------------------
write.csv(sales, "online_retail_II_clean_sales.csv", row.names = FALSE)
write.csv(missing_summary, "missing_value_summary.csv", row.names = FALSE)

# End of Week 1 analysis
