library(dplyr)
library(ggplot2)
library(gt)


opp_data <- read.csv(file = "C:/Users/ross.williams/Downloads/RStudio/R_Opps_report_.csv",
                     header = TRUE,
                     stringsAsFactors = FALSE)

opp_data_filtered_to_won_opps <- opp_data %>%
  filter(Stage == "6 - Closed Won")

# -------------------------------------------------------------------------------------------------------

# Step 1: Calculate the volume of opportunities and the number of distinct service lines by account
acct_serviceline_distribution <- opp_data_filtered_to_won_opps %>%
  group_by(Acct_Id) %>%
  summarise(
    num_opportunities = n(),  # Count of opportunities for each account
    distinct_service_lines = n_distinct(Record_Type_Name)  # Number of distinct service lines for each account
  )

# Step 2: Scatter plot to show the relationship between volume of opportunities and distinct service lines
ggplot(acct_serviceline_distribution, aes(x = num_opportunities, y = distinct_service_lines)) +
  geom_point() +
  labs(
    title = "Distribution of Opportunity Volume vs. Distinct Service Lines per Account",
    x = "Number of Opportunities",
    y = "Number of Distinct Service Lines"
  ) +
  theme_minimal()


ggplot(acct_serviceline_distribution, aes(x = as.factor(num_opportunities), y = distinct_service_lines)) +
  geom_boxplot() +
  labs(
    title = "Distribution of Distinct Service Lines by Opportunity Volume",
    x = "Number of Opportunities",
    y = "Number of Distinct Service Lines"
  ) +
  theme_minimal()

# ------------------------------------------------------------------------------------------------------------


# Step 1: Summarize data by account
sales_regression <- opp_data %>%
  group_by(Acct_Id) %>%
  summarise(
    num_opportunities = n(),  # Number of opportunities
    distinct_service_lines = n_distinct(Record_Type_Name),  # Distinct service lines
    total_acv = sum(ACV_for_Quotas, na.rm = TRUE)  # Total sales (ACV)
  )

# Step 2: Calculate correlation between number of opportunities and total sales
cor_opps_sales <- cor(sales_regression$num_opportunities, sales_regression$total_acv, use = "complete.obs")

# Step 3: Calculate correlation between number of distinct service lines and total sales
cor_service_lines_sales <- cor(sales_regression$distinct_service_lines, sales_regression$total_acv, use = "complete.obs")

# Step 4: Print the results
cat("Correlation between number of opportunities and total sales:", cor_opps_sales, "\n")
cat("Correlation between distinct service lines and total sales:", cor_service_lines_sales, "\n")

# Step 5: Visualize the relationships

# Scatter plot for number of opportunities vs. total sales
ggplot(sales_regression, aes(x = num_opportunities, y = total_acv)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Relationship Between Number of Opportunities and Total Sales",
    x = "Number of Opportunities",
    y = "Total Sales (ACV)"
  ) +
  theme_minimal()

# Scatter plot for distinct service lines vs. total sales
ggplot(sales_regression, aes(x = distinct_service_lines, y = total_acv)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Relationship Between Distinct Service Lines and Total Sales",
    x = "Number of Distinct Service Lines",
    y = "Total Sales (ACV)"
  ) +
  theme_minimal()



# --------------------------------------------------------------------


combined_data_upsell <- bind_rows(group1_upsell, group2_upsell)

# Remove duplicates based on Opp_ID, Close_Date, and Record_Type_Name columns
final_data_upsell <- combined_data_upsell %>%
  distinct(Opp_ID, Close_Date, Record_Type_Name, .keep_all = TRUE)

# Arrange the final data by Acct_Id and Close_Date in ascending order
sorted_data_upsell <- final_data_upsell %>%
  arrange(Acct_Id, as.Date(Close_Date, format = "%m/%d/%Y"))  # Convert Close_Date to Date type for proper sorting

# Convert Close_Date to Date format if it's not already a Date object
service_line_purchase_cycle <- sorted_data_upsell %>%
  mutate(Close_Date = mdy(Close_Date)) # Assuming Close_Date is in m/d/yyyy format

# Group by Acct_Id and Record_Type_Name, calculate cycles and filter
service_line_purchase_cycle <- service_line_purchase_cycle %>%
  group_by(Acct_Id, Record_Type_Name) %>%
  arrange(Acct_Id, Record_Type_Name, Close_Date) %>%
  mutate(cycle_days = as.numeric(difftime(Close_Date, lag(Close_Date), units = "days"))) %>%
  filter(!is.na(cycle_days) & cycle_days <= 1000) %>%  # Filter out cycles > 1000 days
  summarise(
    avg_cycle = mean(cycle_days),
    median_cycle = median(cycle_days),
    num_opportunities = n() + 1, # Including the first purchase
    most_recent_purchase = max(Close_Date) # Most recent purchase date
  ) %>%
  filter(num_opportunities > 1) # Filter out Acct_Id's with less than 2 purchases

# -----------------------------------------------------------------------------------------------


# Step 1: Summarize data by account (distinct service lines and total ACV)
avg_median_sales <- opp_data_filtered_to_won_opps %>%
  group_by(Acct_Id) %>%
  summarise(
    distinct_service_lines = n_distinct(Record_Type_Name),  # Number of distinct service lines per account
    total_acv = sum(ACV_for_Quotas, na.rm = TRUE)  # Total sales (ACV) for each account
  )

# Step 2: Group by distinct service lines and summarize ACV metrics + count of accounts
service_lines_summary <- avg_median_sales %>%
  group_by(distinct_service_lines) %>%
  summarise(
    sum_acv = sum(total_acv),  # Sum of ACV
    avg_acv = mean(total_acv),  # Average ACV
    median_acv = median(total_acv),  # Median ACV
    num_accounts = n()  # Number of accounts per distinct service line
  )

# Step 3: Use gt to format the table with dollar signs and account count
service_lines_summary %>%
  gt() %>%
  tab_header(
    title = "Summary of ACV by Number of Distinct Service Lines",
    subtitle = "Sum, Average, Median ACV, and Number of Accounts per Service Line"
  ) %>%
  fmt_currency(
    columns = vars(sum_acv, avg_acv, median_acv),  # Format these columns as currency
    currency = "USD"  # Specify the currency format as USD
  ) %>%
  cols_label(
    distinct_service_lines = "Distinct Service Lines",
    sum_acv = "Total ACV (USD)",
    avg_acv = "Average ACV (USD)",
    median_acv = "Median ACV (USD)",
    num_accounts = "# of Accounts"  # Label for the new column
  ) %>%
  tab_options(
    table.font.size = "medium",  # Adjust font size
    heading.title.font.size = "large",
    heading.subtitle.font.size = "medium"
  )


