library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(lubridate)
library(tinytex)
library(salesforcer)
library(scales)


sf_auth()

# ----------- First step involves separating all Referring Account FIELDS and then combining them ----

# 1. Referring Firm
query0 <- "SELECT id, Referring_CPA__c FROM opportunity"

referring_firm <- sf_query(query0) %>%
  filter(!is.na(Referring_CPA__c) & Referring_CPA__c != "")

referring_firm <- referring_firm %>% 
  select(Id, Account_Id = Referring_CPA__c)


# 2. Referral 1 Acct
query <- "SELECT id, referral_1_account__c FROM opportunity"

referral_1_acct_data <- sf_query(query) %>%
  filter(!is.na(Referral_1_Account__c) & Referral_1_Account__c != "")

referral_1_acct_data <- referral_1_acct_data %>% 
  select(Id, Account_Id = Referral_1_Account__c)


# 3. Referral 2 Account
query2 <- "SELECT id, referral_2_account__c FROM opportunity"

referral_2_acct<- sf_query(query2) %>%
  filter(!is.na(Referral_2_Account__c) & Referral_2_Account__c != "") %>% 
  select(Id, Account_Id = Referral_2_Account__c)

# ----------- Bringing in AccountID and CPA Contact Acct Separately BEFORE binding --------------------
# 4. CPA Contact Acct
query7 <- "SELECT id, CPA_Contact_Account__c FROM opportunity"

cpa_contact_acct <- sf_query(query7) %>% 
  filter(!is.na(CPA_Contact_Account__c) & CPA_Contact_Account__c != "")

cpa_contact_acct <- cpa_contact_acct %>% 
  rename(Id = Opp_Id)

# 5. End Client Account
query8 <- "SELECT id, AccountId FROM Opportunity"

endclient_acct <- sf_query(query8) %>% 
  filter(!is.na(AccountId) & AccountId != "") %>% 
  rename(Account_Id = AccountId)

endclient_acct <- endclient_acct %>%  
  rename(Id = Opp_Id)

# ------ Conecting End CLient and CPA Contact Acct to each other (ec_cpa_opp_summary) ------
# Appending CPA Contact and End Client df's
ec_cpa_appended <- bind_rows(cpa_contact_acct, endclient_acct) %>% 
  distinct(Id, Account_Id, .keep_all = TRUE)

ec_cpa_complete <- left_join(ec_cpa_appended, opp_fields, by = "Id")

ec_cpa_opp_summary <- ec_cpa_complete %>%
  filter(StageName == "6 - Closed Won") %>%
  group_by(Account_Id) %>%
  summarise(
    D_total_acv = dollar(round(sum(ACV_for_Quotas__c, na.rm = TRUE), 0)), # Sum, round, and format as currency
    D_total_tcv = dollar(round(sum(TCV_Gross__c, na.rm = TRUE), 0)),      # Sum, round, and format as currency
    record_type_count = n_distinct(Record_Type_Name__c)                   # Count distinct Record_Type_Name__c
  )

ec_cpa_opp_summary<- ec_cpa_opp_summary %>%
  left_join(account_names, by = "Account_Id")


# Combining End Client Opps with Referral Ranked Opps to get view on total sales

EndClient_Referral_Summary <- full_join(ranked_accounts, ec_cpa_opp_summary, by = "Name", relationship = "many-to-many")

write.csv(EndClient_Referral_Summary, file = "EndClient_Referral_Summary.csv", row.names = FALSE)

# Joining CPA Contact/End Client Summary with Account Ranked Stackings D_Sales & R_Sales, () --------
ranked_acc_with_EC_Summary <- left_join()


# ----------- End Clients per CPA Contact -----------
# 5. Bringing in END CLIENT and CPA CONTACT ACCOUNT
query9 <- "SELECT id, CPA_Contact_Account__c, AccountId FROM opportunity"

endclient_cpacont_group <- sf_query(query9) %>% 
  filter(!is.na(CPA_Contact_Account__c) & CPA_Contact_Account__c != "")

endclient_cpacont_group <- endclient_cpacont_group %>% 
  filter(AccountId != CPA_Contact_Account__c)

cpa_contact_ec_Summary <- left_join(endclient_cpacont_group, opp_fields, by = "Id")
  
endClient_summary_with_opps <- cpa_contact_ec_Summary %>%
  filter(StageName == "6 - Closed Won") %>%
  group_by(CPA_Contact_Account__c) %>%
  summarise(
    distinct_accounts = n_distinct(AccountId),
    total_opportunities = n(),
    distinct_record_types = n_distinct(Record_Type_Name__c)
  )

endClient_summary_with_opps %>% 
  summarise(
    avg_distinct_accounts = mean(distinct_accounts, na.rm = TRUE),
    avg_distinct_record_types = mean(distinct_record_types, na.rm = TRUE)
  ) %>%  print()

endClient_summary_with_opps_names <- endClient_summary_with_opps %>% 
  left_join(
    account_names %>%  select(-Client_Type__c), by = c("CPA_Contact_Account__c" = "Account_Id"))

# --------- Bringing in ACCOUNT NAMES FROM ACCOUNT TABLE -------
query6 <- "SELECT id, Name, Client_Type__c FROM Account"

account_names <- sf_query(query6) %>% 


# --------- Merge Referring Account ---------------------------------

# Appending all 3 data frames that contained referring accounts
appended.frames <- bind_rows(referring_firm,referral_2_acct,referral_1_acct_data) %>% 
  distinct(Id, Account_Id, .keep_all = TRUE )

# --------- Bringing Opportunity Accessory Fields ---------------------------------
query3 <- "SELECT id, ACV_for_Quotas__c, Record_Type_Name__c, StageName, shov_revenue_type__c, CloseDate, TCV_Gross__c, 
OwnerId FROM opportunity"

opp_fields <- sf_query(query3)

# Merge Opp fields with REFERRING appended_frames
merged_referral_accts <- left_join(appended.frames, opp_fields, by = "Id")
print(merged_referral_accts)

# --------- Bringing in ALL Opportunities with All Fields ----------------------------------------
query5 <- "SELECT id, ACV_for_Quotas__c, Record_Type_Name__c, StageName, shov_revenue_type__c, CloseDate, TCV_Gross__c, 
CPA_Contact_Account__c, Referring_CPA__c, referral_1_account__c, Referral_2_Account__c FROM opportunity"

all_opps_and_feilds <- sf_query(query5)

# --- Adding 'Has Referral' Column

all_opps_and_feilds <- all_opps_and_feilds %>%
  mutate(
    category = if_else(
      !is.na(Referring_CPA__c) & Referring_CPA__c != "" |
        !is.na(Referral_2_Account__c) & Referral_2_Account__c != "" |
        !is.na(Referral_1_Account__c) & Referral_1_Account__c != "",
      "Has Referral",
      "No Referral"
    )
  )

# --------- VISUAL - Win % Table for 2018 - 24'------
# Step 1: Calculate win percentage by year and referral status
win_percentage <- all_opps_and_feilds %>%
  filter(year(CloseDate) >= 2018, StageName %in% c("6 - Closed Won", "7 - Closed Lost")) %>%
  mutate(
    year = year(CloseDate),
    category = if_else(
      !is.na(Referring_CPA__c) & Referring_CPA__c != "" |
        !is.na(Referral_2_Account__c) & Referral_2_Account__c != "" |
        !is.na(Referral_1_Account__c) & Referral_1_Account__c != "",
      "Win % with Referral",
      "Win % without Referral"
    )
  ) %>%
  group_by(year, category) %>%
  summarise(
    win_percentage = (sum(StageName == "6 - Closed Won") / n()) * 100,
    .groups = "drop"
  )


# Step 2: Calculate total count of opportunities by year
total_count <- all_opps_and_feilds %>%
  filter(year(CloseDate) >= 2018, StageName %in% c("6 - Closed Won", "7 - Closed Lost")) %>%
  mutate(year = year(CloseDate)) %>%
  group_by(year) %>%
  summarise(
    total_opps = n(),
    .groups = "drop"
  ) %>%
  mutate(
    category = "Total Count of Opps", 
    win_percentage = as.integer(round(total_opps))  # Round and convert to integer for no decimals
  ) %>%
  select(year, category, win_percentage)


# Step 3: Combine win_percentage and total_count summaries
win_percentage_summary <- bind_rows(win_percentage, total_count) %>%
  # Format percentages to one decimal place with % sign; keep total counts as integers
  mutate(across(starts_with("win_percentage"), 
                ~ if_else(category == "Total Count of Opps", 
                          as.character(round(as.numeric(.))), # Total Count with no decimals
                          paste0(round(as.numeric(.), 1), "%") # Win % with one decimal place and % sign
                ))) %>%
  # Pivot to wider format
  pivot_wider(
    names_from = year,
    values_from = win_percentage
  )


# --------- Ranking Accounts by Referral ACV, (ranked_accounts, referral_summary, referral_summary_AcctNames) ------------------
# Step 1: Filter, Group, and Summarize
referral_summary <- merged_referral_accts %>%
  filter(StageName == "6 - Closed Won") %>%
  group_by(Account_Id) %>%
  summarise(
    R_total_acv = sum(ACV_for_Quotas__c, na.rm = TRUE),             # Sum ACV and rename
    R_total_tcv = sum(TCV_Gross__c, na.rm = TRUE),                  # Sum TCV and rename
    referral_opp_count = n(),                                       # Count opportunities and rename
    referral_record_type_count = n_distinct(Record_Type_Name__c)    # Count distinct Record_Type_Name__c and rename
  )

# Merging Acct names with Referral Accounts
referral_summary_AcctNames <- referral_summary %>% 
  left_join(account_names, by = "Account_Id")

# Ranking Accounts by Referral ACV
ranked_accounts <- referral_summary_AcctNames %>%
  arrange(desc(R_total_acv)) %>%
  mutate(
    R_total_tcv = dollar(round(R_total_tcv, 0)),  # Format R_total_tcv as currency with no decimals
    R_total_acv = dollar(round(R_total_acv, 0))   # Format R_total_acv as currency with no decimals
  ) %>%
  select(Name, R_total_acv, R_total_tcv, referral_opp_count, referral_record_type_count)

write.csv(ranked_accounts, "ranked_accounts_by_acv.csv", row.names = FALSE)
# --------- Pivot Wider, Referral Summary Table-----------------------------------------
yearly_referralSummary_table <- merged_referral_accts %>%
  filter(StageName == "6 - Closed Won") %>%
  mutate(
    year = year(CloseDate),
    ACV_for_Quotas__c = replace_na(ACV_for_Quotas__c, 0) # Replace NA with 0 if necessary
  ) %>%
  filter(year >= 2018) %>%
  group_by(year) %>%
  summarise(
    opportunity_count = n_distinct(Id),                                # Distinct opportunity count
    referral_acv = dollar(sum(ACV_for_Quotas__c[!duplicated(Id)]), accuracy = 1), # Unique ACV per opportunity
    distinct_referral_accounts = n_distinct(Account_Id)                # Distinct referral accounts per year
  ) %>%
  mutate(across(c(referral_acv, opportunity_count, distinct_referral_accounts), as.character)) %>%  # Convert all to character
  pivot_longer(
    cols = c(referral_acv, opportunity_count, distinct_referral_accounts), 
    names_to = "metric", 
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = year,
    values_from = value
  )

# --------- referral_percentage_by_year VISUAL ------

# Step 1: Calculate total sales and referral percentage for each year
referral_percentage_by_year <- all_opps_and_feilds %>%
  filter(StageName == "6 - Closed Won", year(CloseDate) >= 2018) %>%
  mutate(
    year = year(CloseDate),
    referral_status = if_else(
      !is.na(Referring_CPA__c) & Referring_CPA__c != "" |
        !is.na(Referral_2_Account__c) & Referral_2_Account__c != "" |
        !is.na(Referral_1_Account__c) & Referral_1_Account__c != "",
      "Referral",
      "Non-Referral"
    )
  ) %>%
  group_by(year) %>%
  summarise(
    total_sales = sum(ACV_for_Quotas__c, na.rm = TRUE),
    referral_sales = sum(ACV_for_Quotas__c[referral_status == "Referral"], na.rm = TRUE),
    referral_percentage = (referral_sales / total_sales) * 100
  ) %>%
  ungroup()

# Print the result to verify data correctness
print(referral_percentage_by_year)

# --------- Plotting Referral Data ------
# Step 2: Create the dual-axis line chart
ggplot(referral_percentage_by_year) +
  geom_line(aes(x = year, y = total_sales), color = "blue", size = 1.2) +
  geom_line(aes(x = year, y = referral_percentage * max(total_sales) / 100), color = "red", size = 1.2) +
  scale_y_continuous(
    name = "Total Sales (in dollars)", 
    sec.axis = sec_axis(~ . * 100 / max(referral_percentage_by_year$total_sales), name = "Referral Sales as % of Total")
  ) +
  scale_x_continuous(breaks = unique(referral_percentage_by_year$year)) +
  labs(
    title = "Total Sales and Percentage of Referral Sales by Year",
    x = "Year"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.y.right = element_text(color = "red"),
    axis.title.y.left = element_text(color = "blue")
  )


