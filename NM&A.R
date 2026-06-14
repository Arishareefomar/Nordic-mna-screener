# ============================================================
# Nordic Industrials M&A Screener
# Step 1: Import and clean Bloomberg export
# ============================================================


# ----------------------------
# 1. Load packages
# ----------------------------

packages <- c(
  "readxl",
  "dplyr",
  "janitor",
  "stringr",
  "readr"
)

installed_packages <- rownames(installed.packages())

for (pkg in packages) {
  if (!(pkg %in% installed_packages)) {
    install.packages(pkg)
  }
}

library(readxl)
library(dplyr)
library(janitor)
library(stringr)
library(readr)


# ----------------------------
# 2. Import Bloomberg file
# ----------------------------

# First try the normal project-folder path.
file_path <- "data/Complete BBR.xlsx"

# If R cannot find it, select the file manually.
if (!file.exists(file_path)) {
  file_path <- file.choose()
}

bb_raw <- read_excel(
  file_path,
  skip = 2,
  na = c("#N/A Review", "#N/A N/A", "N/A", "NA", "")
) %>%
  clean_names()

# Check import
dim(bb_raw)
names(bb_raw)


# ----------------------------
# 3. Remove Bloomberg junk rows
# ----------------------------

# Bloomberg export contains:
# - one summary row at the top
# - possible blank rows
# - possible disclaimer rows at the bottom

bb_clean <- bb_raw %>%
  filter(!is.na(short_name)) %>%
  filter(!str_detect(as.character(ticker_7), "BLOOMBERG|None"))

# Check cleaning
dim(bb_clean)
head(bb_clean, 3)


# ----------------------------
# 4. Rename the key identity columns
# ----------------------------

# Important:
# Bloomberg exported duplicate ticker fields.
# The correct ticker field in our file is ticker_7.

bb_clean2 <- bb_clean %>%
  rename(
    ticker = ticker_7,
    company_name = short_name,
    country = cntry_terrtry_of_dom,
    sector = gics_sector_41
  ) %>%
  mutate(
    country = str_trim(country),
    sector = str_to_lower(str_trim(sector))
  )

# Check country and sector
bb_clean2 %>%
  count(country, sort = TRUE)

bb_clean2 %>%
  count(sector, sort = TRUE)


# ----------------------------
# 5. Filter to Industrials
# ----------------------------

industrials <- bb_clean2 %>%
  filter(sector == "industrials")

# Check final universe
dim(industrials)


# ----------------------------
# 6. Check that the required columns exist
# ----------------------------

required_cols <- c(
  "ticker",
  "company_name",
  "country",
  "sector",
  "market_cap",
  "price_d_1",
  "p_e",
  "revenue_t12m",
  "ev",
  "ev_sales_t12m",
  "ev_ebitda_t12m",
  "ebitda_to_net_sales_q",
  "ni_mrgn_adj_lf",
  "roe_lf",
  "roic_lf",
  "eps_1_yr_gr_q",
  "avg_d_val_traded_3m_m_3",
  "free_float_percent",
  "shares_out_lf",
  "x52wk_high",
  "x52wk_low",
  "curncy",
  "c_ce_lf",
  "mrkt_sec_other_st_invts_lf",
  "net_debt_lf",
  "rev_1_yr_gr_q",
  "ebitda_1yr_growth_lf",
  "sales_gr_t12m"
)

setdiff(required_cols, names(industrials))

# ----------------------------
# 7. Create clean M&A dataset
# ----------------------------

mna_data <- industrials %>%
  transmute(
    ticker = ticker,
    company_name = company_name,
    country = country,
    sector = sector,
    
    market_cap = market_cap,
    price = price_d_1,
    pe_ratio = p_e,
    revenue_t12m = revenue_t12m,
    enterprise_value = ev,
    ev_sales_t12m = ev_sales_t12m,
    ev_ebitda_t12m = ev_ebitda_t12m,
    
    ebitda_margin = ebitda_to_net_sales_q,
    net_income_margin = ni_mrgn_adj_lf,
    roe = roe_lf,
    roic = roic_lf,
    eps_growth = eps_1_yr_gr_q,
    
    avg_daily_value_traded_3m = avg_d_val_traded_3m_m_3,
    free_float_pct = free_float_percent,
    shares_outstanding = shares_out_lf,
    
    high_52w = x52wk_high,
    low_52w = x52wk_low,
    
    currency = curncy,
    cash = c_ce_lf,
    short_term_investments = mrkt_sec_other_st_invts_lf,
    net_debt = net_debt_lf,
    
    revenue_growth = rev_1_yr_gr_q,
    ebitda_growth = ebitda_1yr_growth_lf,
    sales_growth = sales_gr_t12m
  )

glimpse(mna_data)
dim(mna_data)

# ----------------------------
# 8. Add calculated M&A variables
# ----------------------------

mna_data <- mna_data %>%
  mutate(
    ebitda_est = if_else(
      !is.na(ev_ebitda_t12m) & ev_ebitda_t12m != 0,
      enterprise_value / ev_ebitda_t12m,
      NA_real_
    ),
    
    net_debt_to_ebitda = if_else(
      !is.na(ebitda_est) & ebitda_est != 0,
      net_debt / ebitda_est,
      NA_real_
    ),
    
    distance_from_52w_high = if_else(
      !is.na(high_52w) & high_52w != 0,
      price / high_52w - 1,
      NA_real_
    ),
    
    distance_from_52w_low = if_else(
      !is.na(low_52w) & low_52w != 0,
      price / low_52w - 1,
      NA_real_
    )
  )

mna_data %>%
  select(
    ticker,
    company_name,
    country,
    market_cap,
    enterprise_value,
    ev_ebitda_t12m,
    ebitda_est,
    net_debt,
    net_debt_to_ebitda,
    distance_from_52w_high
  ) %>%
  head(10)


# ----------------------------
# 9. Check missing values
# ----------------------------

mna_data %>%
  summarise(
    companies = n(),
    missing_ev_ebitda = sum(is.na(ev_ebitda_t12m)),
    missing_ev_sales = sum(is.na(ev_sales_t12m)),
    missing_ebitda_margin = sum(is.na(ebitda_margin)),
    missing_roic = sum(is.na(roic)),
    missing_net_debt_to_ebitda = sum(is.na(net_debt_to_ebitda)),
    missing_revenue_growth = sum(is.na(revenue_growth))
  )

# ----------------------------
# 10. Build attractiveness score
# ----------------------------

mna_scored <- mna_data %>%
  mutate(
    valuation_score =
      percent_rank(-ev_ebitda_t12m) * 0.60 +
      percent_rank(-ev_sales_t12m) * 0.40,
    
    profitability_score =
      percent_rank(ebitda_margin) * 0.35 +
      percent_rank(net_income_margin) * 0.25 +
      percent_rank(roe) * 0.20 +
      percent_rank(roic) * 0.20,
    
    growth_score =
      percent_rank(revenue_growth) * 0.40 +
      percent_rank(ebitda_growth) * 0.30 +
      percent_rank(eps_growth) * 0.30,
    
    leverage_score =
      percent_rank(-net_debt_to_ebitda),
    
    market_signal_score =
      percent_rank(distance_from_52w_high),
    
    mna_score =
      valuation_score * 0.30 +
      profitability_score * 0.25 +
      growth_score * 0.20 +
      leverage_score * 0.15 +
      market_signal_score * 0.10
  ) %>%
  arrange(desc(mna_score))

#Inspecting the top names properly:
top25_check <- mna_scored %>%
  select(
    ticker,
    company_name,
    country,
    market_cap,
    avg_daily_value_traded_3m,
    ev_ebitda_t12m,
    ev_sales_t12m,
    ebitda_margin,
    roic,
    revenue_growth,
    net_debt_to_ebitda,
    mna_score
  ) %>%
  head(25)

View(top25_check)

# ----------------------------
# 11. Create investable / quality-filtered universe
# ----------------------------

mna_filtered <- mna_data %>%
  filter(!is.na(ev_ebitda_t12m),
         !is.na(ev_sales_t12m),
         !is.na(ebitda_margin),
         !is.na(roic),
         !is.na(revenue_growth),
         !is.na(net_debt_to_ebitda)) %>%
  
  # Remove obvious valuation/data outliers
  filter(ev_ebitda_t12m > 0,
         ev_ebitda_t12m < 25,
         ev_sales_t12m > 0,
         ev_sales_t12m < 10) %>%
  
  # Remove strange profitability cases
  filter(ebitda_margin > 0,
         ebitda_margin < 60) %>%
  
  # Remove companies with very weak recent revenue trend
  filter(revenue_growth > -25) %>%
  
  # Remove highly leveraged companies
  filter(net_debt_to_ebitda < 5)

dim(mna_filtered)

# ----------------------------
# 12. Score Version 2
# ----------------------------

mna_scored_v2 <- mna_filtered %>%
  mutate(
    valuation_score =
      percent_rank(-ev_ebitda_t12m) * 0.60 +
      percent_rank(-ev_sales_t12m) * 0.40,
    
    profitability_score =
      percent_rank(ebitda_margin) * 0.35 +
      percent_rank(net_income_margin) * 0.25 +
      percent_rank(roe) * 0.20 +
      percent_rank(roic) * 0.20,
    
    growth_score =
      percent_rank(revenue_growth) * 0.40 +
      percent_rank(ebitda_growth) * 0.30 +
      percent_rank(eps_growth) * 0.30,
    
    leverage_score =
      percent_rank(-net_debt_to_ebitda),
    
    market_signal_score =
      percent_rank(distance_from_52w_high),
    
    mna_score =
      valuation_score * 0.30 +
      profitability_score * 0.25 +
      growth_score * 0.20 +
      leverage_score * 0.15 +
      market_signal_score * 0.10
  ) %>%
  arrange(desc(mna_score))

#View new top 20
mna_scored_v2 %>%
  select(
    ticker,
    company_name,
    country,
    market_cap,
    ev_ebitda_t12m,
    ev_sales_t12m,
    ebitda_margin,
    roic,
    revenue_growth,
    net_debt_to_ebitda,
    mna_score
  ) %>%
  head(20) 

# ----------------------------
# 13. Add size filter for more realistic M&A targets
# ----------------------------

mna_filtered_size <- mna_filtered %>%
  filter(market_cap >= 1000000000)

dim(mna_filtered_size)

# ----------------------------
# 14. Score Version 3: quality + size filtered
# ----------------------------

mna_scored_v3 <- mna_filtered_size %>%
  mutate(
    valuation_score =
      percent_rank(-ev_ebitda_t12m) * 0.60 +
      percent_rank(-ev_sales_t12m) * 0.40,
    
    profitability_score =
      percent_rank(ebitda_margin) * 0.35 +
      percent_rank(net_income_margin) * 0.25 +
      percent_rank(roe) * 0.20 +
      percent_rank(roic) * 0.20,
    
    growth_score =
      percent_rank(revenue_growth) * 0.40 +
      percent_rank(ebitda_growth) * 0.30 +
      percent_rank(eps_growth) * 0.30,
    
    leverage_score =
      percent_rank(-net_debt_to_ebitda),
    
    market_signal_score =
      percent_rank(distance_from_52w_high),
    
    mna_score =
      valuation_score * 0.30 +
      profitability_score * 0.25 +
      growth_score * 0.20 +
      leverage_score * 0.15 +
      market_signal_score * 0.10
  ) %>%
  arrange(desc(mna_score))

#Top 20 score version 3
mna_scored_v3 %>%
  select(
    ticker,
    company_name,
    country,
    market_cap,
    avg_daily_value_traded_3m,
    ev_ebitda_t12m,
    ev_sales_t12m,
    ebitda_margin,
    roic,
    revenue_growth,
    net_debt_to_ebitda,
    mna_score
  ) %>%
  head(20)

#Inspecting Veidekke: 
mna_scored_v3 %>%
  filter(company_name == "VEIDEKKE ASA") %>%
  select(
    ticker,
    company_name,
    country,
    market_cap,
    currency,
    avg_daily_value_traded_3m,
    ev_ebitda_t12m,
    ev_sales_t12m,
    ebitda_margin,
    net_income_margin,
    roe,
    roic,
    revenue_growth,
    ebitda_growth,
    net_debt_to_ebitda,
    distance_from_52w_high,
    mna_score
  )

# ----------------------------
# 15. Compare Veidekke to the filtered universe
# ----------------------------

veidekke_comparison <- mna_scored_v3 %>%
  summarise(
    veidekke_ev_ebitda = ev_ebitda_t12m[company_name == "VEIDEKKE ASA"],
    median_ev_ebitda = median(ev_ebitda_t12m, na.rm = TRUE),
    
    veidekke_ev_sales = ev_sales_t12m[company_name == "VEIDEKKE ASA"],
    median_ev_sales = median(ev_sales_t12m, na.rm = TRUE),
    
    veidekke_ebitda_margin = ebitda_margin[company_name == "VEIDEKKE ASA"],
    median_ebitda_margin = median(ebitda_margin, na.rm = TRUE),
    
    veidekke_roic = roic[company_name == "VEIDEKKE ASA"],
    median_roic = median(roic, na.rm = TRUE),
    
    veidekke_revenue_growth = revenue_growth[company_name == "VEIDEKKE ASA"],
    median_revenue_growth = median(revenue_growth, na.rm = TRUE),
    
    veidekke_net_debt_to_ebitda = net_debt_to_ebitda[company_name == "VEIDEKKE ASA"],
    median_net_debt_to_ebitda = median(net_debt_to_ebitda, na.rm = TRUE)
  )

veidekke_comparison

# ----------------------------
# 16. Find companies with similar low-margin / construction-like profile
# ----------------------------

construction_like <- mna_scored_v3 %>%
  filter(
    ev_sales_t12m < 1,
    ebitda_margin < 10
  ) %>%
  arrange(ev_ebitda_t12m)

construction_like %>%
  select(
    ticker,
    company_name,
    country,
    market_cap,
    ev_ebitda_t12m,
    ev_sales_t12m,
    ebitda_margin,
    roic,
    revenue_growth,
    net_debt_to_ebitda,
    mna_score
  ) %>%
  head(20)

# ----------------------------
# 17. Create final shortlist table
# ----------------------------

target_shortlist <- mna_scored_v3 %>%
  select(
    ticker,
    company_name,
    country,
    market_cap,
    currency,
    ev_ebitda_t12m,
    ev_sales_t12m,
    ebitda_margin,
    roic,
    revenue_growth,
    net_debt_to_ebitda,
    distance_from_52w_high,
    mna_score
  ) %>%
  head(10)

target_shortlist
write_csv(target_shortlist, "target_shortlist_top10.csv")

# ----------------------------
# 18. Final selected targets
# ----------------------------

final_targets <- mna_scored_v3 %>%
  filter(company_name %in% c(
    "VEIDEKKE ASA",
    "AF GRUPPEN ASA",
    "PER AARSLEFF HOL"
  )) %>%
  select(
    ticker,
    company_name,
    country,
    market_cap,
    currency,
    ev_ebitda_t12m,
    ev_sales_t12m,
    ebitda_margin,
    roic,
    revenue_growth,
    net_debt_to_ebitda,
    distance_from_52w_high,
    mna_score
  ) %>%
  arrange(desc(mna_score))

final_targets
write_csv(final_targets, "final_selected_targets.csv")

final_targets %>%
  print(width = Inf)

# ----------------------------
# 19. Create report-ready final target table
# ----------------------------

final_targets_report <- final_targets %>%
  mutate(
    market_cap_bn = market_cap / 1000000000,
    ev_ebitda_t12m = round(ev_ebitda_t12m, 2),
    ev_sales_t12m = round(ev_sales_t12m, 2),
    ebitda_margin = round(ebitda_margin, 2),
    roic = round(roic, 2),
    revenue_growth = round(revenue_growth, 2),
    net_debt_to_ebitda = round(net_debt_to_ebitda, 2),
    distance_from_52w_high = round(distance_from_52w_high, 2),
    mna_score = round(mna_score, 3)
  ) %>%
  select(
    ticker,
    company_name,
    country,
    currency,
    market_cap_bn,
    ev_ebitda_t12m,
    ev_sales_t12m,
    ebitda_margin,
    roic,
    revenue_growth,
    net_debt_to_ebitda,
    distance_from_52w_high,
    mna_score
  )

final_targets_report
write_csv(final_targets_report, "final_targets_report.csv")

final_targets_report %>%
  print(width = Inf)

#Peer comparison chart
library(ggplot2)
# ----------------------------
# 20. Improved EV/EBITDA chart
# ----------------------------

ev_ebitda_chart <- ggplot(
  final_targets_report,
  aes(
    x = reorder(company_name, ev_ebitda_t12m),
    y = ev_ebitda_t12m
  )
) +
  geom_col() +
  geom_text(
    aes(label = paste0(ev_ebitda_t12m, "x")),
    vjust = -0.4,
    size = 4
  ) +
  labs(
    title = "EV/EBITDA comparison of selected M&A targets",
    subtitle = "Lower multiples may indicate more attractive valuation, subject to business quality and risk",
    x = NULL,
    y = "EV/EBITDA T12M"
  ) +
  theme_minimal()

ev_ebitda_chart

#Saving the chart
ggsave(
  filename = "ev_ebitda_selected_targets.png",
  plot = ev_ebitda_chart,
  width = 9,
  height = 5,
  dpi = 300
)

# ----------------------------
# 21. ROIC comparison chart
# ----------------------------

roic_chart <- ggplot(
  final_targets_report,
  aes(
    x = reorder(company_name, roic),
    y = roic
  )
) +
  geom_col() +
  geom_text(
    aes(label = paste0(roic, "%")),
    vjust = -0.4,
    size = 4
  ) +
  labs(
    title = "ROIC comparison of selected M&A targets",
    subtitle = "Higher ROIC indicates stronger capital efficiency",
    x = NULL,
    y = "ROIC (%)"
  ) +
  theme_minimal()

roic_chart

#Save roic chart
ggsave(
  filename = "roic_selected_targets.png",
  plot = roic_chart,
  width = 9,
  height = 5,
  dpi = 300
)

# ----------------------------
# 22. Revenue growth comparison chart
# ----------------------------

revenue_growth_chart <- ggplot(
  final_targets_report,
  aes(
    x = reorder(company_name, revenue_growth),
    y = revenue_growth
  )
) +
  geom_col() +
  geom_text(
    aes(label = paste0(revenue_growth, "%")),
    vjust = -0.4,
    size = 4
  ) +
  labs(
    title = "Revenue growth comparison of selected M&A targets",
    subtitle = "Higher revenue growth may indicate stronger recent business momentum",
    x = NULL,
    y = "Revenue growth (%)"
  ) +
  theme_minimal()

revenue_growth_chart

ggsave(
  filename = "revenue_growth_selected_targets.png",
  plot = revenue_growth_chart,
  width = 9,
  height = 5,
  dpi = 300
)

# ----------------------------
# 23. Clean final table for report
# ----------------------------

final_targets_table <- final_targets_report %>%
  mutate(
    market_cap = paste0(round(market_cap_bn, 1), "bn ", currency),
    ev_ebitda = paste0(ev_ebitda_t12m, "x"),
    ev_sales = paste0(ev_sales_t12m, "x"),
    ebitda_margin = paste0(ebitda_margin, "%"),
    roic = paste0(roic, "%"),
    revenue_growth = paste0(revenue_growth, "%"),
    net_debt_to_ebitda = paste0(net_debt_to_ebitda, "x"),
    mna_score = round(mna_score, 3)
  ) %>%
  select(
    company_name,
    country,
    market_cap,
    ev_ebitda,
    ev_sales,
    ebitda_margin,
    roic,
    revenue_growth,
    net_debt_to_ebitda,
    mna_score
  )

final_targets_table

#See all columns
final_targets_table %>%
  print(width = Inf)

# ----------------------------
# 24. Save main outputs
# ----------------------------

write_csv(mna_scored_v3, "full_scored_universe.csv")
write_csv(target_shortlist, "target_shortlist_top10.csv")
write_csv(final_targets_report, "final_targets_report.csv")
write_csv(final_targets_table, "final_targets_table.csv")






