library(dplyr)
library(tidyr)
library(tibble)
library(readr)
library(stringr)
library(forcats)
library(purrr)

#PROBLEM 1

#Part A

#Step 1 
sales_raw <- tibble(
  sale_id = 1:8,
  customer_id = c("C1", "C2", "C1", "C3",
                  "C2", "C4", "C5", "C2"),
  item_raw = c(" Pen", "PEN ", "Notebook", " pen ",
               "FOLDER", "Folder ", " ERASER ", "pen"),
  amount_raw = c("$10.00", "$20.00", NA, "$15.00",
                 "$30.00", "$25.00", NA, "$20.00"),
  discount_raw = c("0%", "10%", "5%", "0%",
                   "20%", "10%", "0%", "0%"),
  code_raw = c("ST-101", "ON-102", "ST-103", "ON-104",
               "bad", "ST-106", "ON-107X", "on-108"),
  tags = c("gift;urgent", "online", "gift", "member",
           "online;member", "gift", "member", "gift;member")
)

# Check the original data
print(sales_raw)

#Step 2
sales_clean <- sales_raw %>%
  mutate(
    item_clean = item_raw %>%
      str_trim() %>%
      str_squish() %>%
      str_to_lower()
  )

print(sales_clean)

#Step 3
sales_clean <- sales_clean %>%
  mutate(
    amount = parse_number(amount_raw),
    discount_rate = parse_number(discount_raw) / 100,
    net_amount = amount * (1 - discount_rate)
  )

print(sales_clean)

# Step 4
sales_clean <- sales_clean %>%
  mutate(
    size = case_when(
      is.na(net_amount) ~ "unrecorded",
      net_amount >= 20 ~ "large",
      TRUE ~ "small"
    )
  )

# Display the required columns
sales_clean %>%
  select(sale_id, item_clean, amount,
         discount_rate, net_amount, size)

#Step 5
# item_clean standardizes item names by removing extra spaces
# and converting letters to lowercase.
# parse_number() extracts numerical values from the original
# amount and discount strings.
# discount_rate stores the discount as a proportion.
# net_amount calculates the sale amount after the discount.
# Missing amounts remain NA and are categorized as unrecorded.

#Part B

#Step 1
sales_codes <- sales_clean %>%
  mutate(
    code_clean = str_to_upper(str_trim(code_raw)),
    valid_code = str_detect(
      code_clean,
      "^(ST|ON)-[0-9]{3}$"
    )
  )

print(sales_codes)

#Step 2
invalid_codes <- sales_codes %>%
  filter(!valid_code) %>%
  select(sale_id, code_raw, code_clean)

print(invalid_codes)

#Step 3
valid_code_parts <- sales_codes %>%
  filter(valid_code) %>%
  select(sale_id, code_clean) %>%
  separate_wider_delim(
    code_clean,
    delim = "-",
    names = c("channel", "number")
  )

print(valid_code_parts)

#Step 4
sale_tags <- sales_clean %>%
  select(sale_id, tags) %>%
  separate_longer_delim(
    tags,
    delim = ";"
  )

print(sale_tags)

#Part C

#Step 1
customer_summary <- sales_clean %>%
  summarise(
    sales_n = n(),
    known_n = sum(!is.na(net_amount)),
    large_n = sum(net_amount >= 20, na.rm = TRUE),
    large_share = if (sum(!is.na(net_amount)) == 0) {
      NA_real_
    } else {
      sum(net_amount >= 20, na.rm = TRUE) /
        sum(!is.na(net_amount))
    },
    .by = customer_id
  ) %>%
  arrange(customer_id)

print(customer_summary)

#Step 2
# C1 explanation:
# The numerator is the number of C1 sales with a known net amount
# of at least 20. The denominator is the number of C1 sales with
# a non-missing net amount. C1 has two sales, but one amount is
# missing, so large_share is 0 / 1 = 0.

#Step 3
sales_factors <- sales_clean %>%
  mutate(
    item_factor = fct_infreq(factor(item_clean)),
    item_group = fct_lump_n(
      factor(item_clean),
      n = 2,
      other_level = "Other"
    )
  )

print(sales_factors)

#Step 4
# Levels of item_factor
levels(sales_factors$item_factor)

# Counts for item_factor
sales_factors %>%
  count(item_factor, .drop = FALSE)

# Levels of item_group
levels(sales_factors$item_group)

# Counts for item_group
sales_factors %>%
  count(item_group, .drop = FALSE)

#Step 5
# Explanation:
# fct_infreq() changes the order of the existing factor levels
# without combining categories. fct_lump_n() combines less frequent
# categories into an "Other" category.

#PROBLEM 2

#Part A

#Step 1
reports_raw <- tibble(
  form = c("A", "A", "A", "B", "B", "B"),
  line = c(1, 2, 3, 1, 2, 3),
  customer_id = c("C1", NA, "C2", "C3", NA, "C4"),
  round = c(1, 3, 1, 1, 2, 1),
  primary_amount = c(10, -999, 20, NA, 0, 30),
  backup_amount = c(NA, 15, 25, NA, 99, NA)
)

customers <- tibble(
  customer_id = c("C1", "C2", "C3", "C5"),
  region = c("North", "South", NA, "West")
)

print(reports_raw)
print(customers)

#Step 2
reports_labeled <- reports_raw %>%
  arrange(form, line) %>%
  group_by(form) %>%
  fill(customer_id, .direction = "down") %>%
  ungroup()

print(reports_labeled)

#Step 3
reports_clean <- reports_labeled %>%
  mutate(
    primary_clean = na_if(primary_amount, -999),
    amount = coalesce(primary_clean, backup_amount)
  )

reports_clean %>%
  select(
    form, line, customer_id, round,
    primary_amount, backup_amount, amount
  ) %>%
  print()

# Explanation:
# A recorded primary amount, including zero, is kept because
# coalesce() uses the first non-missing value. The backup amount
# is used only when primary_clean is NA.

#Step 4
# Mark original records before completing the data

reports_marked <- reports_clean %>%
  select(customer_id, round, amount) %>%
  mutate(recorded_row = TRUE)

# Add rounds 1, 2, and 3 for every customer

reports_complete <- reports_marked %>%
  complete(
    customer_id,
    round = 1:3,
    fill = list(recorded_row = FALSE)
  ) %>%
  arrange(customer_id, round)

print(reports_complete)

#Step 5
before_completion <- reports_marked %>%
  summarise(
    original_rows = n(),
    added_rows = 0L,
    missing_amounts = sum(is.na(amount))
  )

print(before_completion)

after_completion <- reports_complete %>%
  summarise(
    original_rows = sum(recorded_row),
    added_rows = sum(!recorded_row),
    missing_amounts = sum(is.na(amount))
  )

print(after_completion)

#Step 6
# Explicit missing amount: an existing row with amount = NA
reports_marked %>%
  filter(is.na(amount))

# Implicit missing rows: combinations added by complete()
reports_complete %>%
  filter(!recorded_row)

#Part B

#Step 1
# Check customer metadata IDs
customers %>%
  summarise(
    missing_ids = sum(is.na(customer_id)),
    duplicate_ids = sum(duplicated(customer_id[!is.na(customer_id)]))
  ) %>%
  print()

# Check repeated customer-round combinations
reports_clean %>%
  count(customer_id, round) %>%
  filter(n > 1) %>%
  print()

#Step 2
customers_marked <- customers %>%
  mutate(customer_found = TRUE)

reports_joined <- reports_clean %>%
  left_join(
    customers_marked,
    by = join_by(customer_id),
    relationship = "many-to-one",
    na_matches = "never"
  ) %>%
  mutate(
    customer_found = coalesce(customer_found, FALSE)
  )

reports_joined %>%
  select(customer_id, round, amount, region, customer_found) %>%
  print()

#Step 3
# IDs found only in reports_clean
reports_only_ids <- reports_clean %>%
  distinct(customer_id) %>%
  anti_join(
    customers %>% distinct(customer_id),
    by = join_by(customer_id),
    na_matches = "never"
  )

print(reports_only_ids)

# IDs found only in customers
customers_only_ids <- customers %>%
  distinct(customer_id) %>%
  anti_join(
    reports_clean %>% distinct(customer_id),
    by = join_by(customer_id),
    na_matches = "never"
  )

print(customers_only_ids)

# Explanation:
# C3 has a matching customer record, so customer_found is TRUE,
# even though its region is NA. C4 has no matching customer record,
# so customer_found is FALSE. A missing region alone cannot tell
# us whether a customer record exists.

#Part C

#Step 1
# Compare reports before and after joining
reports_duplicated <- reports_clean %>%
  left_join(
    customers_bad,
    by = join_by(customer_id),
    relationship = "many-to-many"
  )

# Compare row counts
nrow(reports_clean)
nrow(reports_duplicated)

# Compare total amounts, ignoring missing values
sum(reports_clean$amount, na.rm = TRUE)
sum(reports_duplicated$amount, na.rm = TRUE)

reports_duplicated %>%
  count(customer_id, round) %>%
  filter(n > 1) %>%
  print()

#Step 2
# Explanation:
# With relationship = "many-to-one", the join would report an
# error if a report matched multiple customer records.
# Allowing "many-to-many" permits the join but does not resolve
# conflicting customer information or prevent duplicated reports.

#Step 3
reports_wide <- reports_complete %>%
  select(customer_id, round, amount) %>%
  pivot_wider(
    names_from = round,
    values_from = amount,
    names_prefix = "round_"
  )

print(reports_wide)

#Step 3
reports_wide <- reports_complete %>%
  select(customer_id, round, amount) %>%
  pivot_wider(
    names_from = round,
    values_from = amount,
    names_prefix = "round_"
  )

print(reports_wide)

#Step 4
reports_long <- reports_wide %>%
  pivot_longer(
    cols = starts_with("round_"),
    names_to = "round",
    names_prefix = "round_",
    values_to = "amount",
    values_drop_na = TRUE
  )

print(reports_long)
nrow(reports_long)

# Explanation:
# pivot_longer() with values_drop_na = TRUE removes rows with
# missing amounts. It does not recover unknown amounts because
# NA does not tell us what the actual amount should have been.

#PROBLEM 3

#Part A

#Step 1
#Null hypothesis: the population means are equal.
#Alternative hypothesis: the population means are different.

#Step 2
two_sample_t <- function(x, y, equal_var = FALSE, alpha = 0.05) {
  
  # Sample sizes
  nx <- length(x)
  ny <- length(y)
  
  # Sample means
  mean_x <- mean(x)
  mean_y <- mean(y)
  
  # Sample variances
  s2x <- var(x)
  s2y <- var(y)
  
  # Choose pooled or Welch test
  if (equal_var == TRUE) {
    
    # Pooled two-sample t-test
    sp2 <- ((nx - 1) * s2x + (ny - 1) * s2y) /
      (nx + ny - 2)
    
    SE <- sqrt(sp2 * (1 / nx + 1 / ny))
    
    df <- nx + ny - 2
    
    method <- "Pooled"
    
  } else {
    
    # Welch two-sample t-test
    SE <- sqrt(s2x / nx + s2y / ny)
    
    df <- (s2x / nx + s2y / ny)^2 /
      ((s2x / nx)^2 / (nx - 1) +
         (s2y / ny)^2 / (ny - 1))
    
    method <- "Welch"
  }
  
  # Calculate t-statistic
  t_stat <- (mean_x - mean_y) / SE
  
  # Calculate two-sided p-value
  p_value <- 2 * pt(
    abs(t_stat),
    df = df,
    lower.tail = FALSE
  )
  
  # Decide whether to reject the null hypothesis
  reject <- p_value < alpha
  
  # Return the results
  return(list(
    method = method,
    t_stat = t_stat,
    df = df,
    p_value = p_value,
    reject = reject
  ))
}

#Step 4
x_check <- c(12, 15, 14, 10, 13)
y_check <- c(9, 11, 8, 10, 7, 12, 9, 8)

# Run the function with its default arguments
welch_result <- two_sample_t(x_check, y_check)

# Run the function assuming equal variances
pooled_result <- two_sample_t(
  x_check,
  y_check,
  equal_var = TRUE
)

print(welch_result)
print(pooled_result)

#Step 5
# Verify the Welch test
welch_reference <- t.test(
  x_check, y_check,
  var.equal = FALSE
)

# Verify the pooled test
pooled_reference <- t.test(
  x_check, y_check,
  var.equal = TRUE
)

# Compare Welch results
welch_differences <- c(
  t_stat = abs(
    welch_result$t_stat -
      unname(welch_reference$statistic)
  ),
  df = abs(
    welch_result$df -
      unname(welch_reference$parameter)
  ),
  p_value = abs(
    welch_result$p_value -
      welch_reference$p.value
  )
)

print(welch_differences)
print(welch_differences < 1e-8)

# Compare pooled results
pooled_differences <- c(
  t_stat = abs(
    pooled_result$t_stat -
      unname(pooled_reference$statistic)
  ),
  df = abs(
    pooled_result$df -
      unname(pooled_reference$parameter)
  ),
  p_value = abs(
    pooled_result$p_value -
      pooled_reference$p.value
  )
)

print(pooled_differences)
print(pooled_differences < 1e-8)

# Explanation:
# The function's t-statistic, degrees of freedom, and p-value
# are checked against t.test(). Differences below 1e-8 indicate
# agreement to the required numerical tolerance.

#Part B

#Step 1
B <- 1000
n_x <- 20
n_y <- 40
alpha <- 0.05

scenarios <- data.frame(
  scenario = c(
    "equal_null",
    "unequal_null",
    "equal_shift",
    "unequal_shift"
  ),
  mean_x = c(0, 0, 0, 0),
  mean_y = c(0, 0, 1, 1),
  sd_x = c(1, 3, 1, 3),
  sd_y = c(1, 1, 1, 1)
)

# Set the seed once before the simulation
set.seed(2026)

#Step 2
simulation_results <- data.frame(
  scenario = character(8000),
  replicate = integer(8000),
  method = character(8000),
  t_stat = numeric(8000),
  df = numeric(8000),
  p_value = numeric(8000),
  reject = logical(8000)
)

#Step 3
# Run the simulation

row_index <- 1

for (i in 1:nrow(scenarios)) {
  
  for (b in 1:B) {
    
    # Generate fresh independent samples
    x <- rnorm(
      n = n_x,
      mean = scenarios$mean_x[i],
      sd = scenarios$sd_x[i]
    )
    
    y <- rnorm(
      n = n_y,
      mean = scenarios$mean_y[i],
      sd = scenarios$sd_y[i]
    )
    
    # Run Welch's test
    result_welch <- two_sample_t(
      x, y,
      equal_var = FALSE,
      alpha = alpha
    )
    
    # Save Welch results
    simulation_results[row_index, ] <- list(
      scenarios$scenario[i],
      b,
      result_welch$method,
      result_welch$t_stat,
      result_welch$df,
      result_welch$p_value,
      result_welch$reject
    )
    
    row_index <- row_index + 1
    
    # Run pooled test on the SAME samples
    result_pooled <- two_sample_t(
      x, y,
      equal_var = TRUE,
      alpha = alpha
    )
    
    # Save pooled results
    simulation_results[row_index, ] <- list(
      scenarios$scenario[i],
      b,
      result_pooled$method,
      result_pooled$t_stat,
      result_pooled$df,
      result_pooled$p_value,
      result_pooled$reject
    )
    
    row_index <- row_index + 1
  }
}

# Display the first six rows
head(simulation_results)

# Display the total row count
nrow(simulation_results)

#Step 4
simulation_summary <- simulation_results %>%
  summarise(
    mean_t = mean(t_stat),
    sd_t = sd(t_stat),
    rejection_rate = mean(reject),
    .by = c(scenario, method)
  )

# Display results rounded to three decimal places
simulation_summary %>%
  mutate(
    across(
      c(mean_t, sd_t, rejection_rate),
      ~ round(.x, 3)
    )
  ) %>%
  print()

#Step 5
# Plot rejection rates

barplot(
  height = simulation_summary$rejection_rate,
  names.arg = paste(
    simulation_summary$scenario,
    simulation_summary$method,
    sep = "\n"
  ),
  las = 2,
  ylab = "Rejection Rate",
  main = "Rejection Rates by Scenario and Test",
  ylim = c(
    0,
    max(0.05, simulation_summary$rejection_rate) * 1.15
  )
)

# The nominal significance level
abline(h = 0.05, lty = 2)

#Part C
# Short Report:
# Under the null scenarios, the rejection rates should be close
# to 0.05 when the tests maintain the intended Type I error rate.
# When the population means differ, rejection rates generally
# increase, reflecting greater power to detect a real difference.
# With unequal variances and unequal sample sizes, the pooled
# test can have an inflated Type I error rate, so higher power
# alone is not enough to judge a test; Type I error control matters.
# Small differences from 0.05 can occur due to simulation variation.

