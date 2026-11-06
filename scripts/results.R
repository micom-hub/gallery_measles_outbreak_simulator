# results.R
# 
# This script runs the tau-leaping SEIR-V model for every facility we have data
# for from MDHHS. It runs 100 iterations per facility. The goal is to then
# use the results to stratify risk by facility for inter-facility comparisons
# 
# Author: Jack Jacobs (jckjcbs@umich.edu)
#         MI-Support (micom-misupport@umich.edu)

here::i_am("scripts/results.R")

library(here)
library(tidyverse)
library(glue)
library(readxl)
library(adaptivetau)

source(here("R","seir_v.R"))

# Get the MDHHS facility data for Example Co
facilities <- tibble(
  name = "EXAMPLE childcare",
  level = "Childcare",
  county = "Example",
  n_student = 50,
  complete = 30,
  provisional = 10,
  incomplete = 0,
  n_waiver = 10,
  n_medical = 2,
  n_religious = 5,
  n_phil = 3
) |> 
  mutate(no_dose = n_student - complete - provisional)


#### Cumulative cases comparison ####

# Loop through the schools
cumulation_iter <- function(i) {
  # Grab simulation results and obtain the average
  facility <- facilities |> slice(i)
  if (facility$n_student < 5) { next }
  results <- tau_SEIR.V(
    facility,
    t_max = 250,
    models = 100
  ) |> 
    mutate(day = ceiling(time)) |> 
    summarise(
      across(S:Rv2, last),
      across(exp:rec_v2, sum),
      .by = c(model, day)
    ) |> 
    pivot_longer(
      cols = exp:rec_v2,
      names_to = "transition",
      values_to = "sum"
    ) |> 
    filter(transition %in% c("inf","inf_v1","inf_v2")) |> 
    summarise(sum = sum(sum), .by = model:day) |> 
    mutate(cumsum = cumsum(sum), .by = model) |> 
    summarise(
      daily_avg = mean(cumsum),
      daily_25pct = quantile(cumsum, probs = 0.25),
      daily_median = median(cumsum),
      daily_75pct = quantile(cumsum, probs = 0.75),
      .by = day
    ) |> 
    mutate(name = facility$name) |> 
    relocate(name)
  
  if (i %% 25 == 0) cat(glue("\nCumulative cases iter {i} complete"))
  return(results)
}

# Bind results
all_results <- map(1:nrow(facilities), cumulation_iter)
results_example <- bind_rows(!!!all_results) |> 
  # Add the school student counts and level to these results
  left_join(
    facilities |> 
      select(name, level, n_student),
    join_by(name)
  ) |> 
  relocate(level, n_student, .after = name) |> 
  # Convert every case count to a proportion
  mutate(across(
    daily_avg:daily_75pct,
    \(x) x / n_student
  )) |> 
  select(-n_student)

# Add a "mean" ribbon
mean_results_example <- results_example |> 
  summarise(
    across(daily_avg:daily_75pct, mean),
    .by = c(level, day)
  ) |> 
  mutate(name = level |> case_match(
    "Childcare" ~ "Average of Example Co Childcares",
    "Kindergarten" ~ "Average of Example Co Kindergartens",
    "Seventh" ~ "Average of Example Co Seventh Grades"
  )) |> 
  relocate(name)
results_example <- mean_results_example |> 
  bind_rows(results_example)

# Save results
results_example |> 
  write_rds(here("data/daily_cumulative_cases.rds"), compress = "gz")
cat("\nFINISHED cumulative cases simulations")


#### Marginal vaccine impact, viz_df ####

marginal_iter <- function(i) {
  # Generate different starting points
  status_quo <- facilities |> slice(i)
  if (status_quo$n_student < 5) { return() }
  plus1 <- status_quo |> allocate_doses(1)
  plus5 <- status_quo |> allocate_doses(5)
  plus10 <- status_quo |> allocate_doses(10)
  
  # Simulate all of them
  real_results <- status_quo |> 
    tau_SEIR.V(t_max = 250, models = 100) |> 
    mutate(name = "status quo") |> 
    relocate(name)
  plus1_results <- plus1 |> 
    tau_SEIR.V(t_max = 250, models = 100) |> 
    mutate(name = "1 more dose") |> 
    relocate(name)
  plus5_results <- plus5 |> 
    tau_SEIR.V(t_max = 250, models = 100) |> 
    mutate(name = "5 more doses") |> 
    relocate(name)
  plus10_results <- plus10 |> 
    tau_SEIR.V(t_max = 250, models = 100) |> 
    mutate(name = "10 more doses") |> 
    relocate(name)
  
  # Concatenate them and discard antecedents
  full_results <- bind_rows(
    real_results, plus1_results, plus5_results, plus10_results
  )
  rm(real_results, plus1, plus5, plus10,
     plus1_results, plus5_results, plus10_results)
  
  # Reshape for visualization
  viz_df <- full_results |> 
    mutate(
      day = ceiling(time),
      infections = inf + inf_v1 + inf_v2
    ) |> 
    summarise(infections = sum(infections), .by = c(name, model, day)) |> 
    mutate(cum_infections = cumsum(infections), .by = c(name, model)) |> 
    summarise(avg_cum_infections = mean(cum_infections), .by = c(name, day)) |> 
    mutate(name = name |> fct_relevel(
      "status quo","1 more dose","5 more doses","10 more doses"
    )) |> 
    mutate(facility = status_quo$name) |> 
    relocate(facility)
  
  if (i %% 25 == 0) cat(glue("\nMarginal cases iter {i} complete"))
  return(viz_df)
}

# Loop through the schools
all_results <- map(1:nrow(facilities), marginal_iter)

# Bind results
results_example <- bind_rows(!!!all_results) |> 
  # Add the school student counts to these results
  left_join(
    facilities |> 
      select(name, level, n_student),
    join_by(facility == name)
  ) |> 
  relocate(level, n_student, .after = facility)

# Save results
results_example |> 
  write_rds(
    here("data","marginal_vax_facility_results.rds"),
    compress = "gz"
  )
cat("\nFINISHED marginal cases simulations")
