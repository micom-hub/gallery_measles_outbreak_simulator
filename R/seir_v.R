# sier_v.R
# 
# Script for testing and deploying the stochastic tau model for outbreak 
# modeling using MDHHS waiver data
# 
# Author: Jack Jacobs (jckjcbs@umich.edu)
#         MI-Support (micom-misupport@umich.edu)
# 
# This script borrows from Marisa Eisenberg's tau-leaping code

here::i_am("R/seir_v.R")

library(here)
library(tidyverse)
library(scales)
library(glue)
library(slider)
library(roxygen2)
library(readxl)
library(adaptivetau)
# library(future)
# library(parallelly)
# library(furrr)


#### Data loading ####

#' Generate a fake facility's vax data: used for testing
#' 
#' @returns facility vax waiver data for a fake facility, N=500, coverage=0.85
# generate_fake_data <- function(N = 500, coverage = 0.85) {
generate_fake_data <- function(
  protected = 80, unprotected = 20,
  name = "a Fake School"
) {
  return(tibble(
    name = name,
    county = "fake county",
    n_student = protected + unprotected,
    complete = protected,
    provisional = 0,
    incomplete = 0,
    n_waiver = unprotected,
    n_medical = 0,
    n_religious = 0,
    n_phil = 0,
    coverage = protected / (protected + unprotected)
  ))
}


#### tau-leaping SEIR-V ####

tau_SEIR.V <- function(
    waiver_row,
    params = list(
      beta = 1.875,       # transmission rate: 15 * 0.125 = 1.875 per day
      sigma = 0.0952,     # rate of progression: 1 per 10.5 days
      gamma = 0.125,      # recovery rate: 1 per 8 days
      epsilon_s1 = 0.05,  # impact of 1-dose vax on susceptibility: 1 - 0.95
      epsilon_s2 = 0.003, # impact of 2-dose vax on susceptibility: 1 - 0.997
      epsilon_i = 0.05    # impact of vax on infectiousness: 1 - 0.95
    ),
    inf_i = 1,
    t_max = 100,
    models = 20
) {
  # Specify inputs to tau-leaping algo
  init_values <- c(
    S = waiver_row$n_student - waiver_row$provisional - waiver_row$complete - 
      inf_i,
    E = 0,
    I = inf_i,
    R = 0,
    Sv1 = waiver_row$provisional,
    Ev1 = 0,
    Iv1 = 0,
    Rv1 = 0,
    Sv2 = waiver_row$complete,
    Ev2 = 0,
    Iv2 = 0,
    Rv2 = 0
  )
  transitions <- list(
    c(S = -1, E = 1), c(Sv1 = -1, Ev1 = 1), c(Sv2 = -1, Ev2 = 1),  # exposure
    c(E = -1, I = 1), c(Ev1 = -1, Iv1 = 1), c(Ev2 = -1, Iv2 = 1),  # infection
    c(I = -1, R = 1), c(Iv1 = -1, Rv1 = 1), c(Iv2 = -1, Rv2 = 1)   # recovery
  )
  rate_func <- function(x, p, t) {
    return(c(
      p$beta * x["S"] *                                               # exp
        ((x["I"] + p$epsilon_i * (x["Iv1"] + x["Iv2"])) / sum(x)),
      p$beta * p$epsilon_s1 * x["Sv1"] *                              # exp_v1
        ((x["I"] + p$epsilon_i * (x["Iv1"] + x["Iv2"])) / sum(x)),
      p$beta * p$epsilon_s2 * x["Sv2"] *                              # exp_v2
        ((x["I"] + p$epsilon_i * (x["Iv1"] + x["Iv2"])) / sum(x)),
      p$sigma * x["E"],                                               # inf
      p$sigma * x["Ev1"],                                             # inf_v1
      p$sigma * x["Ev2"],                                             # inf_v2
      p$gamma * x["I"],                                               # rec
      p$gamma * x["Iv1"],                                             # rec_v1
      p$gamma * x["Iv2"]                                              # rec_v2
    ))
  }
  
  # Handle 100% vaccinated case where 'init_values' calc is messed up
  if (init_values["S"] < 0) {
    deficit <- abs(init_values["S"])
    init_values["S"] <- 0
    init_values["Sv2"] <- init_values["Sv2"] - deficit
    init_values["I"] <- init_values["I"] - deficit
    init_values["Iv2"] <- init_values["Iv2"] + deficit
  }

  # Define how I want to collect ssa.adaptivetau's results
  my_ssa.adaptivetau <- function(i) {
    r <- ssa.adaptivetau(
      init_values, transitions, rate_func, params, tf = t_max,
      tl.params = list(maxtau = 0.1), reportTransitions = TRUE
    )
    df <- tibble(
      model = as.character(i),
      time = r$dynamics[, "time"],
      S = r$dynamics[, "S"],
      E = r$dynamics[, "E"],
      I = r$dynamics[, "I"],
      R = r$dynamics[, "R"],
      Sv1 = r$dynamics[, "Sv1"],
      Ev1 = r$dynamics[, "Ev1"],
      Iv1 = r$dynamics[, "Iv1"],
      Rv1 = r$dynamics[, "Rv1"],
      Sv2 = r$dynamics[, "Sv2"],
      Ev2 = r$dynamics[, "Ev2"],
      Iv2 = r$dynamics[, "Iv2"],
      Rv2 = r$dynamics[, "Rv2"],
      exp = r$transitions[, 1],
      exp_v1 = r$transitions[, 2],
      exp_v2 = r$transitions[, 3],
      inf = r$transitions[, 4],
      inf_v1 = r$transitions[, 5],
      inf_v2 = r$transitions[, 6],
      rec = r$transitions[, 7],
      rec_v1 = r$transitions[, 8],
      rec_v2 = r$transitions[, 9]
    )
    
    return(df)
  }
  
  # Run the ssa.adaptivetau algo many times
  # plan(multisession(
  #   workers = availableCores(constraints = "connections-16") - 2L
  # ))
  results <- 1:models |> 
    as.integer() |> as.character() |> 
    map(my_ssa.adaptivetau)
    # future_map(my_ssa.adaptivetau, .options = furrr_options(seed = TRUE))
  
  return(bind_rows(!!!results))
}

#' Get the chance of exceeding 10 new infections
case_counts <- function(results) {
  results |> 
    select(model, inf, inf_v1, inf_v2) |> 
    summarise(across(inf:inf_v2, sum), .by = model) |> 
    mutate(
      inf_v = inf_v1 + inf_v2,
      infections = inf + inf_v
    ) |> 
    select(-matches("inf_v[12]"))
}


#### epiENGAGE plotting ####

#' Get the mean number of infectious persons per day over all simulations
epiENGAGE_plus_highlights_viz_df <- function(
    results,
    params = list(
      beta = 1.875,       # transmission rate: 15 * 0.125 = 1.875 per day
      sigma = 0.0952,     # rate of progression: 1 per 10.5 days
      gamma = 0.125,      # recovery rate: 1 per 8 days
      epsilon_s1 = 0.05,  # impact of 1-dose vax on susceptibility: 1 - 0.95
      epsilon_s2 = 0.003, # impact of 2-dose vax on susceptibility: 1 - 0.997
      epsilon_i = 0.05,   # impact of vax on infectiousness: 1 - 0.95
    )
) {
  daily_outcomes <- results |> 
    mutate(day = ceiling(time)) |> 
    arrange(model, day) |> 
    group_by(model, day) |> 
    summarise(
      across(S:Rv2, last),
      across(exp:rec_v2, sum),
      .groups = "drop"
    )
  
  viz_df <- daily_outcomes |> 
    select(model:Rv2) |> 
    pivot_longer(
      cols = S:Rv2,
      names_to = "compartment",
      values_to = "count"
    ) |> 
    filter(compartment %in% c("I","Iv1","Iv2")) |> 
    summarise(count = sum(count), .by = model:day) |> 
    mutate(model = model |> as.character())
  
  mean_df <- viz_df |> 
    summarise(count = mean(count), .by = day) |> 
    mutate(model = "avg")
  viz_df <- viz_df |> bind_rows(mean_df)
  
  # Restrict plotting to a random sample of 20 models
  model_count <- viz_df |> 
    filter(!(model %in% c("avg","det"))) |> 
    pull(model) |> 
    n_distinct()
  if (model_count > 20) {
    models <- viz_df |> 
      pull(model) |> 
      unique()
    model_sample <- models |> sample(20)
    viz_df <- viz_df |> 
      filter(model %in% c("avg", "det", model_sample))
  }
  
  return(viz_df)
}

plot_epiENGAGE <- function(
    df,
    slide_y = TRUE,
    highlight = "avg",
    name = "this facility"
) {
  # Add a "highlight" col to viz_df and remove stray highlight
  viz_df <- df |> 
    mutate(avg = model == highlight) |> 
    filter((model |> str_detect("\\d")) | avg)
  
  # Calculate 7-day sliding average
  if (slide_y) {
    viz_df <- viz_df |> 
      arrange(model, day) |> 
      group_by(model) |> 
      mutate(sliding_count = count |> slide_dbl(mean, na.rm = T, .before = 6L))
    p <- viz_df |> 
      ggplot(aes(x = day, y = sliding_count, group = model, color = avg)) +
      ylab("Infectious persons (7-day average)")
  } else {
    p <- viz_df |> 
      ggplot(aes(x = day, y = count, group = model, color = avg)) +
      ylab("Infectious persons")
  }
  
  # Determine caption based on highlighting option
  caption_value <- case_when(
    highlight == "avg" ~ glue(
      "This graph displays 20 measles outbreak simulations. The highlighted ",
      "time series displays the daily average value of all 100 simulations."
    ),
    highlight == "det" ~ glue(
      "This graph displays 20 measles outbreak simulations. The highlighted ",
      "time series displays the predictions of a deterministic SEIR-V model."
    )
  )
  
  # Plot outbreak simulations
  p +
    geom_line(
      data = viz_df |> filter(!avg),
      linewidth = 0.5, color = "gray60"
    ) +
    geom_line(
      data = viz_df |> filter(avg),
      linewidth = 1, color = "#3e7c33"
    ) +
    theme_minimal() +
    labs(
      title = glue("Measles outbreak simulations for {name}"),
      subtitle = glue(
        "Simulations assume no interventions and no community transmission"
      ),
      x = "Time (days)",
      caption = caption_value
    )
}


#### Cumulative case comparison over time ####

inf_comp_viz_df <- function(
  sim_results,
  pick_level = c("Childcare","Kindergarten","Seventh")
) {
  # Get all Example Co daycare results and trim to selected schools
  avg_example <- read_rds(here("data","daily_cumulative_cases.rds")) |> 
    filter(name |> str_detect("Average of Example Co")) |> 
    filter(level == pick_level)
  
  # Find N for this facility
  n_student <- sim_results |> 
    slice(1) |> 
    select(S:Rv2) |> 
    as.matrix() |> 
    rowSums()
  
  # Finding daily cumulative cases for this facility
  viz_df <- sim_results |> 
    mutate(day = ceiling(time)) |> 
    arrange(model, day) |> 
    group_by(model, day) |> 
    summarise(across(exp:rec_v2, sum), .groups = "drop") |> 
    mutate(cases = rowSums(pick(inf, inf_v1, inf_v2))) |> 
    mutate(cum_cases = cumsum(cases), .by = model) |> 
    summarise(
      daily_avg = mean(cum_cases),
      daily_25pct = quantile(cum_cases, probs = 0.25),
      daily_median = median(cum_cases),
      daily_75pct = quantile(cum_cases, probs = 0.75),
      .by = day
    ) |> 
    mutate(
      across(starts_with("daily"), \(x) x / n_student),
      name = "Median and middle 50% of\noutcomes for this facility"
    ) |> 
    relocate(name)
  
  # Add mean example value to figure
  viz_df <- avg_example |> 
    select(name, day, daily_avg) |> 
    mutate(
      daily_median = daily_avg,
      daily_25pct = daily_avg, daily_75pct = daily_avg
    ) |> 
    bind_rows(viz_df) |> 
    mutate(name = name |> fct_relevel(
      "Median and middle 50% of\noutcomes for this facility"
    ))
  
  return(viz_df)
}

inf_comp_plot <- function(viz_df, slide_y = TRUE) {
  # Calculate 7-day sliding average
  if (slide_y) {
    viz_df <- viz_df |> 
      arrange(name, day) |> 
      mutate(
        across(
          starts_with("daily"),
          \(x) slide_dbl(x, mean, na.rm = T, .before = 6L)
        ),
        .by = name
      )
    p <- viz_df |> 
      ggplot(aes(
        x = day, y = daily_median, fill = name,
        ymin = daily_25pct, ymax = daily_75pct
      )) +
      ylab("Cumulative percent infected (7-day average)")
  } else {
    p <- viz_df |> 
      ggplot(aes(
        x = day, y = daily_median, fill = name,
        ymin = daily_25pct, ymax = daily_75pct
      )) +
      ylab("Cumulative percent infected")
  }
  
  # Plot
  p +
    geom_ribbon(show.legend = TRUE) +
    geom_line(mapping = aes(color = name)) +
    scale_color_manual(values = c("#3e7c33","#000000")) +
    scale_fill_manual(values = c("#3e7c3333","#00000000")) +
    scale_y_continuous(labels = scales::label_percent()) +
    theme_minimal() +
    labs(
      title = 
        "Comparing this facility's outbreak risk with Example Co. average",
      subtitle = glue(
        "Simulations assume no interventions and no community transmission.\n",
        "Range around this facility's median represents 50% of outcomes."
      ),
      x = "Time (days)", color = "Facility", fill = "Facility"
    )
}


#### Marginal vaccinations ####

# Dose allocation helpers
allocate_dose <- function(data) {
  data |> mutate(
    provisional = case_when(
      no_dose >= 1 ~ provisional + 1,
      provisional == 0 ~ 0,
      provisional > 0 ~ provisional - 1
    ),
    complete = case_when(
      (no_dose == 0) & (provisional > 0) ~ complete + 1,
      .default = complete
    ),
    no_dose = case_when(
      no_dose > 0 ~ no_dose - 1,
      .default = 0
    )
  )
}
allocate_doses <- function(data, num) {
  for (i in 1:num) data <- data |> allocate_dose()
  return(data)
}

#' Get marginal impact of 1, 5, and 10 more doses compared to status quo
marginal_vax_sims <- function(status_quo, params, inf_i, t_max) {
  # Exit if fewer than 5 students
  if (status_quo$n_student < 5) { return() }

  # Generate different starting points
  plus1 <- status_quo |> allocate_doses(1)
  plus5 <- status_quo |> allocate_doses(5)
  plus10 <- status_quo |> allocate_doses(10)

  # Simulate all of them
  real_results <- status_quo |> 
    tau_SEIR.V(
      params = params,
      inf_i = inf_i,
      t_max = t_max,
      models = 100
    ) |> 
    mutate(name = "status quo") |> 
    relocate(name)
  plus1_results <- plus1 |> 
    tau_SEIR.V(
      params = params,
      inf_i = inf_i,
      t_max = t_max,
      models = 100
    ) |> 
    mutate(name = "1 more dose") |> 
    relocate(name)
  plus5_results <- plus5 |> 
    tau_SEIR.V(
      params = params,
      inf_i = inf_i,
      t_max = t_max,
      models = 100
    ) |> 
    mutate(name = "5 more doses") |> 
    relocate(name)
  plus10_results <- plus10 |> 
    tau_SEIR.V(
      params = params,
      inf_i = inf_i,
      t_max = t_max,
      models = 100
    ) |> 
    mutate(name = "10 more doses") |> 
    relocate(name)

  # Concatenate them and discard antecedents
  full_results <- bind_rows(
    real_results, plus1_results, plus5_results, plus10_results
  )
  rm(plus1, plus5, plus10,
    real_results, plus1_results, plus5_results, plus10_results)
  
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
    mutate(
      facility = status_quo$name,
      n_student = status_quo$n_student
    ) |> 
    relocate(facility, n_student)

  return(viz_df)
}

marginal_vax_plot <- function(viz_df, name = "this facility") {
  viz_df |> 
    mutate(name = name |> fct_relevel(
      "status quo","1 more dose","5 more doses","10 more doses"
    )) |> 
    ggplot(aes(x = day, y = avg_cum_infections, color = name)) +
    geom_line(linewidth = 1) +
    labs(
      title = glue(
        "Impacts of additional vaccinations on {name}"
      ),
      subtitle = glue(
        "Simulations assume no interventions and no community transmission."
      ),
      x = "Time (days)", y = "Average cumulative infections"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom", legend.title = element_blank())
}
