# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
#
# Measles Outbreak Simulator for Schools - Shiny app
# Authors: Jack Jacobs (jckjcbs@umich.edu)
#          Stephanie Iorga (siorga@umich.edu)
#          MI-Support (micom-misupport@umich.edu)

here::i_am("app.R")

library(shiny)
library(bslib)
library(shinyWidgets)
library(shinyFeedback)
library(shinyjs)
library(here)
library(tidyverse)
library(glue)

# Load model
source(here("R/seir_v.R"))

# Load data
waiver_data <- tibble(
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
marginal_sim_results <- read_rds(
  here("data","marginal_vax_facility_results.rds")
)

##### Define UI for application #####

ui <- page_fluid(
  useShinyjs(),
  useShinyFeedback(),
  page_navbar(
    title = div(img(src="phlogo.jpg", height = 55, width = 84), ""),
    id = "page",
    theme = bs_theme(bootswatch = "flatly", version = 5),
    tags$head(tags$style(HTML("

        .navbar-default .navbar-nav > .active > a,
        .navbar-default .navbar-nav > .active > a:focus,
        .navbar-default .navbar-nav > .active > a:hover,
        .navbar-default .navbar-nav > li > a:hover {color: #caa877;}
        .dropdown-menu { background-color: white;}
        .dropdown-menu > .active > a {background-color: #8acdec;}
        .dropdown-menu > li > a:hover {color: white; background-color: #8acdec;}


        .sidebar { background-color: #ffe0b4 !important;
                   color: black !important;}

        .abouthead { background-color: #ffe0b4 !important;
                    color: black;}

        .help-block { color: #333333 !important; }

        #date_range_button {
          --bs-btn-color: #ffffff !important;
          --bs-btn-border-color: #ffffff !important;
        }

    "))),
    sidebar = sidebar(
      open = "always",
      title = "Model Inputs",
      conditionalPanel(
        "input.page === 'main_panel'",
        numericInput(
          "protected",
          label = "Vaccinated children",
          value = 80,
          min = 0
        ),
        numericInput(
          "unprotected",
          label = "Unvaccinated children",
          value = 20,
          min = 0
        ),
        numericInput(
          "initial_infected",
          label = "Students initially infected",
          value = 1,
          min = 1
        ),
        textInput(
          "bldg",
          label = "School/Facility name",
          value = "this facility"
        ),
        # move this underneath Vaccination Rate (%)
        helpText(em("Enter values or select from Lookup.")),
        # school lookup
        selectInput(
          "level",
          "Select Schooling Level",
          choices = c(Choose = ".*","Childcare","Kindergarten","Seventh")
        ),
        pickerInput(
          "school",
          "Select School/Childcare",
          choices = NULL,
          options = pickerOptions(liveSearch = TRUE)
        ),
        input_task_button(
          "run_sim", "Run Simulations",
          icon = fontawesome::fa_i("computer", prefer_type = "solid"),
          style = "background-color: #092d57; color: #ffffff;"
        ),
        input_switch("smooth_y", "Smooth infection counts"),
        hr(), # #8bbffe 
        tags$h5("Epidemic Parameters"),
        helpText(em(
          "Caution - Default values reflect published estimates. 
              Significant changes may result in inaccurate projections."
        )),
        actionButton(
          "toggle_params",
          label = "Change Parameters",
          icon = icon("sliders"),
          style = "background-color: #092d57; color: #ffffff;"
        ),
        div(
          id = "param_sliders",
          style = "display: none;",
          sliderInput(
            "r0",
            label = tags$div(
              "Basic reproduction number (R", tags$sub("0"), ")",
              style = "font-size: 0.8rem;"
            ), 
            min = 12, max = 18,
            value = 15,
            ticks = FALSE
          ),
          sliderInput(
            "latent_period",
            label = tags$div(
              "Average latent period (days)",
              style = "font-size: 0.8rem;"
            ),
            min = 7, max = 12,
            value = 10.5,
            step = 0.1, ticks = FALSE
          ),
          sliderInput(
            "infectious_period",
            label = tags$div(
              "Average infectious period (days)",
              style = "font-size: 0.8rem;"
            ),
            min = 3, max = 10,
            value = 5,
            ticks = FALSE
          ),
          sliderInput(
            "min_outbreak",
            label = tags$div(
              "Minimum outbreak size (new infections)",
              style = "font-size: 0.8rem;"
            ),
            min = 3, max = 25,
            value = 10,
            ticks = FALSE
          ),
          sliderInput(
            "ve_sus1",
            label = tags$div(
              "Vaccine efficacy (1-dose) - susceptibility (%)", 
              style = "font-size: 0.75rem;"
            ),
            min = 0, max = 100,
            value = 95,
            step = 0.1, ticks = FALSE
          ),
          sliderInput(
            "ve_sus2",
            label = tags$div(
              "Vaccine efficacy (2-doses) - susceptibility (%)", 
              style = "font-size: 0.75rem;"
            ),
            min = 0, max = 100,
            value = 99.7,
            step = 0.1, ticks = FALSE
          ),
          sliderInput(
            "ve_inf",
            label = tags$div(
              "Vaccine efficacy - infectiousness (%)",
              style = "font-size: 0.75rem;"
            ),
            min = 0, max = 100,
            value = 95,
            ticks = FALSE
          ),
          sliderInput(
            "outbreak_days",
            label = tags$div(
              "Modeled outbreak duration (days)",
              style = "font-size: 0.75rem;"
            ),
            min = 1, max = 1000,
            value = 250,
            ticks = FALSE
          )
        )
      ),
      conditionalPanel(
        "input.page === 'marginal_panel'",
        numericInput(
          "protected_mar",
          label = "Vaccinated children",
          value = 80,
          min = 0
        ),
        numericInput(
          "unprotected_mar",
          label = "Unvaccinated children",
          value = 20,
          min = 0
        ),
        numericInput(
          "initial_infected_mar",
          label = "Students initially infected",
          value = 1,
          min = 1
        ),
        textInput(
          "bldg_mar",
          label = "School/Facility name",
          value = "this facility"
        ),
        helpText(em("Enter values or select from Lookup.")),
        # school lookup
        selectInput(
          "level_mar",
          "Select Schooling Level",
          choices = c(Choose = ".*","Childcare","Kindergarten","Seventh")
        ),
        pickerInput(
          "school_mar",
          "Select School/Childcare",
          choices = NULL,
          options = pickerOptions(liveSearch = TRUE)
        ),
        input_task_button(
          "run_sim_mar", "Run Simulations",
          icon = fontawesome::fa_i("computer", prefer_type = "solid"),
          style = "background-color: #092d57; color: #ffffff;"
        )
      )
    ),
    
    #### Main panel ####
    nav_panel(
      "Home",
      value = "main_panel",
      layout_column_wrap(
        width = 1,
        div(
          class = "alert alert-warning text-center",
          role = "alert",
          tags$h3("School Outbreak Projection Assumptions"),
          tags$p(
            strong("Projections assume no interventions and do not account 
            for infections among non-students in the surrounding community."),
            tags$br(),
            "Active measles control measures could lead to substantially 
            smaller and shorter outbreaks than these projections suggest."
          )
        )
      ),
      card(
        card_header("Simulation results for this facility"),
        card_body(plotOutput("epiENGAGE_plot", height = "500px"))
      ),
      layout_columns(
        class = "equal-height-row",
        width = 1/2,
        div(
          style = "text-align: center",
          card(
            style = "height: 120%",
            card_body(
              tags$h4("Chance of reaching 5 new infections"),
              tags$h2(tags$b(
                style = "color:#3e7c33",
                textOutput("sim1_prob_5")
              ))
            )
          )
        ),
        
        div(
          style = "text-align: center",
          card(
            card_body(
              tags$h4(
                "Likely outbreak size", tags$br(),
                em("if reaches 5 new infections")
              ),
              tags$h3(tags$b(
                style = "color:#3e7c33",
                "Unvaccinated cases:",
                span(textOutput("sim1_unvacc_range"))
              )),
              tags$h3(tags$b(
                style = "color:#3e7c33",
                "Vaccinated cases:",
                span(textOutput("sim1_vacc_range"))
              )) 
            )
          )
        )
      ),
      card(
        card_header("Comparing this facility to the Example County average"),
        card_body(plotOutput("inf_comp_plot", height = "500px"))
      )
    ),
    
    #### Marginal plot ####
    nav_panel(
      "Additional Vaccine Impact",
      value = "marginal_panel",
      layout_column_wrap(
        width = 1,
        div(
          class = "alert alert-warning text-center",
          role = "alert",
          tags$h3("School Outbreak Projection Assumptions"),
          tags$p(
            strong("Projections assume no interventions and do not account 
            for infections among non-students in the surrounding community."),
            tags$br(),
            "Active measles control measures could lead to substantially 
            smaller and shorter outbreaks than these projections suggest."
          )
        )
      ),
      tableOutput("facility_tbl"),
      card(
        card_header("Simulation results for this facility"),
        card_body(plotOutput("marginal_vax_plot", height = "500px"))
      ),
      h3("Average total cases given additional vaccine doses"),
      layout_columns(
        class = "equal-height-row",
        width = 1/4,
        div(
          style = "text-align: center",
          card(
            card_body(
              tags$h4("Status quo"),
              tags$h2(tags$b(
                style = "color:#3e7c33",
                textOutput("mar_0")
              ))
            )
          )
        ),
        div(
          style = "text-align: center",
          card(
            card_body(
              tags$h4("1 more"),
              tags$h2(tags$b(
                style = "color:#3e7c33",
                textOutput("mar_1")
              ))
            )
          )
        ),
        div(
          style = "text-align: center",
          card(
            card_body(
              tags$h4("5 more"),
              tags$h2(tags$b(
                style = "color:#3e7c33",
                textOutput("mar_5")
              ))
            )
          )
        ),
        div(
          style = "text-align: center",
          card(
            card_body(
              tags$h4("10 more"),
              tags$h2(tags$b(
                style = "color:#3e7c33",
                textOutput("mar_10")
              ))
            )
          )
        )
      )
    ),
    
    #### About Tab ####
    nav_panel(
      "About",
      tags$h3("Measles Vaccination", style = "margin-bottom: 4px;"),
      tags$p(
        HTML(
          "Two doses of the MMR vaccine are recommended by doctors as the best 
          way to protect against measles, mumps, and rubella. For additional 
          information about measles vaccines, visit the
          <a href= 'https://www.cdc.gov/measles/vaccines/index.html' 
          target='_blank'>CDC's MMR vaccination webpage</a>."
        ), 
        style = "margin-top: 0px; margin-bottom:0px;"
      ),
      tags$h3("Purpose", style = "margin-bottom: 4px;"),
      tags$p(HTML(
        "This measles outbreak simulator is a risk communication tool designed
        to visualize measles outbreak risks associated with educational 
        facilities. This dashboard was inspired by 
        the University of Texas's <a href=
        'https://epiengage-measles.tacc.utexas.edu/' target='_blank'>
        epiENGAGE Mealses Outbreak Simulator</a>."
      )),
      tags$h3("Model"),
      tags$p(HTML(
        "The model used for each simulation is a stochastic, continuous-time
        compartmental model implemented according to <a href=
        'https://epiengage-measles.tacc.utexas.edu/assets/epiENGAGE_Measles_Outbreak_Simulator%E2%80%93Model%20Details-2025.pdf'
        target='_blank'>the specification used by University of Texas's 
        epiENGAGE simulator</a> using the R-based <a href=
        'https://cran.r-project.org/web/packages/adaptivetau/index.html' 
        target='_blank'>adaptivetau</a> library. We have made one modification:
        in addition to the existing unvaccinated and two-dose \"tracks,\" we 
        have added a third \"track\" of S-E-I-R compartments to model children 
        who have received only a single dose of a measles vaccine so far. These 
        children are labeled \"provisional\" in the MDHHS waiver data."
      )),
      tags$p(HTML(
        "The default parameters are based on estimates that are widely used by 
        public health agencies: (1) a basic reproduction number (R0) of 15 
        <a href='https://www.ecdc.europa.eu/en/measles/facts' 
        target='_blank'>ECDC's Factsheet about measles</a>, (2) an average 
        latent period of 10.5 days <a href =
        'https://www.cdc.gov/measles/hcp/communication-resources/clinical-
        diagnosis-fact-sheet.html' target= '_blank'>CDC's Measles Clinical 
        Diagnosis Factsheet</a>, (3) an average infectious period of 5 days, 
        derived from a total infectious period of approximately 8 days <a href=
        'https://www.cdc.gov/measles/hcp/communication-resources/clinical-
        diagnosis-fact-sheet.html' target= '_blank'>CDC's Measles Clinical 
        Diagnosis Factsheet</a>, assuming students remain home after symptom 
        onset, (4) a vaccine efficacy of 99.7% in preventing infection <a 
        href='https://royalsocietypublishing.org/doi/abs/10.1098/rsif.2010.0086'
        target= '_blank'>van Boven et al. 2010</a>, and (5) a 95% reduction in 
        infectiousness among vaccinated individuals <a 
        href='https://wwwnc.cdc.gov/eid/article/30/9/24-0150_article' 
        target = '_blank'>Tranter et al. 2024</a>. Parameter ranges are based 
        on <a href='https://www.ecdc.europa.eu/en/measles/facts' 
        target='_blank'>ECDC's Factsheet about measles</a> and <a 
        href='https://academic.oup.com/biomet/article-abstract/57/1/141/
        284497?redirectedFrom=fulltext&login=false' target= '_blank'>Bailey and 
        Alfa-Steinberger 1970</a>."
      )),
      tags$h3("Contact"),
      tags$p(HTML(
        "This tool was created by the MI-Support initiative of <a href=
        'https://www.micom-insight.net/' target='_blank'>Michigan Public Health 
        Integrated Center for Outbreak Analytics and Modeling (MICOM)</a> in 
        partnership with a local health department in Michigan."
      )),
      tags$p(HTML(
        "For any comments or inquiries related to this Measles Outbreak 
        Simulator, please email us at <a href=
        'mailto:micom-misupport@umich.edu' target='_blank'>
        micom-misupport@umich.edu</a>."
      )),
      tags$p(HTML(
        "To submit a project request to MI-Support, please submit a request 
        using <a href='https://micom-hub.org/contact_us.html' target='_blank'>
        this form</a>."
      )),
      tags$div(
        style = "text-align: center; margin-top: 50px;",
        tags$img(src = "MiSupport_LogoPlainBlue_1@3x.png", width = "150px")
      )
    )
  )
)

##### Build server #####

server <- function(input, output, session) {

  # Initialize the school input as grayed out
  disable("school")
  disable("school_mar")
  
  # expand to view param sliders once user clicks on Change Parameters button
  observeEvent(input$toggle_params, {
    toggle(id = "param_sliders", anim = TRUE, animType = "slide")
  })

  # Update the choices in the school dropdown
  dropdown_schools <- reactive({
    # Require the "level" input
    req(input$level != ".*")

    # Define the logic of school options
    waiver_data |> 
      filter((level == input$level) & (county == "Example")) |> 
      arrange(name) |> 
      pull(name)
  })

  # Activate the school chooser widget when a level is chosen
  observeEvent(input$level, {
    if (input$level == ".*") {
      disable("school")
      updatePickerInput(session, "school", choices = NULL)
    } else {
      enable("school")
      updatePickerInput(session, "school", choices = dropdown_schools())
    }
  })

  # Update the choices in the school dropdown
  dropdown_schools_mar <- reactive({
    # Require the "level" input
    req(input$level_mar != ".*")

    # Define the logic of school options
    marginal_sim_results |> 
      filter(level == input$level_mar) |> 
      arrange(facility) |> 
      pull(facility) |> 
      unique()
  })

  # Activate the school chooser widget when a level is chosen
  observeEvent(input$level_mar, {
    if (input$level_mar == ".*") {
      disable("school_mar")
      updatePickerInput(session, "school_mar", choices = NULL)
    } else {
      enable("school_mar")
      updatePickerInput(session, "school_mar", choices = dropdown_schools_mar())
    }
  })
  
  # If user selects a school, then update the relevant input fields
  observeEvent(input$school, {
    # Grab the relevant facility input values
    daycare <- waiver_data |> 
      filter(name |> str_detect(glue("^{input$school}$")))
    
    # Manage the no-selection case
    if (nrow(daycare) != 1) {
      daycare <- tibble(
        n_student = 100,
        complete = 80,
        provisional = 0,
        incomplete = 0,
        n_waiver = 20
      )
    }
    
    # This is imperfect modeling of "provisional" status as fully vaccinated.
    updateNumericInput(
      inputId = "protected",
      value = daycare$complete + daycare$provisional
    )
    updateNumericInput(
      inputId = "unprotected",
      value = daycare$incomplete + daycare$n_waiver
    )
    updateTextInput(
      inputId = "bldg",
      value = daycare$name
    )
  })
  observeEvent(input$school_mar, {
    # Grab the relevant facility input values
    daycare <- waiver_data |> 
      filter(name |> str_detect(glue("^{input$school_mar}$")))
    
    # Manage the no-selection case
    if (nrow(daycare) != 1) {
      daycare <- tibble(
        n_student = 100,
        complete = 80,
        provisional = 0,
        incomplete = 0,
        n_waiver = 20
      )
    }
    
    # This is imperfect modeling of "provisional" status as fully vaccinated.
    updateNumericInput(
      inputId = "protected_mar",
      value = daycare$complete + daycare$provisional
    )
    updateNumericInput(
      inputId = "unprotected_mar",
      value = daycare$incomplete + daycare$n_waiver
    )
    updateTextInput(
      inputId = "bldg_mar",
      value = daycare$name
    )
  })
  
  measles_params <- reactive({
    list(
      beta = input$r0 / input$infectious_period,
      sigma = 1 / input$latent_period,
      gamma = 1 / input$infectious_period,
      epsilon_s1 = 1 - (input$ve_sus1 / 100),
      epsilon_s2 = 1 - (input$ve_sus2 / 100),
      epsilon_i = 1 - (input$ve_inf / 100)
    )
  })
  
  # simulation results
  sim_results <- reactive({
    req(is.numeric(c(input$protected, input$unprotected)))

    waiver_row <- generate_fake_data(input$protected, input$unprotected)
    
    # defining parameters based on user input
    params <- measles_params()
  
    tau_SEIR.V(
      waiver_row = waiver_row,
      params = params,
      inf_i = input$initial_infected,
      t_max = round(input$outbreak_days),
      models = 100
    )
  }) |> 
    bindEvent(input$run_sim)
  
  # Generate a stable viz_df based on simulation results
  epiENGAGE_viz_df <- reactive({
    sim_results() |> epiENGAGE_plus_highlights_viz_df()
  })
  
  # call for plotly function 
  output$epiENGAGE_plot <- renderPlot({
    # Isolate the plotting steps triggered by the smooth_y input
    p <- epiENGAGE_viz_df() |> 
      plot_epiENGAGE(
        slide_y = input$smooth_y,
        highlight = "avg",
        name = input$bldg
      )
    
    # produce plot
    p +
      theme(
        legend.position = "none",
        plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 16),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 14),
        plot.caption = element_text(size = 14, hjust = 0, color = "gray33")
      )
  })
  
  # Generate a stable viz_df based on simulation results and schooling level
  inf_comp_viz_df_reactive <- reactive({
    sim_results() |> inf_comp_viz_df(pick_level = input$level)
  })
  
  # call for plotly function 
  output$inf_comp_plot <- renderPlot({
    # Isolate the plotting steps triggered by the smooth_y input
    p <- inf_comp_viz_df_reactive() |> 
      inf_comp_plot(slide_y = input$smooth_y)
    
    # produce plot
    p +
      theme(
        plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 16),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 14),
        plot.caption = element_text(size = 14, hjust = 0, color = "gray33"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.position = "bottom"
      )
  })
  
  # probability of exceeding 10 infections - updating card text
  output$prob_5 <- renderText({
    results <- sim_results()
    cc <- case_counts(results)
    
    over_5 <- cc |> 
      filter(infections >= 5) |> 
      nrow()
    
    chance <- round(100 * (over_5 / nrow(cc))) |> as.character()
    
    if (chance == 0) {
      "<1%"
    } else {
      glue("{chance}%")
    }
  })
  
  # probability of exceeding 10 infections - updating card text
  output$sim1_prob_5 <- renderText({
    results <- sim_results()
    cc <- case_counts(results)
    
    over_5 <- cc |> 
      filter(infections >= 5) |> 
      nrow()
    
    chance <- round(100 * (over_5 / nrow(cc))) |> as.character()
    
    if (chance == 0) {
      "<1%"
    } else {
      glue("{chance}%")
    }
  })
  
  # unvaccinated case range - updating card text
  output$unvacc_range <- renderText({
    results <- sim_results()
    cc <- case_counts(results)
    
    over_5 <- cc |> 
      filter(infections >= 5)
    
    if(nrow(over_5) == 0) {
      "Fewer than 5 new infections"
    } else {
      top <- max(over_5$inf)
      bottom <- min(over_5$inf)
      
      glue("{bottom} - {top}")
    }
  })
  
  # vaccinated case range - updating card text
  output$vacc_range <- renderText({
    results <- sim_results()
    cc <- case_counts(results)
    
    over_5 <- cc |> 
      filter(infections >= 5)
    
    if(nrow(over_5) == 0) {
      "Fewer than 5 new infections"
    } else {
      top <- max(over_5$inf_v)
      bottom <- min(over_5$inf_v)
      
      glue("{bottom} - {top}")  
    }
  })
  
  # unvaccinated case range - updating card text
  output$sim1_unvacc_range <- renderText({
    results <- sim_results()
    cc <- case_counts(results)
    
    over_5 <- cc |> 
      filter(infections >= 5)
    
    if(nrow(over_5) == 0) {
      "Fewer than 5 new infections"
    } else {
      top <- max(over_5$inf)
      bottom <- min(over_5$inf)
      
      glue("{bottom} - {top}")
    }
  })
  
  # vaccinated case range - updating card text
  output$sim1_vacc_range <- renderText({
    results <- sim_results()
    cc <- case_counts(results)
    
    over_5 <- cc |> 
      filter(infections >= 5)
    
    if(nrow(over_5) == 0) {
      "Fewer than 5 new infections"
    } else {
      top <- max(over_5$inf_v)
      bottom <- min(over_5$inf_v)
      
      glue("{bottom} - {top}")  
    }
  })

  # marginal simulation results
  sim_results_mar <- reactive({
    req(is.numeric(c(input$protected_mar, input$unprotected_mar)))

    waiver_row <- generate_fake_data(
      input$protected_mar, input$unprotected_mar
    ) |> 
      mutate(no_dose = n_student - complete - provisional)
    
    # defining parameters based on user input
    params <- measles_params()
  
    marginal_vax_sims(
      status_quo = waiver_row,
      params = params,
      inf_i = input$initial_infected_mar,
      t_max = round(input$outbreak_days)
    )
  }) |> 
    bindEvent(input$run_sim_mar)
  
  # Marginal vaccine plot
  output$marginal_vax_plot <- renderPlot({
    req(is.numeric(c(input$protected_mar, input$unprotected_mar)))

    p <- sim_results_mar() |> 
      marginal_vax_plot(name = input$bldg_mar)
    
    p +
      theme(
        legend.position = "bottom",
        plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 16),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        plot.caption = element_text(size = 14, hjust = 0, color = "gray33")
      )
  })
  
  # Marginal vaccine values
  output$mar_0 <- renderText({
    req(is.numeric(c(input$protected_mar, input$unprotected_mar)))

    sim_results_mar() |> 
      filter(day == 250) |> 
      filter(name == "status quo") |> 
      pull(avg_cum_infections) |> 
      round(digits = 1) |> 
      format(nsmall = 1) |> 
      str_c(" cases")
  })
  output$mar_1 <- renderText({
    req(is.numeric(c(input$protected_mar, input$unprotected_mar)))

    sim_results_mar() |> 
      filter(day == 250) |> 
      filter(name == "1 more dose") |> 
      pull(avg_cum_infections) |> 
      round(digits = 1) |> 
      format(nsmall = 1) |> 
      str_c(" cases")
  })
  output$mar_5 <- renderText({
    req(is.numeric(c(input$protected_mar, input$unprotected_mar)))

    sim_results_mar() |> 
      filter(day == 250) |> 
      filter(name == "5 more doses") |> 
      pull(avg_cum_infections) |> 
      round(digits = 1) |> 
      format(nsmall = 1) |> 
      str_c(" cases")
  })
  output$mar_10 <- renderText({
    req(is.numeric(c(input$protected_mar, input$unprotected_mar)))

    sim_results_mar() |> 
      filter(day == 250) |> 
      filter(name == "10 more doses") |> 
      pull(avg_cum_infections) |> 
      round(digits = 1) |> 
      format(nsmall = 1) |> 
      str_c(" cases")
  })
  
  # Render MDHHS table for this facility
  output$facility_tbl <- renderTable({
    req(input$school_mar != ".*")

    waiver_data |> 
      filter(name == input$school_mar) |> 
      select(-county, -no_dose) |> 
      rename(
        Facility = name,
        Students = n_student,
        Complete = complete,
        Provisional = provisional,
        Incomplete = incomplete,
        Waivers = n_waiver,
        Medical = n_medical,
        Religious = n_religious,
        Philosophical = n_phil
      ) |> 
      mutate(across(
        Students:Philosophical,
        \(num) num |> round() |> format()
      ))
  })
  
  # add invalid input warnings where necessary
  observe({
    vaccinated <- input$protected
    unvaccinated <- input$unprotected

    req(input$initial_infected)
    
    if (input$initial_infected > unvaccinated) {
      showFeedbackDanger(
        inputId = "initial_infected",
        text = "Invalid input: The number of initially infected students 
        cannot exceed the number of unvaccinated students. Please adjust."
      )
    } else if (input$initial_infected < 1) {
      showFeedbackDanger(
        inputId = "initial_infected",
        text = "Invalid input: The number of initially infected students must
        be 1 or greater."
      )
    } else {
      hideFeedback("initial_infected")
    }
  })
  observe({
    vaccinated <- input$protected_mar
    unvaccinated <- input$unprotected_mar

    req(input$initial_infected_mar)
    
    if (input$initial_infected_mar > unvaccinated) {
      showFeedbackDanger(
        inputId = "initial_infected_mar",
        text = "Invalid input: The number of initially infected students 
        cannot exceed the number of unvaccinated students. Please adjust."
      )
    } else if (input$initial_infected_mar < 1) {
      showFeedbackDanger(
        inputId = "initial_infected_mar",
        text = "Invalid input: The number of initially infected students must
        be 1 or greater."
      )
    } else {
      hideFeedback("initial_infected_mar")
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
