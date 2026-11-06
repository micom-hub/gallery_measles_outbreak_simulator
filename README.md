# Simulating measles outbreak outcomes in educational facilities

[Link to GitHub repo](https://github.com/micom-hub/gallery_measles_outbreak_simulator) (contact "micom-misupport -at- umich -dot- edu" for access)
Authored by Jack Jacobs, Stephanie Iorga, MI-Support, [MICOM](https://www.micom-insight.net/)

This repository contains code that allows users to run a local R Shiny app that is intended to facilitate conversations about measles risk management between local health department officials and educational facility administrators. It was developed primarily between July 2025 and January 2026.

This app was developed as a MI-Support project in collaboration with a Michigan local health department.

## How to run the Shiny app

### Environment setup

First, install R and RStudio. [Instructions are provided by Posit here.](https://posit.co/download/rstudio-desktop/)

Second, install R necessary R libraries by running the following code in [Rstudio's R console](https://docs.posit.co/ide/user/ide/guide/code/console.html):
```r
install.packages(c(
  "shiny","bslib","shinyWidgets","shinyFeedback","shinyjs","here","tidyverse",
  "glue","scales","slider","roxygen2","readxl","adaptivetau"
))
```

### Run the app

Open [app/app.R](app/app.R) in the [RStudio source pane](https://docs.posit.co/ide/user/ide/guide/ui/ui-panes.html), then click the "Run App" button in the top right of this pane.

For more detailed instructions on running an R Shiny app locally, see [Section 1.3 of the *Mastering Shiny* ebook](https://mastering-shiny.org/basic-app.html#running).
