##############################################################################################
# Copyright © 2023 Lene J. Kjær, Michael P. Ward, Anette E. Boklund, Lars E. Larsen,
# Charlotte K. Hjulsager, and Carsten T. Kirkeby
#
# This file is part of the Shiny app for the ENIGMA HPAI model version 1.0.
# Licensed under GPLv3 or later. See <https://www.gnu.org/licenses/>.
##############################################################################################

## ---- PACKAGES ----
library(tidyverse)
library(sf)
library(surveillance)
library(shiny)
library(shinydashboard)
library(tsibble)
library(ggplot2)
library(ggpubr)
library(grid)
library(gridExtra)
library(spdep)
library(fanplot)
library(qs)
library(countrycode)
library(DT)
library(lubridate)
library(readxl)

## ---- LOAD NEWEST WOAH/WAHIS / MODEL OBJECTS ----
tt <- tempfile()
download.file("http://www.enigmahpai.org/ENIGMA2023/for_ai.car", tt, mode = "wb")
qs::qload(tt)
updateDate <- strftime(
  as.Date(substring(filename, 7, 14), format = "%Y%m%d"),
  format = "%d/%m/%Y"
)
file.remove(tt)

## ---- FOOTNOTE FUNCTION ----
makeFootnote <- function(footnoteText = format(Sys.time(), "%d %b %Y"),
                         size = .7, color = grey(.5)) {
  require(grid)
  pushViewport(viewport())
  grid.text(
    label = footnoteText,
    x = unit(1,"npc") - unit(2,"mm"),
    y = unit(1,"mm"),
    just = c("right","bottom"),
    gp = gpar(cex = size, col = color)
  )
  popViewport()
}

## =============================================================================
## >>>>>>>>>>>>>>>>>>>> ONLY NEW CODE STARTS HERE <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
## =============================================================================

## ---- MINIMAL FIX: EXTEND stsObj BY FUTURE WEEKS ----
extend_sts_weeks <- function(sts, n_ahead = 4) {
  
  old_epochs <- epoch(sts)
  last_epoch <- tail(old_epochs, 1)
  
  new_epochs <- seq(from = last_epoch + 1, length.out = n_ahead)
  
  new_counts <- matrix(
    NA_integer_,
    nrow = n_ahead,
    ncol = ncol(sts),
    dimnames = list(NULL, colnames(sts))
  )
  
  sts_ext <- sts
  sts_ext@observed <- rbind(observed(sts), new_counts)
  epoch(sts_ext) <- c(old_epochs, new_epochs)
  
  sts_ext
}

## =============================================================================
## >>>>>>>>>>>>>>>>>>>> ONLY NEW CODE ENDS HERE <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
## =============================================================================


## ---- UI ----
ui <- fluidPage(
  tags$head(includeHTML("head.html"), includeHTML("head_ljkjaer.html")),
  
  titlePanel(
    fluidRow(
      column(3, img(height = 150, width = 240, src = "ENIGMAlogo.png")),
      column(6, offset = 2, " "),
      column(6, offset = 2, "ENIGMA HPAI model version 1.1"),
      column(1, img(height = 160, width = 130, src = "ku_logo_uk_v.png"))
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      
      dateRangeInput(
        "dateRange",
        paste0(
          "Select Date Range (needs to be within ",
          strftime(minDate, "%d/%m/%Y"), " and ",
          strftime(endDate, "%d/%m/%Y"),
          ")."
        ),
        min   = as.Date(minDate),
        max   = as.Date(maxdate),
        start = as.Date(minDate),
        end   = as.Date(endDate),
        format = "dd/mm/yyyy"
      ),
      
      radioButtons(
        "graph","Select:",
        c(
          "Map of detections" = "hpai_map",
          "Timeseries"       = "hpai_timeseries",
          "Model fit"        = "country_modelfit",
          "Forecasting"      = "forecasting"
        ),
        selected = "forecasting"
      ),
      
      conditionalPanel(
        condition = "input.graph == 'country_modelfit'",
        selectInput(
          "predictions_options",
          "Select country",
          choices = districts2plot2,
          selected = "Summed all countries",
          selectize = FALSE
        )
      ),
      
      conditionalPanel(
        condition = "input.graph == 'forecasting'",
        selectInput(
          "forecasting_options",
          "Select country",
          choices = districts2plot2,
          selected = "Summed all countries"
        )
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Model output",
          downloadButton("downloadPlot", "Download"),
          plotOutput("graph", height = 500, width = 900),
          textOutput("info"),
          DTOutput("table")
        ),
        tabPanel("Model description")
      )
    )
  )
)

## ---- SERVER ----
server <- function(input, output) {
  
  start <- reactive({
    as.numeric(floor(difftime(input$dateRange[1], minDate, units = "weeks") + 1))
  })
  
  end <- reactive({
    as.numeric(floor(difftime(input$dateRange[2], minDate, units = "weeks") + 1))
  })
  
  ## ---- FORECASTING (MINIMALLY MODIFIED) ----
  sim <- reactive({
    
    ## >>> NEW: extend model time axis <<<
    model_ext <- final_model
    model_ext$stsObj <- extend_sts_weeks(final_model$stsObj, 4)
    
    startSim <- end() - 1
    endSim   <- startSim + 4
    
    y.start <- observed(final_model$stsObj)[startSim, ]
    
    simulate(
      model_ext,
      nsim   = 500,
      seed   = 1,
      subset = (startSim + 1):endSim,
      y.start = y.start
    )
  })
  
  ## ---- FORECAST PLOT ----
  AllCountry_forecasting <- reactive({
    plot(
      sim(),
      observed = FALSE,
      type = "fan",
      main = "Summed all countries",
      fan.args = list(ln = c(10, 50, 90)),
      xaxis = list(epochsAsDate = TRUE),
      par.settings = list(mar = c(7,5,4,2))
    )
    recordPlot()
  })
  
  output$graph <- renderPlot({
    if (input$graph == "forecasting") {
      replayPlot(req(AllCountry_forecasting()))
    }
  })
  
  output$info <- renderText({
    if (input$graph == "forecasting") {
      paste0(
        "Simulation-based 4 week forecast based on the modelling framework described in ",
        "Kjær et al. (2023). Forecasting starts one week before the end of the ",
        "selected period to account for reporting delays."
      )
    }
  })
  
}

## ---- RUN APP ----
shinyApp(ui, server)
