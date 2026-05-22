## Copyright © 2023 Lene J. Kjær, Michael P. Ward, Anette E. Boklund, Lars E. Larsen, Charlotte K. Hjulsager, and Carsten T. Kirkeby

##############################################################################################
# This file is part of the Shiny app for the ENIGMA HPAI model.
# Interactive version: leaflet maps + plotly time-series + interactive model-fit + original surveillance fan plots + interactive forecast map. This version keeps the v14 layout/logo handling, starts on map of detections, fixes model-fit colours/legend/downloads, and suppresses console output from surveillance model-fit plots.
# Static PNG downloads are retained.
##############################################################################################

# ---- Packages ----
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
library(leaflet)
library(plotly)

#setwd("C:/Users/hzs315/Documents/ckirkeby.github.io/ENIGMA2023/")

# ---- Read newest model/data object ----
set_trust_promises(TRUE)
tt <- tempfile()
download.file("http://www.enigmahpai.org/ENIGMA2023/for_ai.car", tt, mode = "wb")
qs::qload(tt)
updateDate <- strftime(as.Date(substring(filename, 7, 14), format = "%Y%m%d"), format = "%d/%m/%Y")
file.remove(tt)

# ---- Helper functions ----
makeFootnote <- function(footnoteText = format(Sys.time(), "%d %b %Y"),
                         size = .7,
                         color = grey(.5)) {
  require(grid)
  pushViewport(viewport())
  grid.text(
    label = footnoteText,
    x = unit(1, "npc") - unit(2, "mm"),
    y = unit(1, "mm"),
    just = c("right", "bottom"),
    gp = gpar(cex = size, col = color)
  )
  popViewport()
}

safe_max <- function(x, add = 0) {
  out <- suppressWarnings(max(x, na.rm = TRUE))
  if (!is.finite(out)) out <- 0
  out + add
}

# Convert sequential week index in selected range to approximate dates.
# This is mainly used for plotly x-axis labels.
week_dates_from_range <- function(date_start, n) {
  as.Date(date_start) + weeks(seq_len(n) - 1)
}

# Extract the exact fitted component means used by surveillance::plot(..., type = "fitted"),
# without drawing the base R surveillance plot and without printing anything to the console.
# This follows the relevant part of surveillance:::plotHHH4_fitted1():
#   meanHHH(model$coefficients, terms(model))
# followed by either rowSums() over all countries or extraction of a single country.
extract_hhh4_fitted_components <- function(model, total = TRUE, unit = NULL,
                                           start_vec = NULL, end_vec = NULL,
                                           hide0s = TRUE,
                                           plot_name = NULL) {
  stopifnot(inherits(model, "hhh4"))

  stsObj <- model$stsObj
  unit_names <- colnames(stsObj)

  # Match a country/unit name to its column index if needed.
  if (!isTRUE(total)) {
    if (is.null(unit)) stop("A unit must be supplied when total = FALSE.")
    if (is.character(unit)) {
      unit_idx <- match(unit, unit_names)
      if (is.na(unit_idx)) stop("Selected unit '", unit, "' was not found in model$stsObj.")
    } else {
      unit_idx <- as.integer(unit)
      if (is.na(unit_idx) || unit_idx < 1 || unit_idx > length(unit_names)) {
        stop("Selected unit index is outside the available model units.")
      }
    }
  }

  obs_all <- if (isTRUE(total)) {
    rowSums(observed(stsObj), na.rm = TRUE)
  } else {
    observed(stsObj)[, unit_idx]
  }

  start0 <- surveillance:::yearepoch2point(
    stsObj@start,
    stsObj@freq,
    toleft = TRUE
  )

  tp <- start0 + seq_along(obs_all) / stsObj@freq

  start_num <- if (is.null(start_vec)) {
    start0
  } else {
    surveillance:::yearepoch2point(start_vec, stsObj@freq)
  }

  end_num <- if (is.null(end_vec)) {
    tp[length(tp)]
  } else {
    surveillance:::yearepoch2point(end_vec, stsObj@freq)
  }

  tp_in_range <- which(tp >= start_num & tp <= end_num)

  # This returns the same fitted component matrices used by the surveillance plot:
  # endemic, epi.own and epi.neighbours.
  mean_hhh <- surveillance:::meanHHH(model$coefficients, terms(model))

  comp <- if (isTRUE(total)) {
    as.data.frame(sapply(mean_hhh, rowSums, na.rm = TRUE))
  } else {
    as.data.frame(sapply(mean_hhh, "[", i = TRUE, j = unit_idx))
  }

  keep <- model$control$subset %in% tp_in_range
  comp <- comp[keep, , drop = FALSE]

  nml <- tolower(names(comp))
  get_col <- function(patterns, fallback) {
    hits <- unique(unlist(lapply(patterns, function(z) grep(z, nml, perl = TRUE))))
    if (length(hits) > 0) hits[1] else fallback
  }

  idx_end <- get_col(c("(^|\\.)endemic$", "endemic", "^end$"), 1)
  idx_own <- get_col(c("epi\\.own", "own", "within", "^ar$", "autoregressive"), min(2, ncol(comp)))
  idx_ne  <- get_col(c("epi\\.neigh", "neigh", "neighbor", "between", "^ne$"), min(3, ncol(comp)))

  data.frame(
    endemic = as.numeric(comp[[idx_end]]),
    within = as.numeric(comp[[idx_own]]),
    between = as.numeric(comp[[idx_ne]])
  )
}

# ---- UI asset helpers ----
# When using runApp("path/to/app.R"), Shiny does not always use the script folder
# as the working directory. Therefore we explicitly register the www folder.
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || is.na(x)) y else x

get_app_dir <- function() {
  ofiles <- vapply(sys.frames(), function(x) x$ofile %||% NA_character_, character(1))
  ofiles <- ofiles[!is.na(ofiles)]
  if (length(ofiles) > 0) {
    return(dirname(normalizePath(tail(ofiles, 1), winslash = "/", mustWork = FALSE)))
  }
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

app_dir <- get_app_dir()
www_candidates <- unique(c(
  file.path(app_dir, "www"),
  file.path(dirname(app_dir), "www"),
  file.path(getwd(), "www"),
  file.path(dirname(getwd()), "www"),
  "C:/Users/hzs315/Documents/ckirkeby.github.io/ENIGMA2023/www",
  "C:/Users/hzs315/Documents/GitHub/ckirkeby.github.io/ENIGMA2023/www",
  "C:/Users/zxd598/Documents/GitHub/ckirkeby.github.io/ENIGMA2023/www"
))
www_dir <- www_candidates[file.exists(www_candidates) & dir.exists(www_candidates)]
www_dir <- if (length(www_dir) > 0) www_dir[1] else NA_character_

if (length(www_dir) > 0 && !is.na(www_dir)) {
  try(addResourcePath("enigma_www", normalizePath(www_dir, winslash = "/", mustWork = TRUE)), silent = TRUE)
}

asset_path <- function(src) {
  if (length(www_dir) > 0 && !is.na(www_dir) && file.exists(file.path(www_dir, src))) {
    paste0("enigma_www/", src)
  } else if (file.exists(src)) {
    src
  } else if (file.exists(file.path("www", src))) {
    src
  } else {
    NA_character_
  }
}

logo_img <- function(src, height = NULL, width = NULL, fallback = "ENIGMA", class = "logo-fallback") {
  resolved <- asset_path(src)
  if (!is.na(resolved)) {
    img(src = resolved, height = height, width = width, class = class)
  } else {
    fallback_class <- if (identical(class, "sidebar-logo")) "sidebar-logo-fallback" else class
    div(class = fallback_class, fallback)
  }
}

# ---- UI ----
ui <- fluidPage(
  tags$head(
    if (file.exists("head.html")) includeHTML("head.html"),
    if (file.exists("head_ljkjaer.html")) includeHTML("head_ljkjaer.html"),
    tags$style(HTML("
      body { background: #f4f6f8; color: #24313f; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Arial, sans-serif; }
      .container-fluid { padding: 0 24px 24px 24px; }
      .app-hero { margin: 18px 0 18px 0; padding: 0; border-radius: 22px; background: linear-gradient(105deg, #7b1f24 0%, #5b171c 36%, #f8fafc 100%); color: white; box-shadow: 0 16px 38px rgba(31,41,51,0.18); overflow: hidden; }
      .app-hero .row { margin-left: 0; margin-right: 0; }
      .app-hero .col-sm-8 { background: transparent; padding: 24px 28px; min-height: 142px; }
      .app-hero .col-sm-4 { background: transparent; padding: 20px 28px; min-height: 142px; display: flex; align-items: center; justify-content: flex-end; }
      .hero-title { font-size: 34px; font-weight: 800; letter-spacing: 0.2px; margin: 0; }
      .hero-subtitle { font-size: 15px; opacity: 0.9; margin-top: 8px; max-width: 900px; }
      .hero-meta { margin-top: 12px; font-size: 13px; opacity: 0.92; }
      .hero-logos { display: flex; gap: 22px; align-items: center; justify-content: flex-end; flex-wrap: wrap; width: 100%; }
      .hero-logos img { object-fit: contain; background: transparent !important; padding: 0 !important; border-radius: 0 !important; box-shadow: none !important; border: 0 !important; mix-blend-mode: multiply; }
      .logo-fallback, .ku-fallback, .small-logo-fallback { display: inline-flex; align-items: center; justify-content: center; border-radius: 16px; background: rgba(255,255,255,0.14); color: white; border: 1px solid rgba(255,255,255,0.28); font-weight: 800; text-align: center; }
      .logo-fallback { width: 230px; height: 112px; font-size: 30px; }
      .ku-fallback { width: 118px; height: 112px; font-size: 20px; }
      .small-logo-fallback { width: 130px; height: 44px; font-size: 12px; color: #7b1f24; background: #fff; border: 1px solid #ead7d7; }
      .well { background: white; border: 0; border-radius: 20px; box-shadow: 0 10px 28px rgba(31,41,51,0.10); padding: 18px; }
      .main-panel-card { background: white; border-radius: 20px; padding: 18px; box-shadow: 0 10px 28px rgba(31,41,51,0.10); }
      .nav-tabs { border-bottom: 1px solid #e5e7eb; }
      .nav-tabs > li > a { border-radius: 12px 12px 0 0; color: #7b1f24; font-weight: 600; }
      .nav-tabs > li.active > a, .nav-tabs > li.active > a:focus, .nav-tabs > li.active > a:hover { color: #24313f; border: 1px solid #e5e7eb; border-bottom-color: white; }
      .btn-default { border-radius: 12px; border: 1px solid #d8dee5; background: #fff; box-shadow: 0 3px 8px rgba(31,41,51,0.08); font-weight: 600; }
      .form-control, .selectize-input { border-radius: 10px; border-color: #d8dee5; }
      .control-label { font-weight: 700; color: #24313f; }
      .plot-card { background: #ffffff; border: 1px solid #edf0f2; border-radius: 18px; padding: 14px; box-shadow: inset 0 0 0 1px rgba(255,255,255,0.5); }
      .plot-caption { margin-top: 14px; font-size: 14px; line-height: 1.45; color: #334155; }
      .leaflet-container { background: #f7f7f7; border-radius: 14px; }
      .sidebar-section { padding: 12px 0; border-top: 1px solid #ead7d7; margin-top: 14px; }
      .disclaimer { font-size: 12px; line-height: 1.35; color: #536271; }
      .logo-row { display: flex; gap: 10px; align-items: center; flex-wrap: wrap; margin-top: 8px; }
      .logo-row img.sidebar-logo { display: inline-block !important; object-fit: contain !important; object-position: left center !important; background: transparent !important; border: 0 !important; box-shadow: none !important; border-radius: 0 !important; padding: 0 !important; mix-blend-mode: normal !important; opacity: 1 !important; max-width: 100% !important; width: auto !important; }
      .logo-row .sidebar-logo-fallback { display: inline-flex; align-items: center; justify-content: center; border-radius: 10px; color: #7b1f24; background: #fff; border: 1px solid #ead7d7; font-weight: 800; font-size: 12px; width: 140px; height: 44px; text-align: center; }
    "))
  ),

  div(
    class = "app-hero",
    fluidRow(
      column(
        8,
        h1(class = "hero-title", "ENIGMA HPAI model"),
        div(class = "hero-subtitle", "Interactive surveillance outputs for highly pathogenic avian influenza detections and short-term forecasts in Europe."),
        div(class = "hero-meta", "Model version 1.2")
      ),
      column(
        4,
        div(
          class = "hero-logos",
          logo_img("ENIGMAlogo.png", height = 132, width = 225, fallback = "ENIGMA", class = "logo-fallback"),
          logo_img("ku_logo_uk_v.png", height = 132, width = 112, fallback = "KU", class = "ku-fallback")
        )
      )
    )
  ),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      dateRangeInput(
        "dateRange",
        paste0(
          "Select Date Range (needs to be within ",
          strftime(minDate, format = "%d/%m/%Y"),
          " and ",
          strftime(endDate, format = "%d/%m/%Y"),
          ", see model description for details about date range)."
        ),
        min = as.Date(minDate),
        max = as.Date(maxdate),
        start = minDate,
        end = strftime(endDate, format = "%Y-%m-%d"),
        format = "dd/mm/yyyy"
      ),

      radioButtons(
        "graph",
        "Select:",
        c(
          "Map of detections" = "hpai_map",
          "Timeseries" = "hpai_timeseries",
          "Model fit" = "country_modelfit",
          "Forecasting" = "forecasting"
        ),
        selected = "hpai_map"
      ),

      conditionalPanel(
        condition = "input.graph == 'hpai_map'",
        radioButtons(
          "map_layer",
          "Map layer:",
          c(
            "Total detections" = "total",
            "Detections per 10,000 km²" = "area",
            "Detection locations" = "points"
          ),
          selected = "total"
        )
      ),

      conditionalPanel(
        condition = "input.graph == 'country_modelfit'",
        selectInput(
          "predictions_options",
          "Country selection (Model fit and Forecast fan chart only)",
          choices = districts2plot2,
          selected = "Summed all countries",
          selectize = FALSE
        )
      ),

      conditionalPanel(
        condition = "input.graph == 'forecasting'",
        selectInput(
          "forecasting_options",
          "Country selection (Model fit and Forecast fan chart only)",
          choices = districts2plot2,
          selected = "Summed all countries"
        ),
        radioButtons(
          "forecast_display",
          "Forecast display:",
          c("Forecast map" = "map", "Forecast fan chart" = "fan"),
          selected = "map"
        ),
        conditionalPanel(
          condition = "input.forecast_display == 'map'",
          sliderInput(
            "forecast_week",
            "Forecast week:",
            min = 1, max = 4, value = 1, step = 1,
            animate = animationOptions(interval = 1200, loop = TRUE)
          ),
          radioButtons(
            "forecast_metric",
            "Map value:",
            c("Median forecast" = "q50", "Mean forecast" = "mean", "90% quantile" = "q90"),
            selected = "q50"
          )
        )
      ),

      HTML('<hr style = "border-color: #800000; height: 5px;width: 100%">'),
      div(
        span("The purpose of this webpage is to show the predictions of the ENIGMA HPAI model. This model has been developed during research at the University of Copenhagen in "),
        span(a("the ENIGMA project, ", href = "https://ivh.ku.dk/forskning/dyrevelfaerd-og-sygdomsbekaempelse/projektside/enigma/", target = "_blank")),
        span("2021-2024. The ENIGMA project is based in "),
        span(a("the Avian Influenza Epidemiology Subgroup ", href = "https://ivh.ku.dk/english/research/animal-welfare-and-disease-control/avian-influenza-epidemiology/", target = "_blank")),
        span("at the University of Copenhagen and is part of the "),
        span(a("Danish Veterinary Contingency Consortium (DKVET) ", href = "https://dkvet.dk/english/about/", target = "_blank")),
        span("funded by the Danish Food and Veterinary Administration. R code and data used for the ENIGMA HPAI model is available at "),
        span(a("GitHub.", href = "https://github.com/ckirkeby/ckirkeby.github.io/tree/main/ENIGMA2023", target = "_blank"))
      ),
      br(),
      h5(HTML("<i>Disclaimer: The model and data reflects the state of knowledge available on the date of dispatch. The University of Copenhagen and DKVET cannot be held liable for any errors, inaccuracies or inconsistencies with regard to text and/or data contained therein. Therefore, the University of Copenhagen and DKVET accept no responsibility or liability arising out of, or in connection with the information provided.</i>")),
      br(),
      h5(HTML("The ENIGMA project is co-funded by the European Union through the European Partnership on Animal Health and Welfare")),
      div(
        class = "logo-row",
        logo_img("EN-Co-Funded-by-the-EU_POS-1024x215.png", height = 40, width = NULL, fallback = "Co-funded by EU", class = "sidebar-logo"),
        logo_img("EUP AH&W_color.png", height = 54, width = NULL, fallback = "EUP AH&W", class = "sidebar-logo")
      ),
      HTML('<hr style = "border-color: #800000; height: 5px;width: 100%">'),
      strong(a("Contact Admin", href = "mailto:ckir@sund.ku.dk")),
      br(), br(),
      p(strong(em(paste0("WOAH-WAHIS data and model updated: ", updateDate)))),
      br(),
      p("Copyright © 2023 Lene J. Kjær, Michael P. Ward, Anette E. Boklund, Lars E. Larsen, Charlotte K. Hjulsager, and Carsten T. Kirkeby", style = "font-size:80%;"),
      br(),
      p("THE SOFTWARE IS PROVIDED AS IS AND, TO THE MAXIMUM EXTENT PERMITTED UNDER APPLICABLE LAW, WE PROVIDE NO WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, BY STATUTE OR OTHERWISE, INCLUDING BUT NOT LIMITED TO ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, TITLE, OR NON-INFRINGMENT. WE DO NOT WARRANT THAT THE SOFTWARE WILL FUNCTION UNINTERRUPTED, THAT IT WILL MEET ANY REGULATORY REQUIREMENTS, THAT IT IS ERROR-FREE, OR THAT ANY ERRORS WILL BE CORRECTED.", style = "font-size:80%;"),
      br(),
      div(
        logo_img("gplv3-with-text-136x68.png", height = 40, width = NULL, fallback = "GPLv3", class = "sidebar-logo"),
        br(),
        span("The ENIGMA HPAI model is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. The ENIGMA HPAI model is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See ", style = "font-size:80%;"),
        span(a("the GNU General Public License", href = "https://www.gnu.org/licenses/gpl-3.0.html", target = "_blank", style = "font-size:80%;")),
        span(" for more details.", style = "font-size:80%;")
      )
    ),

    mainPanel(
      width = 9,
      class = "main-panel-card",
      tabsetPanel(
        tabPanel(
          "Model output",
          br(),
          downloadButton("downloadPlot", "Download static PNG"),
          br(), br(),
          div(class = "plot-card", uiOutput("interactive_output")),
          div(class = "plot-caption", textOutput("info")),
          DTOutput("table")
        ),

        tabPanel(
          "Model description",
          h3("Endemic-epidemic modelling of highly pathogenic avian influenza in Europe"),
          p("The ENIGMA HPAI model results and graphs presented in this shiny app are based on the modelling framework described in Kjær et al. (2023). In this study, we utilized readily available data from the World Organization for Animal Health (WOAH-WAHIS)* on highly pathogenic avian influenza (HPAI) H5 detections in wild and domestic birds together with a time-series modelling framework (Meyer et al. 2014) to predict HPAI H5 detections within Europe. This framework decomposes time series data into endemic and epidemic components, where the epidemic component can take into account within-country transmission and between-country transmission as well as short-distance (from directly neighbouring countries) and long-distance (transmission follows a distance-decay algorithm) transmission."),
          p("The WOAH-WAHIS data have revealed several shifts in the seasonality since 2016, thus we only use recent data to reflect the newest transmission patterns. The results presented here are from a model fitted to data from the latest 1 1/4 year (to ensure enough data for model fitting) that includes long-range transmission, and seasonality in both the endemic and epidemic component (one seasonal wave for each component)."),
          p("Due to the model being based on the most recent HPAI H5 detection data, the latest date that can be chosen is the last date, from which we have HPAI H5 data from WOAH-WAHIS (updated weekly), and the earliest date that can be chosen in this app is 1 1/4 year before the last date. Forecasting will always be 4 weeks ahead from the week before the last date chosen (to account for delays in WOAH-WAHIS reporting). For more details see Kjær et al. (2023) and Meyer et al. (2014)."),
          br(), br(),
          p(a("Kjær, L. J., M. P. Ward, A. E. Boklund, L. E. Larsen, C. K. Hjulsager, and C. T. Kirkeby. Using surveillance data for early modelling of highly pathogenic avian influenza in Europe reveals a seasonal shift in transmission, 2016-2022. 2023. Scientific Reports 13:15396.", href = "https://doi.org/10.1038/s41598-023-42660-7", target = "_blank", style = "font-size:85%;")),
          p(a("Meyer, S., L. Held, and M. Höhle. 2014. Spatio-Temporal Analysis of Epidemic Phenomena Using the R Package surveillance. J. Stat. Softw. 77.", href = "https://doi.org/10.18637/jss.v077.i11", target = "_blank", style = "font-size:85%;")),
          br(), br(),
          div(
            h4(HTML("<b>*Copyright of data extracted from WAHIS</b>"), style = "background-color: #800000; width: 100%; color:white;font-size:85%;"),
            h5(HTML("<b>Disclaimer and caption</b>"), style = "text-align:left;background-color: #ffffff;width: 50%;font-size:85%;"),
            p(HTML("<i>Data extracted by Lene Jung Kjær and Carsten Kirkeby, University of Copenhagen. WOAH bears no responsibility for the integrity or accuracy of the data contained herein, including but not limited to any deletion, manipulation, or reformatting of data that may have occurred beyond its control.</i>"), style = "text-align:justify;background-color: #ffffff;width: 100%;font-size:85%;")
          )
        )
      )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {

  # ---- Reactive date helpers ----
  startWeek <- reactive({
    startweekno <- isoweek(input$dateRange[1])
    startyearno <- isoyear(input$dateRange[1])
    if (startyearno == 2023 && startweekno == 53) {
      if (input$dateRange[1] < "2024-01-01") startweekno <- 52
      if (input$dateRange[1] >= "2024-01-01") startweekno <- 1
    }
    as.numeric(startweekno)
  })

  startYear <- reactive({
    startyearno <- isoyear(input$dateRange[1])
    startweekno <- isoweek(input$dateRange[1])
    if (startyearno == 2023 && startweekno == 53) {
      if (input$dateRange[1] < "2024-01-01") startyearno <- 2023
      if (input$dateRange[1] >= "2024-01-01") startyearno <- 2024
    }
    as.numeric(startyearno)
  })

  endWeek <- reactive({
    endweekno <- isoweek(input$dateRange[2])
    endyearno <- isoyear(input$dateRange[2])
    if (endyearno == 2023 && endweekno == 53) {
      if (input$dateRange[2] < "2024-01-01") endweekno <- 52
      if (input$dateRange[2] >= "2024-01-01") endweekno <- 1
    }
    as.numeric(endweekno)
  })

  endYear <- reactive({
    endyearno <- isoyear(input$dateRange[2])
    endweekno <- isoweek(input$dateRange[2])
    if (endyearno == 2023 && endweekno == 53) {
      if (input$dateRange[2] < "2024-01-01") endyearno <- 2023
      if (input$dateRange[2] >= "2024-01-01") endyearno <- 2024
    }
    as.numeric(endyearno)
  })

  startYearWeek <- reactive({
    yearweek(paste0(startYear(), " W", startWeek()))
  })

  endYearWeek <- reactive({
    yearweek(paste0(endYear(), " W", endWeek()))
  })

  start <- reactive({
    as.numeric(floor(difftime(input$dateRange[1], minDate, units = "weeks") + 1))
  })

  end <- reactive({
    as.numeric(floor(difftime(input$dateRange[2], minDate, units = "weeks") + 1))
  })

  # ---- Data for maps and tables ----
  europe_mapData1 <- reactive({
    europe_data_weekly %>%
      filter(yearWeek >= startYearWeek(), yearWeek <= endYearWeek()) %>%
      group_by(ADM0_A3) %>%
      summarise(no_outbreaks = sum(no_outbreaks, na.rm = TRUE), .groups = "drop")
  })

  europe_mapData <- reactive({
    t1 <- merge(europeanCountries, europe_mapData1(), by = "ADM0_A3", all.x = TRUE)
    t1$no_outbreaks[is.na(t1$no_outbreaks)] <- 0
    t1$outbreaksArea <- t1$no_outbreaks / (t1$area_sqkm / 10000)
    st_as_sf(t1)
  })

  europe_data_sf <- reactive({
    europe_data %>%
      filter(yearWeek >= startYearWeek(), yearWeek <= endYearWeek()) %>%
      mutate(Longitude = as.numeric(Longitude), Latitude = as.numeric(Latitude)) %>%
      filter(!is.na(Longitude), !is.na(Latitude), Latitude < 74.0) %>%
      st_as_sf(coords = c("Longitude", "Latitude"), crs = "EPSG:4326")
  })

  # ---- Simulation for forecasting ----
  sim <- reactive({
    startSim <- min(end() - 1)
    endSim <- min(startSim + 4)
    y.start <- observed(AI_sts)[startSim, ]
    simulate(
      final_model,
      nsim = 500,
      seed = 1,
      subset = (startSim + 1):endSim,
      y.start = y.start
    )
  })

  # ---- Interactive output selector ----
  output$interactive_output <- renderUI({
    if (input$graph == "hpai_map") {
      leafletOutput("hpai_leaflet", height = 720)
    } else if (input$graph == "hpai_timeseries") {
      plotlyOutput("hpai_plotly", height = 720)
    } else if (input$graph == "country_modelfit") {
      plotlyOutput("model_fit_plotly", height = 760)
    } else if (input$graph == "forecasting" && identical(input$forecast_display, "fan")) {
      plotOutput("hpai_static", height = 760, width = "100%")
    } else if (input$graph == "forecasting") {
      leafletOutput("forecast_leaflet", height = 720)
    } else {
      plotOutput("hpai_static", height = 760, width = "100%")
    }
  })

  # ---- Interactive leaflet map ----
  output$hpai_leaflet <- renderLeaflet({
    map_data <- st_transform(europe_mapData(), 4326)
    point_data <- st_transform(europe_data_sf(), 4326)

    selected_var <- if (input$map_layer == "area") "outbreaksArea" else "no_outbreaks"
    selected_title <- if (input$map_layer == "area") "HPAI / 10,000 km²" else "HPAI total"

    map_data$selected_value <- map_data[[selected_var]]

    pal <- colorNumeric(
      palette = "Reds",
      domain = map_data$selected_value,
      na.color = "transparent"
    )

    labels <- sprintf(
      "<strong>%s</strong><br/>HPAI detections: %s<br/>HPAI / 10,000 km²: %s",
      map_data$ADM0_A3,
      map_data$no_outbreaks,
      round(map_data$outbreaksArea, 2)
    ) %>% lapply(htmltools::HTML)

    m <- leaflet(map_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(selected_value),
        fillOpacity = if (input$map_layer == "points") 0.15 else 0.75,
        color = "black",
        weight = 1,
        opacity = 1,
        label = labels,
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#444444",
          bringToFront = TRUE
        )
      )

    if (input$map_layer == "points" && nrow(point_data) > 0) {
      m <- m %>%
        addCircleMarkers(
          data = point_data,
          radius = 3,
          stroke = FALSE,
          fillColor = "darkred",
          fillOpacity = 0.65,
          group = "Detection locations"
        )
    }

    if (input$map_layer != "points") {
      m <- m %>% addLegend(
        pal = pal,
        values = map_data[[selected_var]],
        title = selected_title,
        position = "topright"
      )
    }

    m
  })

  # ---- Interactive plotly graphs ----
  # Timeseries: weekly bars with the earlier Plotly range-slider overview.
  # Month/year labels are applied to the x-axis; the bars themselves are unchanged.
  hpai_timeseries_plotly <- reactive({
    obs <- observed(AI_sts[start():end(), , drop = FALSE])
    y <- rowSums(obs, na.rm = TRUE)
    df <- tibble(
      date = week_dates_from_range(input$dateRange[1], length(y)),
      detections = y,
      hover = paste0("Week starting: ", format(date, "%d %b %Y"), "<br>Detections: ", detections)
    )

    plot_ly(
      df,
      x = ~date,
      y = ~detections,
      type = "bar",
      marker = list(color = "#5b5b5b"),
      hovertemplate = "Week starting: %{x|%d %b %Y}<br>Detections: %{y}<extra></extra>",
      name = "Detections"
    ) %>%
      layout(
        dragmode = "zoom",
        showlegend = FALSE,
        margin = list(l = 70, r = 25, t = 25, b = 90),
        yaxis = list(title = "No. detected cases", fixedrange = FALSE),
        xaxis = list(
          title = "",
          type = "date",
          tickformat = "%b\n%Y",
          dtick = "M1",
          tickangle = -45,
          showgrid = TRUE,
          rangeslider = list(
            visible = TRUE,
            thickness = 0.18,
            bgcolor = "rgba(245,245,245,0.85)",
            bordercolor = "rgba(180,180,180,0.8)",
            borderwidth = 1
          )
        )
      ) %>%
      config(displaylogo = FALSE, scrollZoom = TRUE)
  })

  # Interactive model fit.
  # This recreates the original surveillance fitted-component plot with ggplot/plotly.
  # The base surveillance plot is drawn on a temporary PNG device and suppressed;
  # only the interactive ggplotly version is shown in the Shiny app.
  model_fit_data_plotly <- reactive({
    idx <- start():end()

    is_total <- identical(input$predictions_options, "Summed all countries")

    if (is_total) {
      title_txt <- "Summed all countries"
      comp <- extract_hhh4_fitted_components(
        final_model,
        total = TRUE,
        start_vec = c(startYear(), startWeek()),
        end_vec = c(endYear(), endWeek() + 0.1),
        hide0s = TRUE,
        plot_name = title_txt
      )
      observed_y <- rowSums(observed(final_model$stsObj[idx, , drop = FALSE]), na.rm = TRUE)
    } else {
      unit <- which(districts2plot2 == input$predictions_options)
      col_id <- districts2plot[unit]
      title_txt <- districts2plot2[unit]
      comp <- extract_hhh4_fitted_components(
        final_model,
        total = FALSE,
        unit = col_id,
        start_vec = c(startYear(), startWeek()),
        end_vec = c(endYear(), endWeek() + 0.1),
        hide0s = TRUE,
        plot_name = title_txt
      )
      observed_y <- observed(final_model$stsObj)[idx, col_id]
    }

    # Use the Shiny date range for the weekly x-axis. Align defensively with the
    # number of fitted component rows returned by surveillance.
    dates <- as.Date(minDate) + weeks(idx - 1)
    n <- min(nrow(comp), length(dates), length(observed_y))
    comp <- comp[seq_len(n), , drop = FALSE]
    dates <- dates[seq_len(n)]
    observed_y <- observed_y[seq_len(n)]

    comp_wide <- tibble(
      date = dates,
      endemic = as.numeric(comp$endemic),
      within = as.numeric(comp$within),
      between = as.numeric(comp$between)
    ) %>%
      mutate(
        cum_endemic = endemic,
        cum_within = endemic + within,
        cum_between = endemic + within + between
      )

    obs_df <- tibble(date = dates, observed = as.numeric(observed_y))

    ymax <- max(c(comp_wide$cum_between, observed_y), na.rm = TRUE)
    if (!is.finite(ymax)) ymax <- 1

    list(title = title_txt, components = comp_wide, observed = obs_df, ymax = ymax * 1.30)
  })

  model_fit_static_ggplot <- reactive({
    d <- model_fit_data_plotly()

    comp_long <- d$components %>%
      select(date, endemic, within, between) %>%
      pivot_longer(
        cols = c(endemic, within, between),
        names_to = "component",
        values_to = "value"
      ) %>%
      mutate(
        component = factor(
          component,
          levels = c("between", "within", "endemic"),
          labels = c("Epidemic between", "Epidemic within", "Endemic")
        )
      )

    ggplot() +
      geom_area(
        data = comp_long,
        aes(x = date, y = value, fill = component),
        position = "stack",
        colour = NA,
        alpha = 1
      ) +
      geom_point(
        data = d$observed %>% filter(observed > 0),
        aes(x = date, y = observed),
        colour = "black",
        size = 1.3
      ) +
      scale_fill_manual(
        values = c(
          "Endemic" = "brown2",
          "Epidemic within" = "#4292C6",
          "Epidemic between" = "orange"
        ),
        breaks = c("Epidemic between", "Epidemic within", "Endemic")
      ) +
      scale_y_continuous(limits = c(0, d$ymax), expand = expansion(mult = c(0, 0.02))) +
      scale_x_date(date_breaks = "3 months", date_labels = "%Y\n%b") +
      labs(title = d$title, x = "Years and quarters", y = "No. detected cases", fill = NULL) +
      theme_classic(base_size = 16) +
      theme(
        legend.position = "top",
        plot.title = element_text(face = "bold", hjust = 0.5),
        panel.grid.minor = element_blank()
      )
  })

  output$model_fit_plotly <- renderPlotly({
    d <- model_fit_data_plotly()
    comp <- d$components
    obs <- d$observed %>% filter(observed > 0)

    plot_ly() %>%
      add_trace(
        data = comp,
        x = ~date,
        y = ~endemic,
        type = "scatter",
        mode = "lines",
        stackgroup = "fit",
        line = list(width = 0.5, color = "brown2"),
        fillcolor = "rgba(238, 59, 59, 0.85)",
        name = "Endemic",
        customdata = ~round(endemic, 1),
        hovertemplate = paste(
          "Date: %{x|%d %b %Y}",
          "<br>Endemic: %{customdata}",
          "<extra></extra>"
        )
      ) %>%
      add_trace(
        data = comp,
        x = ~date,
        y = ~within,
        type = "scatter",
        mode = "lines",
        stackgroup = "fit",
        line = list(width = 0.5, color = "#4292C6"),
        fillcolor = "rgba(66, 146, 198, 0.85)",
        name = "Epidemic within",
        customdata = ~round(within, 1),
        hovertemplate = paste(
          "Date: %{x|%d %b %Y}",
          "<br>Epidemic within: %{customdata}",
          "<extra></extra>"
        )
      ) %>%
      add_trace(
        data = comp,
        x = ~date,
        y = ~between,
        type = "scatter",
        mode = "lines",
        stackgroup = "fit",
        line = list(width = 0.5, color = "orange"),
        fillcolor = "rgba(255, 165, 0, 0.85)",
        name = "Epidemic between",
        customdata = ~round(between, 1),
        hovertemplate = paste(
          "Date: %{x|%d %b %Y}",
          "<br>Epidemic between: %{customdata}",
          "<extra></extra>"
        )
      ) %>%
      add_markers(
        data = obs,
        x = ~date,
        y = ~observed,
        name = "Observed",
        marker = list(color = "black", size = 6),
        hovertemplate = paste(
          "Date: %{x|%d %b %Y}",
          "<br>Observed detections: %{y}",
          "<extra></extra>"
        )
      ) %>%
      layout(
        title = list(text = d$title, x = 0.5, y = 0.98, font = list(size = 24)),
        dragmode = "zoom",
        margin = list(l = 90, r = 35, t = 120, b = 110),
        xaxis = list(
          title = "Years and quarters",
          type = "date",
          tickformat = "%Y<br>%b",
          dtick = "M3",
          rangeslider = list(visible = TRUE)
        ),
        yaxis = list(
          title = "No. detected cases",
          range = c(0, d$ymax),
          fixedrange = FALSE
        ),
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = 1.06,
          yanchor = "bottom",
          traceorder = "normal",
          font = list(size = 13)
        ),
        hovermode = "x unified"
      ) %>%
      config(displaylogo = FALSE, scrollZoom = TRUE)
  })

  # Helper: extract simulation array as time x country x simulation.
  # surveillance::simulate.hhh4(..., nsim > 1, simplify = TRUE) returns an
  # object of class "hhh4sims", which is an array with dimensions:
  # forecast week x country/unit x simulation.
  forecast_sim_array <- reactive({
    sim_obj <- sim()
    arr <- if (is.array(sim_obj)) unclass(sim_obj) else NULL
    if (is.null(arr) || length(dim(arr)) != 3) {
      arr <- tryCatch(as.array(sim_obj), error = function(e) NULL)
    }
    validate(need(!is.null(arr) && length(dim(arr)) == 3,
                  "Could not extract simulations as a forecast week x country x simulation array."))
    arr
  })

  # Helper to turn the simulation object into a matrix with rows = forecast weeks
  # and columns = simulation runs. Used for the fan chart.
  forecast_sim_matrix <- reactive({
    arr <- forecast_sim_array()

    if (input$forecasting_options == "Summed all countries") {
      out <- apply(arr, c(1, 3), sum, na.rm = TRUE)
    } else {
      unit <- which(districts2plot2 == input$forecasting_options)
      unit_id <- districts2plot[unit]
      country_names <- colnames(observed(AI_sts))
      if (!is.null(dimnames(arr)[[2]]) && names(unit_id) %in% dimnames(arr)[[2]]) {
        unit_index <- match(names(unit_id), dimnames(arr)[[2]])
      } else if (unit_id <= dim(arr)[2]) {
        unit_index <- unit_id
      } else {
        unit_index <- match(names(unit_id), country_names)
      }
      validate(need(!is.na(unit_index), "Could not match selected country to simulation output."))
      out <- as.matrix(arr[, unit_index, , drop = FALSE][, 1, ])
    }

    out
  })

  forecasting_plotly <- reactive({
    sim_mat <- forecast_sim_matrix()
    validate(need(nrow(sim_mat) > 0, "No forecast simulations available."))

    q05 <- apply(sim_mat, 1, quantile, probs = 0.05, na.rm = TRUE)
    q10 <- apply(sim_mat, 1, quantile, probs = 0.10, na.rm = TRUE)
    q25 <- apply(sim_mat, 1, quantile, probs = 0.25, na.rm = TRUE)
    q50 <- apply(sim_mat, 1, quantile, probs = 0.50, na.rm = TRUE)
    q75 <- apply(sim_mat, 1, quantile, probs = 0.75, na.rm = TRUE)
    q90 <- apply(sim_mat, 1, quantile, probs = 0.90, na.rm = TRUE)
    q95 <- apply(sim_mat, 1, quantile, probs = 0.95, na.rm = TRUE)
    mn <- rowMeans(sim_mat, na.rm = TRUE)

    df <- tibble(
      date = as.Date(input$dateRange[2]) + weeks(seq_len(nrow(sim_mat)) - 1),
      q05 = as.numeric(q05), q10 = as.numeric(q10), q25 = as.numeric(q25),
      q50 = as.numeric(q50), q75 = as.numeric(q75), q90 = as.numeric(q90), q95 = as.numeric(q95),
      mean = as.numeric(mn)
    )

    prev_idx <- max(1, end() - 1)
    if (input$forecasting_options == "Summed all countries") {
      previous_observed <- sum(observed(AI_sts)[prev_idx, ], na.rm = TRUE)
      plot_title <- "Summed all countries"
    } else {
      unit <- which(districts2plot2 == input$forecasting_options)
      unit_id <- districts2plot[unit]
      previous_observed <- observed(AI_sts)[prev_idx, unit_id]
      plot_title <- districts2plot2[unit]
    }

    obs_df <- tibble(
      date = as.Date(minDate) + weeks(prev_idx - 1),
      observed = as.numeric(previous_observed)
    )

    p <- ggplot(df, aes(x = date)) +
      geom_ribbon(aes(ymin = q05, ymax = q95, text = paste0("Week: ", format(date, "%d %b %Y"), "<br>5%-95%: ", round(q05, 1), "-", round(q95, 1))), fill = "#fff7bc", alpha = 0.70) +
      geom_ribbon(aes(ymin = q10, ymax = q90, text = paste0("Week: ", format(date, "%d %b %Y"), "<br>10%-90%: ", round(q10, 1), "-", round(q90, 1))), fill = "#fec44f", alpha = 0.55) +
      geom_ribbon(aes(ymin = q25, ymax = q75, text = paste0("Week: ", format(date, "%d %b %Y"), "<br>25%-75%: ", round(q25, 1), "-", round(q75, 1))), fill = "#d95f0e", alpha = 0.45) +
      geom_line(aes(y = q50, text = paste0("Week: ", format(date, "%d %b %Y"), "<br>Median: ", round(q50, 1))), color = "#b30000", linewidth = 1.0) +
      geom_line(aes(y = mean, text = paste0("Week: ", format(date, "%d %b %Y"), "<br>Mean: ", round(mean, 1))), color = "white", linewidth = 0.9) +
      geom_point(data = obs_df, aes(x = date, y = observed, text = paste0("Previous observed week: ", format(date, "%d %b %Y"), "<br>Observed: ", observed)), inherit.aes = FALSE, size = 2.8, color = "black") +
      labs(title = plot_title, x = "Forecast week", y = "No. detected/predicted cases") +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold", hjust = 0.5))

    ggplotly(p, tooltip = "text") %>%
      layout(
        dragmode = "zoom",
        xaxis = list(rangeslider = list(visible = TRUE)),
        yaxis = list(fixedrange = FALSE)
      ) %>%
      config(displaylogo = FALSE, scrollZoom = TRUE)
  })

  output$forecasting_plotly <- renderPlotly({
    req(input$graph == "forecasting")
    forecasting_plotly()
  })


  forecast_map_domain <- reactive({
    arr <- forecast_sim_array()
    metric <- input$forecast_metric
    if (is.null(metric)) metric <- "q50"

    vals <- lapply(seq_len(dim(arr)[1]), function(w) {
      apply(arr[w, , , drop = FALSE][1, , ], 1, function(x) {
        x <- as.numeric(x)
        if (metric == "mean") {
          mean(x, na.rm = TRUE)
        } else if (metric == "q90") {
          unname(quantile(x, 0.90, na.rm = TRUE))
        } else {
          unname(quantile(x, 0.50, na.rm = TRUE))
        }
      })
    })
    max_val <- suppressWarnings(max(unlist(vals), na.rm = TRUE))
    if (!is.finite(max_val) || max_val <= 0) max_val <- 1
    c(0, max_val)
  })

  forecast_map_data <- reactive({
    arr <- forecast_sim_array()
    week <- input$forecast_week
    validate(need(week >= 1 && week <= dim(arr)[1], "Selected forecast week is outside the simulation horizon."))

    country_codes <- colnames(observed(AI_sts))
    n_units <- min(length(country_codes), dim(arr)[2])

    vals <- lapply(seq_len(n_units), function(i) {
      x <- as.numeric(arr[week, i, ])
      c(
        mean = mean(x, na.rm = TRUE),
        q10 = unname(quantile(x, 0.10, na.rm = TRUE)),
        q50 = unname(quantile(x, 0.50, na.rm = TRUE)),
        q90 = unname(quantile(x, 0.90, na.rm = TRUE))
      )
    })

    fc <- as_tibble(do.call(rbind, vals)) %>%
      mutate(ADM0_A3 = country_codes[seq_len(n_units)])

    map_data <- europeanCountries %>%
      left_join(fc, by = "ADM0_A3") %>%
      st_as_sf() %>%
      st_transform(4326)

    map_data
  })

  output$forecast_leaflet <- renderLeaflet({
    map_data <- forecast_map_data()
    metric <- input$forecast_metric
    if (is.null(metric)) metric <- "q50"
    metric_title <- c(q50 = "Median forecast", mean = "Mean forecast", q90 = "90% quantile")[[metric]]
    metric_title <- ifelse(is.null(metric_title), "Forecast", metric_title)

    map_data$forecast_value <- map_data[[metric]]
    pal <- colorNumeric("YlOrRd", domain = forecast_map_domain(), na.color = "#f1f5f9")

    forecast_date <- as.Date(input$dateRange[2]) + weeks(input$forecast_week - 1)

    labels <- sprintf(
      "<strong>%s</strong><br/>Forecast week %s: %s<br/>Mean: %s<br/>Median: %s<br/>10%%-90%% interval: %s-%s",
      map_data$ADM0_A3,
      input$forecast_week,
      format(forecast_date, "%d %b %Y"),
      round(map_data$mean, 1),
      round(map_data$q50, 1),
      round(map_data$q10, 1),
      round(map_data$q90, 1)
    ) %>% lapply(htmltools::HTML)

    leaflet(map_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(forecast_value),
        fillOpacity = 0.78,
        color = "white",
        weight = 1,
        opacity = 1,
        label = labels,
        highlightOptions = highlightOptions(weight = 2, color = "#111827", bringToFront = TRUE)
      ) %>%
      addLegend(
        pal = pal, values = forecast_map_domain(),
        title = paste0(metric_title, "<br>fixed scale"),
        position = "topright"
      )
  })



  forecast_map_static_ggplot <- reactive({
    map_data <- forecast_map_data()
    metric <- input$forecast_metric
    if (is.null(metric)) metric <- "q50"
    metric_title <- c(q50 = "Median forecast", mean = "Mean forecast", q90 = "90% quantile")[[metric]]
    metric_title <- ifelse(is.null(metric_title), "Forecast", metric_title)
    map_data$forecast_value <- map_data[[metric]]

    ggplot(map_data) +
      geom_sf(aes(fill = forecast_value), color = "white", linewidth = 0.15) +
      scale_fill_gradientn(
        colours = hcl.colors(10, "YlOrRd", rev = FALSE),
        limits = forecast_map_domain(),
        na.value = "grey92",
        name = paste0(metric_title, "
fixed scale")
      ) +
      labs(
        title = paste0(metric_title, " of HPAI detections, forecast week ", input$forecast_week),
        subtitle = paste0("Forecast based on simulations from ", strftime(input$dateRange[2], format = "%d/%m/%Y")),
        caption = "ENIGMA HPAI model"
      ) +
      theme_void(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "right"
      )
  })

  output$hpai_plotly <- renderPlotly({
    req(input$graph == "hpai_timeseries")
    hpai_timeseries_plotly()
  })

  # ---- Static plots retained for model fit, forecasting, and downloadable output ----
  hpai_map_static <- reactive({
    myMap1 <- ggplot() +
      geom_sf(data = europe_mapData(), aes(fill = no_outbreaks), color = "black") +
      theme_void() +
      scale_fill_gradientn(colours = hcl.colors(10, "Reds", rev = TRUE)) +
      theme(
        legend.position = "inside",
        legend.position.inside = c(0.91, 0.98),
        legend.justification = c(0.5, 1),
        legend.title = element_text(size = 9),
        plot.margin = unit(c(1, -0.5, 1, 1), "cm")
      ) +
      labs(fill = expression(paste("HPAI total")))

    myMap2 <- ggplot() +
      geom_sf(data = europe_mapData(), aes(fill = outbreaksArea), color = "black") +
      theme_void() +
      scale_fill_gradientn(colours = hcl.colors(10, "Reds", rev = TRUE)) +
      theme(
        legend.position = "inside",
        legend.position.inside = c(0.95, 0.99),
        legend.justification = c(0.5, 1),
        legend.title = element_text(size = 9),
        plot.margin = unit(c(1, 1, 1, -0.5), "cm")
      ) +
      labs(fill = expression(paste("HPAI/10,000 km"^"2")))

    myMap3 <- ggplot(europe_mapData()) +
      geom_sf(fill = "seashell", show.legend = FALSE) +
      geom_sf(data = europe_data_sf(), mapping = aes(), col = "darkred", alpha = 0.5, size = 0.3, show.legend = FALSE) +
      theme_void()

    print(ggarrange(myMap1, myMap2, myMap3, ncol = 3))
    recordPlot()
  })

  hpai_timeseries_static <- reactive({
    obs <- observed(AI_sts[start():end(), , drop = FALSE])
    y <- rowSums(obs, na.rm = TRUE)
    df <- tibble(
      date = week_dates_from_range(input$dateRange[1], length(y)),
      detections = y
    )

    myPlot <- ggplot(df, aes(x = date, y = detections)) +
      geom_col(width = 6) +
      labs(x = "Week starting date", y = "No. detected cases") +
      theme_minimal(base_size = 13)

    print(myPlot)
    recordPlot()
  })

  allCountry_modelfit <- reactive({
    myPlot <- plot(
      final_model,
      type = "fitted",
      total = TRUE,
      hide0s = TRUE,
      ylab = "No. detected cases",
      xlab = "Years and quarters",
      ylim = c(0, safe_max(rowSums(observed(final_model$stsObj[start():end(), , drop = FALSE]), na.rm = TRUE) * 1.60, add = 300)),
      par.settings = list(mar = c(7.4, 6.6, 6.6, 3.2), cex.lab = 1.35, cex.axis = 1.20, cex.main = 1.35),
      xaxis = list(epochsAsDate = TRUE),
      start = c(startYear(), startWeek()),
      end = c(endYear(), endWeek() + 0.1),
      col = c("brown2", "#4292C6", "orange"),
      names = "Summed all countries",
      legend.args = list(
        x = "top",
        inset = c(0, 0),
        horiz = TRUE,
        bty = "n",
        cex = 0.90,
        x.intersp = 0.75,
        legend = c("Epidemic component, between-country", "Epidemic component, within-country", "Endemic"),
        col = c("orange", "#4292C6", "brown2")
      )
    )
    recordPlot()
  })

  country_modelfit <- reactive({
    unit <- which(districts2plot2 == input$predictions_options)
    myPlot <- plot(
      final_model,
      type = "fitted",
      units = districts2plot[unit],
      hide0s = TRUE,
      names = districts2plot2[unit],
      ylab = "No. detected cases",
      xlab = "Years and quarters",
      ylim = c(0, safe_max(observed(final_model$stsObj)[start():end(), districts2plot[unit]] * 1.60, add = 40)),
      xaxis = list(epochsAsDate = TRUE),
      par.settings = list(mar = c(7.4, 6.6, 6.6, 3.2), cex.lab = 1.35, cex.axis = 1.20, cex.main = 1.35),
      start = c(startYear(), startWeek()),
      end = c(endYear(), endWeek() + 0.1),
      legend.args = list(
        x = "top",
        inset = c(0, 0),
        horiz = TRUE,
        bty = "n",
        cex = 0.90,
        x.intersp = 0.75,
        legend = c("Epidemic component, between-country", "Epidemic component, within-country", "Endemic"),
        col = c("orange", "#4292C6", "brown2")
      ),
      col = c("brown2", "#4292C6", "orange")
    )
    recordPlot()
  })

  AllCountry_forecasting <- reactive({
    myPlot <- plot(
      sim(),
      observed = FALSE,
      ylab = "No. of detected/predicted cases",
      xlab = "Forecast week",
      main = "Summed all countries",
      type = "fan",
      means.args = list(),
      xaxis = list(
        epochsAsDate = TRUE,
        xaxis.tickFreq = list("%d" = atChange, "%m" = atChange),
        xaxis.labelFreq = list("%d" = atMedian),
        xaxis.labelFormat = "%G\n\n%d-%b"
      ),
      fan.args = list(ln = c(10, 50, 90)),
      par.settings = list(pch = 16, cex = 1.25, mar = c(8.2, 8.8, 5.2, 3.0), cex.lab = 1.35, cex.axis = 1.25, cex.main = 1.45)
    )
    title(xlab = "Forecast week", line = 3.0, cex.lab = 1.55)
    title(ylab = "No. of detected/predicted cases", line = 4.0, cex.lab = 1.55)
    grid(col = "grey88")
    box(col = "grey55")
    print(myPlot)
    recordPlot()
  })

  country_forecasting <- reactive({
    unit <- which(districts2plot2 == input$forecasting_options)
    myPlot <- plot(
      sim()[, districts2plot[unit], ],
      observed = FALSE,
      ylab = "No. of detected/predicted cases",
      xlab = "Forecast week",
      main = districts2plot2[unit],
      type = "fan",
      ylim = c(0, quantile(sim()[, districts2plot[unit], ], 0.99, na.rm = TRUE)),
      means.args = list(),
      xaxis = list(
        epochsAsDate = TRUE,
        xaxis.tickFreq = list("%d" = atChange, "%m" = atChange),
        xaxis.labelFreq = list("%d" = atMedian),
        xaxis.labelFormat = "%G\n\n%d-%b"
      ),
      fan.args = list(ln = c(10, 50, 90)),
      par.settings = list(pch = 16, cex = 1.25, mar = c(8.2, 8.8, 5.2, 3.0), cex.lab = 1.35, cex.axis = 1.25, cex.main = 1.45)
    )
    title(xlab = "Forecast week", line = 3.0, cex.lab = 1.55)
    title(ylab = "No. of detected/predicted cases", line = 4.0, cex.lab = 1.55)
    grid(col = "grey88")
    box(col = "grey55")
    print(myPlot)
    recordPlot()
  })

  output$hpai_static <- renderPlot({
    if (input$graph == "country_modelfit") {
      if (input$predictions_options == "Summed all countries") {
        replayPlot(req(allCountry_modelfit()))
      } else {
        replayPlot(req(country_modelfit()))
      }
    }

    if (input$graph == "forecasting") {
      if (input$forecasting_options == "Summed all countries") {
        replayPlot(req(AllCountry_forecasting()))
      } else {
        replayPlot(req(country_forecasting()))
      }
    }
  })

  # ---- Table ----
  hpai_table <- reactive({
    data.frame(
      keyName = names(colSums(observed(final_model$stsObj)[start():end(), ], na.rm = TRUE)),
      value = colSums(observed(final_model$stsObj)[start():end(), ], na.rm = TRUE),
      row.names = NULL
    ) %>%
      mutate(Country = countrycode(keyName, "iso3c", "country.name")) %>%
      select(Country, value) %>%
      arrange(desc(value)) %>%
      rename("#HPAI detections" = "value") %>%
      datatable(
        extensions = c("Buttons"),
        options = list(
          dom = "frtBip",
          buttons = list(list(
            extend = "csv",
            text = "Download csv",
            filename = paste0(
              "HPAIdetections_",
              strftime(input$dateRange[1], format = "%d/%m/%Y"),
              "_to_",
              strftime(input$dateRange[2], format = "%d/%m/%Y")
            ),
            exportOptions = list(modifier = list(page = "all"))
          ))
        )
      )
  })

  output$table <- renderDT(server = FALSE, {
    if (input$graph == "hpai_map") hpai_table()
  })

  # ---- Caption text ----
  output$info <- renderText({
    if (input$graph == "hpai_map") {
      paste0(
        "Number of reported highly pathogenic avian influenza (H5 subtype) detections between ",
        strftime(input$dateRange[1], format = "%d/%m/%Y"),
        " and ",
        strftime(input$dateRange[2], format = "%d/%m/%Y"),
        ". The interactive map can be zoomed and panned. Hover over countries to see total detections and detections per 10,000 km². Below is a table of the total number of detections per country ordered from highest to lowest."
      )
    } else if (input$graph == "hpai_timeseries") {
      paste0(
        "Number of reported highly pathogenic avian influenza (H5 subtype) detections shown over time between ",
        strftime(input$dateRange[1], format = "%d/%m/%Y"),
        " and ",
        strftime(input$dateRange[2], format = "%d/%m/%Y"),
        ", summed over 37 European countries. Use the plot toolbar, mouse wheel, or range slider to zoom."
      )
    } else if (input$graph == "country_modelfit") {
      if (input$predictions_options == "Summed all countries") {
        paste0(
          "Overall model fit aggregated over all the 37 countries shown for ",
          strftime(input$dateRange[1], format = "%d/%m/%Y"),
          " to ",
          strftime(input$dateRange[2], format = "%d/%m/%Y"),
          ". The plot shows the relative contribution of model components. Dots show the actual counts of reported detections."
        )
      } else {
        unit <- which(districts2plot2 == input$predictions_options)
        paste0(
          "Model fit for ", districts2plot2[unit], " shown for ",
          strftime(input$dateRange[1], format = "%d/%m/%Y"),
          " to ",
          strftime(input$dateRange[2], format = "%d/%m/%Y"),
          ". The plot shows the relative contribution of model components. Dots show the actual counts of reported detections."
        )
      }
    } else if (input$graph == "forecasting") {
      if (identical(input$forecast_display, "map")) {
        metric_label <- c(q50 = "median", mean = "mean", q90 = "90% quantile")[[input$forecast_metric]]
        paste0(
          "Simulation-based 4 week forecast shown geographically. The map displays the ", metric_label,
          " predicted number of HPAI H5 detections for forecast week ", input$forecast_week,
          " in each country. Use the forecast-week slider in the sidebar to move through the forecast horizon; hover over countries for values."
        )
      } else if (input$forecasting_options == "Summed all countries") {
        paste0(
          "Simulation-based 4 week forecast based on the modelling framework described in Kjær et al. (2023). The plot shows weekly number of predicted highly pathogenic avian influenza (H5 subtype) detections aggregated over all 37 countries included in the model. The fan chart represents the 10%, 50% and 90% quantiles of the simulations (N=500) each week; their mean is displayed as a white line. The black dot to the left of the forecast is the observed last observation used as the starting point for the forecast."
        )
      } else {
        unit <- which(districts2plot2 == input$forecasting_options)
        paste0(
          "Simulation-based 4 week forecast for ", districts2plot2[unit],
          ". The fan chart represents the 10%, 50% and 90% quantiles of the simulations (N=500) each week; their mean is displayed as a white line. The black dot to the left of the forecast is the observed last observation used as the starting point for the forecast."
        )
      }
    }
  })

  # ---- Download static PNG ----
  getPlotName <- reactive({
    if (input$graph == "hpai_map") {
      "hpai_map_static()"
    } else if (input$graph == "hpai_timeseries") {
      "hpai_timeseries_static()"
    } else if (input$graph == "country_modelfit") {
      if (input$predictions_options == "Summed all countries") "allCountry_modelfit()" else "country_modelfit()"
    } else if (input$graph == "forecasting") {
      if (input$forecasting_options == "Summed all countries") "AllCountry_forecasting()" else "country_forecasting()"
    }
  })

  output$downloadPlot <- downloadHandler(
    filename = function() {
      if (input$graph == "forecasting" && identical(input$forecast_display, "map")) {
        paste0("forecast_map_week", input$forecast_week, "_", Sys.Date(), ".png")
      } else if (input$graph == "forecasting" && input$forecasting_options != "Summed all countries") {
        paste0(substr(getPlotName(), 1, nchar(getPlotName()) - 2), "_", input$forecasting_options, "_", Sys.Date(), ".png")
      } else if (input$graph == "country_modelfit" && input$predictions_options != "Summed all countries") {
        paste0(substr(getPlotName(), 1, nchar(getPlotName()) - 2), "_", input$predictions_options, "_", Sys.Date(), ".png")
      } else {
        paste0(substr(getPlotName(), 1, nchar(getPlotName()) - 2), "_", Sys.Date(), ".png")
      }
    },
    contentType = "image/png",
    content = function(file) {
      png(file, width = 13, height = 8, units = "in", res = 600)
      if (input$graph == "forecasting" && identical(input$forecast_display, "map")) {
        print(forecast_map_static_ggplot())
      } else if (input$graph == "country_modelfit") {
        print(model_fit_static_ggplot())
      } else {
        replayPlot(eval(parse(text = getPlotName())))
      }
      makeFootnote(paste0(
        "ENIGMA model results based on model from Kjær et al. (2023), using WOAH-WAHIS data from ",
        strftime(input$dateRange[1], format = "%d/%m/%Y"),
        " to ",
        strftime(input$dateRange[2], format = "%d/%m/%Y"),
        ". Downloaded on ",
        format(Sys.time(), "%d/%m/%Y")
      ))
      dev.off()
    }
  )
}

# ---- Launch app ----
shinyApp(ui, server)
