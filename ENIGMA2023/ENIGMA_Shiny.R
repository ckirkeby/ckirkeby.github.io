
## Copyright © 2023 Lene J. Kjær, Michael P. Ward, Anette E. Boklund, Lars E. Larsen, Charlotte K. Hjulsager, and Carsten T. Kirkeby

##############################################################################################
# This file is part of the Shiny app for the ENIGMA HPAI model version 1.0.

# The ENIGMA HPAI model is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

# The ENIGMA HPAI model is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

# You should have received a copy of the GNU General Public License along with the ENIGMA HPAI model. If not, see <https://www.gnu.org/licenses/>.
##############################################################################################



# load required packages
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

### READ IN newest OIE DATA ###
##link to where files are - the below code will pick the newest file in the folder

tt <- tempfile()
download.file("http://www.enigmahpai.org/ENIGMA2023/for_ai.car", tt, mode="wb")
qs::qload(tt)
updateDate <-strftime(as.Date(substring(filename, 7,14), format='%Y%m%d'),format = '%d/%m/%Y')
file.remove(tt)
endDate <- Sys.Date()
maxdate<- Sys.Date()


## FUNCTION TO MAKE FOOTNOTES IN DOWNLOADED PLOTS FROM SHINY ##
makeFootnote <- function(footnoteText=
                           format(Sys.time(), "%d %b %Y"),
                         size= .7, color= grey(.5))
{
  require(grid)
  pushViewport(viewport())
  grid.text(label= footnoteText ,
            x = unit(1,"npc") - unit(2, "mm"),
            y= unit(1, "mm"),
            just=c("right", "bottom"),
            gp=gpar(cex= size, col=color))
  popViewport()
}

## SHINY APP ##
shinyApp(
  ui <- fluidPage(
    # for Carstens tracker
    tags$head(includeHTML("head.html"),includeHTML("head_ljkjaer.html")),
    # Title and logo
    titlePanel(
      # titlePanel(title=div(img(src="ku_logo_uk_v.png",height = 100, width = 80),"ENIGMA HPAI model version 1.0",img(src="ENIGMAlogo.png",height = 120, width = 160), windowTitle = "ENIGMA HPAI model"),
      # ),
      
      fluidRow(
        column(3, img(height = 150, width = 240, src = "ENIGMAlogo.png")),
        column(6, offset=2," "),
        column(6, offset=2,"ENIGMA HPAI model version 1.1"),
        column(1, img(height = 160, width =130, src = "ku_logo_uk_v.png"))
      )
    ),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
      
      # Sidebar panel for inputs ----
      sidebarPanel(
        #data range input
        # dateRangeInput("dateRange", paste0("Select Date Range (needs to be within 27/09/2021 and ",strftime(maxdate, format = '%d/%m/%Y'),", see model description for details about date range)."),
        #                min=as.Date(mindate), max=as.Date(maxdate),
        #                start = "2021-09-27",
        #                end = strftime(maxdate, format = '%Y-%m-%d'), format='dd/mm/yyyy'),
        minDate <- as.Date("2021-09-27"),
        dateRangeInput("dateRange", paste0(maxdate,"Select Date Range (needs to be within", strftime(mindate, format = '%d/%m/%Y'), "and ",strftime(endDate, format = '%d/%m/%Y'),", see model description for details about date range)."),
                       min=as.Date(mindate), max=as.Date(maxdate),
                       start = strftime(minDate, format = '%Y-%m-%d'),
        end = strftime(endDate, format = '%Y-%m-%d'), format='dd/mm/yyyy'),
      
      
      
      # buttons to picks which graphs/maps to see
      radioButtons("graph","Select:",
                   c("Map of detections"="hpai_map",
                     "Timeseries" = "hpai_timeseries",
                     "Model fit" = "country_modelfit",
                     "Forecasting" = "forecasting"),selected="forecasting"),
      
      #if predictions are chose, here you select country
      conditionalPanel(
        condition = "input.graph == 'country_modelfit'",
        selectInput("predictions_options","Select country (only countries with > 10 detections total are shown)", choices=districts2plot2, selected=("Summed all countries"))
      ),
      
      # if forecasting is chosen, here you pick country
      conditionalPanel(
        condition = "input.graph == 'forecasting'",
        selectInput("forecasting_options","Select country (only countries with > 10 detections total are shown)", choices=districts2plot2, selected=("Summed all countries"))
      ),
      #text under the panel
      HTML('<hr style = "border-color: #800000; height: 5px;width: 100%">'),
      div(span("The purpose of this webpage is to show the predictions of the ENIGMA HPAI model. This model has been developed during research at the University of Copenhagen in "),span(a("the ENIGMA project, " , href="https://ivh.ku.dk/forskning/dyrevelfaerd-og-sygdomsbekaempelse/projektside/enigma/",target="_blank")), span("2021-2024. The ENIGMA project is based in "), span(a("the Avian Influenza Epidemiology Subgroup ", href="https://ivh.ku.dk/english/research/animal-welfare-and-disease-control/avian-influenza-epidemiology/",target="_blank")),span("at the University of Copenhagen and is part of the "),span(a("Danish Veterinary Contingency Consortium (DKVET) ", href="https://dkvet.dk/english/about/", target="_blank")),span("funded by the Danish Food and Veterinary Administration. R code and data used for the ENIGMA HPAI model is available at "), span(a("GitHub.", href="https://github.com/ckirkeby/ckirkeby.github.io/tree/main/ENIGMA2023", target="_blank"))),
      br(),
      
      h5(HTML("<i>Disclaimer: The model and data reflects the state of knowledge available on the date of dispatch. The University of Copenhagen and DKVET cannot be held liable for any errors, inaccuracies or inconsistencies with regard to text and/or data contained therein. Therefore, the University of Copenhagen and DKVET accept no responsibility or liability arising out of, or in connection with the information provided.</i>")),
      br(),
      h5(HTML("The ENIGMA project is co-funded by the European Union through the European Partnership on Animal Health and Welfare")),
      div(img(src="EN-Co-Funded-by-the-EU_POS-1024x215.png",height = 40, width = 160), img(src="EUP AH&W_color.png",height = 60, width = 160)),
      HTML('<hr style = "border-color: #800000; height: 5px;width: 100%">'),
      strong(a("Contact Admin",
               href="mailto:ckir@sund.ku.dk")),
      br(),
      br(),
      p(strong(em(paste0("WOAH-WAHIS data and model updated: ", updateDate)))),
      br(),
      p("Copyright © 2023 Lene J. Kjær, Michael P. Ward, Anette E. Boklund, Lars E. Larsen, Charlotte K. Hjulsager, and Carsten T. Kirkeby",style = "font-size:80%;"),
      br(),
      p("THE SOFTWARE IS PROVIDED AS IS AND, TO THE MAXIMUM EXTENT PERMITTED UNDER APPLICABLE LAW, WE PROVIDE NO WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, BY STATUTE OR OTHERWISE, INCLUDING BUT NOT LIMITED TO ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, TITLE, OR NON-INFRINGMENT. WE DO NOT WARRANT THAT THE SOFTWARE WILL FUNCTION UNINTERRUPTED, THAT IT WILL MEET ANY REGULATORY REQUIREMENTS, THAT IT IS ERROR-FREE, OR THAT ANY ERRORS WILL BE CORRECTED.",style = "font-size:80%;"),
      br(),
      div(img(src="gplv3-with-text-136x68.png",height = 40, width = 80),
          br(),
          span("The ENIGMA HPAI model is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. The ENIGMA HPAI model is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See ",style = "font-size:80%;"), span(a("the GNU General Public License", href="https://www.gnu.org/licenses/gpl-3.0.html",target="_blank",style = "font-size:80%;")), span(" for more details.",style = "font-size:80%;"))
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ model description and model output panel ----
      tabsetPanel(
        tabPanel("Model output", downloadButton("downloadPlot", "Download"),plotOutput("graph",height = 500, width = 900), textOutput("info"), DTOutput("table")),
        
        
        tabPanel("Model description",
                 
                 h3("Endemic-epidemic modelling of highly pathogenic avian influenza in Europe"),
                 p("The ENIGMA HPAI model results and graphs presented in this shiny app are based on the modelling framework described in Kjær et al. (2023). In this study, we utilized readily available data from the World Organization for Animal Health (WOAH-WAHIS)* on highly pathogenic avian influenza (HPAI) H5 detections in wild and domestic birds together with a time-series modelling framework (Meyer et al. 2014) to predict HPAI H5 detections within Europe. This framework decomposes time series data into endemic and epidemic components, where the epidemic component can take into account within-country transmission and between-country transmission as well as short-distance (from directly neighbouring countries) and long-distance (transmission follows a distance-decay algorithm) transmission."),
                 p("The WOAH-WAHIS data have revealed several shifts in the seasonality between 2016-2024, thus we only use recent data to reflect the newest transmission patterns. The results presented here are from a model fitted to data from the latest 1 1/4 year (to ensure enough data for model fitting) that includes long-range transmission, and seasonality in both the endemic and epidemic component (one seasonal wave for each component)."),
                 p("Due to the model being based on the most recent HPAI H5 detection data, the latest date that can be chosen is the last date, from which we have HPAI H5 data from WOAH-WAHIS (updated weekly), and the earliest date that can be chosen in this app is 1 1/4 year before the last date. Forecasting will always be 4 weeks ahead from the week before the last date chosen (to account for delays in WOAH-WAHIS reporting). For more details see Kjær et al. (2023) and Meyer et al. (2014)."),
                 br(),
                 br(),
                 p(a("Kjær, L. J., M. P. Ward, A. E. Boklund, L. E. Larsen, C. K. Hjulsager, and C. T. Kirkeby. Using surveillance data for early modelling of highly pathogenic avian influenza in Europe reveals a seasonal shift in transmission, 2016-2022. 2023. Scientific Reports 13:15396.",href="https://doi.org/10.1038/s41598-023-42660-7",target="_blank",style = "font-size:85%;")),
                 p(a("Meyer, S., L. Held, and M. Höhle. 2014. Spatio-Temporal Analysis of Epidemic Phenomena Using the R Package surveillance. J. Stat. Softw. 77.", href= "https://doi.org/10.18637/jss.v077.i11", target="_blank",style = "font-size:85%;")),
                 br(),
                 br(),
                 div(
                   h4(HTML("<b>*Copyright of data extracted from WAHIS</b>"),style = "
                          background-color: #800000; width: 100%; color:white;font-size:85%;"),
                   h5(HTML("<b>Disclaimer and caption</b>"), style="text-align:left;background-color: #ffffff;width: 50%;font-size:85%;"),
                   p(HTML("<i>Data extracted by Lene Jung Kjær and Carsten Kirkeby, University of Copenhagen. WOAH bears no responsibility for the integrity or accuracy of the data contained herein, but not limited to, any deletion, manipulation, or reformatting of data that may have occurred beyond its control. For some events, incorrect data have recently been detected in the figures containing the quantitative information of the outbreaks. WOAH is currently undertaking considerable efforts to solve these issues at the source and provide a dataset that is fully consistent with that reported by countries. The remaining fields of the table are not impacted by these issues. We will keep our users informed as the situation develops.</i>"),style="text-align:justify;background-color: #ffffff;width: 100%;font-size:85%;"))
                 
        )
        
      )
    )
  )
),


# Define server
server <- function(input, output) {
  ## REACTIVE FUNCTIONS ##
  # reactive functions take user inputs fx. date or graph type and produce outputs or variables based on this
  
  # many of the below functions just create timevariables out of the dates chosen. These variables are used for plotting
  startWeek <- reactive({
    startweekno<- isoweek(input$dateRange[1])
    startyearno<- isoyear(input$dateRange[1])
    if(startyearno==2023 && startweekno==53){
      if(input$dateRange[1] < '2024-01-01'){
        startweekno<-52
        
      }
      else if(input$dateRange[1] >='2024-01-01'){
        startweekno<- 1
      }
      
    }
    return(as.numeric(startweekno))
  })
  
  startYear <- reactive({
    startyearno<-isoyear(input$dateRange[1])
    startweekno <- isoweek(input$dateRange[1])
    if(startyearno==2023 && startweekno==53){
      if(input$dateRange[1] < '2024-01-01'){
        startyearno<- 2023
        
      }
      else if(input$dateRange[1] >='2024-01-01'){
        startyearno <- 2024
      }
    }
    return(as.numeric(startyearno))
  })
  
  endWeek <- reactive({
    endweekno <-isoweek(input$dateRange[2])
    endyearno <- isoyear(input$dateRange[2])
    if(endyearno==2023 && endweekno==53){
      if(input$dateRange[2] < '2024-01-01'){
        endweekno<-52
        
      }
      else if(input$dateRange[2] >='2024-01-01'){
        endweekno<- 1
      }
      
    }
    return(as.numeric(endweekno))
  })
  
  endYear <- reactive({
    endyearno<-isoyear(input$dateRange[2])
    endweekno <-isoweek(input$dateRange[2])
    if(endyearno==2023 && endweekno==53){
      if(input$dateRange[2] < '2024-01-01'){
        endyearno<- 2023
        
      }
      else if(input$dateRange[2] >='2024-01-01'){
        endyearno <- 2024
      }
    }
    return(as.numeric(endyearno))
  })
  
  startYearWeek <- reactive ({
    yearweek(paste0(startYear(), ' ', paste0('W',startWeek())))
  })
  
  endYearWeek <-  reactive({
    yearweek(paste0(endYear(), ' ', paste0('W',endWeek())))
  })
  
  start <- reactive ({
    as.numeric(floor(difftime(input$dateRange[1], minDate,units="weeks")+1))
  })
  
  end <-  reactive({
    as.numeric(floor(difftime(input$dateRange[2], minDate,units="weeks")+1))
  })
  
  #now we calculate number of outbreaks for each country for the time period chosen for plotting
  europe_mapData1 <-reactive ({
    europe_data_weekly %>%
      filter(yearweek>=startYearWeek() & yearweek <=endYearWeek()) %>%
      group_by(ADM0_A3) %>% summarise(no_outbreaks = sum(no_outbreaks, na.rm=T))
  })
  
  #Merge with shapefiles of Europe for plotting
  europe_mapData<- reactive ({
    t1 <- merge(europeanCountries, europe_mapData1(),by="ADM0_A3")
    t1$outbreaksArea <- t1$no_outbreaks/(t1$area_sqkm/10000)
    return(st_as_sf(t1))
  })
  europe_data_sf <-reactive({
    ##to use in shiny graphs
    europe_data_sf <- europe_data %>%
      filter(yearweek>=startYearWeek() & yearweek <=endYearWeek()) %>%
      mutate(Longitude=as.numeric(Longitude), Latitude=as.numeric(Latitude) ) %>%
      filter(Latitude <74.0) %>%
      st_as_sf(coords=c("Longitude","Latitude"),crs="EPSG:4326")
    return(europe_data_sf)
  })
  ## simulations 3 weeks ahead of the last week of the time period chosen.
  sim <- reactive({
    startSim <-end()-1
    endSim<-startSim+4
    y.start <- observed(AI_sts)[startSim, ]
    AI_stsSim <- simulate(final_model,
                          nsim = 500, seed = 1, subset =(startSim+1):endSim,y.start = y.start)
    return(AI_stsSim)
  })
  # her we have reactive functions to create each of the graphs/maps
  #map
  hpai_map <- reactive({
    myMap1 <- ggplot() +
      geom_sf(data= europe_mapData(),aes(fill = no_outbreaks),color="black")+
      theme_void()+
      scale_fill_gradientn(colours=hcl.colors(10, "Reds",rev=T)) +
      theme(legend.position = c(0.91, 0.98),
            legend.justification=c(0.5, 1),legend.title=element_text(size=9),plot.margin=unit(c(1,-0.5,1,1), "cm"))+
      labs(fill=expression(paste("HPAI total")))
    
    myMap2 <- ggplot() +
      geom_sf(data= europe_mapData(),aes(fill = outbreaksArea),color="black")+
      theme_void()+
      scale_fill_gradientn(colours=hcl.colors(10, "Reds",rev=T)) +
      theme(legend.position = c(0.95, 0.99),
            legend.justification=c(0.5, 1),legend.title=element_text(size=9),plot.margin=unit(c(1,1,1,-0.5), "cm"))+
      labs(fill=expression(paste("HPAI/10,000 km"^"2")))
    
    myMap3 <- ggplot(europe_mapData())+
      geom_sf(fill="seashell", show.legend=FALSE)+
      geom_sf(europe_data_sf(),mapping=aes(fill="darkred",alpha=0.5),col="darkred", alpha=0.5,size=0.3,show.legend=FALSE)+
      theme_void()
    
    print(ggarrange(myMap1,myMap2,myMap3, ncol=3))
    recordPlot()
  })
  #here we create table to accompany map
  hpai_table <- reactive({
    myTable<- data.frame(keyName=names(colSums(observed(final_model$stsObj)[start():end(),])), value=colSums(observed(final_model$stsObj)[start():end(),]), row.names=NULL) %>%
      mutate(Country=countrycode(keyName,"iso3c","country.name")) %>%
      select(Country, value) %>%
      arrange(desc(value)) %>%
      rename("#HPAI detections"="value") %>%
      datatable(extensions = c('Buttons'),
                options = list(
                  dom = 'frtBip',
                  buttons = list(list(extend = "csv", text = "Download csv", filename = paste0('HPAIdetections_',strftime(input$dateRange[1], format='%d/%m/%Y'), '_to_', strftime(input$dateRange[2], format='%d/%m/%Y')),
                                      exportOptions = list(
                                        modifier = list(page = "all"))))))
    
    return(myTable)
    
  })
  
  
  # time series
  hpai_timeseries <- reactive({
    par(mar = c(7, 5, 4.1, 2.1))
    myPlot <- plot(AI_sts[start():end(),], type = observed ~ time, ylab="No. detected cases", xlab="Years and quarters", ylim=c(0,max(rowSums(observed(AI_sts[start():end(),]),na.rm=T))+100))
    print(myPlot)
    recordPlot()
  })
  
  # model fit, summed all countries
  allCountry_modelfit<- reactive({
    myPlot <-plot(final_model, type = "fitted", total = TRUE,hide0s = TRUE, ylab="No. detected cases", xlab="Years and quarters",ylim=c(0,(max(rowSums(observed(final_model$stsObj[start():end(), ]), na.rm=T)))+50),par.settings = list(mar = c(7, 5, 4.1, 2.1)), xaxis=list(epochsAsDate=TRUE),start=c(startYear(),startWeek()), end=c(endYear(),endWeek()+0.1),col=c('brown2','#4292C6','orange'),names="Summed all countries", legend.args=list(x="topright", inset=c(-0.1,0.003), horiz=T,legend = c("Epidemic component,between-country","Epidemic component,within-country", "Endemic"),col=c('orange','#4292C6','brown2')))
    print(myPlot)
    recordPlot()
  })
  
  # model fit, individual countries
  country_modelfit <- reactive({
    unit<-  which(districts2plot2==input$predictions_options)
    myPlot <-plot(final_model, type = "fitted", units = districts2plot[unit], hide0s = TRUE,  names=districts2plot2[unit],ylab="No. deteced cases",xlab="Years and quarters",ylim=c(0,(max(observed(final_model$stsObj)[start():end(),districts2plot[unit]], na.rm=T))+7),xaxis=list(epochsAsDate=TRUE),par.settings = list(mar = c(7, 5, 4.1, 2.1)),  start=c(startYear(),startWeek()), end=c(endYear(),endWeek()+0.1),legend.args=list(x="topright",inset=c(-0.1,0.003), horiz=T,legend = c("Epidemic component,between-country","Epidemic component,within-country", "Endemic"),col=c('orange','#4292C6','brown2')),col=c('brown2','#4292C6','orange'))
    print(myPlot)
    recordPlot()
  })
  # forecasting, summed all countries
  AllCountry_forecasting <- reactive({
    myPlot <-plot(sim(), observed=FALSE,ylab="",xlab="", main="Summed all countries",type="fan", means.args = list(),xaxis=list(epochsAsDate=TRUE, xaxis.tickFreq=list("%d"=atChange, "%m"=atChange),xaxis.labelFreq=list("%d"=atMedian), xaxis.labelFormat="%G\n\n%d-%b"), ylim=range(sim()*0.8),fan.args=list(ln = c(10,50,90)),par.settings=list(pch=1,cex=1.2,mar = c(7, 5, 4.1, 2.1)))
    #title(xlab="Time", line=1, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
    title(ylab="No. of detected/predicted cases", line=3, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
    grid()
    print(myPlot)
    recordPlot()
  })
  
  # forecasting, individual countries
  country_forecasting <- reactive({
    unit<-  which(districts2plot2==input$forecasting_options)
    myPlot <-plot(sim()[,districts2plot[unit],],  observed=FALSE,ylab="", xlab="",main=districts2plot2[unit], type="fan", ylim=c(0, quantile(sim()[,districts2plot[unit],], 0.99)),means.args = list(),xaxis=list(epochsAsDate=TRUE, xaxis.tickFreq=list("%d"=atChange, "%m"=atChange),xaxis.labelFreq=list("%d"=atMedian), xaxis.labelFormat="%G\n\n%d-%b"), fan.args=list(ln = c(10,50,90)),par.settings=list(pch=1,cex=1.2,mar = c(7, 5, 4.1, 2.1)))
    title(xlab="Time", line=1)
    title(ylab="No. of detected/predicted cases", line=3, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
    grid()
    print(myPlot)
    recordPlot()
  })
  
  
  #functions to get plot names dependent on user input - this will be used when downloading files to create file name
  getPlotName <- reactive({
    if(input$graph=="country_modelfit"){
      if(input$predictions_options== "Summed all countries"){filename<- "allCountry_modelfit()"} else {filename <- "country_modelfit()"}
    }else
      if(input$graph=="forecasting"){
        if(input$forecasting_options== "Summed all countries"){filename <-"AllCountry_forecasting()"}else{filename <- "country_forecasting()"}
      }else{filename <- paste0(input$graph,"()") }
    return(filename)
  })
  ## RENDER PLOT ###
  # this creates the actual graphs/maps on the output tab using the respective reactive functions for these graphs/maps
  output$graph <- renderPlot({
    if(input$graph=="hpai_map"){
      replayPlot(req(hpai_map()))
    }
    if(input$graph=="hpai_timeseries") {replayPlot(req(hpai_timeseries()))}
    if(input$graph=="country_modelfit"){
      if(input$predictions_options== "Summed all countries"){replayPlot(req(allCountry_modelfit()))
      } else {replayPlot(req(country_modelfit()))}
    }
    if(input$graph=="forecasting"){
      if(input$forecasting_options== "Summed all countries"){replayPlot(req(AllCountry_forecasting()))
      }else{replayPlot(req(country_forecasting()))}
    }
  })
  ## RENDER TEXT ##
  # here we create figure text for each graph/map
  output$info <- renderText({
    if(input$graph=="hpai_map"){
      paste0("Number of reported highly pathogenic avian influenza (H5 subtype) detections between ",strftime(input$dateRange[1], format='%d/%m/%Y'), " and ", strftime(input$dateRange[2],format='%d/%m/%Y'), ", in 37 European countries shown geographically as total number of detections (left), number of detections per 10,000 km² (middle), and as individual detection locations (right). Below is a table of the total number of detections per country ordered from highest to lowest.")
    } else
      if(input$graph=="hpai_timeseries") {
        paste0("Number of reported highly pathogenic avian influenza (H5 subtype) detections shown over time between ",strftime(input$dateRange[1], format= '%d/%m/%Y'), " and ", strftime(input$dateRange[2],format='%d/%m/%Y'), ", summed over 37 European countries.")
        
      } else
        if(input$graph=="country_modelfit"){
          if(input$predictions_options== "Summed all countries"){
            paste0("Overall model fit aggregated over all the 37 countries shown for ", strftime(input$dateRange[1], format= '%d/%m/%Y'), " to ", strftime(input$dateRange[2],format='%d/%m/%Y'),". The plot shows the relative contribution of model components based on the modelling framework described in Kjær et al. (2023). Dots show the actual counts of reported highly pathogenic avian influenza (H5 subtype) detections in domestic and wild birds. Note that zero/missing detections have been omitted.")
          } else {
            unit<-  which(districts2plot2==input$predictions_options)
            paste0("Model fit for ", districts2plot2[unit]," shown for ", strftime(input$dateRange[1], format='%d/%m/%Y'), " to ", strftime(input$dateRange[2],format='%d/%m/%Y'), ". The plot shows the relative contribution of model components based on the modelling framework described in Kjær et al. (2023). Dots show the actual counts of reported highly pathogenic avian influenza (H5 subtype) detections in domestic and wild birds. Note that zero/missing detections have been omitted.")
          }
        } else
          if(input$graph=="forecasting"){
            if(input$forecasting_options== "Summed all countries") {
              paste0("Simulation-based 4 week forecast based on the modelling framework described in Kjær et al. (2023). The plots show weekly number of predicted highly pathogenic avian influenza (H5 subtype) detections aggregated over all 37 countries included in the model. The fan chart represents the 10%, 50% and 90% quantiles of the simulations (N=500) each week; their mean is displayed as a white line. The black dot to the left of the graph depicts the number of detections from week ", isoweek(input$dateRange[2])-1, " in ", isoyear(input$dateRange[2]), ". As forecasting consists of sequential calls to the negative binomial distributions developed in the model, the mean at each time point is determined by using the parameter estimates and the counts simulated at the previous time point. Thus, we used the second to last week of data (to account for delays in WOAH-WAHIS reporting) to forecast 4 weeks ahead. For further details, see Kjær et al. (2023).")
            }else{
              unit<-  which(districts2plot2==input$forecasting_options)
              paste0("Simulation-based 4 week forecast based on the modelling framework described in Kjær et al. (2023). The plots show weekly number of predicted highly pathogenic avian influenza (H5 subtype) detections for ",districts2plot2[unit],". The fan chart represents the 10%, 50% and 90% quantiles of the simulations (N=500) each week; their mean is displayed as a white line. The black dot to the left of the graph depicts the number of detections from week ", isoweek(input$dateRange[2])-1, " in ", isoyear(input$dateRange[2]), ". As forecasting consists of sequential calls to the negative binomial distributions developed in the model, the mean at each time point is determined by using the parameter estimates and the counts simulated at the previous time point. Thus, we used the second to last week of data (to account for delays in WOAH-WAHIS reporting) to forecast 4 weeks ahead. For further details, see Kjær et al. (2023).")
            }
          }
  })
  #CREATE TABLE ON THE MAP PAGE ONLY
  output$table <- renderDT(server=FALSE,{
    if(input$graph=="hpai_map"){
      hpai_table()
    }
  })
  
  ## DOWNLOAD GRAPHS/MAPS ###
  output$downloadPlot <- downloadHandler(
    
    filename = function() {
      if(input$graph=="forecasting" & input$forecasting_options!= "Summed all countries"){
        paste0(substr(getPlotName(),1,nchar(getPlotName())-2), "_", input$forecasting_options,"_", Sys.Date(),".png")
      }else if (input$graph=="country_modelfit" & input$predictions_options!= "Summed all countries"){
        paste0(substr(getPlotName(),1,nchar(getPlotName())-2), "_", input$predictions_options,"_", Sys.Date(),".png")
      } else{paste0(substr(getPlotName(),1,nchar(getPlotName())-2), "_",  Sys.Date(),".png")}},
    content = function(file) {
      png(file,width = 13, height = 8, units = 'in', res = 600)
      replayPlot(eval(parse(text = getPlotName())))
      makeFootnote(paste0("ENIGMA model results based on model from Kjær et al. (2023), using WOAH-WAHIS data from ", strftime(input$dateRange[1],format='%d/%m/%Y')," to ",strftime(input$dateRange[2],format='%d/%m/%Y'), ". Downloaded on ", format(Sys.time(), "%d/%m/%Y")))
      dev.off()
    }
    
  )
}

)
# Create Shiny app ----
shinyApp(ui, server)