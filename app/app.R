## app.R ##
library(shinydashboard)
library(png)
library(shinydashboardPlus)
library(shiny)
library(dplyr)
library(tidyverse)
library(magrittr)
library(highcharter)
library(shinyjs)
library(tmap)
library(tmaptools)
library(leaflet)
library(mapdeck)

key <- ''    ## put your own token here
mapdeck(token = key)

setwd("//path//to//directory")
title <- tags$a(href='https://www.techfitlab.com',
                icon("diamond"), 'TechFit')

sap_2016 <- read.csv(file="sap_2016.csv",header=TRUE,sep=",")

iegeo_sab <- read_shape(file="Small100.shp", as.sf = TRUE) 

df <- read.csv(file="building1.csv",header=TRUE,sep=",")

iegeo_data <- sap_2016 %>%
  dplyr::select(GUID) %>%
mutate(`Households No Car` = sap_2016$T15_1_NC + sap_2016$T15_1_NSC) %>%
  mutate(`Households 1 Car` = sap_2016$T15_1_1C) %>%
  mutate(`Households 2 Cars` = sap_2016$T15_1_2C) %>%
  mutate(`Households 3 Cars` = sap_2016$T15_1_3C) %>%
  mutate(`Households 4 or more Cars` = sap_2016$T15_1_GE4C) %>%
  mutate(`Total Households` = sap_2016$T6_2_TH)

iemap <- append_data(iegeo_sab,iegeo_data,key.shp = "GUID", key.data = "GUID")

iemap <- iemap[!is.na(iemap$`Total Households`), ]

m_County <- c("Select All", as.character(sort(unique(iemap$COUNTYNAME))))
m_Ed <- c("Select All", as.character(sort(unique(iemap$EDNAME))))

s2_County <- as.character(sort(unique(iemap$COUNTYNAME)))
s2_Ed <- as.character(sort(unique(iemap$EDNAME)))

ui <- dashboardPagePlus(skin='black',
  dashboardHeaderPlus(title = title),
  dashboardSidebar(width=275,
                   
                   # The dynamically-generated user panel
                   uiOutput("userpanel"),
                   
                   # Side Bar Menu
                   sidebarMenu(style = "position: Scroll; overflow: visible;",id = "sidebarmenu",
                               
                               menuItem("Overview", tabName = "iaa", icon = icon("th")),
                   
                               menuItem("CSO Dashboard", tabName = "cso", icon = icon("desktop"),
                                        badgeLabel = "new",
                                        badgeColor = "green"),
                               
                               conditionalPanel(
                                 "input.sidebarmenu === 'cso'",
                                 # a. FILTERS
                                 useShinyjs(),
                                 div(id = "form",
                                     tags$hr(),
                                     selectInput("i2_county", "County", choices = m_County,bookmarkButton(id = "bookmark1")),
                                     selectInput("i2_ed", "Electoral District", choices = m_Ed,bookmarkButton(id = "bookmark2")),
                                     column(6,offset = 6,height = 100,style='padding100px;',
                                            actionButton("reset_button", "Reset",icon = icon("repeat")))
                                 ))
                               )
                  ),
  dashboardBody(
    
    tabItems( 
      tabItem(tabName = "iaa",
            fluidRow(column(10, offset = 0.5,h1("MAPDECK"))),
            br(), 
            fluidRow(column(10, offset = 2.5,mapdeckOutput('map_value', width = 1400, height = 800))),
            br()
            ),
      tabItem(tabName = "cso",
       fluidRow(column(10, offset = 0.5, h1("CSO DASHBOARD"))),
       fluidRow(style="height:50px;",
                valueBoxOutput("count1",width = 3),
                valueBoxOutput("count2",width = 3),
                valueBoxOutput("count3",width = 3),
                valueBoxOutput("count4",width = 3)
              ),
       br(),
       br(),
       fluidRow(column(10, offset = 2.5,leafletOutput('map1', width = 1400, height = 800)))
          )
      )
  )
)

server <- function(input, output) {
  
  addClass(selector = "body", class = "sidebar-collapse")
  
  # Reset Button
  
  # Need to exclude the buttons from themselves being bookmarked
  setBookmarkExclude(c("bookmark1", "bookmark2"))
  
  # Trigger bookmarking with either button
  observeEvent(input$bookmark1, {
    session$doBookmark()
  })
  observeEvent(input$bookmark2, {
    session$doBookmark()
  })
  
  js_click_line <- JS("function(event) {Shiny.onInputChange('line_clicked', [event.point.category]);}")
  
  observeEvent(input$reset_button, {
    reset("form")
  })
  
  id <- NULL
  
  observeEvent(input$reset_button, {
    
    id <<- showNotification(
      paste("Filters are Reset"),
      duration = 5, 
      type = "message"
    )
  })
  
  # Data
  
  filt_iemap1 <- reactive({
    iemap %>%
      filter(
        if (input$i2_county == "Select All") {COUNTYNAME %in% s2_County} else {COUNTYNAME == input$i2_county},
        if (input$i2_ed == "Select All") {EDNAME %in% s2_Ed} else {EDNAME == input$i2_ed}
      )
  })
  
  # Value Box 1
  output$count1 <- renderValueBox({
    hc12 <- filt_iemap1() %>%
      summarise(Totalhousehold = round(sum(as.numeric(`Total Households`)),digits = 0))
    valueBox(paste0(hc12), "TOTAL HOSUEHOLDS", icon = icon("users"),
             color = "green"
    )
  })
  
  # Value Box 2
  output$count2 <- renderValueBox({
    hc13 <- filt_iemap1() %>%
      summarise(Totalhousehold = round(sum(as.numeric(`Households No Car`)),digits = 0))
    valueBox( paste0(hc13), "TOTAL HOSUEHOLDS NO CAR", icon = icon("users"),
              color = "olive"
    )
  })
  
  
  # Value Box 3
  output$count3 <- renderValueBox({
    hc14 <- filt_iemap1() %>%
      summarise(Totalhousehold = sum(`Households 1 Car`))
    valueBox(paste0(hc14), "TOTAL HOUSEHOLDS 1 CAR", icon = icon("user-circle-o"),
             color = "blue"
    )
  })
  
  # Value Box 4
  output$count4 <- renderValueBox({
    hc15 <- filt_iemap1() %>%
      summarise(Totalhousehold = sum(as.numeric(`Households 4 or more Cars`)))
    valueBox(paste0(hc15), "TOTAL HOSUEHOLDS 3+ CARS", icon = icon("user-circle-o"),
             color = "blue"
    )
  })
  
  # Maps
  
  mappalette <- reactive ({
    colorNumeric("Reds", filt_iemap1()$`Total Households`)
  })
  
  #Popup
  mappopup <- reactive ({
    paste(sep = "<br/>",
          "<b>ELECTORAL DISTRICT: </b>",filt_iemap1()$EDNAME,
          "<i>Total Households</i>",filt_iemap1()$`Total Households`,
          "<i>Households 1 Car</i>",filt_iemap1()$`Households 1 Car`,
          "<i>Households 2 Cars</i>", filt_iemap1()$`Households 2 Cars`,
          "<i>Households 3 Cars</i>", filt_iemap1()$`Households 3 Cars`,
          "<i>Households 4 or more Cars</i>", filt_iemap1()$`Households 4 or more Cars`)
  })
  
  output$map1 <- renderLeaflet({
    leaflet(filt_iemap1()) %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      setView(lng = "-7.8454008244374", lat="53.1452412771661", zoom = 6) %>%
      addProviderTiles("CartoDB.Positron")
    
  }) # render Leaflet
  
  observe({
    pal1 <- mappalette()
    
    leafletProxy("map1", data = filt_iemap1()) %>%
      addPolygons(stroke = FALSE,
                  smoothFactor = 0.2,
                  fillOpacity = .75,
                  popup = mappopup(),
                  color = ~pal1(iemap$`Total Households`)
      ) %>%
      addMiniMap(position = "bottomleft", width = 150, height = 150,
                 collapsedWidth = 19, collapsedHeight = 19, zoomLevelOffset = -5,
                 zoomLevelFixed = FALSE, centerFixed = FALSE, zoomAnimation = TRUE,
                 toggleDisplay = TRUE, autoToggleDisplay = TRUE, minimized = TRUE,
                 aimingRectOptions = list(color = "#ff7800", weight = 1, clickable = TRUE),
                 shadowRectOptions = list(color = "#000000", weight = 1, clickable = TRUE,
                                          opacity = 0, fillOpacity = 0), strings = list(hideText = "Hide MiniMap", showText = "Show MiniMap"),
                 tiles = (providers$OpenStreetMap), mapOptions = list()) %>%
      addLegend("bottomright", pal = mappalette(), values = ~filt_iemap1()$`Total Households`,
                title = "INTELLIGENT ACQUISITION",
                opacity = 1)
  })
  
  output$map_value <- renderMapdeck({
    mapdeck(token = key,
            style = mapdeck_style('satellite-streets')
            ,pitch = 60
            ,zoom = 10
    ) %>%
      add_grid(
        data = df[1:364270, ]
        , lat = "LATITUDE"
        , lon = "LONGITUDE"
        , cell_size = 500
        , elevation_scale = 15
        , layer_id = "grid_layer"
      )
  }) # render MapDeck
  
  
}

shinyApp(ui, server)
