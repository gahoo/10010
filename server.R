
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)
library(jsonlite)
library(leaflet)
library(DT)
library(scales)

source('misc.R')

shinyServer(function(input, output) {
  
  subsetbroadband<-reactive({
    columnFilter<-function(column){
      if(is.NULL(input[[column]])){
        column_filter = unique(as.character(broadband[[column]]))
      }else{
        column_filter = input[[column]]
      }
      column_filter
    }
    
    PROVINCEfilter = columnFilter('PROVINCE')
    SPEEDfilter = columnFilter('SPEED')
    TARIFF_TYPEfilter = columnFilter('TARIFF_TYPE')
    subset(broadband, SPEED %in% SPEEDfilter &
             PROVINCE %in% PROVINCEfilter &
             TARIFF_TYPE %in% TARIFF_TYPEfilter)
  })
  
  boundsbroadband<-reactive({
    cities<-c()
    if(is.null(input$map_geojson_click)|input$tbl_bounds){
      bounds<-subset(broadband.lat.long,
                     latitude < input$map_bounds$north &
                       latitude > input$map_bounds$south &
                       longitude < input$map_bounds$east &
                       longitude > input$map_bounds$west)
      cities<-bounds$CITY
    }else{
      cities<-input$map_geojson_click$properties$parent
    }
    subset(subsetbroadband(), CITY %in% cities)
  })
  
  observe({
    broadband_sub<-subsetbroadband()
    colorpal<-colorNumeric('OrRd', broadband_sub[[input$show]])
    fillColors<-colorpal(broadband_sub[[input$show]])
    names(fillColors)<-broadband_sub$CITY
    city_names<-unique(as.character(broadband_sub$CITY))
    geocities_style<-lapply(city_names, function(city_name){
      mapCityStyle(geocity=geocities[[city_name]],
                   city_name=city_name,
                   fillColor=fillColors[[city_name]],
                   weight = 1, color = fillColors[[city_name]],
                   opacity = 1, fillOpacity = 0.8)
    })
    
    leafletProxy("map") %>%
      clearGeoJSON() %>%
      addGeoJSON(geocities_style)
  })
  
  observe({
    broadband_sub<-subsetbroadband()
    colorpal<-colorNumeric('OrRd', broadband_sub[[input$show]])
    leafletProxy("map", data=broadband_sub) %>%
      clearControls() %>%
      addLegend(position = "bottomright",
                pal = colorpal,
                bins =5,
                values = as.formula(sprintf("~%s", input$show)))
  })
  
  output$map<-renderLeaflet({
    leaflet() %>%
      setView(lng=104, lat=36, zoom=4) %>%
      addTiles()
  })
  
  output$helper<-renderText({
    str(input)
    as.character(input$map_geojson_click)
  })
  
  output$broadband_tbl<-renderDataTable({
    bounds<-boundsbroadband()
    bounds$SPEED<-bounds$SPEED.number
    columns<-c("PROVINCE", "CITY", "TARIFF_TYPE",  "PRICE", "SPEED", "PRODUCT_NAME", "SPEED_PRICE")
    bounds<-bounds[columns]
    perform_price<-"SPEED/(PRICE * TARIFF_TYPE)"
    colnames(bounds)[7]<-perform_price
    datatable(bounds) %>%
      formatStyle(perform_price,
                  fontWeight = styleInterval(median(bounds[[perform_price]]),
                                             c('normal', 'bold'))) %>%
      formatStyle(
        perform_price,
        background = styleColorBar(bounds[[perform_price]], 'tomato'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        'PRICE',
        background = styleColorBar(bounds[['PRICE']], 'coral'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        'SPEED',
        background = styleColorBar(bounds[['SPEED']], 'crimson'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
    
    })
  
  output$cities_bar_plot<-renderPlot({
    ggplot(boundsbroadband(), 
           aes_string(x='PRODUCT_NAME', y=input$show, fill='SPEED'))+
      geom_bar(stat='identity', position='dodge')+
      facet_wrap(~CITY, scales="free_x", nrow=1)+
      theme(axis.text.x = element_text(angle = 90))
  })
  
  output$cities_dot_plot<-renderPlot({
    ggplot(boundsbroadband(), 
           aes(y=CITY, x=SPEED_PRICE, color=SPEED, size=SPEED.number, alpha=PRICE))+
      geom_point() +
      facet_grid(PROVINCE ~ ., space='free', scales='free')
  })
  
  output$SPEED_filter<-renderUI({
    selectInput('SPEED', 'SPEED',
                choices=c(unique(as.character(broadband$SPEED)), 'NULL'),
                selected='NULL')
  })
  
  output$PROVINCE_filter<-renderUI({
    selectInput('PROVINCE', 'PROVINCE',
                choices=c(unique(as.character(broadband$PROVINCE)), 'NULL'),
                selected='NULL')
  })
  
  output$TARIFF_TYPE_filter<-renderUI({
    selectInput('TARIFF_TYPE', 'TARIFF_TYPE',
                choices=c(unique(as.character(broadband$TARIFF_TYPE)), 'NULL'),
                selected='NULL')
  })

})
