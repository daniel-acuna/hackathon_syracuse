library(shinydashboard)
library(shiny)
library(leaflet)
library(sqldf)
library(rgdal)
library(ggmap)
library(ggplot2)
library(plyr)
library(sqldf)
library(RColorBrewer)
library(cowplot)
library(lubridate)
library(caret)
library(stringr)
library(RColorBrewer)
library(xgboost)

#city_map <- get_map(unlist(geocode("Syracuse University, Syracuse, NY")), zoom = 13)

prediction_formula_history <- formula(overall ~ CART_TYPE + MUNL + MUNR + length + width + class + yearOverlay +
                                        previous_yearRated + previous_overall + previous_pavement +
                                        previous_crack + previous_patch + years_back)

prediction_formula_potholes <- formula(n_potholes ~ CART_TYPE + MUNL + MUNR + length + width + class + yearOverlay +
                                         previous_yearRated + overall + pavement + crack + patch + previous_overall + previous_pavement +
                                         previous_crack + previous_patch + I(2016 - yearRated))

temporal_data2 <- readRDS('data/road_temporal_data.rds')

# select smallest year back
# latest_temporal_data <- sqldf('
#                               select td.*
#                               from
#                               (select STREET_ID, min(years_back) as years_back from temporal_data2 group by STREET_ID) latest_year_rated,
#                               temporal_data2 td
#                               where td.STREET_ID = latest_year_rated.STREET_ID and td.years_back = latest_year_rated.years_back')

latest_temporal_data <- readRDS('data/latest_temporal_data.rds')

year_2015 <- readRDS('data/final_shape_data.rds')
shape_2015 <- readRDS('data/final_shape_data_sp.rds')
shape_data <- shape_2015@data
color_palette <- colorRampPalette(brewer.pal(11, "RdBu"))(10)
xgboost_final_model <- xgb.load('model/xgboost_final_model.model')
color_list <- sample(color_palette, dim(shape_2015@data)[1], replace = TRUE)



function(input, output, session) {
  
  # # Route select input box
  # output$routeSelect <- renderUI({
  #   live_vehicles <- getMetroData("VehicleLocations/0")
  #   
  #   routeNums <- sort(unique(as.numeric(live_vehicles$Route)))
  #   # Add names, so that we can add all=0
  #   names(routeNums) <- routeNums
  #   routeNums <- c(All = 0, routeNums)
  #   selectInput("routeNum", "Route", choices = routeNums, selected = routeNums[2])
  # })
  # 
  # # Locations of all active vehicles
  # vehicleLocations <- reactive({
  #   input$refresh # Refresh if button clicked
  #   
  #   # Get interval (minimum 30)
  #   interval <- max(as.numeric(input$interval), 30)
  #   # Invalidate this reactive after the interval has passed, so that data is
  #   # fetched again.
  #   invalidateLater(interval * 1000, session)
  #   
  #   getMetroData("VehicleLocations/0")
  # })
  # 
  # # Locations of vehicles for a particular route
  # routeVehicleLocations <- reactive({
  #   if (is.null(input$routeNum))
  #     return()
  #   
  #   locations <- vehicleLocations()
  #   
  #   if (as.numeric(input$routeNum) == 0)
  #     return(locations)
  #   
  #   locations[locations$Route == input$routeNum, ]
  # })
  # 
  # # Get time that vehicles locations were updated
  # lastUpdateTime <- reactive({
  #   vehicleLocations() # Trigger this reactive when vehicles locations are updated
  #   Sys.time()
  # })
  # 
  # # Number of seconds since last update
  # output$timeSinceLastUpdate <- renderUI({
  #   # Trigger this every 5 seconds
  #   invalidateLater(5000, session)
  #   p(
  #     class = "text-muted",
  #     "Data refreshed ",
  #     round(difftime(Sys.time(), lastUpdateTime(), units="secs")),
  #     " seconds ago."
  #   )
  # })
  # 
  # output$numVehiclesTable <- renderUI({
  #   locations <- routeVehicleLocations()
  #   if (length(locations) == 0 || nrow(locations) == 0)
  #     return(NULL)
  #   
  #   # Create a Bootstrap-styled table
  #   tags$table(class = "table",
  #              tags$thead(tags$tr(
  #                tags$th("Color"),
  #                tags$th("Direction"),
  #                tags$th("Number of vehicles")
  #              )),
  #              tags$tbody(
  #                tags$tr(
  #                  tags$td(span(style = sprintf(
  #                    "width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
  #                    dirColors[4]
  #                  ))),
  #                  tags$td("Northbound"),
  #                  tags$td(nrow(locations[locations$Direction == "4",]))
  #                ),
  #                tags$tr(
  #                  tags$td(span(style = sprintf(
  #                    "width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
  #                    dirColors[1]
  #                  ))),
  #                  tags$td("Southbound"),
  #                  tags$td(nrow(locations[locations$Direction == "1",]))
  #                ),
  #                tags$tr(
  #                  tags$td(span(style = sprintf(
  #                    "width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
  #                    dirColors[2]
  #                  ))),
  #                  tags$td("Eastbound"),
  #                  tags$td(nrow(locations[locations$Direction == "2",]))
  #                ),
  #                tags$tr(
  #                  tags$td(span(style = sprintf(
  #                    "width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
  #                    dirColors[3]
  #                  ))),
  #                  tags$td("Westbound"),
  #                  tags$td(nrow(locations[locations$Direction == "3",]))
  #                ),
  #                tags$tr(class = "active",
  #                        tags$td(),
  #                        tags$td("Total"),
  #                        tags$td(nrow(locations))
  #                )
  #              )
  #   )
  # })
  # 
  # Store last zoom button value so we can detect when it's clicked
  lastSearchButtonValue <- NULL
  initialLocation <- NULL
  output$busmap <- renderLeaflet({
    new_data <- model.matrix(prediction_formula_history, latest_temporal_data)
    new_data[, 'years_back'] <- new_data[, 'years_back'] + (input$year - 2016)
    predictions <- data.frame(predicted_overall = predict(xgboost_final_model, new_data), 
               STREET_ID = latest_temporal_data$STREET_ID)
    shape_2015@data <- merge(shape_data, predictions, by='STREET_ID', all.x = TRUE)
    
    color_list <- sample(color_palette, dim(shape_2015@data)[1], replace = TRUE)
    greens = colorNumeric("RdBu", domain = c(0, 10))
    
    map <- leaflet(shape_2015) %>%
      addTiles('https://{s}.tile.thunderforest.com/neighbourhood/{z}/{x}/{y}.png') %>%
      addPolylines(color=~greens(predicted_overall)) %>%
      addLegend(pal=greens, values=c(0,5, 10), title = "Road condition")
    
    if (is.null(initialLocation)) {
      location <- data.frame(lon=-76.13452, lat=43.03857)
      initialLocation <- TRUE
      print("Conditions set")
      map <- map %>% setView(lng=location$lon, lat=location$lat, zoom=15)
    } 
    
    # rezoom <- "first"
    # # If zoom button was clicked this time, and store the value, and rezoom
    # if (!identical(lastSearchButtonValue, input$searchButton)) {
    #   lastSearchButtonValue <<- input$searchButton
    #   rezoom <- "always"
    #   print(input$address)
    #   location <- unlist(geocode(input$address))
    #   map <- map %>% setView(lng=location[1], lat=location[2], zoom=15)
    # }
    
    map
  })
  
  
  # observe({
  #   year_prediction <- input$year
  #   print(year_prediction)
  #   new_data <- model.matrix(prediction_formula_history, latest_temporal_data)
  #   new_data[, 'years_back'] <- new_data[, 'years_back'] + (year_prediction - 2016)
  #   predictions <- data.frame(predicted_overall = predict(xgboost_final_model, new_data), 
  #                             STREET_ID = latest_temporal_data$STREET_ID)
  #   shape_2015@data <- merge(shape_data, predictions, by='STREET_ID', all.x = TRUE)
  #   
  #   color_list <- sample(color_palette, dim(shape_2015@data)[1], replace = TRUE)
  #   greens = colorNumeric("RdBu", domain = c(0, 10))
  #   leafletProxy("map", data = shape_2015) %>% clearShapes() %>% addPolylines(color=c('red'))
  # })
  
  # observe({
  #   if (is.null(input$address))
  #     return()
  #   isolate({
  #     location <- unlist(geocode(input$address))
  #     previousLocation <- location
  #     map <- leafletProxy("map") %>% setView(lng = location$lon, lat = location$lat, zoom=13)
  #   })
  # })
}