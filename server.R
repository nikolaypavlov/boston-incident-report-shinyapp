library(leaflet)
library(ggplot2)
library(maps)
library(RSocrata)
library(lubridate)


token <- "zLERqu43Qw004KHhV9LaC5FzQ"
#url <- "https://data.cityofboston.gov/resource/9w2z-8tut.json?$where=year=2015&month=1"
df <- c()
year <- 0
month <- 0

# From a future version of Shiny
# bindEvent <- function(eventExpr, callback, env=parent.frame(), quoted=FALSE) {
#   eventFunc <- exprToFunction(eventExpr, env, quoted)
#   
#   initialized <- FALSE
#   invisible(observe({
#     eventVal <- eventFunc()
#     if (!initialized)
#       initialized <<- TRUE
#     else
#       isolate(callback())
#   }))
# }

shinyServer(function(input, output, session) {
  
  dateScope <- reactive({
    if (is.null(input$date))
      return(df[FALSE,])
      
    if (year(input$date) != year | month(input$date) != month) {
      url <- "https://data.cityofboston.gov/resource/9w2z-8tut.json?$where=year="
      url <- paste0(url, year(input$date), "&month=", month(input$date))
      year <- year(input$date)
      month <- month(input$date)
      df <- read.socrata(url)
    }
    df <- df[as.Date(df$fromdate) == as.Date(input$date), ]
  })
    
  makeReactiveBinding('selectedReport')
  
  # The cities that are within the visible bounds of the map
  reportsInBounds <- reactive({
    
    df <- dateScope()
    
    if (is.null(input$map_bounds))
      return(df[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    df[,"location.latitude"] <- as.numeric(df[,"location.latitude"])
    df[,"location.longitude"] <- as.numeric(df[,"location.longitude"])
    
    subset(df,
            location.latitude >= latRng[1] & location.latitude <= latRng[2] &
            location.longitude >= lngRng[1] & location.longitude <= lngRng[2])
  })
  
  # The top N cities (by population) that are within the visible bounds
  # of the map
  topreportsInBounds <- reactive({
    rep <- reportsInBounds()
  })
  
  
  
  # Create the map; this is not the "real" map, but rather a proxy
  # object that lets us control the leaflet map on the page.
  map <- createLeafletMap(session, 'map')
  
  observe({
    if (is.null(input$map_click))
      return()
    selectedReport <<- NULL
  })
  
  observe({
    map$clearMarkers()
    reports <- topreportsInBounds()
    
    if (nrow(reports) == 0)
      return()
    
    map$addMarker(
      reports$location.latitude,
      reports$location.longitude,
      row.names(reports),
      list(
        weight=1.2,
        fill=TRUE,
        color='#4A9'
      )
    )
  })
  
  observe({
    event <- input$map_marker_click
    if (is.null(event))
      return()
    map$clearPopups()
    
    isolate({
      reports <- topreportsInBounds()
      rep <- reports[row.names(reports) == event$id,]
      selectedReport <<- rep
      content <- as.character(tagList(
        tags$strong(rep$incident_type_description),
        tags$br(),
        paste("Time:", format(strptime(rep$fromdate, "%Y-%m-%dT%H:%M:%S"), "%H:%M")),
        tags$br(),
        paste("Weapon:", rep$weapontype),
        tags$br(),
        paste("Shooting:", rep$shooting)
      ))
      map$showPopup(event$lat, event$lng, content, event$id)
    })
  })
  
  output$data <- renderTable({
    if (nrow(topreportsInBounds()) == 0)
      return(NULL)
    
    t <- topreportsInBounds()
    names <- c("incident_type_description","main_crimecode", "weapontype", "shooting", "domestic")
    t <- t[,names]
    names(t) <- c("Incident", "Crimecode", "Weapon", "Shooting", "Domestic")
    t
  }, include.rownames = FALSE)
  
  output$dailyCrimeStats <- renderPlot({
    ds <- dateScope()
    ds$fromdate <- strptime(ds$fromdate, "%Y-%m-%dT%H:%M:%S")
    factors <- factor(hour(ds$fromdate), levels=0:23)
    count <- tapply(ds$fromdate, factors, length)
    
    if (!all(is.na(count))) {
      ds <- data.frame(hour=0:23, count=count)
      p <- ggplot(ds, aes(x = hour, y = count)) + geom_bar(stat="identity")
      p <- p + ylim(c(0, max(count)))
      p <- p + ylab('Number of incidents')
      print(p) 
    } else {
      return(NULL)
    } 
      
  })
})