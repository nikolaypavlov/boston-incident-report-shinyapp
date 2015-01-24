library(leaflet)
library(ShinyDash)

shinyUI(fluidPage(
  #tags$head(tags$link(rel='stylesheet', type='text/css', href='styles.css')),
  leafletMap(
    "map", "100%", 400,
    initialTileLayer = '//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png', 
    initialTileLayerAttribution = HTML('Maps by <a href="http://www.mapbox.com/">Mapbox</a>'),
    options=list(
      center = c(42.3581, -71.0636),
      zoom = 13,
      maxBounds = list(list(17, -180), list(59, 180))
    )
  ),
  
  hr(),
  fluidRow(
    column(6, 
           h2('Boston Crime Incident Reports')
    ),
    column(5,
           dateInput('date', 'Choose Date', value = "2015-01-07", format = "yyyy-mm-dd")
    )
  ),
  fluidRow(
    column(6,
           h4('Details'),
           tableOutput('data')
    ),
    column(5,
           h4('Incidents per Hour'),
           plotOutput('dailyCrimeStats', width='100%', height='250px'),
           h4('How to use?'),
           p('- Use calendar to choose day'),
           p('- Zoom in area you interested in on map'),
           p('- Click on point of interest to get more details'),
           p('Data provided by data.cityofboston.gov')
    ) 
  )
))