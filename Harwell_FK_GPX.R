library(rgdal)
library(leaflet)
library(lubridate)
library(sp)
library(ggplot2)
library(plyr)
library(htmltools)
library(htmlwidgets)

# setwd("C:/SATELLITE_STUFF/GPX_Tom_Tom")
setwd("C:/RICARDO-AEA/SATELLITE_STUFF/GPX_Tom_Tom")
# load GPX file from Strava
gpx_file_Federico <- "Rientro_Lavoro.gpx"

# load GPX way POINTS data
# this is alreadt a spatialPolygon dataframe
wp <- readOGR(gpx_file_Federico, layer = "track_points")

# elevation and timestamp
head(wp[,c('ele', 'time')])

max(wp$ele) - min(wp$ele) # height climbed in meters

# the total distance travelled during the hike
hike.dists <- spDists(wp, segments=TRUE)
sum(hike.dists) # about 11.8km hike

# convert timestamps to datetime-objects
wp$time <- ymd_hms(wp$time, tz = "UTC-1")  # UK time
wp <- as.data.frame(wp)

# Plot height
# -----------

names(wp)

p <- ggplot(wp, aes(x=time, y=ele))
p + geom_point() + labs(x='Hiking time', y='Elevations (meters)')
# + geom_line() + geom_smooth() + 
# p + geom_line()


# popout timestamp for each waypoint
popup_info <- paste0("<p><strong>Lon: </strong>", 
                     wp$coords.x1,
                     "<strong>,  Lat: </strong>", 
                     wp$coords.x2,
                     "<br><strong>Time: </strong>", 
                     wp$time)


########################################################################

# Reading-in and displaying the GPX-tracks

track <- readOGR(gpx_file_Federico, layer = "tracks", verbose = FALSE)
# leaflet() %>% addTiles() %>% addPolylines(data=track)

m <- leaflet() %>%
  
  # Add tiles
  addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
  addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  
  addLegend(position = 'bottomright',opacity = 0.4, 
            colors = 'blue', 
            labels = 'Gimillan-Grausson',
            title = 'Hikes Italy, region Aosta') %>%
  
  
#    addMarkers(lng=wp$coords.x1, lat=wp$coords.x2,  
#               popup=as.vector(popup_info), # use the timestamp as popup-content 
#               group='Photo markers') %>%
  
  addCircles(lng=wp$coords.x1, lat=wp$coords.x2,  
             popup=as.vector(popup_info),
            weight = 3, radius=60, 
            color="#ffffff", stroke = FALSE, fillOpacity = 0, 
             group='Time & Position') %>%

  addPolylines(data=track, color='blue',
             group='Route') %>%
  
  # Layers control
  addLayersControl(position = 'bottomright',
                   baseGroups = c("Topographical", "Road map", "Satellite"),
                   overlayGroups = c("Route", "Time & Position"),
                   options = layersControlOptions(collapsed = FALSE))

m


saveWidget(m,
           file="Harwell_Federico.html",
           selfcontained = FALSE)


##########################################################################


