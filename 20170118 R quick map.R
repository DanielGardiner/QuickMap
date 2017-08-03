
# set working directory to same folder that this R script is saved in 

wd = dirname(rstudioapi::getActiveDocumentContext()$path)

setwd(wd)

# load packages and functions 

library(MRAtools)
library(leaflet)
library(htmlwidgets)
source("20170116 quick map functions.R")

# read in data from clipboard 

temp = read.excel()

# extract out as postcode

postcodes = as.character(temp[, 1])

# apply get.geog function

data = get.geog(odbc.name = "phegisdb", pcodes = postcodes, geog = c("la", "hpt", "phec"))

# set working directory to shape file subfolder 

setwd(paste0(wd, "/shape files"))

# look at all available shape files 

list.files()[grep("shp$", list.files())]

# load desired shape files

centre.map = spTransform(readOGR(".", "20161213_PHE_Centres_En_-_from_1st_July_2015"), 
                         CRS("+proj=longlat +datum=WGS84"))

hpt.map = spTransform(readOGR(".", "20170131_Health_Protection_Teams_En"), 
                         CRS("+proj=longlat +datum=WGS84"))

sw.la.map = spTransform(readOGR(".", "20170118_Local_Authority_Districts_SW_Only"), 
                        CRS("+proj=longlat +datum=WGS84"))






#--------------------------------------------------
# BASIC MAP

leaflet() %>% 
  # add open street view map
  addTiles(group = "OpenStreetMap") %>% 
  # add cartodb map 
  addProviderTiles("CartoDB.Positron", group = "CartoDB.Positron") %>% 
  # add centre shape file layer
  addPolygons(data = centre.map, fill = TRUE, fillOpacity = 0.4,
              stroke = TRUE,
              opacity = 1,
              color = "black",
              fillColor = "grey",
              popup = paste("<b><a>", centre.map@data$PHECNM, "</a></b>"),
              group = "PHE Centre layer") %>% 
  # add markers
  addMarkers(lng=data$longitude,
             lat=data$latitude,
             popup = data$postcodes,
             #clusterOptions = markerClusterOptions(),
             group = "Points") %>% 
  # add layers control
  addLayersControl(baseGroups = c("OpenStreetMap", "CartoDB.Positron"),
                   overlayGroups = c("Points", "PHE Centre layer"),
                   options = layersControlOptions(collapsed = FALSE))


#--------------------------------------------------
# MORE INFO MAP


leaflet() %>% 
  # add open street view map
  addTiles(group = "OpenStreetMap") %>% 
  # add cartodb map 
  addProviderTiles("CartoDB.Positron", group = "CartoDB.Positron") %>% 
  # add centre shape file layer
  addPolygons(data = centre.map, fill = TRUE, fillOpacity = 0.4,
              weight = 5,
              stroke = TRUE,
              opacity = 1,
              color = "black",
              fillColor = "grey",
              popup = paste("<b><a>", centre.map@data$PHECNM, "</a></b>"),
              group = "Centre layer") %>% 
  # add south west health protection team layer
  addPolygons(data = hpt.map, 
              fill = TRUE, 
              fillOpacity = 0.3,
              weight = 3,
              stroke = TRUE,
              opacity = 1,
              color = "black",
              fillColor = "grey",
              popup = paste("<b><a>", hpt.map@data$HPTNM, "</a></b>"),
              group = "Health Protection Team layer") %>% 
  # add south west local authority layer
  addPolygons(data = sw.la.map, fill = TRUE, fillOpacity = 0.2,
              weight = 2,
              stroke = TRUE,
              opacity = 1,
              color = "black",
              fillColor = "grey",
              popup = paste("<b><a>", sw.la.map@data$GSS_NM, "</a></b>"),
              group = "Local Authority layer") %>% 
  # add case markers
  addMarkers(lng=data$longitude,
             lat=data$latitude,
             popup = data$postcode,
             #clusterOptions = markerClusterOptions(),
             group = "Cases") %>% 
  addCircleMarkers(lng=data$longitude,
                   lat=data$latitude,
                   #popup = data$postcode,
                   color = "red",
                   opacity = 0.5,
                   fillOpacity = 0,
                   group = "Cases") %>% 
  # add layers control
  addLayersControl(baseGroups = c("OpenStreetMap", "CartoDB.Positron"),
                   overlayGroups = c("Cases", "Local Authority layer", 
                                     "Health Protection Team layer", "Centre layer"),
                   options = layersControlOptions(collapsed = FALSE))




#--------------------------------------------
# for daiga

m = leaflet() %>% 
  # add open street view map
  addTiles(group = "OpenStreetMap") %>% 
  # add cartodb map 
  addProviderTiles("CartoDB.Positron", group = "CartoDB.Positron") %>% 
  # add centre shape file layer
  addPolygons(data = centre.map, fill = TRUE, fillOpacity = 0.4,
              weight = 5,
              stroke = TRUE,
              opacity = 1,
              color = "black",
              fillColor = "grey",
              popup = paste("<b><a>", centre.map@data$PHECNM, "</a></b>"),
              group = "Centre layer") %>% 
  # add south west local authority layer
  addPolygons(data = sw.la.map, fill = TRUE, fillOpacity = 0.2,
              weight = 2,
              stroke = TRUE,
              opacity = 1,
              color = "black",
              fillColor = "grey",
              popup = paste("<b><a>", sw.la.map@data$GSS_NM, "</a></b>"),
              group = "South West Local Authority layer") %>% 
  # add case markers
  addMarkers(lng=data$longitude,
             lat=data$latitude,
             popup = data$postcode,
             #clusterOptions = markerClusterOptions(),
             group = "Cases") %>% 
  # add layers control
  addLayersControl(baseGroups = c("OpenStreetMap", "CartoDB.Positron"),
                   overlayGroups = c("Cases", "South West Local Authority layer", "PHE Centre layer"),
                   options = layersControlOptions(collapsed = FALSE))

setwd("C:/Users/daniel.gardiner/Desktop")

saveWidget(m, file = "Case_Map.html")




