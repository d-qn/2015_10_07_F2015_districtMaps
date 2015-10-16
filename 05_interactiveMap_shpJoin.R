library(swiTheme)
library(swiMap)
library(dplyr)
library(leaflet)
library(readxl)
require(rgdal)
require(rgeos)
require(maptools)

############################################################################################
###		SETTINGS
############################################################################################

## raw data files
vote2011.file <- "input/ef2011_bydistrict.csv"

# settings
party.sub <- c('PLR', 'PDC', 'PS', 'UDC', 'PVL', 'PBD', 'PES')

############################################################################################
###		LOAD DATA
############################################################################################

## 1. MAP DATA
path.ch <- getPathShp('CH')
co <- readOGR(path.ch, layer = 'municipalities-without-lakes')
# reproject coordintes in the standard projection: http://gis.stackexchange.com/questions/45263/converting-geographic-coordinate-system-in-r 
co <- spTransform(co, CRS("+init=epsg:4326"))

# assign the district ID as ID
## --> for district not present (== 0), use the canton ID * 100 !!!
co@data$id <- ifelse(co@data$BEZIRKSNR == 0, co@data$KANTONSNR * 100, co@data$BEZIRKSNR)


#http://gis.stackexchange.com/questions/63577/joining-polygons-in-r
co.joined <- unionSpatialPolygons(co, co@data$id)
#plot(co)
#plot(co.joined)

# Convert SpatialPolygons to data frame
co.df <- as(co, "data.frame")
# Aggregate desired data attributes by ID list
ids <- data.frame(id = unique(co@data$id))
rownames(ids) <- unlist(ids)
  
# Reconvert data frame to SpatialPolygons
di <- SpatialPolygonsDataFrame(co.joined, ids)

## load communes data and its mapping bfs# to district name
communes.read <- loadCommunesCHgeographicalLevels()[,1:4]
ofsId2district <- read_excel(dir(system.file("extdata", package="swiMap"), "be-b-00.04-rgs-01\\.xls", full.names = T), skip = 28, sheet = 3)
bounds <- which(!is.na(ofsId2district[,1]))[1:2]
ofsId2district <- as.data.frame(ofsId2district[bounds[1]:bounds[2]-1,3:4])
colnames(ofsId2district) <- c('ofsid', 'name')

di@data$districtName <- ofsId2district[match(di@data$id, ofsId2district$ofsid), 'name']


## 2. Load votes data 
vote <- read.csv(vote2011.file, row.names = 1)
stopifnot(di@data$id %in% rownames(vote) )

idx <- match(di@data$id,rownames(vote))

partis <- as.matrix(vote[idx,party.sub])
## compute for each district the most popular party
di@data <- cbind(di@data, 
  vote[idx,],
  maxParty = colnames(partis)[max.col(partis)][idx],
  maxPc = partis[matrix(c(1:nrow(partis), max.col(partis)), ncol = 2, nrow = nrow(partis))][idx]
)

############################################################################################
###		COLORS
############################################################################################

colorsMaxParty <- structure(
  c('#FCDB06', '#FF7D00', '#255FF6', '#FF0000', '#006A49'),
  names  = levels(di@data$maxParty))
bins <- round(seq(0, 100, by = 5))

palF <- colorFactor(colorsMaxParty, di@data$maxParty)

pal_UDC <- colorBin("BuGn", 0:100, bins = bins)
pal_PS  <- colorBin("Reds", 0:100, bins = bins)
pal_PLR <- colorBin("Blues", 0:100, bins = bins)
pal_PDC <- colorBin("Oranges", 0:100, bins = bins)
pal_PBD <- colorBin("YlOrRd", 0:100, bins = bins)

popup_max <- paste0("<strong>", di$districtName, "</strong>",
  "<br><strong>", di$maxParty, " ", round(di$maxPc),"%</strong>"
)


mb_tiles <- 'http://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}'
mb_attribution <- 'Tiles &copy; Esri &mdash; Esri, DeLorme, NAVTEQ'
m <- leaflet(data = di) %>% 
  addTiles(urlTemplate = mb_tiles, attribution = mb_attribution) %>% 
  setView(8.2, 46.8, zoom = 8)

map <- m %>% addPolygons(fillColor = ~palF(maxParty), fill = T, stroke = T, 
  color = "white", fillOpacity = 0.9, opacity = 0.7, weight = 1,
  popup = popup_max, group = "maxParty") %>%
  addLegend("bottomright", pal = palF, values = ~maxParty,
    title = "Parti", opacity = 0.9, layerId = "maxParty")

map %>% addPolygons(fillColor = ~palUDC(UDC), fill = T, stroke = T, 
  color = "white", fillOpacity = 0.9, opacity = 0.7, weight = 1,
  popup = popup_max, group = "UDC") %>%
  addLayersControl(
    baseGroups = c("maxParty", "UDC"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% hideGroup("UDC")



saveWidget(map, file="frontalierEmploi_map.html",  selfcontained = F, libdir = "js")



