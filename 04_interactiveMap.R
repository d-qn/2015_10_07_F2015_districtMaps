library(swiTheme)
library(swiMap)
library(dplyr)
library(leaflet)
library(readxl)
require(rgdal)
require(rgeos)

############################################################################################
###		SETTINGS
############################################################################################
## input files
translation.file <- "input/translations.csv"

district.file <- "input/districtCH_map.csv"

vote2011.file <- "input/ef2011_district.csv"
vote2011byParty.file <- "input/ef2011_party.csv"


# settings
party.sub <- c('PLR', 'PDC', 'PS', 'UDC', 'PVL', 'PBD', 'PES')
## output files


txt <- read.csv(translation.file, row.names = 1, stringsAsFactors = F)

############################################################################################
###		PLOT
############################################################################################

### TO DO: LOOP BY LANGUAGE 
lang <- 'fr'


co.df <- read.csv(district.file)
votes.read <- read.csv(vote2011.file)

# transform votes.read to a matrix
partis <- as.matrix(votes.read %>% select(one_of(party.sub)))[which(!duplicated(co.df$id)),]

# compute the most popular party by district 
maxParty <- data.frame(
  maxParty = colnames(partis)[max.col(partis)], 
  maxPc = partis[matrix(c(1:nrow(partis), max.col(partis)), ncol = 2, nrow = nrow(partis))]
)
rownames(maxParty) <- co.df$id[which(!duplicated(co.df$id))]

# combine in one data.frame
data <- cbind(
  co.df,
  votes.read,
  maxParty[match(co.df$id, rownames(maxParty)),]
) 


hist(partis[matrix(c(1:nrow(partis), max.col(partis)), ncol = 2, nrow = nrow(partis))])

############################################################################################
###		COMBINE DATA
############################################################################################

colorRange <- c(swi_spal[1], "#663333")
pal <- colorFactor(colorRamp(colorRange), NULL, ordered = T)

state_popup <- paste0("<strong>Commune: </strong>",
                      ch$GEMNAME,
                      "<br><strong>Total des emplois en 2012: </strong>", ch$emplois,
                      "<br>Dont <strong>", ch$pc, "% </strong>occupés par des frontaliers",
                      "<br><strong>Nombre total de travailleurs frontaliers en 2014: </strong>", round(ch$abs)
)


mb_tiles <- 'http://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}'
mb_attribution <- 'Tiles &copy; Esri &mdash; Esri, DeLorme, NAVTEQ'
m <- leaflet(data = data) %>% 
  addTiles(urlTemplate = mb_tiles, attribution = mb_attribution) %>% 
  setView(8.227512, 46.818188, zoom = 8)
map <- m %>% addPolygons(lng = ~long, lat = ~lat, 
  fillColor = ~pal(maxParty), fill = T, fillOpacity = 0.9, opacity = 1, weight = 1)

saveWidget(map, file="frontalierEmploi_map.html",  selfcontained = F, libdir = "js")



