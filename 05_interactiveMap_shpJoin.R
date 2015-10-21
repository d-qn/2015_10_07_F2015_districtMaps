if(!require(devtools)){
  install.packages("devtools")
  library(devtools)
}
if(!require(rgdal)) {
  install.packages("rgdal", repos="http://cran.us.r-project.org")
  require(rgdal)
}
if(!require(swiMap)) {
  install_github("d-qn/swiMap")
  require(swiMap)
}
if(!require(readxl)) {
  devtools::install_github("hadley/readxl")
  require(readxl)
}
if(!require(dplyr)) {
  install.packages("dplyr", repos="http://cran.us.r-project.org")
  require(dplyr)
}
if(!require(leaflet)) {
  install.packages("leaflet", repos="http://cran.us.r-project.org")
  require(leaflet)
}
if(!require(htmlwidgets)) {
  install.packages("htmlwidgets", repos="http://cran.us.r-project.org")
  require(htmlwidgets)
}
if(!require(maptools)) {
  install.packages("maptools", repos="http://cran.us.r-project.org")
  require(maptools)
}

############################################################################################
###		SETTINGS
############################################################################################

## raw data files
vote2015.file <- "input/ef2015_bydistrict.csv"

vote2011.file <- "input/ef2011_bydistrict.csv"
voteDiff.file <- "input/ef2015_2011_bydistrict.csv"

text.file <- "input/translations_interactiveMap.csv"

# settings
party.sub <- structure(
  c('#255FF6', '#FF7D00', '#FF0000', '#006A49', '#00E7A7', '#FCDB06', '#17A25A', '#333366'),
  names = c('PLR', 'PDC', 'PS', 'UDC', 'PVL', 'PBD', 'PES', "Autres.partis")
)

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

# Convert SpatialPolygons to data frame
co.df <- as(co, "data.frame")
# Aggregate desired data attributes by ID list
ids <- data.frame(id = unique(co@data$id))
rownames(ids) <- unlist(ids)

# Reconvert data frame to SpatialPolygons
di <- SpatialPolygonsDataFrame(co.joined, ids)

## load communes data and its mapping bfs# to district name
ofsId2district <- read_excel(dir(system.file("extdata", package="swiMap"), "be-b-00.04-rgs-01\\.xls", full.names = T), skip = 28, sheet = 3)
bounds <- which(!is.na(ofsId2district[,1]))[1:2]
ofsId2district <- as.data.frame(ofsId2district[bounds[1]:bounds[2]-1,3:4])
colnames(ofsId2district) <- c('ofsid', 'name')

di@data$districtName <- ofsId2district[match(di@data$id, ofsId2district$ofsid), 'name']


### 2. Load votes data  ####
vote     <- read.csv(vote2015.file, row.names = 1)
vote2011 <- read.csv(vote2011.file, row.names = 1)
voteDiff <- read.csv(voteDiff.file, row.names = 1)

stopifnot(di@data$id %in% rownames(vote) )
stopifnot(di@data$id %in% rownames(vote2011) )
stopifnot(di@data$id %in% rownames(voteDiff) )


idx <- match(di@data$id,rownames(vote))

stopifnot(all(names(party.sub) %in% colnames(vote)))

partis <- as.matrix(vote[idx, 1:match("Autres.partis", colnames(vote))])
## bind the parti vote data and compute for each district the most popular party
di@data <- cbind(
  di@data,
  vote[idx,],
  maxParty = colnames(partis)[max.col(partis)][idx],
  maxPc = partis[matrix(c(1:nrow(partis), max.col(partis)), ncol = 2, nrow = nrow(partis))][idx]
)
# order levels of 'maxParty' by the most dominate party
#relevel(di@data$maxParty)

voteDiff <- select(voteDiff, -canton)
stopifnot(colnames(voteDiff) %in% names(party.sub), ncol(voteDiff) == 8)
colnames(voteDiff) <- paste0(colnames(voteDiff), "_diff")

partis2011 <- as.matrix(vote2011[idx, 1:match("Autres.partis", colnames(vote2011))])
vote2011 <- select(vote2011, -canton, - Participation.en..)
# take the # of voters
stopifnot(names(party.sub) %in% colnames(vote2011), ncol(vote2011) == 9)
colnames(vote2011)[-ncol(vote2011)] <- paste0(colnames(vote2011)[-ncol(vote2011)], "_2011")

# bind all the data
di@data <- cbind(
  di@data,
  vote2011[idx,],
  voteDiff[idx,],
  maxParty_2011 = colnames(partis2011)[max.col(partis2011)][idx],
  maxPc_2011 = partis2011[matrix(c(1:nrow(partis2011), 
    max.col(partis2011)), ncol = 2, nrow = nrow(partis2011))][idx]
)

###### Get some statistics
table(di@data$maxParty)
(table(di@data$maxParty) / length(di@data$maxParty)) * 100

############################################################################################
###	 Load translation
############################################################################################

txt <- read.csv(text.file, row.names = 1, stringsAsFactors = F)

############################################################################################
###		MAP!
############################################################################################

# 1. Define colors
colorsMaxParty <- structure(
  party.sub[match(levels(di@data$maxParty), names(party.sub))],
  names  = levels(di@data$maxParty))
maxV <- ceiling(max(partis) / 10) * 10
bins <- round(seq(0, maxV, by = 5))

palF <- colorFactor(colorsMaxParty, di@data$maxParty)

binColorByParty <- function(party) {
  colorBin(colorRamp(c("white", party.sub[party]), interpolate="spline"), 0:maxV, bins = bins)
}
pal.party <- sapply(names(party.sub), binColorByParty)


############## LOOP BY LANGUAGE!
for (i in 1:ncol(txt)) {
  lang <- colnames(txt)[i]

  # 2. Define popups
  popup <- paste0("<strong>", di$districtName,"</strong> (", di$canton ,")<br>",
    '<p style="color:#808080;display:inline"><strong>',
    txt[paste0("short.", di$maxParty),lang], " ", round(di$maxPc),"%</p></strong><br>",
    '<p, li style="font-size: 0.8em">',
	 '<i>',
	#     txt['pop.Participation',lang], ": ",  round(di$`Participation.en..`), "%, ",
	     txt['pop.electeurs',lang], ": ", di$`Electeurs.inscrits`, "</i>",
	"<ul>" ,
    "<li>", txt['short.UDC',lang], ": ", round(di$UDC, 1), "%</li>",
    "<li>", txt['short.PS',lang],  ": ", round(di$PS, 1),   "%</li>",
    "<li>", txt['short.PLR',lang], ": ", round(di$PLR, 1), "%</li>",
    "<li>", txt['short.PDC',lang], ": ", round(di$PDC, 1), "%</li>",
    "<li>", txt['short.PES',lang], ": ", round(di$PES, 1), "%</li>",
    "<li>", txt['short.PBD',lang], ": ", round(di$PBD, 1), "%</li>",
    "<li>", txt['short.PVL',lang], ": ", round(di$PVL, 1), "%</li>",
    "<li>", txt['short.Autres.partis',lang], ": ", round(di$Autres.partis, 1), "%</li>",
    "</ul>"
  )

  popup2011 <- paste0("<strong>", di$districtName,"</strong> (", di$canton ,")<br>",
    '<p style="color:#808080;display:inline"><strong>',
    txt[paste0("short.", di$maxParty_2011),lang], " ", round(di$maxPc_2011),"% (2011)</p></strong><br>",
    '<p, li style="font-size: 0.8em">',
    '<i>',
    #     txt['pop.Participation',lang], ": ",  round(di$`Participation.en..`), "%, ",
    txt['pop.electeurs',lang], ": ", di$`Electeurs.inscrits`, "</i>",
    "<ul>" ,
    "<li>", txt['short.UDC',lang], ": ", round(di$UDC_2011, 1), "%</li>",
    "<li>", txt['short.PS',lang],  ": ",  round(di$PS_2011, 1),   "%</li>",
    "<li>", txt['short.PLR',lang], ": ", round(di$PLR_2011, 1), "%</li>",
    "<li>", txt['short.PDC',lang], ": ", round(di$PDC_2011, 1), "%</li>",
    "<li>", txt['short.PES',lang], ": ", round(di$PES_2011, 1), "%</li>",
    "<li>", txt['short.PBD',lang], ": ", round(di$PBD_2011, 1), "%</li>",
    "<li>", txt['short.PVL',lang], ": ", round(di$PVL_2011, 1), "%</li>",
    "<li>", txt['short.Autres.partis',lang], ": ", round(di$Autres.partis_2011, 1), "%</li>",
    "</ul>"
  )
  
  #popup for #voters
  popupCircle <- paste0(
    "<strong>", di$districtName,"</strong> (", di$canton ,")<br>",
    txt['pop.electeurs',lang], " : ", di$Electeurs.inscrits
  )
  centroids <- coordinates(di)
  idx <- match(di@data$id, rownames(centroids))
  di@data <- cbind(di@data, lng = centroids[idx,1], lat = centroids[idx,2])

  ### MAP!
  mb_tiles <- 'http://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}'
  mb_attribution <- txt["footer", lang]

  m <- leaflet(data = di) %>%
    addTiles(urlTemplate = mb_tiles, attribution = mb_attribution) %>%
    setView(8.4, 46.8, zoom = 8)

  map1 <- m %>% 
  addPolygons(fillColor = ~palF(maxParty), fill = T, stroke = T,
    color = "white", fillOpacity = 0.9, opacity = 0.7, weight = 1,
    popup = popup, group = txt["group.partiDominant", lang]) %>%
  addPolygons(fillColor = ~palF(maxParty_2011), fill = T, stroke = T,
    color = "white", fillOpacity = 0.9, opacity = 0.7, weight = 1,
    popup = popup2011, group = txt["group.partiDominant2011", lang]) %>%  
  addLegend(
    "bottomright", colors = colorsMaxParty,
    title = "", opacity = 0.9, layerId = "legend",
    labels = txt[paste0("full.", names(colorsMaxParty)),lang]) %>%
  addCircleMarkers(
    lng = ~lng, lat = ~lat,
    radius = ~sqrt(Electeurs.inscrits) / 50,
    color = "#black", popup  = popupCircle,
    stroke = FALSE, fillOpacity = 0.5,
    group = txt["group.electorateSize", lang]) %>%
  addLegend(position = "topright",
    title = txt['title.partiDominant', lang],
    opacity = 0, colors = NULL, labels = NULL) %>%
  addLayersControl(
    baseGroups = c(txt["group.partiDominant", lang], txt["group.partiDominant2011", lang]),
    overlayGroups = txt["group.electorateSize", lang],
    options = layersControlOptions(collapsed = FALSE)) %>%
  hideGroup(txt["group.electorateSize", lang])

  saveWidget(map1, file=paste0("districtMap_dominatingParty_", lang,".html"),
    selfcontained = F, libdir = "js")

  
	##### PLOT DIFF MAP
  popUpDiffSmall <- function(party) {
    diff <- eval(parse(text = paste0("di$", party, "_diff")))
    paste0(
      "<strong>", di$districtName,"</strong> (", di$canton ,")",
      '<p style="color:#808080;display:inline"><strong><br>',
      txt[paste0("short.", party),lang], ": ", ifelse(diff > 0, "+", ""),
        round(diff, 1),"%</p></strong><br>",
      " (", round(eval(parse(text = paste0("di$", party, "_2011"))), 1), "% in 2011)"
      )
  }
  popup.party <- sapply(names(party.sub), popUpDiffSmall, simplify = F)
  
  range <- ceiling(max(abs(voteDiff)) / 10) * c(-10, 10)
  dbins <- c(min(range), -8, -4, -2, -1, 0, 1, 2, 4, 8, max(range))
  colD <- colorRampPalette(rev(c("#336666", "white", "#75582B")), 
    interpolate = "spline")(length(dbins)-1)
  names(colD) <- paste0(gsub(",", " - ", gsub("(\\[|\\]|\\()", "",
    levels(cut(unlist(voteDiff), breaks = dbins)))), "%")
  names(colD)[1] <- gsub("^.*\\ - ", "> ", names(colD)[1] )
  names(colD)[length(colD)] <- paste0("> ", gsub(" \\- .*$", "%", names(colD)[length(colD)] ))
  palD <- colorBin(colD, domain = voteDiff, bins = dbins)
  
  ## MAP EVOLUTION 2011 VS 2015
  map2 <- m

  addPartyPolygon2 <- function(map, party) {
    map %>% addPolygons(fillColor = ~palD(eval(parse(text=paste0(party, "_diff")))),
    fill = T, stroke = T,
    color = "white", fillOpacity = 0.9, opacity = 0.7, weight = 1,
    popup = popup.party[[party]], group = txt[paste0("full.", party),lang])
  }
  for(party in names(party.sub)) {
    map2 <- addPartyPolygon2(map2, party)
  }

  map2 <- map2 %>%
    addCircleMarkers(
      lng = ~lng, lat = ~lat,
      radius = ~sqrt(Electeurs.inscrits) / 50,
      color = "#black", popup  = popupCircle,
      stroke = FALSE, fillOpacity = 0.5,
      group = txt["group.electorateSize", lang]
    ) %>%
    addLegend(position = "topright",
      title = txt['title.evolution', lang],
      opacity = 0, colors = NULL, labels = NULL) %>%
    addLegend( "bottomright", col = colD, title = txt["legend.evo", lang], opacity = 1,
      layerId = "legend2", labels = names(colD)) %>%
    addLayersControl(
      baseGroups = txt[paste0("full.", names(party.sub)),lang],
      overlayGroups = txt["group.electorateSize", lang],
      options = layersControlOptions(collapsed = FALSE)
    ) %>% showGroup(txt[paste0("full.", "UDC"),lang]) %>%
    hideGroup(txt["group.electorateSize", lang])

  saveWidget(map2, file = paste0("districtMap_Evolution_", lang,".html"),
    selfcontained = F, libdir = "js")
}
