library(swiTheme)
library(swiMap)
library(readxl)
library(dplyr)
library(animation)
require(rgdal)
require(rgeos)
require(gtable)
require("RColorBrewer")

font <- "Open Sans"
fontH <- "Open Sans Semibold"
key.size <- unit(2, "lines")
keyText.size <- 20
legend.pos <- c(0.5, -0.05) 
cityText.size <- 12
district.size <- 0.08
canton.size <- 0.5

############ HELPERS ##############

theme_map <- function(base_size = 9, base_family = font) {
  theme_bw(base_size = base_size, base_family = base_family) %+replace% theme(
      axis.line = element_blank(), axis.text = element_blank(), 
      axis.ticks = element_blank(), axis.title = element_blank(), 
      panel.background = element_blank(), panel.border = element_blank(), 
      panel.grid = element_blank(), panel.margin = unit(0, "lines"), 
      plot.background = element_blank(), legend.justification = c(0, 0),
      plot.margin = unit(c(3, 0, 1.2, 0), "lines"),
      legend.key = element_rect(colour = NA),
      legend.key.width=unit(2,"line"),
      legend.text=element_text(size = keyText.size),
      plot.title = element_text(hjust = 0, vjust = 0, size = 50))
} 

############ SETTINGS ##############

lang.file <- "data/su-f-01.06.01.04.09_w-2013.xls"
vote.current <- "data/su-f-17.02.03.03.zb.2011.d.xls"

party.sub <- c('PLR', 'PDC', 'PS', 'UDC', 'PVL', 'PBD', 'PES')


############ 1 PLOT LANGUAGE MAP and the largest cities ##############

lang.read <- read_excel(lang.file, skip = 3)
# remove NA rows
lang.read <- lang.read[!is.na(lang.read[,2]),]
colnames(lang.read)[1:2] <- c('canton', 'name')
# remove NA columsn which are the error%
lang.read <- lang.read[, !is.na(colnames(lang.read))]
# convert to numeric
lang.read[,3:ncol(lang.read)] <- apply(lang.read[,3:ncol(lang.read)], 2, as.numeric)
lang <- (select(lang.read, Allemand, Français, Italien, Romanche) / lang.read$`Population totale`) * 100 >= 30
rownames(lang) <- lang.read$name
lang[which(is.na(lang), 2)] <- "FALSE"
lang <- sapply(colnames(lang), function(lg) ifelse(lang[,lg], paste(lg, ""), ""))
lang <- apply(lang, 1, paste, collapse = ", ", sep="")
lang <- gsub(" $", "", gsub("(, )+", "", lang))



## load communes data and its mapping bfs# to district name
communes.read <- loadCommunesCHgeographicalLevels()[,1:4]
ofsId2district <- read_excel(dir(system.file("extdata", package="swiMap"), "be-b-00.04-rgs-01\\.xls", full.names = T), skip = 28, sheet = 3)
bounds <- which(!is.na(ofsId2district[,1]))[1:2]
ofsId2district <- as.data.frame(ofsId2district[bounds[1]:bounds[2]-1,3:4])
colnames(ofsId2district) <- c('ofsid', 'name')

path.ch <- getPathShp('CH')
co <- readOGR(path.ch, layer = 'municipalities-without-lakes')
# reproject coordintes in the standard projection: http://gis.stackexchange.com/questions/45263/converting-geographic-coordinate-system-in-r 
co <- spTransform(co, CRS("+init=epsg:4326"))

# assign the district ID as ID
## --> for district not present, use the canton ID * 100 !!!
co@data$id <- ifelse(co@data$BEZIRKSNR == 0, co@data$KANTONSNR * 100, co@data$BEZIRKSNR)
# transform to data.frame
co.df <- fortify(co,  region = "id")
co.df$id <- as.numeric(co.df$id)

# load canton borders
ct <- readOGR(path.ch, layer = 'swissBOUNDARIES3D_1_2_TLM_KANTONSGEBIET')
ct <- spTransform(ct, CRS("+init=epsg:4326"))
ct.df <- formatShp(ct)

co.df$districtName <- ofsId2district[match(co.df$id, ofsId2district$ofsid),'name']
co.df$var <- lang[match(unlist(co.df$districtName), names(lang))]


# plot swiss districts map
co.map <- ggplot(co.df, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = var), colour = "#f7f5ed", size = district.size) +
  theme_map() + coord_fixed(ratio = 1.5) + theme(legend.position = legend.pos, 
  legend.direction = "horizontal", legend.justification = "center",
  legend.key.size = key.size) 


map <- co.map + scale_fill_manual(values = swi_rpal) + 
  geom_path(data = ct.df, colour = "#f7f5ed", size = canton.size) +
  ggtitle("Régions linguistiques de Suisse") + guides(fill = guide_legend(title=NULL))

  
# http://stackoverflow.com/questions/21997715/add-ggplot-annotation-outside-the-panel-or-two-titles
grobMe <- function(gg, text = "Elections fédérales 2015", footer ="@duc_qn | swissinfo.ch | source: Office fédéral de la statistique") {

#   g <- gtable_add_grob(ggplotGrob(gg), 
#     grobTree(textGrob(text, x = 0,  y = unit(0.18, "npc"), hjust = 0, 
#     gp = gpar(fontsize = 20, fontfamily = fontH, fontface = 'italic', col = "#666666"))), t=1, l=4)
#   
#   grid.arrange(g, sub = textGrob(footer, x = 0.95, vjust = -0.1, hjust = 1,
#     gp = gpar(fontsize = 8, fontfamily = font, col = "#CCCCCC")))
 
  grid.arrange(gg, main =  textGrob(text, x = 0.01, hjust = 0, vjust = 4,
    gp = gpar(fontsize = 40, fontfamily = font, fontface = 'italic', col = "#CCCCCC")),
    sub = textGrob(footer, x = 0.95, vjust = -0.1, hjust = 1,
    gp = gpar(fontsize = 16, fontfamily = font, col = "#CCCCCC"))) 
}


## largest cities

### #### add largest Swiss cities
# library(ggmap)
# cities <- data.frame(
#   names = c("Zurich", "Geneva", "Basel", "Bern", "Lausanne"),
#   size = c(1102, 493, 486, 349, 317)
# )
# cities <- cbind(cities, geocode(paste(cities$names, ", Switzerland")))
# write.csv(cities, file = "data/cities.csv", row.names = F)
cities <-read.csv("data/cities.csv")



map2 <- co.map + scale_fill_manual(values = alpha(swi_rpal, 0.3), guide = F) + 
  geom_path(data = ct.df, colour = "#f7f5ed", size = district.size) +
  theme(legend.key.size = key.size) +
  ggtitle("5 plus grandes agglomérations de Suisse") + guides(fill = F)

map2 <- map2 + geom_point(data = cities, aes(x = lon, y = lat, size = size, group = NULL), alpha=0.7, colour = "#4C4C4C") + 
  geom_text(data = cities,  aes(x = lon, y = lat - 0.07, label = names, group = NULL), size = cityText.size) +
  scale_size_area(max_size = 20)

#pdf("test.pdf", width = 10, height = 11)
saveGIF({

  grobMe(map)
  grobMe(map2)
  
  ############ CHOROPLETH ##############
  # load elections data
  elec.read <- read_excel(vote.current, skip = 3, sheet = 2)
  elec.read <- elec.read[!is.na(elec.read[,2]),]
  
  # transform OFS ID to numeric, remove non-numerical rows
  elec.read[,1] <- as.numeric(unlist(elec.read[,1]))
  elec.read <- elec.read[!is.na(elec.read[,1]),]
  # rename columns
  colnames(elec.read)[1:3] <- c('ofsid', 'name', 'canton')
  colnames(elec.read) <- gsub(" (\\w+\\)|\\(.*\\))$", "", gsub("^ +", "", colnames(elec.read)))
  
  
  partis <- as.matrix(elec.read[,match(party.sub, colnames(elec.read))])
  partis <- cbind(partis, "Autres partis" = round((100 - rowSums(partis, na.rm = T)), 2))
  #replace NA by 0
  partis[which(is.na(partis), T)] <-0
  
  co2partis <- match(co.df$id, elec.read$ofsid)
  
  naIdx<- which(is.na(co2partis))
  
  if(length(naIdx) > 0) {
    warning("Some districts could not be matched!!", "\n")
    print( unique(co.df[naIdx, c('id', 'districtName')]))
    
    ## hack for non-matched bfs id!
    hackID <- data.frame(
      id = c(311, 312, 313, 314, 315, 316),
      idOld = c(303, 303, 302, 304, 305, 301)
    )
    # one-line super match 
    co2partis[naIdx] <- match(hackID$idOld, elec.read$ofsid)[match(co.df$id[naIdx], hackID$id)]
    stopifnot(all(!is.na(co2partis)))
  }
  
  
  
  py <- 'UDC'
  # nbreaks <- 13
  # breaks <- unique(round(quantile(partis, probs = seq(0, 1, 1/nbreaks))))
  # log breaks
  breaks <- c(0, 2^(0:6), 100)
  
  for(py in colnames(partis)) {
    
    co.df$var <- cut(partis[co2partis,py], breaks, include.lowest = T)
    
    co.map <- ggplot(co.df, aes(x = long, y = lat, group = group)) + 
      geom_polygon(aes(fill = var), colour = "#f7f5ed", size = district.size) +
      theme_map() + coord_fixed(ratio = 1.5) + theme(legend.position = legend.pos, 
      legend.direction = "horizontal", legend.justification = "center",
      legend.key.size = key.size) + guides(fill=guide_legend(nrow=2)) +
      geom_path(data = ct.df, colour = "#f7f5ed", size = canton.size)
    
    choro <- co.map + 
#       scale_fill_manual(drop = FALSE, 
#       values = colorRampPalette(swi_spal, interpolate = "spline")(length(breaks)),
      scale_fill_brewer(palette ="Purples", drop = FALSE,
      labels = paste0(gsub(",", "-", gsub("(\\[|\\]|\\()", "", levels(co.df$var))), "%")) +
    ggtitle(py)
    
    grobMe(choro)
  }
}, movie.name = "test.gif", interval = 6, nmax = 50, ani.width = 1280, ani.height = 1200, loop = TRUE)
#dev.off()
