library(swiTheme)
library(swiMap)
library(readxl)
library(dplyr)
require(rgdal)
require(rgeos)
require(gtable)

font <- "Open Sans"
fontH <- "Open Sans Semibold"
############ HELPERS ##############

theme_map <- function(base_size = 9, base_family = font) {
  theme_bw(base_size = base_size, base_family = base_family) %+replace% theme(
      axis.line = element_blank(), axis.text = element_blank(), 
      axis.ticks = element_blank(), axis.title = element_blank(), 
      panel.background = element_blank(), panel.border = element_blank(), 
      panel.grid = element_blank(), panel.margin = unit(0, "lines"), 
      plot.background = element_blank(), legend.justification = c(0, 0),
      plot.margin = unit(c(4, 0, 1.5, 0), "lines"),
      plot.title = element_text(hjust = 0, vjust = 0, size = 20))
} 

############ SETTINGS ##############

lang.file <- "data/su-f-01.06.01.04.09_w-2013.xls"

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
  geom_polygon(aes(fill = var), colour = "#f7f5ed", size = 0.03) +
  theme_map() + coord_fixed(ratio = 1.6) + theme(legend.position = c(0.5, -0.01), 
  legend.direction = "horizontal", legend.justification = "center",
  legend.key.size = unit(0.9, "lines")) 


map <- co.map + scale_fill_manual(values = swi_rpal) + 
  geom_path(data = ct.df, colour = "#f7f5ed", size = 0.3) +
  ggtitle("Régions linguistiques de la Suisse") + guides(fill = guide_legend(title=NULL))

  
# http://stackoverflow.com/questions/21997715/add-ggplot-annotation-outside-the-panel-or-two-titles
grobMe <- function(gg, text = "Elections fédérales 2015") {
  g <- gtable_add_grob(ggplotGrob(gg), grobTree(textGrob(text, x = 0,  y = unit(0.18, "npc"), hjust = 0, gp = gpar(fontsize = 18, fontfamily = fontH, 
  fontface = 'italic', col = "grey"))), t=1, l=4)
  grid.draw(g)    
}




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
  geom_path(data = ct.df, colour = "#f7f5ed", size = 0.3) +
  ggtitle("5 plus grandes agglomérations de Suisse") + guides(fill = F)

map2 <- map2 + geom_point(data = cities, aes(x = lon, y = lat, size = size, group = NULL), alpha=0.7, colour = "#4C4C4C") + 
  geom_text(data = cities,  aes(x = lon, y = lat - 0.07, label = names, group = NULL)) +
  scale_size_area(max_size = 20)

pdfswi_long("test.pdf")
grid.newpage()
grobMe(map)
grid.newpage()
grobMe(map2)
dev.off()


