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
      plot.title = element_text(hjust = 0, size = 20))
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
# assign the district ID as ID
## --> for district not present, use the canton ID * 100 !!!
co@data$id <- ifelse(co@data$BEZIRKSNR == 0, co@data$KANTONSNR * 100, co@data$BEZIRKSNR)
# transform to data.frame
co.df <- fortify(co,  region = "id")
co.df$id <- as.numeric(co.df$id)

# load canton borders
ct <- readOGR(path.ch, layer = 'swissBOUNDARIES3D_1_2_TLM_KANTONSGEBIET')
ct.df <- formatShp(ct)

co.df$districtName <- ofsId2district[match(co.df$id, ofsId2district$ofsid),'name']
co.df$var <- lang[match(unlist(co.df$districtName), names(lang))]


# plot swiss districts map
co.map <- ggplot(co.df, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = var), colour = "#f7f5ed", size = 0.05) +
  theme_map() + scale_fill_manual(values = swi_rpal) + coord_fixed(ratio = 1)
map <- co.map + geom_path(data = ct.df, colour = "#f7f5ed", size = 0.25) 

map <- map + ggtitle("Régions linguistiques de la Suisse") + 
  theme(legend.position = c(0.5, -0.01), legend.direction = "horizontal", legend.justification = "center",
  legend.key.size = unit(0.9, "lines")) + 
  guides(fill=guide_legend(title=NULL))
  
# http://stackoverflow.com/questions/21997715/add-ggplot-annotation-outside-the-panel-or-two-titles
g <- gtable_add_grob(ggplotGrob(map), grobTree(textGrob("Election fédérales 2015", x = 0, hjust = 0, gp = gpar(fontsize = 18, fontfamily = fontH, fontface = 'italic', col = "grey"))), t=1, l=4)
grid.draw(g)  






