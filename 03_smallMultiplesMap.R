library(swiTheme)
library(swiMap)
library(dplyr)
library(animation)
require(gtable)
require(RColorBrewer)

############################################################################################
###		SETTINGS
############################################################################################

## input files

translation.file <- "input/translations.csv"

district.file <- "input/districtCH_map.csv"
canton.file <- "input/cantonCH_map.csv"

language.file <- "input/languages.csv"
cities.file <- "input/cities.csv"

vote2011.file <- "input/ef2011_district.csv"
vote2011byParty.file <- "input/ef2011_party.csv"
## output files


## graphic settings

font <- "Open Sans"
fontH <- "Open Sans Semibold"
fontL <- "Open Sans Light"

keyText.size <- 22
legend.pos <- c(0.5, -0.05) 
cityText.size <- 13
district.size <- 0.08
canton.size <- 0.5

breaks <- c(0, 2^(0:6), 100)

txt <- read.csv(translation.file, row.names = 1, stringsAsFactors = F)

### TO DO: LOOP BY LANGUAGE 
lang <- 'fr'

########		HELPERS     ########

# ggplot2 map theme
theme_map <- function(base_size = 9, base_family = font) {
  theme_bw(base_size = base_size, base_family = base_family) %+replace% theme(
    axis.line = element_blank(), axis.text = element_blank(), 
    axis.ticks = element_blank(), axis.title = element_blank(), 
    panel.background = element_blank(), panel.border = element_blank(), 
    panel.grid = element_blank(), panel.margin = unit(0, "lines"), 
    plot.background = element_blank(), legend.justification = c(0, 0),
    plot.margin = unit(c(3, 0, 1.2, 0), "lines"),
    legend.key = element_rect(colour = NA),
    legend.key.width=unit(3.5,"line"),
    legend.text=element_text(size = keyText.size),
    plot.title = element_text(hjust = 0, vjust = 0, face = "plain", size = 38))
} 

############################################################################################
###		PLOT
############################################################################################

# load geographical data
co.df <- read.csv(district.file)
ct.df <- read.csv(canton.file)

###     1.Language map   ###
langRegions <- read.csv(language.file)

idx <- match(paste0("lang.legend.", langRegions[,1]), row.names(txt))
stopifnot(all(!is.na(idx)))
co.df$var <- factor(txt[idx,lang], levels = c(txt['lang.legend.Allemand', lang], txt['lang.legend.Français', lang], txt['lang.legend.Italien', lang], 
                                              txt['lang.legend.Allemand Romanche', lang], txt['lang.legend.Allemand Français', lang]))
levels(co.df$var) <- paste(" ", levels(co.df$var), "  ")


# plot 
lang.palette <- c('#ab3d3f', '#366096', '#336666', '#669999', '#996699')

baseDistrict.map <- ggplot(co.df, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = var), colour = "#f7f5ed", size = district.size) +
  geom_path(data = ct.df, colour = "#f7f5ed", size = canton.size) + 
  theme_map() + coord_fixed(ratio = 1.5) + theme(legend.position = legend.pos, 
                                                 legend.direction = "horizontal", legend.justification = "center") + 
  guides(fill = guide_legend(title=NULL, override.aes = list(colour = NULL)))

map.lang <- baseDistrict.map + scale_fill_manual(values = lang.palette) + 
  ggtitle(txt["title.lang", lang]) 



###     2.Cities map   ###
cities <- read.csv(cities.file)  
cities$name <- txt[match(paste0("cities.", cities$names), rownames(txt)), lang]

map.cities <- baseDistrict.map + scale_fill_manual(values = alpha(lang.palette, 0.4)) +
  geom_point(data = cities, aes(x = lon, y = lat, size = size, group = NULL), alpha=0.7, colour = "#4C4C4C") +
  geom_text(data = cities,  aes(x = lon, y = lat - 0.08, label = name, group = NULL), size = cityText.size, family = fontL) +
  scale_size_area(max_size = 18) + guides(fill = FALSE, size = guide_legend(title = txt['cities.legendTitle',lang])) + 
  theme(legend.title = element_text(size = 14)) +
  ggtitle(txt['title.cities', lang])

#maps <- list(lang = map.lang, cities = map.cities)
maps <- list()

###   3. Vote map  
votes.read <- read.csv(vote2011.file)
votesByParty.read <- read.csv(vote2011byParty.file)

mapPercentage <- function(co.df, var, breaks = breaks, title = "", colors = c('#F7ECEC', '#893132')) {
  
  co.df$var <- cut(var, breaks, include.lowest = T)
  levels(co.df$var) <- paste0(gsub(",", "-", gsub("(\\[|\\]|\\()", "", levels(co.df$var))), "%  ")
  
  co.map <- ggplot(co.df, aes(x = long, y = lat, group = group)) + 
    geom_polygon(aes(fill = var), colour = "#f7f5ed", size = district.size) +
    theme_map() + coord_fixed(ratio = 1.5) + theme(legend.position = "bottom", 
    legend.direction = "horizontal", legend.justification = "center") +
    geom_path(data = ct.df, colour = "#f7f5ed", size = canton.size) +
    geom_point(data = cities, aes(x = lon, y = lat, size = size, group = NULL), shape = 1, colour = "black") + 
    scale_size_area(max_size = 18) +
    guides(size = FALSE, fill = guide_legend(title=NULL, override.aes = list(colour = NULL)))
  
  co.map + scale_fill_manual(drop = FALSE, values = colorRampPalette(colors, interpolate = "spline")(length(breaks))) + 
    ggtitle(title)
}

# map participation 
breaksParticipation <- seq(floor(min(votes.read[,'Participation.en..'])), ceiling(max(votes.read[,'Participation.en..'])), 
    ceiling(diff(range(votes.read[,'Participation.en..'])) / 9))
participationMap <- mapPercentage(co.df, var = votes.read[,'Participation.en..'], breaksParticipation, 
  title = txt["party.Participation.en..", lang], colors = c('#EFEDE8', '#61471E'))


maps <- c(maps, list(participation = participationMap))

orderedNames <- c(names(sort(votesByParty.read[1,-8], decreasing = T)), names(votesByParty.read)[8])

for(party in orderedNames) {
  
  pc.overall <- round(votesByParty.read[1,match(party, colnames(votesByParty.read))], 1)
  title <- paste0(txt[paste0("party.", party), lang], ": ", pc.overall, "%")
  choro <- mapPercentage(co.df, var = votes.read[,party], breaks = breaks, 
    # title = txt[paste0("party.", party), lang], colors = brewer.pal(9, "Purples"))
   title = title, colors = c('#E9EEEE', '#275453'))  
  
  maps[[party]] <- choro       
}


#### Small multiples
# add header and footer to the graphic
# http://stackoverflow.com/questions/21997715/add-ggplot-annotation-outside-the-panel-or-two-titles
# gridFormat <- function(..., text = txt['header', lang], footer = txt['footer', lang]) {
#   grid.arrange(..., 
#                main =  textGrob(text, x = 0.01, hjust = 0, vjust = 4,gp = gpar(fontsize = 42, fontfamily = fontL, col = "black")),
#                sub = textGrob(footer, x = 0.95, vjust = -0.1, hjust = 1, gp = gpar(fontsize = 16, fontfamily = font, col = "#CCCCCC"))
#   )
# }

# http://www.sthda.com/english/wiki/ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page-r-software-and-data-visualization#add-a-common-legend-for-multiple-ggplot2-graphs

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
legend <- get_legend(maps[[3]])
