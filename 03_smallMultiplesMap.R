if(!require(dplyr)) {
  install.packages("dplyr", repos="http://cran.us.r-project.org")
  require(dplyr)
}
if(!require(ggplot2)) {
  install.packages("ggplot2", repos="http://cran.us.r-project.org")
  require(ggplot2)
}
if(!require(scales)) {
  install.packages("scales", repos="http://cran.us.r-project.org")
  require(scales)
}
if(!require(gridExtra)) {
  install.packages("gridExtra", repos="http://cran.us.r-project.org")
  require(gridExtra)
}

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

keyText.size <- 16
legend.pos <- c(1.5, -0.05)
cityText.size <- 13
district.size <- 0.08
canton.size <- 0.5
cityCircle.size <- 8
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
    plot.margin = unit(c(0, 0, 0, 0), "lines"),
    legend.key = element_rect(colour = NA),
    legend.key.width=unit(2,"line"),
    legend.text=element_text(size = keyText.size),
    plot.title = element_text(hjust = 0.1, vjust = 0, face = "plain", size = 22))
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
  scale_size_area(max_size = cityCircle.size) + guides(fill = FALSE, size = guide_legend(title = txt['cities.legendTitle',lang])) +
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
    scale_size_area(max_size = cityCircle.size) +
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
# http://www.sthda.com/english/wiki/ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page-r-software-and-data-visualization#add-a-common-legend-for-multiple-ggplot2-graphs

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
legend <- get_legend(maps[[3]])


theme_nothing <- function(base_size = 12, base_family = ""){
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(rect          = element_rect(fill = "transparent", colour = NA, color = NA, size = 0, linetype = 0),
      line              = element_blank(),
      text              = element_blank(),
      title             = element_blank(),
      panel.background  = element_blank(),
      axis.ticks.margin = grid::unit(0, "lines"),
      axis.ticks.length = grid::unit(0, "lines"),
      legend.position   = "none",
      panel.margin      = grid::unit(c(0, 0, 0, 0), "lines"),
      plot.margin       = grid::unit(c(0, 0, 0, 0), "lines")
    )
}
blankPlot <- ggplot() + geom_blank(aes(1,1)) + theme_nothing()

maps2 <- lapply(1:length(maps), function(i) {
  map <- maps[[i]]
  map + theme(legend.position="none")
})



grid.arrange(maps2[[1]], maps2[[2]], maps2[[3]], maps2[[4]],
  maps2[[5]], maps2[[6]],maps2[[7]], maps2[[8]], legend, blankPlot,
  # header
  main =  textGrob(txt['header', lang], x = 0.01, hjust = 0, vjust = 4,
  gp = gpar(fontsize = 42, fontfamily = fontL, col = "black")),
  # footer
  sub = textGrob(txt['footer', lang], x = 0.95, vjust = -0.1, hjust = 1,
  gp = gpar(fontsize = 16, fontfamily = font, col = "#CCCCCC")),
  # grid
  ncol=2, nrow = 5, widths = c(2.7, 2.7), heights = c(1.5, 1.5, 1.5, 1.5, 0.4)
)

