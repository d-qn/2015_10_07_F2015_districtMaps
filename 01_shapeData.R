library(swiMap)
library(readxl)
library(dplyr)
require(ggplot2)
require(rgdal)

############################################################################################
###		SETTINGS
############################################################################################

## raw data files
lang.file <- "data/su-f-01.06.01.04.09_w-2013.xls"
vote2011_raw.file <- "data/su-f-17.02.03.03.zb.2011.d.xls"

## output files
district.file <- "input/districtCH_map.csv"
canton.file <- "input/cantonCH_map.csv"

language.file <- "input/languages.csv"
cities.file <- "input/cities.csv"

vote2011.file <- "input/ef2011_district.csv"
vote2011byParty.file <- "input/ef2011_party.csv"

# settings
party.sub <- c('PLR', 'PDC', 'PS', 'UDC', 'PVL', 'PBD', 'PES')
  
############################################################################################
###	1. Load and save geographcial data files (district and cantons)
############################################################################################

path.ch <- getPathShp('CH')
co <- readOGR(path.ch, layer = 'municipalities-without-lakes')
# reproject coordintes in the standard projection: http://gis.stackexchange.com/questions/45263/converting-geographic-coordinate-system-in-r 
co <- spTransform(co, CRS("+init=epsg:4326"))

# assign the district ID as ID
## --> for district not present (== 0), use the canton ID * 100 !!!
co@data$id <- ifelse(co@data$BEZIRKSNR == 0, co@data$KANTONSNR * 100, co@data$BEZIRKSNR)
# transform to data.frame
co.df <- fortify(co,  region = "id")
co.df$id <- as.numeric(co.df$id)

# load canton borders
ct <- readOGR(path.ch, layer = 'swissBOUNDARIES3D_1_2_TLM_KANTONSGEBIET')
# reproject to standard coordindates
ct <- spTransform(ct, CRS("+init=epsg:4326"))
ct.df <- formatShp(ct)


## load communes data and its mapping bfs# to district name
communes.read <- loadCommunesCHgeographicalLevels()[,1:4]
ofsId2district <- read_excel(dir(system.file("extdata", package="swiMap"), "be-b-00.04-rgs-01\\.xls", full.names = T), skip = 28, sheet = 3)
bounds <- which(!is.na(ofsId2district[,1]))[1:2]
ofsId2district <- as.data.frame(ofsId2district[bounds[1]:bounds[2]-1,3:4])
colnames(ofsId2district) <- c('ofsid', 'name')

co.df$districtName <- ofsId2district[match(co.df$id, ofsId2district$ofsid),'name']

#co.df$var <- lang[match(unlist(co.df$districtName), names(lang))]
write.csv(co.df, file = district.file, row.names = F)
write.csv(ct.df, file = canton.file, row.names = F)


############################################################################################
###	2.	Load language Switzerland and ciites
############################################################################################

lang.read <- read_excel(lang.file, skip = 3)
# remove NA rows
lang.read <- lang.read[!is.na(lang.read[,2]),]
colnames(lang.read)[1:2] <- c('canton', 'name')
# remove NA columsn which are the error%
lang.read <- lang.read[, !is.na(colnames(lang.read))]
# convert to numeric
suppressWarnings(lang.read[,3:ncol(lang.read)] <- apply(lang.read[,3:ncol(lang.read)], 2, as.numeric))
lang <- (select(lang.read, Allemand, Français, Italien, Romanche) / lang.read$`Population totale`) * 100 >= 30
rownames(lang) <- lang.read$name
lang[which(is.na(lang), 2)] <- "FALSE"
lang <- sapply(colnames(lang), function(lg) ifelse(lang[,lg], paste(lg, ""), ""))
lang <- apply(lang, 1, paste, collapse = ", ", sep="")
lang <- gsub(" $", "", gsub("(, )+", "", lang))

# ensure map district names match all the language districts

stopifnot(all(!is.na(match(unlist(co.df$districtName), names(lang)))))

write.csv(lang[match(unlist(co.df$districtName), names(lang))], file = language.file, row.names = F)

### #### add largest Swiss cities -- > uncomment this block to rerun
# library(ggmap)
# cities <- data.frame(
#   names = c("Zurich", "Geneva", "Basel", "Bern", "Lausanne"),
#   size = c(1102, 493, 486, 349, 317)
# )
# cities <- cbind(cities, geocode(paste(cities$names, ", Switzerland")))
# write.csv(cities, file = cities.file, row.names = F)

############################################################################################
###	3.	Load vote data
############################################################################################

## Helpers
cleanPartyName <- function(partyName) {
  gsub(" (\\w+\\)|\\(.*\\))$", "", gsub("^ +", "", partyName))
}

matchOrFix_districtID2vote <- function(co2partis, co.df, elec.read) {
  naIdx <- which(is.na(co2partis))
  if(length(naIdx) > 0) {
    cat("Some districts could not be matched!!", "\n", "They will be hacked!", "\n")
    print( unique(co.df[naIdx, c('id', 'districtName')]))
    ## hack for non-matched bfs id!
    hackID <- data.frame(
      id = c(311, 312, 313, 314, 315, 316),
      idOld = c(303, 303, 302, 304, 305, 301)
    )
    # one-line super match to replace 
    co2partis[naIdx] <- match(hackID$idOld, elec.read$ofsid)[match(co.df$id[naIdx], hackID$id)]
  }
  stopifnot(all(!is.na(co2partis)))
  co2partis
}

## a. get the party % by district
elec.read <- read_excel(vote2011_raw.file, skip = 3, sheet = 2)
elec.read <- elec.read[!is.na(elec.read[,2]),]

# transform OFS ID to numeric, remove non-numerical rows
elec.read[,1] <- suppressWarnings(as.numeric(unlist(elec.read[,1])))
elec.read <- elec.read[!is.na(elec.read[,1]),]
# rename columns
colnames(elec.read)[1:3] <- c('ofsid', 'name', 'canton')
colnames(elec.read) <- cleanPartyName(colnames(elec.read))

stopifnot(all(!is.na(match(party.sub, colnames(elec.read)))))

partis <- as.matrix(elec.read[,match(party.sub, colnames(elec.read))])
partis <- cbind(partis, "Autres partis" = round((100 - rowSums(partis, na.rm = T)), 2))
#replace NA by 0
partis[which(is.na(partis), T)] <-0

co2partis <- match(co.df$id, elec.read$ofsid)
co2partis <- matchOrFix_districtID2vote(co2partis, co.df, elec.read)


##  get the participation
elec.read <- read_excel(vote2011_raw.file, skip = 3, sheet = 3)
elec.read <- elec.read[!is.na(elec.read[,2]),]

elec.read[,1] <- suppressWarnings(as.numeric(unlist(elec.read[,1])))
elec.read <- elec.read[!is.na(elec.read[,1]),]
# rename columns
colnames(elec.read)[1:3] <- c('ofsid', 'name', 'canton')
colnames(elec.read) <- cleanPartyName(colnames(elec.read))

co2partisP <- match(co.df$id, elec.read$ofsid)
co2partisP <- matchOrFix_districtID2vote(co2partis, co.df, elec.read)

# 
write.csv(cbind(partis[co2partis,], elec.read[co2partisP,"Participation en %"]), file = vote2011.file, row.names = F)


## b. get the party % overall
elec.read <- read_excel(vote2011_raw.file, skip = 3, sheet = 2)
partis.overall <- elec.read[which(elec.read[,1] == "Suisse"),]
colnames(partis.overall) <- cleanPartyName(colnames(partis.overall))

stopifnot(all(!is.na(match(party.sub, colnames(partis.overall)))))
partis.overall <- partis.overall[,match(party.sub, colnames(partis.overall))]
partis.overall <- cbind(partis.overall, "Autres partis" = round((100 - rowSums(partis.overall, na.rm = T)), 2))
write.csv(partis.overall, file = vote2011byParty.file, row.names = F)


