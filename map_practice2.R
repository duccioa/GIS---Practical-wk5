library(maptools)
library(rgdal)
library(rgeos)
library(ggmap)
library(sp)
library(plyr)
setwd("/Users/duccioa/CLOUD/C07_UCL_SmartCities/London/spatialggplot/")
sport <- readOGR(".", "london_sport")
proj4string(sport) <- CRS("+init=epsg:27700")
sport.wgs84 <- spTransform(sport, "+init=epsg:4326")
p <- ggplot(sport@data, aes(Partic_Per, Pop_2001))
p + geom_point(aes(color = Partic_Per, size = Pop_2001)) + geom_text(size = 4, aes(x = Partic_Per*1.02, y = Pop_2001*1.04, label = name))
sport.f <- fortify(sport, region = "ons_label")
sport.f <- merge(sport.f, sport@data, by.x = "id", by.y = "ons_label")
Map <- ggplot(sport.f, aes(long, lat, group = group, fill = Partic_Per)) + geom_polygon() + 
    coord_equal() + labs(x = "Easting (m)", y = "Northing (m)", fill = "% Sport Partic.") + 
    ggtitle("London Sports Participation")
Map + scale_fill_gradient(low = "white", high = "black")
#ggsave("my_large_plot.png", scale = 3, dpi = 400)

##MERGING DATA
lnd <- readOGR(dsn = ".", "london_sport")
crimeDat <- read.csv("boro.csv")
summary(crimeDat$MajorText)
crimeTheft <- crimeDat[which(crimeDat$MajorText == "Theft & Handling"), ]
crimeAg <- aggregate(CrimeCount ~ Borough, FUN = "sum", data = crimeTheft)
lnd$name %in% crimeAg$Borough
lnd$name[which(!lnd$name %in% crimeAg$Borough)]
levels(crimeAg$Borough)
crimeAg <- rename(crimeAg, replace = c(Borough = "name"))
lnd@data <- join(lnd@data, crimeAg)
lnd_f <- fortify(lnd, region = "name")
lnd_f <- merge(lnd_f, lnd@data, by.x = "id", by.y = "name")
g <- ggplot(lnd_f, aes(long, lat, group = group, fill = CrimeCount)) +geom_polygon() + 
    coord_equal()

##CLIPPING AND INTERSECTING
download.file("http://www.personal.leeds.ac.uk/~georl/egs/lnd-stns.zip", "lnd-stns.zip", method = "curl")
unzip("lnd-stns.zip")
