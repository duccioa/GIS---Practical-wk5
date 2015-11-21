library(maptools)
library(rgdal)
library(rgeos)
library(ggmap)
library(sp)
setwd("/Users/duccioa/CLOUD/C07_UCL_SmartCities/London/")
#Spatial data
BoroughMap <- readOGR(dsn = "./BoundaryData/", layer = "england_lad_2011Polygon")
proj4string(BoroughMap) <- CRS("+init=epsg:27700")
BoroughMap.wgs84 <- spTransform(BoroughMap, CRS("+init=epsg:4326"))

#dataframe
lnd_data <- read.csv("london-borough-profiles.csv", sep = ",", header = T, stringsAsFactors = FALSE)
lnd_data <- lnd_data[,c(1,2,4,5,7,8)]
names(lnd_data) <- c("code", "area_name", "Pop2015", "Household2015", "PopDensity2015", "AverageAge")
lnd_data <- lnd_data[match(lnd_data[,"code"], BoroughMap@data[,"code"], nomatch = FALSE),]
#BoroughMap$name %in% lnd_data$area_name
#BoroughMap$name[which(!BoroughMap$name %in% lnd_data$area_name)]

Borough_f <- fortify(BoroughMap, region = "code")
names(Borough_f)[7] <- "code"
Borough_f <- join(lnd_data, Borough_f)
g <- ggplot(Borough_f, aes(long, lat, group = group, fill = PopDensity2015)) + geom_polygon() +
    coord_equal()
