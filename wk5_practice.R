library(maptools)
library(rgdal)
library(rgeos)
library(ggmap)
library(sp)

wards <- readOGR("./LondonWards", "London_Ward_CityMerged")
proj4string(wards) <- CRS("+init=epsg:27700")
wards.wgs84 <- spTransform(wards, CRS("+init=epsg:4326"))
writeSpatialShape(wards.wgs84, "./LondonWards/wards_wgs84")
central <- readOGR("./tube_shape", "central_line")
stations <- read.csv("./Tube/London stations.csv", sep = ",", stringsAsFactors = FALSE)
lines <- read.csv("./Tube/London Tube Lines.csv", sep = ",", stringsAsFactors = FALSE)

X.from <- as.integer(rep(0, length(lines$From.Station)))
Y.from <- as.integer(rep(0, length(lines$From.Station)))
X.to <- as.integer(rep(0, length(lines$From.Station)))
Y.to <- as.integer(rep(0, length(lines$From.Station)))
PC.from <- as.character(rep("a", length(lines$From.Station)))
PC.to <- as.character(rep("a", length(lines$From.Station)))

for(i in 1:length(lines$From.Station)){
    name.from <- lines$From.Station[i]
    name.to <- lines$To.Station[i]
    X.from[i] <- stations$OS.X[stations$Station == name.from]
    Y.from[i] <- stations$OS.Y[stations$Station == name.from]
    X.to[i] <- stations$OS.X[stations$Station == name.to]
    Y.to[i] <- stations$OS.Y[stations$Station == name.to]
    PC.from[i] <- stations$Postcode[stations$Station == name.from]
    PC.to[i] <- stations$Postcode[stations$Station == name.to]
}

tube <- data.frame(Tube.Line = lines$Tube.Line, 
                   From.Station = lines$From.Station, 
                   X.from, Y.from, PC.from, To.Station = lines$To.Station, X.to, Y.to, PC.to)
rm(X.from, X.to, Y.from, Y.to, PC.to, PC.from, name.to, name.from, i)




