library(maptools)
library(rgdal)
library(rgeos)
library(ggmap)
library(sp)
library(plyr)

#Load stations and wards
wards <- readOGR("./LondonWards", "London_Ward_CityMerged")
proj4string(wards) <- CRS("+init=epsg:27700")#assign correct name for British National Grid
stations <- readOGR("./tube_shape", "TubeStations")
proj4string(stations) <- CRS("+init=epsg:4326")#Assign correct name for wgs84
stations <- spTransform(stations, CRS("+init=epsg:27700"))#transform to British National Grid
#List of line names
line_list <- c("Bakerloo", "Central", "Circle", "District", "DLR", 
               "Hammersmith", "Jubilee", "Metropolitan", 
               "Northern", "Overground", "Piccadilly", 
               "Tramlink", "Victoria", "Waterloo")
#Load tube lines
Bakerloo <- readOGR("./tube_shape", "Bakerloo")
Central <- readOGR("./tube_shape", "central_line")
Circle <- readOGR("./tube_shape", "Circle")
District <- readOGR("./tube_shape", "District")
DLR <- readOGR("./tube_shape", "DLR") 
Hammersmith <- readOGR("./tube_shape", "Hammersmith and City")
Jubilee <- readOGR("./tube_shape", "Jubilee")
Metropolitan <- readOGR("./tube_shape", "Metropolitan")
Northern <- readOGR("./tube_shape", "Northern")
Overground <- readOGR("./tube_shape", "Overground")
Piccadilly <- readOGR("./tube_shape", "Piccadilly")
Tramlink <- readOGR("./tube_shape", "Tramlink")
Victoria <- readOGR("./tube_shape", "Victoria")
Waterloo <- readOGR("./tube_shape", "Waterloo and City")
#Transform lines from wgs84 to British National Grid
change_CRS <- function(){
    for(i in 1:length(line_list)){
        line_name <- get(line_list[i])
        proj4string(line_name) <- CRS("+init=epsg:4326")
        line_name <- spTransform(line_name, CRS("+init=epsg:27700"))
        assign(line_list[i], line_name, envir = .GlobalEnv)
    }
}
change_CRS()

#Create list of RGB colours for tube lines
tube_lines <- data.frame(Line_Name = line_list, R = 0, G = 0, B = 0, stringsAsFactors = FALSE)
tube_lines[1, 2:4] <- c(137, 78, 36)#Bakerloo
tube_lines[2, 2:4] <- c(220,36,31)#Central
tube_lines[3, 2:4] <- c(255,206,0)#Circle
tube_lines[4, 2:4] <- c(0,114,41)#District
tube_lines[5, 2:4] <- c(0,175,173)#DLR
tube_lines[6, 2:4] <- c(215, 153, 175)#Hammersmith
tube_lines[7, 2:4] <- c(134, 143, 152)#Jubilee
tube_lines[8, 2:4] <- c(117, 16, 86)#Metropolitan
tube_lines[9, 2:4] <- c(0,0,0)#Northern
tube_lines[10, 2:4] <- c(232,106,16)#Overground
tube_lines[11, 2:4] <- c(0,25,168)#Piccadilly
tube_lines[12, 2:4] <- c(0,189,25)#Tramlink
tube_lines[13, 2:4] <- c(0,160,226)#Victoria
tube_lines[14, 2:4] <- c(118, 208, 189)#Waterloo
tube_lines[,2:4] <- tube_lines[,2:4]/255
tube_lines <- data.frame(tube_lines, colour = rgb(tube_lines[,2:4]), stringsAsFactors = FALSE)

#Plot
plot(wards, lwd = 0.5)
for(i in 1:length(tube_lines$Line_Name)){
    
    lines(get(tube_lines$Line_Name[i]), lwd =  3, col = tube_lines$colour[i])
}
points(stations, pch = 20, cex = 0.5, col = "black")
#text(stations, label = stations@data$Name, cex = .3)



#3 is the line's ID, 1 is the row of the coordinates x and y of the first point
Central@lines[[3]]@Lines[[1]]@coords[1,2]
###FUNCTIONS###
#Given a SpatialLinesDataFrame, calculate the length of the segment corresponding to the id
seg_length <- function(line, id){#input a line and the id corresponding to the selected segment
    xx1 <- line@lines[[id]]@Lines[[1]]@coords[1,1]#get the coords of the two points on the segment
    xx2 <- line@lines[[id]]@Lines[[1]]@coords[2,1]
    yy1 <- line@lines[[id]]@Lines[[1]]@coords[1,2]
    yy2 <- line@lines[[id]]@Lines[[1]]@coords[2,2]
    x1 <- min(xx1, xx2)#put them in order
    x2 <- max(xx1, xx2)
    y1 <- min(yy1, yy2)
    y2 <- max(yy1, yy2)
    len <- sqrt((x2-x1)^2+(y2-y1)^2)#calculate with pitagora the length of the segment
    return(len)
}
#Given a patialLinesDataFrame, calculate the total length
line_length <- function(line){
    num_seg <- length(line@lines)#how many segments in the line?
    total_length <- 0
    for(i in 1:num_seg){#go through each segment, calculate the length and add it up
        total_length <- total_length + seg_length(line, i)
    }
    return(total_length)
}

####PRACTICE 5
##QUESTION 1 - Which is the longest tube line in the dataset?
#Takes a list of SpatialLinesDataFrame already loaded and the rank required, returns the name and the 
#length of the line corresponding to the rank
which_length <- function(list, rank){
    x <- data.frame(Line = list, Length = rep(0, length(list)), stringsAsFactors = FALSE)#dataframe with line name and length
    for(i in 1:length(list)){#get the name from the first column and feed it to the function line_length
        x[i, 2] <- line_length(get(list[i]))#in order to return the length and place it in the dataframe
    }
    x <- x[order(-x$Length),]
    return(data.frame(LongestLine = x[rank,1], Length = x[rank,2], Units = "m"))
}
which_length(line_list, 1)

##QUESTION 2 - How many stations on the Piccadilly
Piccadilly_buffer <- gBuffer(Piccadilly, width = 0.1)#create a buffer zone around the line
Piccadilly_stations <- stations[Piccadilly_buffer, ]#intersect with the stations
summary(Piccadilly_stations)
plot(Piccadilly_buffer)
points(Piccadilly_stations, col = "red")
##QUESTION 3 - Average price along the wards of each line and which is the highest
#download.file(url = "https://files.datapress.com/london/dataset/average-house-prices-ward-lsoa-msoa/house-prices-Wards.csv",
              #dest = "./house-prices-wards.csv", method = "curl")
#replace in excel "(£)" with "_GBP_" and "-" with ""
house_prices_wards <- read.csv("./house-prices-wards.csv", sep = ",", stringsAsFactors = FALSE)
names(house_prices_wards)[1] <- "GSS_CODE"
wards@data <- join(wards@data, house_prices_wards)#joint geometric data and census
#The function takes the list of tube lines and return the average house price 2014
avg_houseprice_along <- function(List){
    avg_price <- data.frame(Line = List, avg_GBP_HousePrice2014 = 0, stringsAsFactors = FALSE)#create the result dataframe
    for(i in 1:length(avg_price$Line)){
        name <- avg_price[i,1]#take the name of the line
        wards_name <- wards[get(name),]#intersect the line and the wards
        avg_price[i,2] <- mean(wards_name@data$Mean_GBP_2014, na.rm = TRUE)#caclulate avg price
    }
    avg_price <- avg_price[order(-avg_price$avg_GBP_HousePrice2014),]
    return(avg_price)
}
avg_houseprice_along(line_list)

##QUESTION 4
BluePlaques <- readOGR("./LondonBluePlaques/", "LondonBluePlaques")
proj4string(BluePlaques) <- CRS("+init=epsg:4326")#Assign correct name for wgs84
BluePlaques <- spTransform(BluePlaques, CRS("+init=epsg:27700"))#transform to British National Grid
plot(wards)
points(BluePlaques, col = "blue", cex = .1, pch =20)
#Blue plaques around the Jubilee line
Jubilee_buffer <- gBuffer(Jubilee, width = 200)
BluePlaques_Jub <- BluePlaques[Jubilee_buffer,]
plot(wards)
plot(Jubilee_buffer, col = "red", cex = .1, add = TRUE)
points(BluePlaques_Jub, col = "blue", cex = .1, pch =20)
summary(BluePlaques_Jub)

Jubilee_station <- stations[stations@data$Name == ""]



