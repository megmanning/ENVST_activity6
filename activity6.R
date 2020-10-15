#three new packages 
install.packages(c("sp","rgdal","dplyr"))
#package for vector data
library(sp)
#package for reading in spatial data
library(rgdal)
#data management package 
library(dplyr)

#read in shapefiles using readOGR in rgdal does this 
#glaciers in 1966
g1966 <- readOGR("/Users/margaretmanning/GitHub/ENVST_activity6/a06/GNPglaciers/GNPglaciers_1966.shp")

#read in glaciers in 2015
g2015 <- readOGR("/Users/margaretmanning/GitHub/ENVST_activity6/a06/GNPglaciers/GNPglaciers_2015.shp")

str(g2015)

#map the glaciers filling in the polygons with light blue and making the borders grey
plot(g1966, col="lightblue2", border="grey50")

#preview data stored with all accompanying info/measurements for each spatial object
head(g2015@data)

#finding projection of vector object using proj4string
g1966@proj4string

#check glacier names 
g1966@data$GLACNAME
g2015@data$GLACNAME

#fix glacier names so they are the same for each dataset
g2015@data$GLACNAME <- ifelse(g2015@data$GLACNAME == "North Swiftcurrent Glacier",
                              "N. Swiftcurrent Glacier",
                              ifelse(g2015@data$GLACNAME == "Miche Wabun",
                                     "Miche Wabun Glacier",
                                     as.character(g2015@data$GLACNAME)))
#lets combine area; first work with a smaller data frame 
gdf66 <- data.frame(GLACNAME = g1966@data$GLACNAME,
                    area66 = g1966@data$Area1966)
gdf15 <- data.frame(GLACNAME = g2015@data$GLACNAME,
                    area15 = g2015@data$Area2015)

#join all data tables by glacier name 
gALL <- full_join(gdf66,gdf15, by="GLACNAME")

#calculate the % change in area from 1966 to 2015 
gALL$gdiff <- ((gALL$area66-gALL$area15)/gALL$area66)*100

#make a scatterplot of glacier area in 1966 vs. % change in area
plot(gALL$area66, gALL$gdiff, 
xlab = "Glacier Area in 1966", 
ylab = "% Change in Area",
pch = 19,
col = "salmon")

#join data with the spatial data table and overwrite into spatial data table
g1966@data <- left_join(g1966@data, gALL, by="GLACNAME")
#use spplot to shade polygons based on the % change of labels 
#first argument is the spatial object
#second is the column of data to display with the diff colors
#add a title using main
#col changes the color of the borders
spplot(g1966, "gdiff", main="% Change in Area", col="transparent")

#look at the Vulture glacier in 1966
vulture66 <- g1966[g1966@data$GLACNAME == "Vulture Glacier",]
plot(vulture66, main = "Vulture Glacier in 1966", col="slategrey")

#calculate mean and stand dev of % loss
mean(gALL$gdiff)
sd(gALL$gdiff)

#find glacier with largest and smallest % loss
max(gALL$gdiff, na.rm = TRUE)
min(gALL$gdiff, na.rm = TRUE)

#Find glaciers in 1966 with smallest and largest area
max(gALL$area66, na.rm = TRUE)
min(gALL$area66, na.rm = TRUE)

#make a map showing glacier footprints in 1966 and 2015 for glacier with the largest percent loss
#Boulder Glacier had largest % loss 
boulder66 <- g1966[g1966@data$GLACNAME == "Boulder Glacier",]
boulder15 <- g2015[g2015@data$GLACNAME == "Boulder Glacier",]
plot(boulder66, 
     main = "Change in Boulder Glacier From 1966 to 2015", 
     col = "slategrey")
plot(boulder15, 
     col = "red1",
     add = TRUE)

#make a legend
#first create list of all labels 
labels <- c("Extent in 1966", "Extent in 2015")
levels(gALL$TYPE)
labels

legend("topleft",
       legend = labels,
       fill=plotColors,
       bty="n",
       cex=.9)
plotColors <- c("slategrey", "red1")

#Pumpelly Glacier had smallest % loss
pumpelly66 <- g1966[g1966@data$GLACNAME == "Pumpelly Glacier",]
pumpelly15 <- g2015[g2015@data$GLACNAME == "Pumpelly Glacier",]
plot(pumpelly66, 
     main = "Change in Pumpelly Glacier From 1966 to 2015", 
     col = "slategrey")
plot(pumpelly15, 
     col = "red1",
     add = TRUE)
legend("topleft",
       legend = labels,
       fill=plotColors,
       bty="n",
       cex=.9)



