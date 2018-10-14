####################
library(rgdal)
library(rgeos)
library(tidyverse)

DC_li<-read.csv("DC_listings.csv")

#shapefile obtained from http://www.arcgis.com/home/item.html?id=6066adabcebf40e789e9a661af5404ec
dc_neighborhoods_map <- readOGR(dsn="ZillowNeighborhoods-DC",layer="ZillowNeighborhoods-DC")

#processing for map data, explained in meeting
dc_neighborhoods_map_df <- dc_neighborhoods_map
dc_neighborhoods_map_df@data$id <- rownames(dc_neighborhoods_map_df@data)
dc_neighborhoods_map_df.points <- fortify(dc_neighborhoods_map_df, region="id")
neighborhoods.df.cart <- inner_join(dc_neighborhoods_map_df.points, dc_neighborhoods_map_df@data, by="id")

#processing for labelling, explained in meeting
neighborhood_centers <- gCentroid(dc_neighborhoods_map,byid=TRUE)
neighborhood_label_data <- cbind(as.data.frame(neighborhood_centers), dc_neighborhoods_map@data$NAME)
names(neighborhood_label_data)[3] <- "name"

#convert listing coordinates in SpatialPoints object -- note that we get matching projections by using the proj4string value from dc_neighborhoods_map
DC_li_spatial <- SpatialPoints(data.frame(long=DC_li$longitude, lat=DC_li$latitude),proj4string = dc_neighborhoods_map@proj4string)

# use the over() function to overlay the SpatialPoints for listings with the polygons defined in dc_neighborhoods_map
DC_li_overlay <- over(DC_li_spatial,dc_neighborhoods_map)
head(DC_li_overlay) # note that the neighborhood name is stored in the NAME variable

# combine the overlay information with the listing data
DC_li <- cbind(DC_li_overlay,DC_li)

g <- ggplot(neighborhoods.df.cart) + 
  geom_polygon(aes(long,lat, group=group),color="grey50",fill="white") +
  geom_text(data=neighborhood_label_data, aes(x=x,y=y,label=name),size=2) +
  geom_point(data=DC_li,aes(longitude,latitude,color=NAME))+
  coord_map() +
  theme_void()

g

dc_ward_map <- readOGR(dsn="Ward__2002",layer="Ward__2002")
dc_ward_map_df <- dc_ward_map

dc_ward_map_df@data$id <- rownames(dc_ward_map_df@data)
dc_ward_map_df.points <- fortify(dc_ward_map_df, region="id")
ward.df.cart <- inner_join(dc_ward_map_df.points, dc_ward_map_df@data, by="id")

#processing for labelling, explained in meeting
ward_centers <- gCentroid(dc_ward_map,byid=TRUE)
ward_label_data <- cbind(as.data.frame(ward_centers), dc_ward_map@data$NAME)
names(ward_label_data)[3] <- "name"

#convert listing coordinates in SpatialPoints object -- note that we get matching projections by using the proj4string value from dc_neighborhoods_map
DC_li_spatial <- SpatialPoints(data.frame(long=DC_li$longitude, lat=DC_li$latitude),proj4string = dc_ward_map@proj4string)

# use the over() function to overlay the SpatialPoints for listings with the polygons defined in dc_neighborhoods_map
DC_li_overlay <- over(DC_li_spatial,dc_ward_map)
head(DC_li_overlay) # note that the neighborhood name is stored in the NAME variable

# combine the overlay information with the listing data
DC_li <- cbind(DC_li_overlay,DC_li)

names(DC_li)
head(DC_li$WARD2002)

g <- ggplot(ward.df.cart) + 
  geom_polygon(aes(long,lat, group=group),color="grey50",fill="white") +
  geom_text(data=ward_label_data, aes(x=x,y=y,label=name),size=2) +
  geom_point(data=DC_li,aes(longitude,latitude,color=NAME))+
  coord_map() +
  theme_void()

g
