library(rgdal)
library(rgeos)
library(tidyverse)
DC_li <- read.csv("DC_listings.csv")
DC_rev <- read.csv("DC_reviews.csv")

#shapefile obtained from https://www.arcgis.com/home/item.html?id=8aa8ab47efe741ed9e6f6cc03644da59
dc_district_map <- readOGR(dsn="Police_Districts",layer="PolDistPly")
plot(dc_district_map)

#shapefile obtained from http://www.arcgis.com/home/item.html?id=6066adabcebf40e789e9a661af5404ec
dc_neighborhoods_map <- readOGR(dsn="ZillowNeighborhoods-DC",layer="ZillowNeighborhoods-DC")
plot(dc_neighborhoods_map)

neighborhood_centers <- gCentroid(dc_neighborhoods_map,byid=TRUE)

plot(dc_neighborhoods_map)
text(as.data.frame(neighborhood_centers), labels=dc_neighborhoods_map@data$NAME, cex=0.4)


dc_neighborhoods_map_df <- dc_neighborhoods_map
dc_neighborhoods_map_df@data$id <- rownames(dc_neighborhoods_map_df@data)
dc_neighborhoods_map_df.points <- fortify(dc_neighborhoods_map_df, region="id")
neighborhoods.df.cart <- inner_join(dc_neighborhoods_map_df.points, dc_neighborhoods_map_df@data, by="id")

neighborhood_label_data <- cbind(as.data.frame(neighborhood_centers), dc_neighborhoods_map@data$NAME)
names(neighborhood_label_data)[3] <- "name"

ggplot(neighborhoods.df.cart) + 
  geom_polygon(aes(long,lat, group=group),color="grey50",fill="white") +
  geom_text(data=neighborhood_label_data, aes(x=x,y=y,label=name),size=2) +
  coord_map() +
  theme_void()

library(plotly)
g <- ggplot(neighborhoods.df.cart) + 
  geom_polygon(aes(long,lat, group=group,text=NAME),color="grey50",fill="white") +
  coord_map() +
  theme_void()
ggplotly(g)

names(DC_rev)[names(DC_rev)=="listing_id"] <- "id"
DC_rev_and_list <- inner_join(DC_li,DC_rev,by="id")
head(DC_rev_and_list)

class(DC_rev_and_list$date)
DC_rev_and_list$date <- as.Date(DC_rev_and_list$date)
head(DC_rev_and_list$date)
library(lubridate)
month(DC_rev_and_list$date,label=T,abbr=F)

DC_rev_and_list$month <- month(DC_rev_and_list$date,label=T,abbr=F)
DC_rev_and_list$year <- year(DC_rev_and_list$date)
DC_rev_and_list$date <- paste(DC_rev_and_list$year,DC_rev_and_list$month,sep="-")

DC_rev_montly <- summarize(group_by(DC_rev_and_list, date, neighbourhood), number_of_reviews=n())

ggplot(DC_rev_montly, aes(x=date, y=number_of_reviews))+geom_line(aes(group=neighbourhood))

