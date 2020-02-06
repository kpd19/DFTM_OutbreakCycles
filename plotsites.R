library(tidyverse)
library(ggmap)
library(maps)
library(mapdata)
library(raster)
library(rgdal)
library(animation)
library(caret)


trap_dat <- read_csv("full_trap_data.csv")
geo_full <- trap_dat %>% filter(Latitude != "NA")
site_dat <- read_csv("sample_names.csv")

montana <- site_dat %>% filter(state == "MT")
arizona <- geo_full %>% filter(State == "AZ")



flagstaff <- c(35.1983, -111.6513)
mogollon <- c(34.3236, -110.9660)

AZ_dat <- arizona %>% filter(Latitude > mogollon[1]) %>% filter(Longitude < mogollon[2])


height <- max(arizona$Latitude) - min(arizona$Latitude)
width <- max(arizona$Longitude) - min(arizona$Longitude)
sac_borders <- c(bottom  = mogollon[1]  - 0.1 * height,
                 top     = max(arizona$Latitude)  + 0.5 * height,
                 left    = min(arizona$Longitude)  - 0.1 * width,
                 right   = mogollon[2]  + 0.1 * width)

# height <- max(montana$Lat) - min(montana$Lat)
# width <- max(montana$Lon) - min(montana$Lon)
# sac_borders <- c(bottom  = min(montana$Lat)  - 0.1 * height, 
#                  top     = max(montana$Lat)  + 0.1 * height,
#                  left    = min(montana$Lon)  - 0.1 * width,
#                  right   = max(montana$Lon)  + 0.1 * width)
# 


map <- get_stamenmap(sac_borders, zoom = 11, maptype = "terrain")
new_map <- get_stamenmap(sac_borders, zoom = 10, maptype = "terrain")

ggmap(map) + geom_point(data = a, aes(x = Lon, y = Lat,
                                            color = factor(name)), size = 4)

pdf("AZZ_map.pdf")
# ggmap(map) + geom_point(data = montana, aes(x = Lon, y = Lat), color = "white", size = 7) +
#   geom_point(data = montana, aes(x = Lon, y = Lat, color = factor(name)), size = 5)
ggmap(map) + geom_point(data = AZ_dat, aes(x = Longitude, y = Latitude), color = "white", size = 7) +
  geom_point(data = AZ_dat, aes(x = Longitude, y = Latitude, color = factor(PlotName)), size = 5) +
  geom_point(aes(x=flagstaff[2], y = flagstaff[1]), color = "white", size = 7, shape = 17) +
  geom_point(aes(x=flagstaff[2], y = flagstaff[1]), color = "red", size = 5, shape = 17)


dev.off()



see <- as.numeric(arizona[arizona$PlotName=="See Canyon",][9:47])
washington <- as.numeric(arizona[arizona$PlotName=="Washington_Park",][9:47])
baker <- as.numeric(arizona[arizona$PlotName=="Baker_Butte",][9:47])
years <- seq(1979,2017)

pdf("site_males.pdf")
ggplot() + geom_line(aes(x = years, y = washington), color = "blue", size = 2) +
  geom_line(aes(x = years, y = see), color = "red", size = 2) + 
  geom_line(aes(x = years, y = baker), color = "violet", size = 2) + 
  geom_point(aes(x = years, y = washington), color = "blue", size = 3) +
  geom_point(aes(x = years, y = see), color = "red", size = 3) + 
  geom_point(aes(x = years, y = baker), color = "violet", size = 3) +
  xlim(1990,2018) + ylab("Males in pheromone trap") + xlab("Year") +
  theme_bw(base_size = 20)
dev.off()

