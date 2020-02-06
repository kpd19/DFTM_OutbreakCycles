library(tidyverse)
library(ggmap)
library(maps)
library(mapdata)
library(raster)
library(rgdal)
library(animation)

trap_dat <- read_csv("full_trap_data.csv")
states <- c("California", "Arizona", "New Mexico", "Colorado", "Idaho", "Oregon", "Washington")



no_latlong <- trap_dat[is.na(trap_dat$Latitude),] 
no_latlong$max <- apply(no_latlong[,9:47], 1, max, na.rm=TRUE)
no_latlong <- no_latlong[no_latlong$max>0,]

write.csv(no_latlong, "missing_latlongs.csv")


geo_full <- trap_dat %>% filter(Latitude != "NA")
geo_full$max <- apply(geo_full[,9:47], 1, max, na.rm=TRUE)
geo_full <- geo_full[geo_full$max >0,]

us_data <- getData("GADM", country="USA", level = 1)
state_data = us_data[us_data$NAME_1 %in% states, ]
state_data = fortify(state_data)

lat_min <- floor(min(state_data$lat))
lat_max <- ceiling(max(state_data$lat))
long_min <- floor(min(state_data$long))
long_max <- ceiling(max(state_data$long))


DFTM_map <-
  ggplot(data = geo_full) +
  geom_point(aes(x = Longitude, y = Latitude, size = geo_full$max)) +
  geom_path(data = us_data, aes(x = long, y = lat, group = group), color = "black") + 
  theme_classic() +
  # scale_fill_manual(name="", values = c("#E69F00", "#56B4E9", "#009E73")) +
  labs(y = "Latitude", x = "") +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 12, color="black")) +
  scale_y_continuous(limits = c(lat_min,lat_max), breaks = c(30, 35, 40, 45, 50, 55)) +
  scale_x_continuous(limits = c(long_min,long_max)) + 
  scale_size_continuous(name = "Number of Males Trapped", limits =c(0,max(geo_full$max)), breaks = seq(0, max(geo_full$max),20)) +
  theme(legend.position="top")


DFTM_map
