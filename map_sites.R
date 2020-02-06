library(tidyverse)
library(ggmap)
library(maps)
library(mapdata)
library(raster)
library(rgdal)

library("animation")


geo_full <- full_data %>% filter(Latitude != "NA") %>% filter(State == "WA")
max_outbreak <- max(full_data[,9:47], na.rm=TRUE)


us_data <- getData("GADM", country="USA", level = 1)

# what is group in aes?
map_func <- function(state, ){
  
  state_data = us_data[us_data$NAME_1 %in% c(state), ]
  state_data = fortify(state_data)
  
  lat_min <- floor(min(state_data$lat))
  lat_max <- ceiling(max(state_data$lat))
  long_min <- floor(min(state_data$long))
  long_max <- ceiling(max(state_data$long))  
  
  tree_map <-
    ggplot(data = geo_full) +
    geom_point(aes(x = Longitude, y = Latitude, size = geo_full$`2002`)) +
    geom_path(data = state_data, aes(x = long, y = lat, group = group), color = "black") + 
    theme_classic() +
    # scale_fill_manual(name="", values = c("#E69F00", "#56B4E9", "#009E73")) +
    labs(y = "Latitude", x = "") +
    theme(axis.line.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_text(size = 12, color="black"),
          legend.position = c(.75,.9)) +
    scale_y_continuous(limits = c(lat_min,lat_max), breaks = c(30, 35, 40, 45, 50, 55)) +
    scale_x_continuous(limits = c(long_min,long_max)) + 
    scale_size(breaks = seq(0, max_outbreak,10))
  
  tree_map
}


saveVideo({
  n <- 2017-1979 # divide by time
  time = 10 # how many seconds
  par(mfrow=c(1,1))
  for (i in 1:n){
    ani.options(interval = time/n, nmax = n)
    hex.graph(I_D[,i], I_SL[,i])
    mtext(text = paste0("The time is: ", i), side=3, line=0)
    #     mtext(text=paste0("Vac effort: 0.5"), side=3, line=3)
    mtext(text = paste0("Blue: sealions"), side=1, line=1)
    mtext(text = paste0("Red: dogs"), side=1, line=2)
    total.i <- sum(tinf_SL[,1:i])
    total.i <- round(total.i, digits=1)
    total.id <- sum(tinf_D[,1:i])
    total.id <- round(total.id, digits=1)
    mtext(text=paste0("Cumulative infection in sea lions: ", total.i), side= 3, line=2)
    mtext(text=paste0("Cumulative infection in dogs: ", total.id), side= 3, line=1)
  }
}, video.name = "~/Desktop/noSLBn.mp4"
)
