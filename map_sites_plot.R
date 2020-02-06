library(tidyverse)
library(ggmap)
library(maps)
library(mapdata)
library(raster)
library(rgdal)
library(animation)

trap_dat <- read_csv("full_trap_data.csv")

geo_full <- trap_dat %>% filter(Latitude != "NA")

max_outbreak <- max(trap_dat[,9:47], na.rm=TRUE)


us_data <- getData("GADM", country="USA", level = 1)
us_data <- fortify(us_data)

# what is group in aes?
map_func <- function(state1 = "Washington", state2 = "WA", year = 2017 ){
  
  col_year <- year - 1970
  
  state_data = us_data[us_data$NAME_1 %in% c(state1), ]
  state_data = fortify(state_data)
  
  lat_min <- floor(min(state_data$lat))
  lat_max <- ceiling(max(state_data$lat))
  long_min <- floor(min(state_data$long))
  long_max <- ceiling(max(state_data$long))  
  
  state_outbreaks <- geo_full %>% filter(State == state2)  
  outbreak_data <- state_outbreaks %>% dplyr::select(c(col_year,54,55)) 
  outbreak_data <- outbreak_data %>% filter(outbreak_data[,1] != "NA") 
  outbreak_data <- outbreak_data %>% filter(outbreak_data[,1] != 0)
  
  max_outbreak <- max(state_outbreaks[,9:47], na.rm=TRUE)
  
  tree_map <-
    ggplot(data = outbreak_data) +
    geom_point(aes(x = Longitude, y = Latitude, size = outbreak_data[,1])) +
    geom_path(data = state_data, aes(x = long, y = lat, group = group), color = "black") + 
    theme_classic() +
    # scale_fill_manual(name="", values = c("#E69F00", "#56B4E9", "#009E73")) +
    labs(y = "Latitude", x = "") +
    theme(axis.line.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_text(size = 12, color="black")) +
    scale_y_continuous(limits = c(lat_min,lat_max), breaks = c(30, 35, 40, 45, 50, 55)) +
    scale_x_continuous(limits = c(long_min,long_max)) + 
    scale_size_continuous(name = "Number of Males Trapped", limits =c(0,max_outbreak), breaks = seq(0, max_outbreak,20)) +
    theme(legend.position="top") +
    ggtitle(paste0("Year = ", year))
  
  return(tree_map)
}

map_func(state1 = "Arizona", state2 = "AZ", year = 1981)


saveVideo({
  n <- 2017-1980 # divide by time
  time = 20 # how many seconds
  par(mfrow=c(1,1))
  for (i in 1:n){
    ani.options(interval = time/n, nmax = n)
    plot_map <- map_func(state1 = c("Oregon", "Washington"), state2 = c("OR", "WA"), year = 1979 + i)
    print(plot_map)
  }
}, video.name = "~/Desktop/outbreakORWA.mp4"
)

geo_full %>% tidyverse::select(Latitude, Longitude)

