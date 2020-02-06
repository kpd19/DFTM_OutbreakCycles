library('tidyverse')
library('quantmod')
library('zoo')

# be honest about how you're fudging the data
# DONT HAVE INFO ON WHETHER SPRAYED OR NOT, THIS IS KEY
# Where is this information
# 

####################
#     FUNCTIONS 
####################

find_peaks <- function (x, m = 1){
  shape <- diff(sign(diff(x, na.pad = FALSE)))
  pks <- sapply(which(shape < 0), FUN = function(i){
    z <- i - m + 1
    z <- ifelse(z > 0, z, 1)
    w <- i + m + 1
    w <- ifelse(w < length(x), w, length(x))
    if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) return(i + 1) else return(numeric(0))
    print()
  })
  pks <- unlist(pks)
  pks
}


find_peaks2 <- function (x, m = 1, thresh=1){
  shape <- diff(sign(diff(x, na.pad = FALSE)))
  pks <- sapply(which(shape < 0), FUN = function(i){
    z <- i - m + 1
    z <- ifelse(z > 0, z, 1)
    w <- i + m + 1
    w <- ifelse(w < length(x), w, length(x))
    if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) return(i + 1) else return(numeric(0))
  })
  pks <- unlist(pks)
  
  if (!missing(thresh)) {
    if (sign(thresh) < 0) 
      thresh <- -thresh
    pks[x[pks - 1] - coredata(x[pks]) > thresh]
  }
  else pks
  
  return(pks)
}


add_zero <- function(x){
  first_non_na <- which(x!="NA")[1]
  last_non_na <- which(x!="NA")[length(which(x!="NA"))]
  
  x[first_non_na - 1] <- 0
  x[last_non_na + 1] <- 0

  large <- which(x>=4)
  for(i in 1:length(large)){
    x[large[i]-1] <-
      ifelse(is.na(x[large[i]-1]) == TRUE, 0, x[large[i]-1])
    
    x[large[i]+1] <-
      ifelse(is.na(x[large[i]+1]) == TRUE, 0, x[large[i]+1])
  }
    
  return(x)
}



add_zero_spec <- function(x){
  first_non_na <- which(x!="NA")[1]
  last_non_na <- which(x!="NA")[length(which(x!="NA"))]
  
  x[first_non_na - 1] <- 0
  x[last_non_na + 1] <- 0
  x[which(is.na(x) ==TRUE)]
  
  x[first_non_na:last_non_na][which(is.na(x[first_non_na:last_non_na]) ==TRUE)] <- 0
  
  return(x)
}


half_NA_spec <- function(x){
  first_non_na <- which(x!="NA")[1]
  last_non_na <- which(x!="NA")[length(which(x!="NA"))]
  
  x[first_non_na - 1] <- 0
  x[last_non_na + 1] <- 0
  x[which(is.na(x) ==TRUE)]
  
  x[first_non_na:last_non_na][which(is.na(x[first_non_na:last_non_na]) ==TRUE)] <- 0
  
  return(x)
}

plot_peaks <- function(x, threshold = 0){

  x <- add_zero(x)
  
  # plot(x, type = "b", cex = 0.5)
  
  d <- findPeaks(x = x, thresh = threshold) - 1
  e <- x[d]
  # points(x = d, y = e, pch = 19, col = "blue")
  
  return(mean(diff(d)))
  
}

fix_na <- function (x){
  shape <- diff(sign(diff(x, na.pad = FALSE)))
  pks <- sapply(which(shape < 0), FUN = function(i){
    z <- i - m + 1
    z <- ifelse(z > 0, z, 1)
    w <- i + m + 1
    w <- ifelse(w < length(x), w, length(x))
    if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) return(i + 1) else return(numeric(0))
    print()
  })
  pks <- unlist(pks)
  pks
}



power_spectrum <- function(x){
  
  z = spectrum(x,plot=FALSE); #Calculating the power spectrum
  
  period <- 1/z$freq
  power <- sqrt(z$spec)
  half_length_x <- length(x)/2
  period_adj <- period[period <= half_length_x]
  power_adj <- power[period <= half_length_x]
  dom_period <- period_adj[which(power_adj == max(power_adj))]
  
  # plot((1/z$freq),sqrt(z$spec),type="o",xlim = c(0,15), xlab="Period",ylab="Power") 
  
  return(dom_period)
}

trap_data <- read_csv("tblTrapSumAll.csv")
site_data <- read_csv("tblPlotLocation.csv")

head(trap_data)
head(site_data)

full_data <- full_join(trap_data, site_data, by = c("PlotName", "PlotNo", "Region", "PlotID", "NearestForest", "Nearest District", "Type", "State"))


plot(as.numeric(full_data[300,9:47]), type = "l")

full_data %>% filter(PlotName == "Keuterville") %>% select(c(1,9:47)) %>% ggplot() + aes()

full_data$years <- NA
full_data$max <- NA

full_data$years <- rowSums(1 - is.na(full_data[,9:47]))

for(i in 1:dim(full_data)[1]){
  full_data[i,]$max <- max(full_data[i,9:47], na.rm=TRUE)
}

dat <- full_data[full_data$max >0,]
dat <- dat[dat$years>20,]

x <- as.numeric(dat[180,9:47])
plot_peaks(x, threshold=2)

new_x <- add_zero_spec(x)
new_x2 <- new_x[is.na(new_x)==FALSE]
power_spectrum(new_x2)

a <- find_peaks(add_zero(x), m=3)
b <- x[a]

# points(x=a,y=b, col = "red", pch=19)
points(x=a,y=b, col = "violet", pch=19)

pdf("help.pdf", height = 10, width = 10)
par(mfrow=c(5,5))
for(i in 1:100){
  x <- as.numeric(dat[200+i,9:47])
  plot_peaks(x, threshold=3)
}
dev.off()

dat$peak_est <- NA
dat$spec_est <- NA

for(i in 1:dim(dat)[1]){
  x <- as.numeric(dat[i,9:47])
  find_peaks_est <- plot_peaks(x, threshold=2)
  
  dat[i,]$peak_est <- find_peaks_est
  
  new_x <- add_zero_spec(x)
  new_x2 <- new_x[is.na(new_x)==FALSE]
  spectrum_est <- power_spectrum(new_x2)
  
  dat[i,]$spec_est <- spectrum_est
  
} 

dat %>% ggplot() + aes(x = spec_est, y = peak_est) + geom_point() 
