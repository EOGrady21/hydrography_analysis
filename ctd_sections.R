# section plotting code
# updated E. Chisholm March 18, 2020
# in progress to update bathymetry contours
library(oce)
library(ocedata)
library(ggplot2)
library(dplyr)
library(mapview)
source('functions.R')

load('C:/Users/ChisholmE/Documents/sGSL_hydro/2018_CTD.RData')

bathy_data_list <- load_bathy(bathy_fn = 'bathydata.RDS', bathy_noaa_fn = 'bathydata_noaa.RDS',
                              y_range = c(47, 49), x_range = c(-66, -62))

data("topoWorld") # load bathy data from oce

# plot sections

sections <- list(d = c(2, 3, 4, 5, 6, 7),
                 f = c(22, 23, 24, 25), 
                 k = c(18, 17, 4, 20, 21)
)

i <- 1

#for(i in 1:length(sections)){
  sec_data <- create_section(dat = df_full, output =  'oce', stations = sections[[i]])
  
  # plot(sec_b_data, showBottom = TRUE, showStations = TRUE)
  
  df_sec <- create_section(dat = df_full, stations = sections[[i]], output = 'df')
  
  # get straight line between stations
  co_l <- sp::Line(matrix(c(unique(df_sec$longitude)[1:2], 
                        unique(df_sec$latitude)[1:2]), nrow = 2, ncol = 2))
  
  # create oce topo object for bathy
  bathy_sub <- bathy_data_list[[1]] %>%
    dplyr::filter(., x < -63.5 & x > -64) %>%
    dplyr::filter(., y < 49 & y > 47)
  
  # issue with vector allocation
  topo <- as.topo(longitude = bathy_sub$x, latitude = bathy_sub$y,
          z = matrix(bathy_sub$z, nrow = length(bathy_sub$x), 
                     ncol = length(bathy_sub$y)))
  
  plot(sec_data, showBottom = topoWorld)
  
  # not working
  
  #oce code for showing bottom along section from topo object
  
  showBottom <- topoWorld
  
  topo <- bathy_data_list[[1]]
  x <- sec_data
  
  numStations <- length(x@data$station)
  stationIndices <- 1:numStations
  
  firstStation <- x@data$station[[stationIndices[1]]]
  num.depths <- length(firstStation@data$pressure)
  
  xx <- array(NA_real_, numStations)
  
  lon0 <- mean(firstStation[["longitude"]], na.rm=TRUE)
  lat0 <- mean(firstStation[["latitude"]], na.rm=TRUE)
  
  for (ix in 1:numStations) {
    j <- stationIndices[ix]
    # from first station
    
    
   # xx[ix] <- geodDist(lon0, lat0,
    #                   mean(x@data$station[[j]][["longitude"]], na.rm=TRUE),
     #                  mean(x@data$station[[j]][["latitude"]], na.rm=TRUE))
    
    # distance along cruise track
    
    if (ix == 1) {
      xx[ix] <- 0
    } else {
      xx[ix] <- xx[ix-1] + geodDist(mean(x@data$station[[stationIndices[ix-1]]][["longitude"]], na.rm=TRUE),
                                    mean(x@data$station[[stationIndices[ix-1]]][["latitude"]], na.rm=TRUE),
                                    mean(x@data$station[[j]][["longitude"]], na.rm=TRUE),
                                    mean(x@data$station[[j]][["latitude"]], na.rm=TRUE))
    }
    
  }
  
  ii <- seq(1:length(x[["longitude", "byStation"]])) #order of stations
  ## Fine longitude and latitude: roughly
  # topoResolution <- geodDist(0, 0, 0, diff(showBottom[["latitude"]][1:2]))
  topoResolution <- geodDist(0, 0, 0, diff(topo$x[1:2]))
  slon <- x[["longitude", "byStation"]]
  slat <- x[["latitude", "byStation"]]
  sectionSpan <- geodDist(min(slon, na.rm=TRUE), min(slat, na.rm=TRUE),
                          max(slon, na.rm=TRUE), max(slat, na.rm=TRUE))
  nin <- length(slon)
  ## double up on resolution, although perhaps not needed
  nout <- as.integer(1 + 2 * sectionSpan / topoResolution)
  blon <- approx(1:nin, slon[ii], n=nout)$y
  blat <- approx(1:nin, slat[ii], n=nout)$y
  bottom.y <- topoInterpolate(blon, blat, showBottom)
  bottom.x <- approx(1:nin, xx, n=nout)$y
  bottom.x <- c(bottom.x[1], bottom.x, tail(bottom.x, 1))
  usr3 <- par('usr')[3]
  bottom.y <- c(usr3, bottom.y, usr3)
  polygon(bottom.x, bottom.y, col="lightgray")
  
  
  
  
  
  #-----------------------------
  
  # interpolate and plot sections
  png(paste0('sec_', names(sections)[[i]],'_map.png'), width = 500, height = 500)
  plot(sec_data, which = 99, showStations = TRUE)
  dev.off()
  
  png(paste0('sec_', names(sections)[[i]],'_temp.png'), width = 1075, height = 500)
  p <- plot_section(data = df_sec, var = 'temperature', bw = 1, dup = 'mean', method = 'akima', bathy_data_list = bathy_data_list)+
    ggtitle(paste('Section ', names(sections)[[i]]))+
    coord_cartesian(ylim = c(0, 120))
  print(p)
  dev.off()
  
  
  png(paste0('sec_', names(sections)[[i]],'_sal.png'), width = 1000, height = 500)
  p <- plot_section(data = df_sec, var = 'salinity', bw = 1, dup = 'mean', method = 'akima', bathy_data_list = bathy_data_list)+
    ggtitle(paste('Section ', names(sections)[[i]]))+
    coord_cartesian(ylim = c(0, 120))
  print(p)
  dev.off()
  
  print(paste('Completed section', names(sections)[[i]]))
#}
