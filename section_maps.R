#section station maps

# load 


df_full <- dplyr::right_join(df, coords, by = 'station')




library(dplyr)
library(ggplot2)
library(ggbiplot)
library(tidyverse)
library(gridExtra)
library(vegan)
source('~/sGSL_hydro/functions.R')

#find text files
txt_fn <- list.files('IML2018051_txt',full.names = TRUE )

#first file is description of each variable with units
var_fn <- txt_fn[1]

txt_fn <- txt_fn[-1]

oce_dat <- list()
for (i in 1:length(txt_fn)){
  oce_dat[[i]] <- read_iml_txt(file = txt_fn[i])
}

# load bathy data
bathy_data_list <- load_bathy('bathydata.RDS', 'bathydata_noaa.RDS')


#combine all stations into single data frame
ctd_df <- list()
for( ii in 1:length(oce_dat)){
  
  dat <- oce_dat[[ii]]
  ctd_df[[ii]] <- as.data.frame(dat@data)
  ctd_df[[ii]]$station <- ii
  
}

df <- do.call(rbind, ctd_df)


#pull lats and lons and plot stations
lat <- list()
lon <- list()
for (i in 1:length(oce_dat)){
  lat[[i]] <- oce_dat[[i]][['Latitude']]
  lon[[i]] <- oce_dat[[i]][['Longitude']]
}

lats <- unlist(as.numeric(lat))
lons <- unlist(as.numeric(lon))

coords <- NULL
coords$latitude <- lats
coords$longitude <- lons

coords <- as.data.frame(coords)

data("coastlineWorldFine")

clwf <- as.data.frame(coastlineWorldFine@data)
# plot sections

sections <- list(d = c(2, 3, 4, 5, 6, 7),
                 f = c(22, 23, 24, 25), 
                 g = c(19, 20, 28, 27), 
                 k = c(18, 17, 4, 20, 21)
)


for(i in 1:3){
  sec_data <- create_section(dat = df_full, output =  'oce', stations = sections[[i]])
  
  # plot(sec_b_data, showBottom = TRUE, showStations = TRUE)
  
  df_sec <- create_section(dat = df_full, stations = sections[[i]], output = 'df')
  
  df_sec <- df_sec %>%
    dplyr::mutate(., station = as.factor(station)) 
  
  df_sec <- left_join(df_sec, coords, by = 'station')
  
  
  # plot map
  
  df_sing <- df_sec %>%
    dplyr::distinct(., station, latitude, longitude)
  p <- ggplot() +
    geom_polygon(data = clwf, aes(x = longitude, y = latitude))+
    scale_x_continuous(limits = c(-70, -60))+
    scale_y_continuous(limits = c(40, 50))+
    coord_map(xlim = c(-67, -60), ylim = c(45, 50)) +
    theme_classic()+
    geom_point(data = df_sec, aes(x = longitude, y = latitude), shape = 21, size = 5, col = 'darkred')+
    geom_path(data = df_sec, aes(x = longitude, y = latitude), colour = 'red') +
    ggrepel::geom_text_repel(data = df_sing, 
                             aes(x = longitude, y = latitude, label = station), nudge_y = 0.5, size = 14) +
    theme(axis.text = element_text(size = 15))
    
  png(paste0('sect_map', names(sections)[[i]], '.png'), width = 1000, height = 1000)
  print(p)
  dev.off()
  
}

  # 