##Plot IML2018051 QC data

##E. Chisholm 
##December 20th 2019

###READ IN####
#read in IML text files which have QC flags

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

####PLOT####

### January 30 2020 Plotting

#combine all stations into single data frame
ctd_df <- list()
for( ii in 1:length(oce_dat)){
  
  dat <- oce_dat[[ii]]
  ctd_df[[ii]] <- as.data.frame(dat@data)
  ctd_df[[ii]]$station <- ii
  
}
  
df <- do.call(rbind, ctd_df)


# summarise environmental vars

# define temperature threshold for Cold Intermediate Layer
cil_lim <- 1

# find somewhere later in code oops

df <- right_join(df, coords, by = 'station')
z_bot <- bottom_depth(data = df, station = seq(1,30) , bathy_data_list) # not working yet



df_summ <- df %>%
  dplyr::group_by(., station) %>%
  dplyr::summarise(., u_cil = ifelse(is.finite(min(.data$PRES_01[.data$TE90_01 < cil_lim])), min(.data$PRES_01[.data$TE90_01 < cil_lim]), NA), # upper limit of CIL
                   l_cil = ifelse(is.finite(max(.data$PRES_01[.data$TE90_01 < cil_lim])), max(.data$PRES_01[.data$TE90_01 < cil_lim]), NA), # lower limit of CIL
                   t_cil = min(.data$TE90_01), # core temperature of CIL
                   zd_cil = max(.data$PRES_01[.data$TE90_01 < cil_lim]) - min(.data$PRES_01[.data$TE90_01 < cil_lim]), # thickness of CIL
                   zt_cil = .data$PRES_01[.data$TE90_01 == min(.data$TE90_01)], # depth of minimum CIL temperature
                   s_cil = mean(.data$PSAL_01[.data$TE90_01 < cil_lim], na.rm = TRUE), # mean salinity of CIL
                   t_surf = mean(.data$TE90_01[.data$PRES_01 < min(.data$PRES_01[.data$TE90_01 < cil_lim])]), # average temperature of surface layer (above CIL)
                   s_surf = mean(.data$PSAL_01[.data$PRES_01 < min(.data$PRES_01[.data$TE90_01 < cil_lim])]), # average salinity of surface layer (above CIL)
                   t_deep = mean(.data$TE90_01[.data$PRES_01 > max(.data$PRES_01[.data$TE90_01 < cil_lim])]), # average temperature of deep layer (below CIL)
                   s_deep = mean(.data$PSAL_01[.data$PRES_01 > max(.data$PRES_01[.data$TE90_01 < cil_lim])]), # average salinity of deep layer (below CIL)
                   z_max = max(PRES_01))  %>%# max sampling depth
  dplyr::mutate(., z_bot = abs(unlist(z_bot)))

# for NA z_bot use Z_max (station 30)


df_summ <- df_summ %>%
  dplyr::mutate(., z_bot = ifelse(is.na(z_bot), z_max, z_bot))
# remove NA rows

df_summ <- df_summ %>%
  dplyr::filter(., !is.na(u_cil))
 


# plot

p <- ggplot()+
  geom_point(data = df_summ, aes(x = as.factor(station), y = t_cil), stat = 'identity') +
  theme_classic() +
  scale_x_discrete('Station') +
  scale_y_continuous('Temperature [ deg C ]') +
  ggtitle(paste0('Core temperature of CIL (t < ', cil_lim,' deg C)'))


png(paste0('core_CIL_temp_', cil_lim, '.png'))
print(p)
dev.off()

p <- ggplot()+
  geom_point(data = df_summ, aes(x = as.factor(station), y = u_cil), stat = 'identity') +
  theme_classic() +
  scale_x_discrete('Station') +
  scale_y_reverse('Depth [ m ]') +
  ggtitle(paste('Upper limit of CIL (t < ', cil_lim, 'deg C)'))


png(paste0('u_cil_', cil_lim, '.png'))
print(p)
dev.off()

p <- ggplot()+
  geom_point(data = df_summ, aes(x = as.factor(station), y = l_cil), stat = 'identity') +
  theme_classic() +
  scale_x_discrete('Station') +
  scale_y_reverse('Depth [ m ]') +
  ggtitle(paste('Lower limit of CIL (t < ', cil_lim, 'deg C)'))


png(paste0('l_cil_', cil_lim, '.png'))
print(p)
dev.off()

p <- ggplot()+
  geom_point(data = df_summ, aes(x = as.factor(station), y = zd_cil), stat = 'identity') +
  theme_classic() +
  scale_x_discrete('Station') +
  scale_y_continuous('Thickness [ m ]') +
  ggtitle(paste('Thickness of CIL (t < ', cil_lim, 'deg C)'))


png(paste0('zd_cil_', cil_lim, '.png'))
print(p)
dev.off()

p <- ggplot()+
  geom_point(data = df_summ, aes(x = as.factor(station), y = zt_cil), stat = 'identity') +
  theme_classic() +
  scale_x_discrete('Station') +
  scale_y_continuous('Depth [ m ]') +
  ggtitle(paste('Depth of CIL core temperature (t < ', cil_lim, 'deg C)'))


png(paste0('zt_cil_', cil_lim, '.png'))
print(p)
dev.off()

p <- ggplot()+
  geom_point(data = df_summ, aes(x = as.factor(station), y = s_cil), stat = 'identity') +
  theme_classic() +
  scale_x_discrete('Station') +
  scale_y_continuous('Salinity [ PSU ]') +
  ggtitle(paste('Average Salinity of CIL (t < ', cil_lim,' deg C)'))


png(paste0('s_cil_', cil_lim, '.png'))
print(p)
dev.off()


p <- ggplot()+
  geom_point(data = df_summ, aes(x = as.factor(station), y = s_surf), stat = 'identity') +
  theme_classic() +
  scale_x_discrete('Station') +
  scale_y_continuous('Salinity [ PSU ]') +
  ggtitle('Average Salinity of Surface Layer')


png(paste0('s_surf_', cil_lim, '.png'))
print(p)
dev.off()


p <- ggplot()+
  geom_point(data = df_summ, aes(x = as.factor(station), y = t_surf), stat = 'identity') +
  theme_classic() +
  scale_x_discrete('Station') +
  scale_y_continuous('Temperature [ deg C ]') +
  ggtitle('Average Temperature of Surface Layer')


png(paste0('t_surf_', cil_lim, '.png'))
print(p)
dev.off()

# melt data frame for heat map

df_sum_melt <- reshape2::melt(df_summ, id.vars = 'station')
  
# create normalized values for heat map

df_summ_norm <- df_summ %>%
  dplyr::mutate(.,  u_cil = normalit(u_cil)) %>%
  dplyr::mutate(.,  l_cil = normalit(l_cil)) %>%
  dplyr::mutate(.,  t_cil = normalit(t_cil)) %>%
  dplyr::mutate(., zd_cil = normalit(zd_cil)) %>%
  dplyr::mutate(., zt_cil = normalit(zt_cil)) %>%
  dplyr::mutate(., s_cil = normalit(s_cil)) %>%
  dplyr::mutate(., t_surf = normalit(t_surf)) %>%
  dplyr::mutate(., s_surf = normalit(s_surf)) %>%
  dplyr::mutate(., t_deep = normalit(t_deep)) %>%
  dplyr::mutate(., s_deep = normalit(s_deep)) %>%
  #dplyr::mutate(., z_bot = normalit(z_bot)) %>%
  dplyr::mutate(., station = as.factor(station))

# remove NA columns
df_summ_norm <- df_summ_norm %>%
  dplyr::select(., names(df_summ_norm)[!names(df_summ_norm) %in% c('s_deep', 't_deep')])

# reorder based on variable of choice to show pattern
var = 's_cil'
df_summ_norm <- reorder_mat(df_summ_norm, var)


# reshape for heat map plot
df_norm_melt <- reshape2::melt(df_summ_norm, id.vars = 'station')


# get colour pallette

#get cmocean palette
therm_pal <- cmocean('balance')
therm <- therm_pal(n = 100)


p <- ggplot()+
  geom_tile(data = df_norm_melt, aes(x = as.factor(station), y = variable, fill = value), colour = 'black', width = 0.95, height = 0.95, size = 2) +
  theme_classic() +
  scale_fill_gradientn('Normalized \nvalue',colours = therm, na.value = 'white') +
  scale_x_discrete('Station ID') +
  scale_y_discrete()


png(paste0('anomoly_plot_cil', cil_lim,'_', var,'.png'), width = 800, height = 700)
print(p)
dev.off()


p <- ggplot(data = df_norm_melt[df_norm_melt$variable %in% c('u_cil', 'l_cil', 'zt_cil', 's_cil', 'zd_cil'),])+
  geom_tile( aes(x = as.factor(station), y = variable, fill = value), colour = 'black', width = 0.95, height = 0.95, size = 2) +
  theme_classic() +
  scale_fill_gradientn('Normalized \nvalue',colours = therm, na.value = 'white') +
  scale_x_discrete('Station ID') +
  scale_y_discrete()

png(paste0('anomoly_plot_signif_cil', cil_lim,'_', var,'.png'), width = 800, height = 300)
print(p)
dev.off()


# group stations into regions by bottom depth

# stations shallower than 100 m 
# stations between 100 and 200 m 
# exclude station 22 at 400 m

for (i in 1:length(coords$latitude)){
  coords$station[[i]] <- i
}

coords_sub <- coords[coords$station %in% df_summ$station,]

df_reg <- df_summ_norm %>%
  dplyr::mutate(., reg = ifelse(z_bot < 100, 'A', 'B')) %>%
  dplyr::mutate(., lon = coords_sub$longitude) %>%
  dplyr::mutate(., lat = coords_sub$latitude) %>%
  dplyr::filter(., z_bot < 300)

df_reg_summ_norm <- df_reg %>%
  dplyr::group_by(., reg) %>%
  dplyr::summarise(u_cil = mean(u_cil), l_cil = mean(l_cil), t_cil = mean(t_cil), zd_cil = mean(zd_cil), zt_cil = mean(zt_cil), s_cil = mean(s_cil), t_surf = mean(t_surf), s_surf = mean(s_surf), z_bot = mean(z_bot))


df_reg_summ <- df_summ %>%
  dplyr::mutate(., reg = ifelse(z_bot < 100, 'A', 'B')) %>%
  dplyr::mutate(., lon = coords_sub$longitude) %>%
  dplyr::mutate(., lat = coords_sub$latitude) %>%
  dplyr::filter(., z_bot < 300) %>%
  dplyr::group_by(., reg) %>%
  dplyr::summarise(
    u_cil = mean(u_cil),
    l_cil = mean(l_cil),
    t_cil = mean(t_cil),
    zd_cil = mean(zd_cil),
    zt_cil = mean(zt_cil),
    s_cil = mean(s_cil),
    t_surf = mean(t_surf),
    s_surf = mean(s_surf),
    z_bot = mean(z_bot)
  )

write.csv(paste0('reg_summary_', cil_lim,'.csv'), x = df_reg_summ, row.names = FALSE)

# bathymetry

bathy_data_list <- load_bathy('E:/cruise_COR2019002/cor2019002_summary_rproj/COR2019002_cruise_summary/bathydata.RDS', 'E:/cruise_COR2019002/cor2019002_summary_rproj/COR2019002_cruise_summary/bathydata_noaa.RDS')



p <- ggplot()+
  geom_point(data = df_reg, aes(x = lon, y = lat, col = reg), size = 10) +
  geom_path(data = clwf, aes(x = longitude, y = latitude), col = 'darkgreen') +
  scale_x_continuous(limits = c(min(df_reg$lon) - 0.5, max(df_reg$lon)+0.5))+
  scale_y_continuous(limits = c(min(df_reg$lat)-0.5, max(df_reg$lat)+0.5)) +
  theme_classic() +
  #add bathymetry
  stat_contour(data = bathy_data_list[[1]],
               aes(x=x, y=y, z=z),
               breaks=c(-80), 
               size=c(0.6),
               colour = "gray61") +
  
  stat_contour(data = bathy_data_list[[1]],
               aes(x=x, y=y, z=z),
               breaks=c(-90), 
               size=c(0.6),
               colour = "gray41") +
  
  stat_contour(data = bathy_data_list[[1]],
               aes(x=x, y=y, z=z),
               breaks=c(-100), 
               size=c(0.6),
               colour = "black")

png(paste0('region_map_bathy_cil', cil_lim,'.png'), width = 1000, height = 1000)
print(p)
dev.off()



# try to determine which variables show significant differences between regions

phys_var <- names(df_reg)[!names(df_reg) %in% c('station', 'lat', 'lon', 'reg', 'z_bot')]
fit <- list()
for (i in 1:length(phys_var)){
  
  boxplot(df_reg[[phys_var[[i]]]] ~ df_reg$reg, title(main = phys_var[[i]]))
  fit[[i]] <- aov(df_reg[[phys_var[[i]]]] ~ df_reg$reg)
  print(summary(fit))
  
  
}
names(fit) <- phys_var

# upper and lower elimit of CIL and depth of minimum CIL temperature are significant at p < 0.05
# salinity of CIL is significant at p < 0.1

# make table of signif values

pvals <- list()
fvals <- list()
for (i in 1:length(fit)){
  pvals[[i]] <- summary(fit[[i]])[[1]][["Pr(>F)"]][1]
  fvals[[i]] <- summary(fit[[1]])[[1]][["F value"]][1]
}

pf_table <- data.frame(var = phys_var, pval = unlist(pvals), fval = unlist(fvals))

write.csv(paste0('Significance results_CIL', cil_lim,'.csv'), x = pf_table)

boxplot(df_reg$s_cil ~ df_reg$reg)
fit <- aov(df_reg$s_cil ~ df_reg$reg)
summary(fit)

boxplot(df_reg$zt_cil ~ df_reg$reg)
fit <- aov(df_reg$zt_cil ~ df_reg$reg)
summary(fit)

#t_fit <- TukeyHSD(fit)

plot(fit)

# plot some profiles


png('all_station_profiles.png', width = 1500, height = 1000)
profile_plot(dat)
dev.off()

png('all_station_TS.png', width = 1000, height = 1000)
plotTS_EC(x = df, salinity = 'PSAL_01', temperature = 'TE90_01')
dev.off()


png('faceted_TS.png', width = 1000, height = 1000)
plotTS_EC(df, salinity = 'PSAL_01', temperature = 'TE90_01')+
  facet_wrap(~station)
dev.off()



# plot sections

sec_b <- c(2, 19, 23)
sec_a <- c(4, 5, 6, 7)
sec_ab1 <- c(2, 3, 4, 5) # CIL at T < 3
sec_ab2 <- c(2, 19, 21, 24) #CIL at T < 1
sec_ab3 <- c(11, 3, 19)


# add geog info to df

df_full <- dplyr::right_join(df, coords, by = 'station')


# sec_b

sec_b_data <- create_section(dat = df_full, output =  'oce', stations = sec_b)

# plot(sec_b_data, showBottom = TRUE, showStations = TRUE)

df_sec_b <- create_section(dat = df_full, stations = sec_b, output = 'df')

df_sec_b <- df_sec_b %>%
  dplyr::mutate(., station = as.numeric(station)) 

df_sec_b <- left_join(df_sec_b, coords, by = 'station')
  

# interpolate and plot sections
png('sec_b_map.png', width = 500, height = 500)
plot(sec_b_data, which = 99, showStations = TRUE)
dev.off()

png('sec_b_temp.png', width = 1000, height = 500)
plot_section(data = df_sec_b, var = 'temperature', bw = 1, dup = 'mean', method = 'akima', bathy_data_list)+
  ggtitle('Section B')
dev.off()

png('sec_b_sal.png', width = 1000, height = 500)
plot_section(data = df_sec_b, var = 'salinity', bw = 1, dup = 'mean', method = 'akima')+
  ggtitle('Section B')
dev.off()


# sec_a
df_sec_a <- create_section(dat = df_full, stations = sec_a, output = 'df')

df_sec_a <- df_sec_a %>%
  dplyr::mutate(., station = as.numeric(station)) 

df_sec_a <- left_join(df_sec_a, coords, by = 'station')


# interpolate and plot sections
sec_a_data <- create_section(dat = df_full, output =  'oce', stations = sec_a)

png('sec_a_map.png', width = 500, height = 500)
plot(sec_a_data, which = 99, showStations = TRUE)
dev.off()

png('sec_a_temp.png', width = 1000, height = 500)
plot_section(data = df_sec_a, var = 'temperature', bw = 1, dup = 'mean', method = 'akima')+
  ggtitle('Section A')
dev.off()

png('sec_a_sal.png', width = 1000, height = 500)
plot_section(data = df_sec_a, var = 'salinity', bw = 1, dup = 'mean', method = 'akima')+
  ggtitle('Section A')
dev.off()




# sec_ab1
df_sec_ab1 <- create_section(dat = df_full, stations = sec_ab1, output = 'df')

df_sec_ab1 <- df_sec_ab1 %>%
  dplyr::mutate(., station = as.numeric(station)) 

df_sec_ab1 <- left_join(df_sec_ab1, coords, by = 'station')

sec_ab1_data <- create_section(dat = df_full, output =  'oce', stations = sec_ab1)

png('sec_ab1_map.png', width = 500, height = 500)
plot(sec_ab1_data, which = 99, showStations = TRUE)
dev.off()
# interpolate and plot sections

png('sec_ab1_temp.png', width = 1000, height = 500)
plot_section(data = df_sec_ab1, var = 'temperature', bw = 1, dup = 'mean', method = 'akima')+
  ggtitle('Section A / B [1]')
dev.off()


png('sec_ab1_sal.png', width = 1000, height = 500)
 plot_section(data = df_sec_ab1, var = 'salinity', bw = 1, dup = 'mean', method = 'akima') +
   ggtitle('Section A / B [1]')
 dev.off()

 
 # sec_ab2
 df_sec_ab2 <- create_section(dat = df_full, stations = sec_ab2, output = 'df')
 
 df_sec_ab2 <- df_sec_ab2 %>%
   dplyr::mutate(., station = as.numeric(station)) 
 
 df_sec_ab2 <- left_join(df_sec_ab2, coords, by = 'station')
 
 sec_ab2_data <- create_section(dat = df_full, output =  'oce', stations = sec_ab2)
 
 png('sec_ab2_map.png', width = 500, height = 500)
 plot(sec_ab2_data, which = 99, showStations = TRUE)
 dev.off()
 # interpolate and plot sections
 png('sec_ab2_temp.png', width = 1000, height = 500)
 plot_section(data = df_sec_ab2, var = 'temperature', bw = 1, dup = 'mean', method = 'akima')+
   ggtitle('Section A / B [2]')
 dev.off()
 
 png('sec_ab2_sal.png', width = 1000, height = 500)
 plot_section(data = df_sec_ab2, var = 'salinity', bw = 1, dup = 'mean', method = 'akima') +
   ggtitle('Section A / B [2]')
 dev.off()
 


 
 # sec_ab3
 df_sec_ab3 <- create_section(dat = df_full, stations = sec_ab3, output = 'df')
 
 df_sec_ab3 <- df_sec_ab3 %>%
   dplyr::mutate(., station = as.numeric(station)) 
 
 df_sec_ab3 <- left_join(df_sec_ab3, coords, by = 'station')
 
 sec_ab3_data <- create_section(dat = df_full, output =  'oce', stations = sec_ab3)
 
 png('sec_ab3_map.png', width = 500, height = 500)
 plot(sec_ab3_data, which = 99, showStations = TRUE)
 dev.off()
 # interpolate and plot sections
 png('sec_ab3_temp.png', width = 1000, height = 500)
 plot_section(data = df_sec_ab3, var = 'temperature', bw = 1, dup = 'mean', method = 'akima')+
   ggtitle('Section A / B [3]')
 dev.off()
 
 png('sec_ab3_sal.png', width = 1000, height = 500)
 plot_section(data = df_sec_ab3, var = 'salinity', bw = 1, dup = 'mean', method = 'akima') +
   ggtitle('Section A / B [3]')
 dev.off()












for (i in 1:length(oce_dat)){ #for each station of ctd data
  
  dat <- oce_dat[[i]]
  
  #plot( dat[['PSAL_01']], dat[['TE90_01']])
  
  #fix names for default oce plot
  #dat2 <- dat
  
  #names(dat2@data)[3] <- 'temperature'
  #names(dat2@data)[14] <- 'salinity'
  #names(dat2@data)[2] <- 'pressure'
  
  
  #Dan's suggestion for renaming
  
  #dnew <- oceSetData(d, "salinity", d[["PSAL_01"]])
  
  
  #plot oce style
  #plotTS(dat2)
  
  #plot with ggplot
  ctd_df <- as.data.frame(dat@data)
  
  
  
  
  
  
  
  
  
  #plot genmeric TS with QC data
  #p<- plotTS_EC(ctd_df, salinity = 'PSAL_01', temperature = 'TE90_01') +
  #  ggtitle(dat[['Event_Comments']])

  
  #jpeg(filename = paste0("TS_plot_QC", dat[['Event_Comments']], ".jpeg"))
  #print(p)
  #dev.off()
  
  
  p <- plotTS_balloon(ctd_df, salinity = 'PSAL_01', temperature = 'TE90_01', var = 'DOXY_01')
  
  jpeg(filename = paste0("TS_plot_oxy", dat[['Event_Comments']], ".jpeg"))
  print(p)
  dev.off()
}

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

#pull in coastline

library(ocedata)
data("coastlineWorldFine")

clwf <- as.data.frame(coastlineWorldFine@data)

ggplot()+
  geom_point(data = coords, aes(x = longitude, y = latitude))+
  geom_path(data = clwf, aes(x = longitude, y = latitude), col = 'darkgreen') +
  scale_x_continuous(limits = c(min(coords$longitude) - 0.5, max(coords$longitude)+0.5))+
  scale_y_continuous(limits = c(min(coords$latitude)-0.5, max(coords$latitude)+0.5)) +
  theme_classic()



#get minimum temperature for each station
min_temp <- list()
for (i in 1:length(oce_dat)){
  min_temp [[i]] <- min(oce_dat[[i]][['TE90_01']])
}

map_dat <- coords %>%
  dplyr::mutate(., min_temp = unlist(min_temp))

#get cmocean palette
therm_pal <- cmocean('thermal')
therm <- therm_pal(n = 100)

ggplot()+
  geom_point(data = map_dat, aes(x = longitude, y = latitude, col = min_temp))+
  geom_path(data = clwf, aes(x = longitude, y = latitude), col = 'darkgreen') +
  scale_x_continuous(limits = c(min(coords$longitude) - 0.5, max(coords$longitude)+0.5))+
  scale_y_continuous(limits = c(min(coords$latitude)-0.5, max(coords$latitude)+0.5)) +
  theme_classic() +
  scale_color_gradientn(colours = therm )

#depth of minimum temperature?

#difference between maximum depth and depth of min temperature?


#calculate thickness of CIL (t < 1 deg C, t < 0 deg C, t < 2 deg C)

cil0 <- list()
cil1 <- list()
cil2 <- list()

p_range_0 <- list()
p_range_1 <- list()
p_range_2 <- list()

cil_t_0 <- list()
cil_t_1 <- list()
cil_t_2 <- list()

for (i in 1:length(oce_dat)){
  df <- as.data.frame(oce_dat[[i]]@data)
  
  cil0[[i]] <- df[df$TE90_01 < 0 ,]
  
  cil1[[i]] <- df[df$TE90_01 < 1 ,]
  
  cil2[[i]] <- df[df$TE90_01 < 2 ,]
  
  p_range_0[[i]] <- range(cil0[[i]]$PRES_01)
  
  p_range_1[[i]] <- range(cil1[[i]]$PRES_01)
  
  p_range_2[[i]] <- range(cil2[[i]]$PRES_01)
  
  
  cil_t_0[[i]] <- unlist(diff(p_range_0[[i]]))
  
  cil_t_1[[i]] <- unlist(diff(p_range_1[[i]]))
  
  cil_t_2[[i]] <- unlist(diff(p_range_2[[i]]))
  }

#unlist
cil_t_0 <- unlist(cil_t_0)

cil_t_1 <- unlist(cil_t_1)

cil_t_2 <- unlist(cil_t_2)

#set infinites to NA
cil_t_0[cil_t_0 == -Inf] <- NA

cil_t_1[cil_t_1 == -Inf] <- NA

cil_t_2[cil_t_2 == -Inf] <- NA

map_dat <- map_dat %>%
  dplyr::mutate(., CIL_0_thick = cil_t_0) %>%
  dplyr::mutate(., CIL_1_thick = cil_t_1) %>%
  dplyr::mutate(., CIL_2_thick = cil_t_2) 


#get cmocean palette
deep_pal <- cmocean('deep')
deep <- deep_pal(n = 100)

#thicknes of CIL (t <0)
##ALL NA
jpeg('sgsl_CIL_0.jpeg')
ggplot()+
  geom_point(data = map_dat, aes(x = longitude, y = latitude, col = CIL_0_thick), size = 10)+
  geom_path(data = clwf, aes(x = longitude, y = latitude), col = 'darkgreen') +
  scale_x_continuous(limits = c(min(coords$longitude) - 0.5, max(coords$longitude)+0.5), expand = c(0,0))+
  scale_y_continuous(limits = c(min(coords$latitude)-0.5, max(coords$latitude)+0.5), expand = c(0,0)) +
  theme_classic() +
  scale_color_gradientn(colours = deep )+
  labs(col = 'Thickness of CIL [m] \n t < 0 deg C')
dev.off()

#thickness of CIL (t <1)
jpeg('sgsl_CIL_1.jpeg', width = 1000, height = 750)
ggplot()+
  geom_point(data = map_dat, aes(x = longitude, y = latitude, col = CIL_1_thick), size = 10)+
  geom_path(data = clwf, aes(x = longitude, y = latitude), col = 'darkgreen') +
  scale_x_continuous(limits = c(min(coords$longitude) - 0.5, max(coords$longitude)+0.5), expand = c(0,0))+
  scale_y_continuous(limits = c(min(coords$latitude)-0.5, max(coords$latitude)+0.5), expand = c(0,0)) +
  theme_classic() +
  scale_color_gradientn(colours = deep )+
  labs(col = 'Thickness of CIL [m] \n t < 1 deg C')
dev.off()

#thickness of CIL (t <2)
jpeg('sgsl_CIL_2.jpeg', width = 1000, height = 750)
ggplot()+
  geom_point(data = map_dat, aes(x = longitude, y = latitude, col = CIL_2_thick), size = 10)+
  geom_path(data = clwf, aes(x = longitude, y = latitude), col = 'darkgreen') +
  scale_x_continuous(limits = c(min(coords$longitude) - 0.5, max(coords$longitude)+0.5), expand = c(0,0))+
  scale_y_continuous(limits = c(min(coords$latitude)-0.5, max(coords$latitude)+0.5), expand = c(0,0)) +
  theme_classic() +
  scale_color_gradientn(colours = deep ) +
  labs(col = 'Thickness of CIL [m] \n t < 2 deg C')
dev.off()

#add bathymetry contours

#plot mean depth of CIL at different temperatuure levels

cil_d_0 <- list()
cil_d_1 <- list()
cil_d_2 <- list()

for (i in 1:length(oce_dat)){

cil_d_0[[i]]<- mean(cil0[[i]]$PRES_01, na.rm = TRUE)

cil_d_1[[i]] <- mean(cil1[[i]]$PRES_01, na.rm = TRUE)

cil_d_2[[i]] <- mean(cil2[[i]]$PRES_01, na.rm = TRUE)

}

cil_d_0 <- unlist(cil_d_0)
cil_d_1 <- unlist(cil_d_1)
cil_d_2 <- unlist(cil_d_2)


map_dat <- map_dat %>%
  dplyr::mutate(., CIL_0_depth = cil_d_0) %>%
  dplyr::mutate(., CIL_1_depth = cil_d_1) %>%
  dplyr::mutate(., CIL_2_depth = cil_d_2) 


#avg depth of CIL (t <0)
##ALL NA
jpeg('sgsl_CIL_0_d.jpeg')
ggplot()+
  geom_point(data = map_dat, aes(x = longitude, y = latitude, col = CIL_0_depth), size = 10)+
  geom_path(data = clwf, aes(x = longitude, y = latitude), col = 'darkgreen') +
  scale_x_continuous(limits = c(min(coords$longitude) - 0.5, max(coords$longitude)+0.5), expand = c(0,0))+
  scale_y_continuous(limits = c(min(coords$latitude)-0.5, max(coords$latitude)+0.5), expand = c(0,0)) +
  theme_classic() +
  scale_color_gradientn(colours = deep )+
  labs(col = 'Average depth of CIL [m] \n t < 0 deg C')
dev.off()

#avg depth of CIL (t <1)
jpeg('sgsl_CIL_1_d.jpeg', width = 1000, height = 750)
ggplot()+
  geom_point(data = map_dat, aes(x = longitude, y = latitude, col = CIL_1_depth), size = 10)+
  geom_path(data = clwf, aes(x = longitude, y = latitude), col = 'darkgreen') +
  scale_x_continuous(limits = c(min(coords$longitude) - 0.5, max(coords$longitude)+0.5), expand = c(0,0))+
  scale_y_continuous(limits = c(min(coords$latitude)-0.5, max(coords$latitude)+0.5), expand = c(0,0)) +
  theme_classic() +
  scale_color_gradientn(colours = deep )+
  labs(col = 'Average depth of CIL [m] \n t < 1 deg C')
dev.off()

#avg depth of CIL (t <2)
jpeg('sgsl_CIL_2_d.jpeg', width = 1000, height = 750)
ggplot()+
  geom_point(data = map_dat, aes(x = longitude, y = latitude, col = CIL_2_depth), size = 10)+
  geom_path(data = clwf, aes(x = longitude, y = latitude), col = 'darkgreen') +
  scale_x_continuous(limits = c(min(coords$longitude) - 0.5, max(coords$longitude)+0.5), expand = c(0,0))+
  scale_y_continuous(limits = c(min(coords$latitude)-0.5, max(coords$latitude)+0.5), expand = c(0,0)) +
  theme_classic() +
  scale_color_gradientn(colours = deep ) +
  labs(col = 'Average depth of CIL [m] \n t < 2 deg C')
dev.off()



#calculate min and max depth of CIL 


#plot min depth of CIL at different temperatuure levels

cil_dmin_0 <- list()
cil_dmin_1 <- list()
cil_dmin_2 <- list()

for (i in 1:length(oce_dat)){
  
  cil_dmin_0[[i]]<- min(cil0[[i]]$PRES_01, na.rm = TRUE)
  
  cil_dmin_1[[i]] <- min(cil1[[i]]$PRES_01, na.rm = TRUE)
  
  cil_dmin_2[[i]] <- min(cil2[[i]]$PRES_01, na.rm = TRUE)
  
}

cil_dmin_0 <- unlist(cil_dmin_0)
cil_dmin_1 <- unlist(cil_dmin_1)
cil_dmin_2 <- unlist(cil_dmin_2)

cil_dmin_0[cil_dmin_0 == Inf] <- NA
cil_dmin_1[cil_dmin_1 == Inf] <- NA
cil_dmin_2[cil_dmin_2 == Inf] <- NA 


map_dat <- map_dat %>%
  dplyr::mutate(., CIL_0_mindepth = cil_dmin_0) %>%
  dplyr::mutate(., CIL_1_mindepth = cil_dmin_1) %>%
  dplyr::mutate(., CIL_2_mindepth = cil_dmin_2) 



#min depth of CIL (t <0)
##ALL NA
jpeg('sgsl_CIL_0_dmin.jpeg')
ggplot()+
  geom_point(data = map_dat, aes(x = longitude, y = latitude, col = CIL_0_mindepth), size = 10)+
  geom_path(data = clwf, aes(x = longitude, y = latitude), col = 'darkgreen') +
  scale_x_continuous(limits = c(min(coords$longitude) - 0.5, max(coords$longitude)+0.5), expand = c(0,0))+
  scale_y_continuous(limits = c(min(coords$latitude)-0.5, max(coords$latitude)+0.5), expand = c(0,0)) +
  theme_classic() +
  scale_color_gradientn(colours = deep )+
  labs(col = 'Minimum depth of CIL [m] \n t < 0 deg C')
dev.off()

#min depth of CIL (t <1)
jpeg('sgsl_CIL_1_dmin.jpeg', width = 1000, height = 750)
ggplot()+
  geom_point(data = map_dat, aes(x = longitude, y = latitude, col = CIL_1_mindepth), size = 10)+
  geom_path(data = clwf, aes(x = longitude, y = latitude), col = 'darkgreen') +
  scale_x_continuous(limits = c(min(coords$longitude) - 0.5, max(coords$longitude)+0.5), expand = c(0,0))+
  scale_y_continuous(limits = c(min(coords$latitude)-0.5, max(coords$latitude)+0.5), expand = c(0,0)) +
  theme_classic() +
  scale_color_gradientn(colours = deep )+
  labs(col = 'Minimum depth of CIL [m] \n t < 1 deg C')
dev.off()

#min depth of CIL (t <2)
jpeg('sgsl_CIL_2_dmin.jpeg', width = 1000, height = 750)
ggplot()+
  geom_point(data = map_dat, aes(x = longitude, y = latitude, col = CIL_2_mindepth), size = 10)+
  geom_path(data = clwf, aes(x = longitude, y = latitude), col = 'darkgreen') +
  scale_x_continuous(limits = c(min(coords$longitude) - 0.5, max(coords$longitude)+0.5), expand = c(0,0))+
  scale_y_continuous(limits = c(min(coords$latitude)-0.5, max(coords$latitude)+0.5), expand = c(0,0)) +
  theme_classic() +
  scale_color_gradientn(colours = deep ) +
  labs(col = 'Minimum depth of CIL [m] \n t < 2 deg C')
dev.off()

##max depth of CIL

#plot max depth of CIL at different temperatuure levels

cil_dmax_0 <- list()
cil_dmax_1 <- list()
cil_dmax_2 <- list()

for (i in 1:length(oce_dat)){
  
  cil_dmax_0[[i]]<- max(cil0[[i]]$PRES_01, na.rm = TRUE)
  
  cil_dmax_1[[i]] <- max(cil1[[i]]$PRES_01, na.rm = TRUE)
  
  cil_dmax_2[[i]] <- max(cil2[[i]]$PRES_01, na.rm = TRUE)
  
}

cil_dmax_0 <- unlist(cil_dmax_0)
cil_dmax_1 <- unlist(cil_dmax_1)
cil_dmax_2 <- unlist(cil_dmax_2)

cil_dmax_0[cil_dmax_0 == -Inf] <- NA
cil_dmax_1[cil_dmax_1 == -Inf] <- NA
cil_dmax_2[cil_dmax_2 == -Inf] <- NA 


map_dat <- map_dat %>%
  dplyr::mutate(., CIL_0_maxdepth = cil_dmax_0) %>%
  dplyr::mutate(., CIL_1_maxdepth = cil_dmax_1) %>%
  dplyr::mutate(., CIL_2_maxdepth = cil_dmax_2) 



#min depth of CIL (t <0)
##ALL NA
jpeg('sgsl_CIL_0_dmax.jpeg')
ggplot()+
  geom_point(data = map_dat, aes(x = longitude, y = latitude, col = CIL_0_maxdepth), size = 10)+
  geom_path(data = clwf, aes(x = longitude, y = latitude), col = 'darkgreen') +
  scale_x_continuous(limits = c(min(coords$longitude) - 0.5, max(coords$longitude)+0.5), expand = c(0,0))+
  scale_y_continuous(limits = c(min(coords$latitude)-0.5, max(coords$latitude)+0.5), expand = c(0,0)) +
  theme_classic() +
  scale_color_gradientn(colours = deep )+
  labs(col = 'Maximum depth of CIL [m] \n t < 0 deg C')
dev.off()

#min depth of CIL (t <1)
jpeg('sgsl_CIL_1_dmax.jpeg', width = 1000, height = 750)
ggplot()+
  geom_point(data = map_dat, aes(x = longitude, y = latitude, col = CIL_1_maxdepth), size = 10)+
  geom_path(data = clwf, aes(x = longitude, y = latitude), col = 'darkgreen') +
  scale_x_continuous(limits = c(min(coords$longitude) - 0.5, max(coords$longitude)+0.5), expand = c(0,0))+
  scale_y_continuous(limits = c(min(coords$latitude)-0.5, max(coords$latitude)+0.5), expand = c(0,0)) +
  theme_classic() +
  scale_color_gradientn(colours = deep )+
  labs(col = 'Maximum depth of CIL [m] \n t < 1 deg C')
dev.off()

#min depth of CIL (t <2)
jpeg('sgsl_CIL_2_dmax.jpeg', width = 1000, height = 750)
ggplot()+
  geom_point(data = map_dat, aes(x = longitude, y = latitude, col = CIL_2_maxdepth), size = 10)+
  geom_path(data = clwf, aes(x = longitude, y = latitude), col = 'darkgreen') +
  scale_x_continuous(limits = c(min(coords$longitude) - 0.5, max(coords$longitude)+0.5), expand = c(0,0))+
  scale_y_continuous(limits = c(min(coords$latitude)-0.5, max(coords$latitude)+0.5), expand = c(0,0)) +
  theme_classic() +
  scale_color_gradientn(colours = deep ) +
  labs(col = 'Maximum depth of CIL [m] \n t < 2 deg C')
dev.off()

###TODO : standardize colour scale range for comparison between plots


###salinity of CIL



cil_sal_0 <- list()
cil_sal_1 <- list()
cil_sal_2 <- list()

for (i in 1:length(oce_dat)){
  
  cil_sal_0[[i]]<- mean(cil0[[i]]$PSAL_01, na.rm = TRUE)
  
  cil_sal_1[[i]] <- mean(cil1[[i]]$PSAL_01, na.rm = TRUE)
  
  cil_sal_2[[i]] <- mean(cil2[[i]]$PSAL_01, na.rm = TRUE)
  
}

cil_sal_0 <- unlist(cil_sal_0)
cil_sal_1 <- unlist(cil_sal_1)
cil_sal_2 <- unlist(cil_sal_2)

cil_sal_0[cil_sal_0 == Inf] <- NA
cil_sal_1[cil_sal_1 == Inf] <- NA
cil_sal_2[cil_sal_2 == Inf] <- NA 


map_dat <- map_dat %>%
  dplyr::mutate(., CIL_0_sal = cil_sal_0) %>%
  dplyr::mutate(., CIL_1_sal = cil_sal_1) %>%
  dplyr::mutate(., CIL_2_sal = cil_sal_2) 


#get cmocean palette
sal_pal <- cmocean('haline')
haline <- sal_pal(n = 100)


#min depth of CIL (t <0)
##ALL NA
# jpeg('sgsl_CIL_0_dmax.jpeg')
# ggplot()+
#   geom_point(data = map_dat, aes(x = longitude, y = latitude, col = CIL_0_maxdepth), size = 10)+
#   geom_path(data = clwf, aes(x = longitude, y = latitude), col = 'darkgreen') +
#   scale_x_continuous(limits = c(min(coords$longitude) - 0.5, max(coords$longitude)+0.5), expand = c(0,0))+
#   scale_y_continuous(limits = c(min(coords$latitude)-0.5, max(coords$latitude)+0.5), expand = c(0,0)) +
#   theme_classic() +
#   scale_color_gradientn(colours = deep )+
#   labs(col = 'Maximum depth of CIL [m] \n t < 0 deg C')
# dev.off()

#min depth of CIL (t <1)
jpeg('sgsl_CIL_1_sal.jpeg', width = 1000, height = 750)
ggplot()+
  geom_point(data = map_dat, aes(x = longitude, y = latitude, col = CIL_1_sal), size = 10)+
  geom_path(data = clwf, aes(x = longitude, y = latitude), col = 'darkgreen') +
  scale_x_continuous(limits = c(min(coords$longitude) - 0.5, max(coords$longitude)+0.5), expand = c(0,0))+
  scale_y_continuous(limits = c(min(coords$latitude)-0.5, max(coords$latitude)+0.5), expand = c(0,0)) +
  theme_classic() +
  scale_color_gradientn(colours = haline )+
  labs(col = 'Average Salinity of CIL [PSU] \n t < 1 deg C')
dev.off()

#min depth of CIL (t <2)
jpeg('sgsl_CIL_2_sal.jpeg', width = 1000, height = 750)
ggplot()+
  geom_point(data = map_dat, aes(x = longitude, y = latitude, col = CIL_2_sal), size = 10)+
  geom_path(data = clwf, aes(x = longitude, y = latitude), col = 'darkgreen') +
  scale_x_continuous(limits = c(min(coords$longitude) - 0.5, max(coords$longitude)+0.5), expand = c(0,0))+
  scale_y_continuous(limits = c(min(coords$latitude)-0.5, max(coords$latitude)+0.5), expand = c(0,0)) +
  theme_classic() +
  scale_color_gradientn(colours = haline ) +
  labs(col = 'Average Salinity of CIL [PSU] \n t < 2 deg C')
dev.off()



###salinity of entire water column



sal_wc <- list()

for (i in 1:length(oce_dat)){
  
  sal_wc[[i]]<- mean(oce_dat[[i]][['PSAL_01']], na.rm = TRUE)
  
}

sal_wc <- unlist(sal_wc)


map_dat <- map_dat %>%
  dplyr::mutate(., avg_sal = sal_wc)


#get cmocean palette
sal_pal <- cmocean('haline')
haline <- sal_pal(n = 100)




#average salinity of entire water column
jpeg('sgsl_sal.jpeg', width = 1000, height = 750)
ggplot()+
  geom_point(data = map_dat, aes(x = longitude, y = latitude, col = avg_sal), size = 10)+
  geom_path(data = clwf, aes(x = longitude, y = latitude), col = 'darkgreen') +
  scale_x_continuous(limits = c(min(coords$longitude) - 0.5, max(coords$longitude)+0.5), expand = c(0,0))+
  scale_y_continuous(limits = c(min(coords$latitude)-0.5, max(coords$latitude)+0.5), expand = c(0,0)) +
  theme_classic() +
  scale_color_gradientn(colours = haline )+
  labs(col = 'Average Salinity [PSU] \n')
dev.off()


#oxygen of CIL

##TODO: fix so stations with no data are marked by an X (!!!!)





cil_oxy_0 <- list()
cil_oxy_1 <- list()
cil_oxy_2 <- list()

for (i in 1:length(oce_dat)){
  
  cil_oxy_0[[i]]<- mean(cil0[[i]]$DOXY_01, na.rm = TRUE)
  
  cil_oxy_1[[i]] <- mean(cil1[[i]]$DOXY_01, na.rm = TRUE)
  
  cil_oxy_2[[i]] <- mean(cil2[[i]]$DOXY_01, na.rm = TRUE)
  
}

cil_oxy_0 <- unlist(cil_oxy_0)
cil_oxy_1 <- unlist(cil_oxy_1)
cil_oxy_2 <- unlist(cil_oxy_2)

cil_oxy_0[cil_oxy_0 == Inf] <- NA
cil_oxy_1[cil_oxy_1 == Inf] <- NA
cil_oxy_2[cil_oxy_2 == Inf] <- NA 


map_dat <- map_dat %>%
  dplyr::mutate(., CIL_0_oxy = cil_oxy_0) %>%
  dplyr::mutate(., CIL_1_oxy = cil_oxy_1) %>%
  dplyr::mutate(., CIL_2_oxy = cil_oxy_2) 


#get cmocean palette
oxy_pal <- cmocean('oxy')
oxygen <- oxy_pal(n = 100)


#min depth of CIL (t <0)
##ALL NA
# jpeg('sgsl_CIL_0_dmax.jpeg')
# ggplot()+
#   geom_point(data = map_dat, aes(x = longitude, y = latitude, col = CIL_0_maxdepth), size = 10)+
#   geom_path(data = clwf, aes(x = longitude, y = latitude), col = 'darkgreen') +
#   scale_x_continuous(limits = c(min(coords$longitude) - 0.5, max(coords$longitude)+0.5), expand = c(0,0))+
#   scale_y_continuous(limits = c(min(coords$latitude)-0.5, max(coords$latitude)+0.5), expand = c(0,0)) +
#   theme_classic() +
#   scale_color_gradientn(colours = deep )+
#   labs(col = 'Maximum depth of CIL [m] \n t < 0 deg C')
# dev.off()

#min depth of CIL (t <1)
jpeg('sgsl_CIL_1_oxy.jpeg', width = 1000, height = 750)
ggplot()+
  geom_point(data = map_dat[!is.na(map_dat$CIL_1_oxy),], aes(x = longitude, y = latitude, col = CIL_1_oxy), size = 10)+
  geom_path(data = clwf, aes(x = longitude, y = latitude), col = 'darkgreen') +
  scale_x_continuous(limits = c(min(coords$longitude) - 0.5, max(coords$longitude)+0.5), expand = c(0,0))+
  scale_y_continuous(limits = c(min(coords$latitude)-0.5, max(coords$latitude)+0.5), expand = c(0,0)) +
  theme_classic() +
  scale_color_gradientn(colours = oxygen )+
  labs(col = 'Average Dissolved Oxygen of CIL \n t < 1 deg C')
dev.off()

#min depth of CIL (t <2)
jpeg('sgsl_CIL_2_oxy.jpeg', width = 1000, height = 750)
ggplot()+
  geom_point(data = map_dat[!is.na(map_dat$CIL_2_oxy),], aes(x = longitude, y = latitude, col = CIL_2_oxy), size = 10)+
  geom_path(data = clwf, aes(x = longitude, y = latitude), col = 'darkgreen') +
  scale_x_continuous(limits = c(min(coords$longitude) - 0.5, max(coords$longitude)+0.5), expand = c(0,0))+
  scale_y_continuous(limits = c(min(coords$latitude)-0.5, max(coords$latitude)+0.5), expand = c(0,0)) +
  theme_classic() +
  scale_color_gradientn(colours = oxygen ) +
  labs(col = 'Average Dissolved Oxygen of CIL \n t < 2 deg C')
dev.off()





oxy_wc <- list()

for (i in 1:length(oce_dat)){
  
  oxy_wc[[i]]<- mean(oce_dat[[i]][['DOXY_01']], na.rm = TRUE)
  
}

oxy_wc <- unlist(oxy_wc)


map_dat <- map_dat %>%
  dplyr::mutate(., avg_oxy = oxy_wc)



#average salinity of entire water column
jpeg('sgsl_oxy.jpeg', width = 1000, height = 750)
ggplot()+
  geom_point(data = map_dat, aes(x = longitude, y = latitude, col = avg_oxy), size = 10)+
  geom_path(data = clwf, aes(x = longitude, y = latitude), col = 'darkgreen') +
  scale_x_continuous(limits = c(min(coords$longitude) - 0.5, max(coords$longitude)+0.5), expand = c(0,0))+
  scale_y_continuous(limits = c(min(coords$latitude)-0.5, max(coords$latitude)+0.5), expand = c(0,0)) +
  theme_classic() +
  scale_color_gradientn(colours = oxygen )+
  labs(col = 'Average Dissolved Oxygen  \n')
dev.off()


#boxplot to compare CIL through stations
#t < 1

#collect relevant data to plot
bp_dat <- map_dat %>%
  dplyr::select(., CIL_1_mindepth, CIL_1_maxdepth, CIL_1_depth, CIL_1_thick) %>%
  dplyr::mutate(., station = as.factor(seq(1,30)))

map_dat <- map_dat %>%
  dplyr::mutate(., station = factor(seq(1,30), ordered = TRUE))


names(cil1) <- seq(1,30)

cil1_c <- bind_rows(cil1, .id = "column_label") %>%
  dplyr::mutate(column_label = factor(column_label, ordered = TRUE, levels = seq(1,30)))

#boxplot of CIL depths t< 1
jpeg('sgsl_cil_box.jpeg', width = 1000, height = 250)
ggplot()+
  geom_boxplot(data = cil1_c, aes(x = column_label, y = PRES_01)) +
  scale_y_reverse('Depth [db] ')+
  theme_classic()+
  scale_x_discrete('Station ID')+
  ggtitle('Depth of CIL, t < 1 deg C')+
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 20), title = element_text(size = 25))
dev.off()


jpeg('sgsl_ref.jpeg', width = 1000, height = 750)
ggplot()+
  geom_point(data = map_dat, aes(x = longitude, y = latitude), size = 5, shape = 20)+
  geom_path(data = clwf, aes(x = longitude, y = latitude), col = 'darkgreen', size = 3) +
  scale_x_continuous(limits = c(min(coords$longitude) - 0.5, max(coords$longitude)+0.5), expand = c(0,0))+
  scale_y_continuous(limits = c(min(coords$latitude)-0.5, max(coords$latitude)+0.5), expand = c(0,0)) +
  theme_classic() +
  #ggrepel::geom_text_repel(data = map_dat, aes(x = longitude, y = latitude, label = station), size = 10 )+
  ggrepel::geom_label_repel(data = map_dat, aes(x = longitude, y = latitude, label = station), fill = 'gray70', size = 7 ) +
  #add bathymetry
  stat_contour(data = bathy_data_list[[1]],
               aes(x=x, y=y, z=z),
               breaks=c(-80), 
               size=c(0.6),
               colour = "gray61") +
  
  stat_contour(data = bathy_data_list[[1]],
               aes(x=x, y=y, z=z),
               breaks=c(-90), 
               size=c(0.6),
               colour = "gray41") +
  
  stat_contour(data = bathy_data_list[[1]],
               aes(x=x, y=y, z=z),
               breaks=c(-100), 
               size=c(0.6),
               colour = "black")
  
dev.off()

#boxplot of CIL t< 1 salinity
jpeg('sgsl_cil_box_sal.jpeg', width = 1000, height = 250)
ggplot()+
  geom_boxplot(data = cil1_c, aes(x = column_label, y = PSAL_01)) +
  scale_y_continuous('Salinity [PSU] ')+
  theme_classic()+
  scale_x_discrete('Station ID')+
  ggtitle('Salinity of CIL, t < 1 deg C')+
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 20), title = element_text(size = 25))

dev.off()


#boxplot of CIL t< 1 oxygen
jpeg('sgsl_cil_box_oxy.jpeg', width = 1000, height = 250)
ggplot()+
  geom_boxplot(data = cil1_c, aes(x = column_label, y = DOXY_01)) +
  scale_y_continuous('DIssolved Oxygen ')+
  theme_classic()+
  scale_x_discrete('Station ID')+
  ggtitle('Dissolved Oxygen of CIL, t < 1 deg C')+
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 20), title = element_text(size = 25))

dev.off()


#attempt to group stations

#look at boxplot stats for depth of CIL t < 1
st_cil1 <- unique(cil1_c$column_label)

bp <- list()

for (i in 1:length(st_cil1)){
bp[[i]] <- boxplot.stats(cil1_c$PRES_01[cil1_c$column_label == st_cil1[[i]]])

names(bp) <- st_cil1
}


#find range for each box

bp_d <- list()

for (i in 1:length(bp)){
  
  bp_d[[i]] <- bp[[i]]$stats[[4]] - bp[[i]]$stats[[2]]
  
}

install.packages('ggpubr')
library(ggpubr)

#code from : https://www.r-bloggers.com/add-p-values-and-significance-levels-to-ggplots/


#designate high vs low means

cil1_c_means <- cil1_c %>%
  dplyr::group_by(., column_label) %>%
  dplyr::summarize(., pres_mean = mean(PRES_01, na.rm = TRUE))


group <- list()
for (i in 1:length(unique(cil1_c_means$column_label))){
  
  if (cil1_c_means$pres_mean[cil1_c_means$column_label == unique(cil1_c_means$column_label)[[i]]] < mean(cil1_c$PRES_01)){
    group[[i]] <- 'low'
  }else{
    group[[i]] <- 'high'
  }
  
}

names(group) <- st_cil1

#add groups to dataframe

for (i in 1:length(st_cil1)){
  cil1_c$group[cil1_c$column_label == st_cil1[[i]]] <- group[[i]]
}


jpeg('cil1_compare_means_box.jpeg', width = 1000, height = 500)
ggboxplot(cil1_c, x = 'column_label', y = 'PRES_01', col = 'group',
           legend = "none") +
  scale_y_reverse( 'Depth [db] ')+
  scale_x_discrete('Station ID')+
  #scale_color_discrete(palette = therm_pal)+
  rotate_x_text(angle = 45)+
  geom_hline(yintercept = mean(cil1_c$PRES_01, na.rm = TRUE), linetype = 2)+ # Add horizontal line at base mean
  stat_compare_means(method = "anova", label.y = -40)+        # Add global annova p-value
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = ".all.")          +            # Pairwise comparison against all
  ggtitle('Depth of CIL, t < 1 deg C')+
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 20), title = element_text(size = 25))
dev.off()

#compare means from oxygen


#designate high vs low means

cil1_c_means <- cil1_c %>%
  dplyr::group_by(., column_label) %>%
  dplyr::summarize(., oxy_mean = mean(DOXY_01, na.rm = TRUE))


group <- list()
for (i in 1:length(unique(cil1_c_means$column_label))){
  
  if (cil1_c_means$oxy_mean[cil1_c_means$column_label == unique(cil1_c_means$column_label)[[i]]] < mean(cil1_c$DOXY_01)){
    group[[i]] <- 'low'
  }else{
    group[[i]] <- 'high'
  }
  
}

names(group) <- st_cil1

#add groups to dataframe

for (i in 1:length(st_cil1)){
  cil1_c$group[cil1_c$column_label == st_cil1[[i]]] <- group[[i]]
}


jpeg('cil1_compare_means_box_oxy.jpeg', width = 1000, height = 500)
ggboxplot(cil1_c, x = 'column_label', y = 'DOXY_01', col = 'group',
          legend = "none") +
  #scale_y_reverse( 'Depth [db] ')+
  scale_x_discrete('Station ID')+
  #scale_color_discrete(palette = therm_pal)+
  rotate_x_text(angle = 45)+
  geom_hline(yintercept = mean(cil1_c$DOXY_01, na.rm = TRUE), linetype = 2)+ # Add horizontal line at base mean
  stat_compare_means(method = "anova", label.y = 8)+        # Add global annova p-value
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = ".all.")          +            # Pairwise comparison against all
  ggtitle('Dissolved Oxygen of CIL, t < 1 deg C')+
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 20), title = element_text(size = 25))
dev.off()

#compare means from salinity


#designate high vs low means

cil1_c_means <- cil1_c %>%
  dplyr::group_by(., column_label) %>%
  dplyr::summarize(., sal_mean = mean(PSAL_01, na.rm = TRUE))


group <- list()
for (i in 1:length(unique(cil1_c_means$column_label))){
  
  if (cil1_c_means$sal_mean[cil1_c_means$column_label == unique(cil1_c_means$column_label)[[i]]] < mean(cil1_c$PSAL_01, NA.RM = TRUE)){
    group[[i]] <- 'low'
  }else{
    group[[i]] <- 'high'
  }
  
}

names(group) <- st_cil1

#add groups to dataframe

for (i in 1:length(st_cil1)){
  cil1_c$group[cil1_c$column_label == st_cil1[[i]]] <- group[[i]]
}


jpeg('cil1_compare_means_box_sal.jpeg', width = 1000, height = 500)
ggboxplot(cil1_c, x = 'column_label', y = 'PSAL_01', col = 'group',
          legend = "none") +
  #scale_y_reverse( 'Depth [db] ')+
  scale_x_discrete('Station ID')+
  #scale_color_discrete(palette = therm_pal)+
  rotate_x_text(angle = 45)+
  geom_hline(yintercept = mean(cil1_c$PSAL_01, na.rm = TRUE), linetype = 2)+ # Add horizontal line at base mean
  stat_compare_means(method = "anova", label.y = 32.75)+        # Add global annova p-value
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = ".all.")          +            # Pairwise comparison against all
  ggtitle('Salinity of CIL, t < 1 deg C')+
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 20), title = element_text(size = 25))
dev.off()





