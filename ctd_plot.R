


#Plfot CTD data from IML2018051 and COR2019002
#E. Chisholm November 2019



#load in data from ctd_process

load('combined_ctd_data.RData')

####PLOT CTD DATA

#PLOT TS WITH DATA BINNED BY TEMPERATURE ( T<0, T<1, T<3, T<6, T>6)

#PLOT WITH DEPTH BINS AT 50M, 150M

#PLOT SPATIALLY TO SEE IF INTERPOLATION OVER WESTERN MAGDALEN SHALLOWS IS REASONABLE


#PULL IN VPR DATA
#SMALL SCALE vpr TRANSECT SECTION PLOTS OF TEMP AND SALINITY HIGHLIGHTING TEMPERATURE BINS ( T<0, T<1, T<3, T<6, T>6)

#INTERPOLATE OVER CAP 1 AND 2 TRANSECTS?


#PULL IN SST DATA

#CHARACTERIZE SURFACE REGIONS BY TEMPERATURE THRESHOLDS (BINS?)











#######OLDER VERSION PRE NOVEMBER 12TH MEETINGS#####
##test on one cruise at a time
####IML2018051 - Plot####
ctd <- ctd_hud

#switch from oce to dplyr data frame

#insert cruise and station names into data slot
for (i in 1:length(ctd)){
  ctd[[i]] <- oceSetData(ctd[[i]], name = 'cruise', 
                         value = ifelse(ctd[[i]][['date']]< '2019-01-01 00:00:00 UTC',
                                        yes = 'IML2018051', no = 'COR2019002'))
  ctd[[i]] <- oceSetData(ctd[[i]], name = 'station', value = ctd[[i]][['station']])
}

#pull data slot into data frame and combine casts
ctd_df <- list()
for(i in 1:length(ctd)){
  ctd_df[[i]] <- as.data.frame(ctd[[i]]@data, stringsAsFactors = FALSE)
}

ctd_df <- do.call(rbind,ctd_df)




#Quality control

ctd_df <-  ctd_df %>%
  dplyr::filter(., salinity > 28) %>%
  dplyr::filter(., station != 'Test')

#not enouugh, each station must go through individual quality control


#plot TS for each profile

ggplot()+
  geom_point(data = ctd_df, aes(y = temperature, x = salinity))+
  facet_wrap(~station)


png('IML2018051_TS_oxy.png', width = 1500, height = 1500)
plotTS_balloon(ctd_df, var = 'oxygen') +
  facet_wrap(~station ) +
  theme(panel.spacing = unit(2, 'lines'))+
  ggtitle('TS - Oxygen ')
dev.off()


#plot all TS with oxygen
#add discrete oxygen bins?
#plotTS_balloon(ctd_df, var = 'oxygen')

png('IML2018051_temp_profiles.png')
ggplot(ctd_df) +
  geom_point(aes(x = temperature, y = depth))+
  facet_wrap(~station)+
  theme(panel.spacing = unit(2, 'line'))+
  theme_classic()+
  scale_y_reverse('Depth [ db ]')+
  scale_x_continuous('Temperature [ deg C ]')+
  ggtitle("Temperature")
dev.off()

png('IML2018051_sal_profiles.png')
ggplot(ctd_df) +
  geom_point(aes(x = salinity, y = depth))+
  facet_wrap(~station)+
  theme(panel.spacing = unit(2, 'line'))+
  theme_classic()+
  scale_y_reverse('Depth [ db ]')+
  scale_x_continuous('Salinity [ PSU ]')+
  ggtitle("Salinity")
dev.off()

png('IML2018051_oxy_profiles.png')
ggplot(ctd_df) +
  geom_point(aes(x = oxygen, y = depth))+
  facet_wrap(~station)+
  theme(panel.spacing = unit(2, 'line'))+
  theme_classic()+
  scale_y_reverse('Depth [ db ]')+
  scale_x_continuous('Oxygen [ mll ] ')+
  ggtitle("Oxygen")
dev.off()



#plot TS using discrete depth bins
#by station

ctd_df <- ctd_df %>%
  dplyr::mutate(., bin = NA)

for (i in 1:length(ctd_df$depth)){
  if(ctd_df$depth[i] <=10){
    ctd_df$bin[i] <- 1
  }
  if(ctd_df$depth[i] >10 & ctd_df$depth[i] <=75){
    ctd_df$bin[i] <- 2
  }
  if(ctd_df$depth[i] >75 & ctd_df$depth[i] <=150){
    ctd_df$bin[i] <- 3
  }
  if(ctd_df$depth[i] >150 & ctd_df$depth[i] <=500){
    ctd_df$bin[i] <- 4
  }
}

#all together
png('IML2018051_TS_bin2.png')
ggplot(ctd_df)+
  geom_point(aes(y = temperature, x = salinity, col = as.factor(bin)), alpha = 0.5)+
  theme_classic()+
  scale_color_discrete(name = 'Bin' , labels = c('<10m', '10 - 75m', '75-100m', '>150m'))+
  ggtitle('TS bins')
dev.off()

png('COR2019002_TS_bins_facet.png')
ggplot(ctd_df)+
  geom_point(aes(x = salinity, y = temperature, col = as.factor(bin)))+
  facet_wrap(~station)+
  theme_classic()+
  scale_color_discrete(name = 'Bin' , labels = c('<10m', '10 - 50m', '50-100m', '>100m'))
dev.off()


####COR2019002 - Plot#####
##test on one cruise at a time

ctd <- ctd_cor

#switch from oce to dplyr data frame

#insert cruise and station names into data slot
for (i in 1:length(ctd)){
  ctd[[i]] <- oceSetData(ctd[[i]], name = 'cruise', 
                         value = ifelse(ctd[[i]][['date']]< '2019-01-01 00:00:00 UTC',
                                        yes = 'IML2018051', no = 'COR2019002'))
  ctd[[i]] <- oceSetData(ctd[[i]], name = 'station', value = ctd[[i]][['station']])
}

#pull data slot into data frame and combine casts
ctd_df <- list()
for(i in 1:length(ctd)){
  ctd_df[[i]] <- as.data.frame(ctd[[i]]@data, stringsAsFactors = FALSE)
}

ctd_df <- do.call(rbind,ctd_df)

names(ctd_df)[[3]] <- 'depth'


#Quality control

ctd_df <-  ctd_df %>%
  dplyr::filter(., salinity > 28) %>%
  dplyr::filter(., station != c('Test', 'test'))

#not enouugh, each station must go through individual quality control


#plot TS for each profile

ggplot()+
  geom_point(data = ctd_df, aes(y = temperature, x = salinity))+
  facet_wrap(~station)


png('COR2019002_TS_oxy.png', width = 1500, height = 1500)
plotTS_balloon(ctd_df, var = 'oxygen') +
  facet_wrap(~station ) +
  theme(panel.spacing = unit(2, 'lines'))+
  ggtitle('TS - Oxygen ')
dev.off()


#plot all TS with oxygen
#add discrete oxygen bins?
#plotTS_balloon(ctd_df, var = 'oxygen')

png('COR2019002_temp_profiles.png')
ggplot(ctd_df) +
  geom_point(aes(x = temperature, y = depth))+
  facet_wrap(~station)+
  theme(panel.spacing = unit(2, 'line'))+
  theme_classic()+
  scale_y_reverse('Depth [ db ]')+
  scale_x_continuous('Temperature [ deg C ]')+
  ggtitle("Temperature")
dev.off()

png('COR2019002_sal_profiles.png')
ggplot(ctd_df) +
  geom_point(aes(x = salinity, y = depth))+
  facet_wrap(~station)+
  theme(panel.spacing = unit(2, 'line'))+
  theme_classic()+
  scale_y_reverse('Depth [ db ]')+
  scale_x_continuous('Salinity [ PSU ]')+
  ggtitle("Salinity")
dev.off()

png('COR2019002_oxy_profiles.png')
ggplot(ctd_df) +
  geom_point(aes(x = oxygen, y = depth))+
  facet_wrap(~station)+
  theme(panel.spacing = unit(2, 'line'))+
  theme_classic()+
  scale_y_reverse('Depth [ db ]')+
  scale_x_continuous('Oxygen [ mll ] ')+
  ggtitle("Oxygen")
dev.off()



#plot TS using discrete depth bins
#by station

ctd_df <- ctd_df %>%
  dplyr::mutate(., bin = NA)

for (i in 1:length(ctd_df$depth)){
  if(ctd_df$depth[i] <=10){
    ctd_df$bin[i] <- 1
  }
  if(ctd_df$depth[i] >10 & ctd_df$depth[i] <=50){
    ctd_df$bin[i] <- 2
  }
  if(ctd_df$depth[i] >50 & ctd_df$depth[i] <=100){
    ctd_df$bin[i] <- 3
  }
  if(ctd_df$depth[i] >100 & ctd_df$depth[i] <=500){
    ctd_df$bin[i] <- 4
  }
}

#all together
png('COR2019002_TS_bin.png')
ggplot(ctd_df)+
  geom_point(aes(y = temperature, x = salinity, col = as.factor(bin)))+
  theme_classic()+
  scale_color_discrete(name = 'Bin' , labels = c('<10m', '10 - 50m', '50-100m', '>100m'))+
  ggtitle('TS bins')
dev.off()

#compare TS clusters and depth range across stations


png('COR2019002_TS_bins_facet.png')
ggplot(ctd_df)+
  geom_point(aes(x = salinity, y = temperature, col = as.factor(bin)))+
  facet_wrap(~station)+
  theme_classic()+
  scale_color_discrete(name = 'Bin' , labels = c('<10m', '10 - 50m', '50-100m', '>100m'))
dev.off()

##plot spatial range of sampling on each cruise

