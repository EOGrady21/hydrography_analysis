# Analysis of hydrographic data from sGSL
# E. Chisholm
# Feb 2020

# references
# PCA : https://www.datacamp.com/community/tutorials/pca-analysis-r
# Clusters : https://uc-r.github.io/kmeans_clustering
# NMDS : https://rpubs.com/CPEL/NMDS 
# Analysis : https://www.int-res.com/articles/meps2005/302/m302p103.pdf

# Define metrics used to describe water column


# removed because does not apply to all stations
# s_cil = Average salinity of CIL
# t_cil = Average temperature of CIL
# u_cil = upper limit of CIL
# zd_cil = thickness of CIL

# ---------------
# t_min = Minimum temperature of water column
# s_surf = Average salinity of surface layer (above thermocline )
# t_surf = Average temperature of surface layer (above thermocline)
# strat = stratification index (density at 10m / density at bottom - 10m)
# d_therm = depth of thermocline
# dt_min = depth of minimum temperature / total depth at station
# t_bot = temperature at bottom




###READ IN####
#read in IML text files which have QC flags

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


for (i in 1:length(coords$latitude)){
  coords$station[[i]] <- i
}

# add geog info to df

df_full <- dplyr::right_join(df, coords, by = 'station')

z_bot_list <- bottom_depth(data = df_full, station = seq(1,30) , bathy_data_list) 

# set CIL temperature definition
cil_lim <- 1

df_summ <- df %>%
  dplyr::group_by(., station) %>%
  dplyr::summarise(.,  
                   t_min = min(.data$TE90_01), # min temp of water column
                   t_surf = mean(.data$TE90_01[.data$PRES_01 < clined(.data$TE90_01, .data$PRES_01)]), # average temperature of surface layer (above thermocline)
                   s_surf = mean(.data$PSAL_01[.data$PRES_01 < clined(.data$TE90_01, .data$PRES_01)]), # average salinity of surface layer (above thermocline)
                   z_max = max(.data$PRES_01), # max sampling depth
                   strat = ( mean(SIGT_01[.data$PRES_01 == z_max - 10], na.rm = TRUE) - mean(SIGT_01[.data$PRES_01 == 10], na.rm = TRUE))/(.data$z_max - 10 - 10),#stratification index (density at 10m / density at bottom - 10m)
                   d_therm = clined(.data$TE90_01, .data$PRES_01), #depth of thermocline
                   dt_min = PRES_01[.data$TE90_01 == min(.data$TE90_01)] / z_max, #depth of minimum temperature / total depth at station  
                   t_bot = TE90_01[.data$PRES_01 == max(.data$PRES_01)]
                   )  %>%
  dplyr::mutate(., z_bot = abs(unlist(z_bot_list)))

# removed CIL metrics
# u_cil = ifelse(is.finite(min(.data$PRES_01[.data$TE90_01 < cil_lim])), min(.data$PRES_01[.data$TE90_01 < cil_lim]), NA), # upper limit of CIL
# zd_cil = max(.data$PRES_01[.data$TE90_01 < cil_lim]) - min(.data$PRES_01[.data$TE90_01 < cil_lim]), # thickness of CIL
# zt_cil = .data$PRES_01[.data$TE90_01 == min(.data$TE90_01)], # depth of minimum CIL temperature
#  s_cil = mean(.data$PSAL_01[.data$TE90_01 < cil_lim], na.rm = TRUE), # mean salinity of CIL
# sst = mean(TE90_01[.data$PRES_01 < 5]) # surface temperture (average of first 5m)

# for NA z_bot use Z_max (station 30)
df_summ <- df_summ %>%
  dplyr::mutate(., z_bot = ifelse(is.na(z_bot), z_max, z_bot))

# remove infinites in thickness
#df_summ$zd_cil[!is.finite(df_summ$zd_cil)] <- NA

# plot all metrics

df_summ_m <- reshape2::melt(df_summ, id.vars = 'station')

png('env_var_stat.png', width = 1500, height = 500)
ggplot(df_summ_m[!df_summ_m$variable %in% c('z_bot', 'z_max'),])+
  geom_point(aes(y = value, x = as.factor(station)))+
  facet_wrap(~variable, scales = 'free_y') +
  theme_classic() +
  scale_x_discrete('Stations') +
  ggtitle('Environmental variables by station') +
  theme(axis.title = element_text(size = 15))
dev.off()

# plot metrics in space

coords$station <- as.numeric(coords$station)
dat <- right_join(df_summ, coords, by = 'station')

var_nm <- c(names(dat)[!names(dat) %in% c('station', 'z_max', 'z_bot', 'latitude', 'longitude')])

for( v in var_nm){
  
  png(paste0('mapPlot_', v, '.png'), width = 750, height = 750)
  p <- interpPlot(dat, v) +
    #add bathymetry
    stat_contour(data = bathy_data_list[[1]],
                 aes(x=x, y=y, z=z),
                 breaks=c(-50), 
                 size=c(0.6),
                 colour = "gray61") +
  
    
    stat_contour(data = bathy_data_list[[1]],
                 aes(x=x, y=y, z=z),
                 breaks=c(-100), 
                 size=c(0.6),
                 colour = "black")
  print(p)
  dev.off()
}

# normalize metrics 


df_summ_norm <- df_summ %>%
  dplyr::mutate_all(., normalit ) %>%
  dplyr::mutate(., station = as.factor(seq(1,30))) %>%
  dplyr::mutate(., z_bot = unlist(z_bot_list))

# for NA z_bot use Z_max (station 30)
df_summ_norm <- df_summ_norm %>%
  dplyr::mutate(., z_bot = ifelse(is.na(z_bot), -df_summ$z_max[df_summ$station == .data$station], z_bot))


# plot normalized metrics

norm_melt <- reshape2::melt(df_summ_norm, id.vars = 'station')

ggplot(norm_melt) +
  geom_point(aes( y = value, x = as.factor(station)))+
  facet_wrap(~variable, scales = 'free_y')

 
# perform Principal components analysis

pca_res <- prcomp(na.omit(df_summ_norm[c(2, 3, 4, 6, 7, 8, 9)]), center = TRUE, scale. = TRUE)

# plot
png('PCA_12.png', width = 500, height = 500)
ggbiplot::ggbiplot(pca_res, choices = c(1,2), varname.size = 6, varname.adjust = 1) 
dev.off()
png('PCA_23.png', width = 500, height = 500)
ggbiplot::ggbiplot(pca_res, choices = c(2, 3), varname.size = 4, varname.adjust = 1)
dev.off()
png('PCA_34.png', width = 500, height = 500)
ggbiplot::ggbiplot(pca_res, choices = c(3,4))
dev.off()

png('pca_biplot.png')
fviz_pca(pca_res)
dev.off()

png('pca_varplot.png')
fviz_pca_var(pca_res)
dev.off()
# KS plotting
pca_metrics10 <- pca_res
#Get percentage of variation explained by each axis
summary(pca_metrics10)$importance

#Get percentage of variance explained
p1 <- as.character(round(summary(pca_metrics10)$importance[2,1], digits = 2)*100)

#Get scores
pca_scores <- data.frame(pca_metrics10$x)
pca_scores_stn <- cbind(df_summ_norm, pca_scores)

# add geographic info
coords$station <- as.factor(coords$station)
pca_scores_stn_c <- right_join(pca_scores_stn, coords)

pca_scores_stn2_PC1 <- pca_scores_stn_c %>%
  dplyr::select(., station, longitude, latitude, PC1) %>%
  dplyr::mutate(., sign = ifelse(PC1 < 0, "negative", "positive")) %>%
  dplyr::mutate(., pc1_mag = abs(PC1))

#Get (scaled) loadings (PC1): This code comes from a math/eng phd. Its probably right.
PC_axis <- 1
const <- pca_metrics10$sdev[PC_axis]*sqrt(nrow(pca_metrics10$x))
vecs_new <- t(t(pca_metrics10$rotation[,PC_axis])*const)
loadings_scaled <- data.frame(data.frame(vecs_new), metric = row.names(vecs_new))

##PLOT SCORES ON MAP
extent_swGSL <- c("ymin" = 46.75,
                  "ymax" = 49.25,
                  "xmin" = -66.5,
                  "xmax" = -61.5)

proj_wgs1984 <- "+init=epsg:4326 +proj=longlat +ellps=WGS84 + datum=WGS84 +no_defs +towgs84=0,0,0"

#Get bathymetry
#NOAA (low res, but over full area)
bathy_noaa <- readRDS("bathydata_noaa.RDS")
bathy_f_noaa <- marmap::fortify.bathy(bathy_noaa)
bathy_levels <- c(-50,  -100,  -150)

p <- getBaseMap(extent = extent_swGSL, 
                bathy = bathy_f_noaa, 
                bathy_levels = bathy_levels,
                projection = proj_wgs1984) +
  geom_point(data = pca_scores_stn2_PC1, 
             aes(x = longitude, y = latitude, colour = sign, size = pc1_mag),
             alpha = 0.5)
png('map_pca1.png', width = 500, height = 500)
print(p)
dev.off()

# pc 2

p2 <- as.character(round(summary(pca_metrics10)$importance[2,2], digits = 2)*100)

pca_scores_stn2_PC2 <- pca_scores_stn_c %>%
  dplyr::select(., station, longitude, latitude, PC2) %>%
  dplyr::mutate(., sign = ifelse(PC2 < 0, "negative", "positive")) %>%
  dplyr::mutate(., pc_mag = abs(PC2))

#Get (scaled) loadings (PC1): This code comes from a math/eng phd. Its probably right.
PC_axis <- 2
const <- pca_metrics10$sdev[PC_axis]*sqrt(nrow(pca_metrics10$x))
vecs_new <- t(t(pca_metrics10$rotation[,PC_axis])*const)
loadings_scaled <- data.frame(data.frame(vecs_new), metric = row.names(vecs_new))


p <- getBaseMap(extent = extent_swGSL, 
                bathy = bathy_f_noaa, 
                bathy_levels = bathy_levels,
                projection = proj_wgs1984) +
  geom_point(data = pca_scores_stn2_PC2, 
             aes(x = longitude, y = latitude, colour = sign, size = pc_mag),
             alpha = 0.5) +
  ggtitle('PC 2 ')
png('map_pca2.png', width = 500, height = 500)
print(p)
dev.off()

# cluster analysis
library(factoextra)

cl_df <- na.omit(df_summ_norm[c(2, 3, 4, 6, 7, 8, 9)])

cl_dist <- get_dist(cl_df)
png('distmat_clust.png', width = 750, height = 750)
fviz_dist(cl_dist, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
dev.off()

# kmeans clustering
# 2 centers
k2 <- kmeans(cl_df, centers = 2)

# auto plot
p2 <- fviz_cluster(k2, cl_df)

# manual plot
cl_df %>%
  as_tibble() %>%
  mutate(cluster = k2$cluster, 
         station = row.names(cl_df)) %>%
  ggplot(aes(t_surf, t_min, color = factor(cluster), label = station)) +
  geom_text()

# 3 centers
k3 <- kmeans(cl_df, center = 3)

p3 <- fviz_cluster(k3, cl_df)

# 4 centers
k4 <- kmeans(cl_df, center = 4)

p4 <- fviz_cluster(k4, cl_df)

# 5 centers
k5 <- kmeans(cl_df, center = 5)

p5 <- fviz_cluster(k5, cl_df)

# manual plot
png('tsur_tmin_clusterd.png', width = 750, height = 750)
cl_df %>%
  as_tibble() %>%
  mutate(cluster = k5$cluster, 
         station = row.names(cl_df)) %>%
  ggplot(aes(t_surf, t_min, color = factor(cluster), label = station)) +
  geom_text(nudge_x = 0.1, nudge_y = 0.1, show.legend = FALSE) +
  geom_point() +
  scale_color_discrete('Cluster')
dev.off()


png('tsur_ssur_clusterd.png', width = 750, height = 750)
cl_df %>%
  as_tibble() %>%
  mutate(cluster = k5$cluster, 
         station = row.names(cl_df)) %>%
  ggplot(aes(t_surf, s_surf, color = factor(cluster), label = station)) +
  geom_text(nudge_x = 0.1, nudge_y = 0.1, show.legend = FALSE) +
  geom_point() +
  scale_color_discrete('Cluster')
dev.off()



png('dtherm_tmin_clusterd.png', width = 750, height = 750)
cl_df %>%
  as_tibble() %>%
  mutate(cluster = k5$cluster, 
         station = row.names(cl_df)) %>%
  ggplot(aes(d_therm, t_min, color = factor(cluster), label = station)) +
  geom_text(nudge_x = 0.1, nudge_y = 0.1, show.legend = FALSE) +
  geom_point() +
  scale_color_discrete('Cluster')
dev.off()


png('strat_tmin_clusterd.png', width = 750, height = 750)
cl_df %>%
  as_tibble() %>%
  mutate(cluster = k5$cluster, 
         station = row.names(cl_df)) %>%
  ggplot(aes(strat, t_min, color = factor(cluster), label = station)) +
  geom_text(nudge_x = 0.1, nudge_y = 0.1, show.legend = FALSE) +
  geom_point() +
  scale_color_discrete('Cluster')
dev.off()


png('cluster_grid.png', width = 1000, height = 1000)
gridExtra::grid.arrange(p2, p3, p4, p5)
dev.off()

png('cluster_k5.png')
print(p5)
dev.off()

png('clust_elbow.png', width = 500, height = 500)
fviz_nbclust(cl_df, kmeans, method = "wss")
dev.off()

# plot clusters in space
clust_df <- cbind(cl_df, k5 = k5$cluster, k4 = k4$cluster, k3 = k3$cluster, k2 = k2$cluster, lat = coords$latitude, lon = coords$longitude, station = as.factor(seq(1,30)))

png('cluster_map_nopoly.png', width = 1000, height = 1000)
p <- getBaseMap(extent = extent_swGSL, 
                bathy = bathy_f_noaa, 
                bathy_levels = bathy_levels,
                projection = proj_wgs1984)

 p <- p + geom_point(data = clust_df, aes(x = lon, y = lat, shape = as.factor(k5), col = as.factor(k5)), size = 6) +
  labs(shape = 'Cluster', col = 'Cluster') +
   geom_label_repel(data = clust_df, aes(x = lon, y = lat, label = station))
print(p)
dev.off()


# cluster line plot
# https://towardsdatascience.com/clustering-analysis-in-r-using-k-means-73eca4fb7967


library(GGally)
library(plotly)

clustering <- k5
tt <- cl_df
tt$cluster <- as.factor(clustering$cluster)

p <- ggparcoord(data = tt, columns = c(2:6), groupColumn = "cluster", scale = "std", showPoints = TRUE) + 
  labs(x = "Environmental Variable", y = "value (standardized)", title = "Clustering") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12), 
        plot.title = element_text(size = 16), 
        legend.key.size = unit(24, 'points')) 
  
ggplotly(p)

png('cluster_lines.png', width = 1000, height = 500)
print(p)
dev.off()

# plot profiles per cluster

clust <- clust_df$k5

cl_tot <- list()
for (i in 1:length(df$station)){
  cl_tot[[i]] <- clust[df$station[[i]]]
}
cl_tot <- unlist(cl_tot)

df_tot_cl <- df %>%
  dplyr::mutate(., cluster = cl_tot)

png('Temp_clust.png', width = 1500, height = 750)
ggplot(df_tot_cl) +
  geom_point(aes(x = TE90_01, y = PRES_01), col = 'red')+
  scale_y_reverse()+
  facet_wrap(~cluster, nrow = 1) +
  labs(y = 'Pressure [db]', x = 'Temperature [C]') +
  theme_classic() +
  theme(axis.text = element_text(size = 25), 
        strip.text = element_text(size = 25),
        panel.spacing = unit(2, 'lines'))
dev.off()

png('Sal_clust.png', width = 1500, height = 750)
ggplot(df_tot_cl) +
  geom_point(aes(x = PSAL_01, y = PRES_01), col = 'blue')+
  scale_y_reverse()+
  facet_wrap(~cluster, nrow = 1) +
  labs(y = 'Pressure [db]', x = 'Salinity [PSU]') +
  theme_classic()+
  theme(axis.text = element_text(size = 25), 
        strip.text = element_text(size = 25),
        panel.spacing = unit(2, 'lines'))
dev.off()

png('flor_clust.png', width = 1500, height = 750)
ggplot(df_tot_cl) +
  geom_point(aes(x = FLOR_01, y = PRES_01), col = 'green')+
  scale_y_reverse()+
  facet_wrap(~cluster, nrow = 1) +
  labs(y = 'Pressure [db]', x = 'Fluorescence [mV]') +
  theme_classic()+
  theme(axis.text = element_text(size = 25), 
        strip.text = element_text(size = 25),
        panel.spacing = unit(2, 'lines'))
dev.off()

png('oxy_clust.png', width = 1500, height = 750)
ggplot(df_tot_cl) +
  geom_point(aes(x = DOXY_01, y = PRES_01), col = 'black')+
  scale_y_reverse()+
  facet_wrap(~cluster, nrow = 1) +
  labs(y = 'Pressure [db]', x = 'Dissolved Oxygen [ mmol ]') +
  theme_classic()+
  theme(axis.text = element_text(size = 25), 
        strip.text = element_text(size = 25),
        panel.spacing = unit(2, 'lines'))
dev.off()


# summary cluster plots

c_summ <- clust_df %>%
  dplyr::group_by(k5) %>%
  dplyr::summarise(., t_min = mean(t_min),
                   t_surf = mean(t_surf),
                   s_surf = mean(s_surf),
                   strat = mean(strat),
                   d_therm = mean(d_therm),
                   dt_min = mean(dt_min),
                   t_bot = mean(t_bot)
  )


csm <- reshape2::melt(c_summ, id.vars = 'k5')

clust_ord <- c(1, 5, 3, 4, 2)

csm$k5 <- factor(csm$k5, levels = clust_ord)

pal <- cmocean::cmocean('balance')
pal_col <- pal(n = 100)

p <- ggplot(csm)+
  geom_tile(aes(x = variable, y = k5, fill = value))+
  #scale_y_discrete('Cluster')+
  scale_x_discrete(expand = c(0,0))+
  labs(fill = 'Normalized \nMean Value', y = 'Cluster')+
  scale_fill_gradientn(colours = pal_col) +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 16),
        axis.text.x=element_text(angle=45, hjust=1, size = 18))


png('cluster_summary_plot_reorder.png')
print(p)
dev.off()


# NMDS

# calculate euclidean distance matrix
cl_df_dist <- vegdist(cl_df, method = 'euclidean')

# save as csv
cldf_distmat <- 
  as.matrix(cl_df_dist, labels = T)
write.csv(cldf_distmat, "cldf_distmat.csv")


cldf_NMS_k4 <-
  metaMDS(cldf_distmat,
          distance = "euclidean",
          k = 4,
          maxit = 999, 
          trymax = 500,
          wascores = TRUE)

cldf_NMS_k3 <-
  metaMDS(cldf_distmat,
          distance = "euclidean",
          k = 3,
          maxit = 999, 
          trymax = 500,
          wascores = TRUE)

cldf_NMS_k2 <-
  metaMDS(cldf_distmat,
          distance = "euclidean",
          k = 2,
          maxit = 999, 
          trymax = 500,
          wascores = TRUE)
# Shepards test/goodness of fit
goodness(cldf_NMS_k4) # Produces a results of test statistics for goodness of fit for each point

stressplot(cldf_NMS_k4) # Produces a Shepards diagram

# Shepards test/goodness of fit
goodness(cldf_NMS_k3) # Produces a results of test statistics for goodness of fit for each point

stressplot(cldf_NMS_k3) # Produces a Shepards diagram

# Shepards test/goodness of fit
goodness(cldf_NMS_k2) # Produces a results of test statistics for goodness of fit for each point

png('NMDS_k2.png', width = 500, height = 500)
stressplot(cldf_NMS_k2) # Produces a Shepards diagram
dev.off()

# Plotting points in ordination space
png('NMDS_k2_ord.png', width = 750, height = 750)
plot(cldf_NMS_k2, "sites")   # Produces distance 
orditorp(cldf_NMS_k2, "sites", cex = 2)   # Gives points labels
dev.off()

# pearson correlation
cldf_cor <-
  cor(cl_df,
      cldf_NMS_k2$points,
      use = "complete.obs",
      method = "pearson")
write.csv(cldf_cor, file = "cldf_cor_PearsonCor.csv")


# summarize some data for presentation

range(df$PSAL_01)
range(df$DOXY_01)

sts <- unique(df$station)
f_max <- list()
for (i in 1:length(sts)){
  dd <- df[df$station == sts[[i]],]
  
  f_max[[i]] <- dd$PRES_01[dd$FLOR_01 == max(dd$FLOR_01)]
}

f_max <- unlist(f_max)

range(f_max)


df_cl_sum <- df_tot_cl %>%
  dplyr::group_by(cluster) %>%
  dplyr::summarize(., min_temp = min(TE90_01),
                   max_temp = max(TE90_01),
                   mean_temp = mean(TE90_01),
                   min_oxy = min(DOXY_01),
                   max_oxy = max(DOXY_01),
                   mean_oxy = mean(DOXY_01),
                   min_sal = min(PSAL_01),
                   max_sal = max(PSAL_01),
                   mean_sal = mean(PSAL_01)
                   )


# plot sections

sections <- list(d = c(2, 3, 4, 5, 6, 7),
                 f = c(22, 23, 24, 25), 
                 k = c(18, 17, 4, 20, 21)
                 )


for(i in 1:length(sections)){
sec_data <- create_section(dat = df_full, output =  'oce', stations = sections[[i]])

# plot(sec_b_data, showBottom = TRUE, showStations = TRUE)

df_sec <- create_section(dat = df_full, stations = sections[[i]], output = 'df')

df_sec <- df_sec %>%
  dplyr::mutate(., station = as.factor(station)) 

df_sec <- left_join(df_sec, coords, by = 'station')


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
}
