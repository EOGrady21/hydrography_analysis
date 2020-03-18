####functions for sGSL hydro processing####

#E. Chisholm
#November 8th, 2019

#Processing

clined <- function(x, depth=NULL, n.smooth=0, k=2) {
  
  #' Detect the position of a cline
  #'
  #' The cline of a variable is the depth at which it varies most sharply along the cast; here it is computed as the maximum of the moving standard deviation over the (possibly smoothed) cast.
  #'
  #' @inheritParams smooth
  #' @param depth vector of depths at which `x` is measured; optional.
  #' @param n.smooth integer, number of times to smooth the data before computing the moving standard deviation.
  #'
  #' @return When `depth` is `NULL`, return the index of `x` corresponding to the cline, `i`. When `depth` is provided, return `depth[i]`, the value of the depth of the cline.
  #' @export
  #'
  #' @family functions computing remarkable depths
  #' @seealso [slide()] for the underlying implementation of the moving standard deviation, [smooth()] for smoothing.
  #'
  #' @examples
  #' plot(-depth ~ temp, data=d, type="l")
  #' thermocline <- clined(d$temp, d$depth)
  #' abline(h=-thermocline, col="red")
  #'
  #' plot(-depth ~ sal, data=d, type="l")
  #' halocline <- clined(d$sal, d$depth)
  #' abline(h=-halocline, col="red")
  
  #' @author Jean-Olivier Irisson (https://github.com/jiho/castr/blob/master/R/clined.R)
  
  # smooth the profile (if requested)
  x <- smooth(x, k=k, n=n.smooth)
  
  # compute the standard deviation
  s <- slide(x, k=k, stats::sd, na.rm=TRUE)
  # get its maximum
  i <- which.max(s)
  
  # if the depth is provided, extract the corresponding depth
  i <- get_depth(i, depth)
  
  return(i)
}



get_depth <- function(i, depth) {
  
  #' Get the depth at index i
  #'
  #' Simply extracts element `i` of `depth` but includes failsafes for missing depth or missing `i`. Used by other functions.
  #'
  #' @param i element of `depth` to extract.
  #' @inheritParams check_input
  #'
  #' @return If both `i` and `depth` are provided, return `depth[i]`. If `i` is NULL/empty, return NA. If `depth` is NULL return `i`.
  #'
  #' @examples
  #' castr:::get_depth(2, c(34, 35, 36))
  #' castr:::get_depth(2, NULL)
  #' castr:::get_depth(NULL, c(34, 35, 36))
  #' castr:::get_depth(c(), c(34, 35, 36))
  #' 
  #' @author Jean-Olivier Irisson (https://github.com/jiho/castr/blob/master/R/get_depth.R)
  
  if (length(i) > 0) {
    if (!is.null(depth)) {
      i <- depth[i]
    }
  } else {
    i <- NA
  }
  return(i)
}


slide <- function(x, k, fun, n=1, ...) {
  
  #' Apply a function in a sliding window along a vector
  #'
  #' Allows to compute a moving average, moving median, or even moving standard deviation, etc. in a generic way.
  #'
  #' @param x input numeric vector.
  #' @param k order of the window; the window size is 2k+1.
  #' @param fun function to apply in the moving window.
  #' @param n number of times to pass the function over the data.
  #' @param ... arguments passed to `fun`. A usual one is `na.rm=TRUE` to avoid getting `NA`s at the extremities of `x`.
  #'
  #' @details A window of size `2k+1` is centred on element `i` of `x`. All elements from index `i-k` to index `i+k` are sent to function `fun`. The returned value is associated with index `i` in the result. The window is moved to element `i+1` and so on.
  #'
  #' For such sliding window computation to make sense, the data must be recorded on a regular coordinate (i.e. at regular intervals). Otherwise, data points that are far from each other may end up in the same window.
  #'
  #' The extremeties of the input vector are padded with `NA` to be able to center the sliding window from the first to the last elements. This means that, to avoid getting `k` missing values at the beginning and at the end of the result, `na.rm=TRUE` should be passed to `fun`.
  #'
  #' @return The data passed through `fun`, `n` times.
  #' @export
  #'
  #' @seealso [cweights()] to compute weights centered in the middle of the window.
  #'
  #' @examples
  #' # create some data and add random noise
  #' xs <- sin(seq(0, 4*pi, length=100))
  #' x <- xs + rnorm(length(xs), sd=0.25)
  #' plot(x)
  #' lines(xs)
  #' # filter the data in various ways
  #' # moving average
  #' mav   <- slide(x, 3, mean, na.rm=TRUE)
  #' # running moving average
  #' rmav  <- slide(x, 3, mean, na.rm=TRUE, n=4)
  #' # weighted moving average
  #' wmav  <- slide(x, 3, weighted.mean, na.rm=TRUE, w=cweights(3))
  #' # weighted running moving average
  #' wrmav <- slide(x, 3, weighted.mean, na.rm=TRUE, w=cweights(3), n=4)
  #' # moving median
  #' mmed  <- slide(x, 3, median, na.rm=TRUE)
  #' lines(mav, col="red")
  #' lines(rmav, col="red", lty="dashed")
  #' lines(wmav, col="orange")
  #' lines(wrmav, col="orange", lty="dashed")
  #' lines(mmed, col="blue")
  #' # inspect variability around filtered data
  #' plot(slide(x-rmav, 7, sd))
  #' plot(slide(x-mmed, 7, mad))
  
  #' @author Jean-Olivier Irisson (https://github.com/jiho/castr/blob/master/R/slide.R)
  
  # make sure to get a function as the `fun` argument (i.e. avoid name masking)
  if (!is.function(fun)) {
    fun <- get(as.character(substitute(fun)), mode="function")
  }
  
  if (n>=1) {
    # repeat n times
    for (t in 1:n) {
      # pad the extremities of data to be able to compute over the whole vector
      x <- c(rep(NA, times=k), x, rep(NA, times=k))
      
      # apply the rolling function (and remove padding at the extremities)
      x <- sapply((k+1):(length(x)-k), function(i) {
        fun(x[(i-k):(i+k)], ...)
      })
    }
  }
  
  return(x)
}

smooth <- function(x, k=1, n=1) {
  
  #' Smooth a variable
  #'
  #' Smooth a variable in a cast using a weighted moving average.
  #'
  #' @inheritParams slide
  #' @param x vector of the variable of interest. It must have been recorded at approximately regular intervals (see [slide()] as to why).
  #' @param n number of times to smooth the data.
  #'
  #' @return A vector containing the smoothed variable, containing as many elements as the original variable.
  #' @export
  #'
  #' @seealso [slide()] for the underlying implementation.
  #'
  #' @examples
  #' plot(-depth ~ chla, data=d)
  #' lines(-depth ~ smooth(chla), data=d, type="l", col="red")
  #' lines(-depth ~ smooth(chla, k=2, n=5), data=d, type="l", col="blue")
  #' legend(1, 0, legend=c("k=1, n=1", "k=2, n=5"), col=c("red", "blue"), lty="solid")
  
  # compute centered weights
  w <- c(1:k,k+1,k:1)
  w <- w / sum(w)
  # compute the (running) weighted moving average
  slide(x, k=k, stats::weighted.mean, na.rm=TRUE, w=w, n=n)
}

bottom_depth <- function(data, station, bathy_data_list ){
  #' @param data a dataframe with latitude and longitude and station 
  #' @param station a list of stations of interest contained within data
  #' @param bathy_data_list a bathymetry dataframe loaded from \code{\link{load_bathy}}
  #'  
  #'   @return Produces named list (by station) of bathymetry depth at given location
  #'    
  
  #  get bottom depth of each station from noaa bathymetry
  # finds closest point existing in bathymetry matrix
  # should be improved to interpolate bathymetry to a specific point
  res <- list()
  for ( i in 1:length(station)){
    st <- station[[i]]
    
  st_df <- data %>%
    dplyr::filter(., station == st)
  
  # get bottom topo
  
  # find closest lat
 # browser()
  lat_in <- which(abs(bathy_data_list[[1]]$y - unique(st_df$latitude)) == min(abs(bathy_data_list[[1]]$y - unique(st_df$latitude))))
  
  lat <- unique(bathy_data_list[[1]]$y[lat_in])
  
  # find closest lon
  
  lon <-
    unique(bathy_data_list[[1]]$x[bathy_data_list[[1]]$y == lat][
                                    which(abs(bathy_data_list[[1]]$x - unique(st_df$longitude)) == min(abs(
                                      bathy_data_list[[1]]$x - unique(st_df$longitude)
                                    )))])
  
  lat <- na.omit(lat)
  lon <- na.omit(lon)
  
  
  res[[i]] <- bathy_data_list[[1]]$z[bathy_data_list[[1]]$x == lon & bathy_data_list[[1]]$y == lat]
  
  }
  names(res) <- station
  
  return(res)
}



plot_section <- function(data, var, bw, dup, method, bathy_data_list ){
  #' @param data dataframe from create section (output = 'df')
  #' @param var variable to be interpolated over section
  #' @param bw bin width over which to interpolate
  #' @param dup if method is 'akima' or 'interp', passed to interpolate function to handle duplicate values
  #' @param method interpolation method 'akima', 'interp' or 'oce'
  
  diff_lat <- max(data$latitude) - min(data$latitude)
  diff_lon <- max(data$longitude) - min(data$longitude)
  
  if(diff_lon > diff_lat){
    coord_var <- 'longitude'
    coord_b <- 'x'
    op_var <- 'latitude'
  }else{
    coord_var <- 'latitude'
    coord_b <- 'y'
    op_var <- 'longitude'
  }
  
  
  
  if(method == 'akima'){
    interpdf <- akima::interp(x = data[[coord_var]], y = data$pressure, z = data[[var]], duplicate = dup ,linear = TRUE  )
  }
  if(method == 'interp'){
    interpdf <- interp::interp(x = data[[coord_var]], y = data$pressure, z = data[[var]], duplicate = dup ,linear = TRUE  )
  }
  if(method == 'oce'){
    interpdf_oce <- oce::interpBarnes(x = data[[coord_var]], y = data$pressure, z = data[[var]] )
    interpdf <- NULL
    interpdf$x <- interpdf_oce$xg
    interpdf$y <- interpdf_oce$yg
    interpdf$z <- interpdf_oce$zg
  }
  
  #convert to dataframe
  df <- akima::interp2xyz(interpdf, data.frame = TRUE)
  
  # get unique station labels
  lab_df <- data %>%
    dplyr::select(., station, latitude, longitude) %>%
    dplyr::distinct(., station, latitude, longitude)
  
  # get bottom topo
  coord_df <- right_join(df, bathy_data_list[[1]], by = c(x = coord_b))
  
  
  # create path between stations
  
  #multipoint <- sf::st_multipoint(x = matrix(c(lab_df$longitude, lab_df$latitude), ncol = 2))
  #linestring = sf::st_cast(multipoint, "LINESTRING")
  
  # get bathymetry along path for bottom topo
  
  
  # calc distance between points with geosphere::distGeo, use distance on axis instead of lat/lon
  # geosphere::distGeo(p1 = c(lab_df$longitude[1], lab_df$latitude[1]), p2 = c(lab_df$longitude[2], lab_df$latitude[2]))
  
  
  #if(max(lab_df[[op_var]]) - min(lab_df[[op_var]]) <0.1){
  #  coord_df_line <- coord_df
  #}else{
  if (coord_var == 'longitude'){
  coord_df_line <- coord_df %>%
    dplyr::filter(., y.y < max(lab_df[[op_var]]) & y.y > min(lab_df[[op_var]] ))
  }else{
    coord_df_line <- coord_df %>%
      dplyr::filter(., x.y < max(lab_df[[op_var]]) & x.y > min(lab_df[[op_var]] ))
  }
  #}
  
  
    bt <- 0.05
  
  coord_var_bins <- seq(from = min(coord_df_line$x), to = max(coord_df_line$x), by = bt)

  z_bot <- list()
  for( i in 1:length(coord_var_bins)){
  z_bot[[i]] <-  mean(coord_df_line$z.y[coord_df_line$x < coord_var_bins[[i]]], na.rm = TRUE)
  }
  
  c_df <- data.frame(x = coord_var_bins, z = unlist(z_bot))
  
  # find points below bottom
  
  for (i in 1:length(df$x)){
    df$z_bot[[i]] <- abs(min(c_df$z[c_df$x == c_df$x[which(abs(c_df$x-df$x[[i]])==min(abs(c_df$x-df$x[[i]])))]], na.rm = TRUE))
  }
  
  dd <- df %>%
    dplyr::mutate(., bot = ifelse(df$y > df$z_bot, 1, 0))
  
  # get cmocean pallette
  if (var == 'temperature'){
  pal <- cmocean::cmocean('thermal')
  }
  if(var == 'salinity'){
    pal <- cmocean::cmocean('haline')
  }
  
  cmo <- pal(n = 100)
  # plot
  # 
  # p <- ggplot(dd[dd$bot == 0,]) +
  #   geom_tile(aes(x = x, y = y, fill = z)) +
  #   labs(fill = var) +
  #   scale_y_reverse(name = 'Pressure [db]', limits = c( max(dd$y[dd$bot == 0]), 0), expand = c(0,0)) +
  #   scale_x_continuous(name = coord_var, limits = c(min(df$x), max(df$x)), expand = c(0,0)) +
  #   theme_classic() +
  #   geom_contour(aes(x = x, y = y, z= z), col = 'black') +
  #   geom_text_contour(aes(x = x, y = y, z= z),binwidth = bw, col = 'white', check_overlap = TRUE, size = 8)+ #CONTOUR LABELS
  #   scale_fill_gradientn(colours = cmo, na.value = 'white') +
  #   geom_vline(xintercept = lab_df[[coord_var]]) +
  #   ggrepel::geom_label_repel(data = lab_df, aes(x = lab_df[[coord_var]], y = 0, label = lab_df$station), fill = 'gray70', size = 7 ) 
  # 
  
  # attempt to plot with more accurate bathy
  if (var == 'temperature'){
    leg_lab <- expression(paste('Temperature [',degree,' C]', sep = ''))
  }
  if (var == 'salinity'){
    leg_lab <- 'Salinity \n[ PSU ]'
  }
  
  p <- ggplot(df) +
    geom_tile(aes(x = x, y = y, fill = z)) +
    labs(fill = leg_lab) +
    scale_y_reverse(name = 'Pressure [db]',  expand = c(0,0)) + # limits = c( max(dd$y[dd$bot == 0]) + 350, 0),
    scale_x_continuous(name = coord_var, limits = c(min(df$x), max(df$x)), expand = c(0,0)) +
    theme_classic() +
    geom_contour(aes(x = x, y = y, z= z), col = 'black') +
    geom_text_contour(aes(x = x, y = y, z= z),binwidth = bw, col = 'white', check_overlap = TRUE, size = 8)+ #CONTOUR LABELS
    scale_fill_gradientn(colours = cmo, na.value = 'white') +
    geom_vline(xintercept = lab_df[[coord_var]]) +
    ggrepel::geom_label_repel(data = lab_df, aes(x = lab_df[[coord_var]], y = 0, label = lab_df$station), fill = 'gray70', size = 7 )+
    geom_ribbon(data = coord_df_line, aes(x = x, ymax = max(abs(z.y), na.rm = TRUE), ymin = abs(z.y)))+
    theme(legend.text = element_text(size = 15),
          legend.key.size = unit(2, 'lines'),
          legend.title = element_text(size = 16),
          axis.text = element_text(size = 15), 
          axis.title = element_text(size = 16))
  #geom_path(data = coord_df_line, aes(x = x, y = abs(z.y)), size = 10)+
  
  return(p)
}
# create sections

create_section <- function(dat, stations, output = 'df'){
  # output can be 'df or 'oce'
  # uses IML naming scheme
  
data <- as.section(salinity = dat$PSAL_01[dat$station %in% stations], 
                         temperature = dat$TE90_01[dat$station %in% stations],
                         pressure = dat$PRES_01[dat$station %in% stations], 
                         longitude = dat$longitude[dat$station %in% stations],
                         latitude = dat$latitude[dat$station %in% stations], 
                         station = dat$station[dat$station %in% stations])
if( output == 'df'){

ctd_df <- do.call(rbind, data@data)

df_l <- list()
for (i in 1:length(ctd_df)){
  df_l[[i]] <- as.data.frame(ctd_df[[i]]@data)
}
names(df_l) <- stations

df <- bind_rows(df_l, .id = 'station')



return(df)

}else{
  return(data)
}



}

# load bathy data

load_bathy <- function(bathy_fn, bathy_noaa_fn){
  
  #' bathy_fn  "bathydata.RDS"
  #' bathy_noaa_fn "bathydata_noaa.RDS"
  #' 
  #' 

  
  #lat and lon min and max 
  y_min_gsl <- 47.0
  y_max_gsl <- 49
  x_min_gsl <- -66
  x_max_gsl <- -62
  
  
###BATHYMETRY####
##Load bathymetry
#CHS15
b <- readRDS(bathy_fn)
bathy <- marmap::as.bathy(b)
bathy.f_chs <- marmap::fortify.bathy(bathy)
bathy.f_chs$z <- bathy.f_chs$z*-1

#NOAA
bathy_noaa <- readRDS(bathy_noaa_fn)
bathy.f_noaa <- marmap::fortify.bathy(bathy_noaa)

###TIDAL ELLIPSE####
#Get tidal ellipse
#tide_ellipse <- readRDS("cdesp_te.RDS")


bathy.f_noaa <- subset(bathy.f_noaa, y > y_min_gsl & y < y_max_gsl & x > x_min_gsl & x < x_max_gsl) #SWGSL bathymetry extent
bathy.f_chs <- subset(bathy.f_chs, y > y_min_gsl & y < y_max_gsl & x > x_min_gsl & x < x_max_gsl)

lons_gsl <- c(x_min_gsl, x_max_gsl)
lats_gsl <- c(y_min_gsl, y_max_gsl)

lons_list <- list(lons_gsl)
lats_list <- list(lats_gsl)

list(bathy.f_chs)


}





# reorder for anomoly heat map
# idea from: http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization

reorder_mat <- function(dat, var){
  
  dat %>%
    dplyr::arrange(.data[[var]])
  
  dat$station <- factor(dat$station, levels=(dat$station)[order(dat[[var]])])
  
  return(dat)
  
}


# normalize 
# from : https://stackoverflow.com/questions/26950045/group-by-and-scale-normalize-a-column-in-r

normalit<-function(m){
  (m - mean(m, na.rm = TRUE))/sd(m, na.rm = TRUE)
}



#read in IML text files
read_iml_txt <- function(file){
  #require(readr)
  #require(oce)
  ###STEP 1: READ IN DATA IN PARTS####
  
  
  #read in data without meta
  dat <- read_table(file, skip = 11)
  
  #split out units
  units <- dat[1,]
  
  #fix data so it is numeric
  
  #remove unit line
  dat <- dat[-1,]
  
  #change each var to numeric
  for(i in 1:length(dat)){
    dat[i] <- as.numeric(dat[[i]])
  }
  
  #read in metadata
  txt_ln <- readLines(file)
  #assumes same number of metadata lines in each file
  meta <- txt_ln[1:11]
  
  name <- list()
  val <- list()
  for (i in 1:length(meta)){
    loc <- gregexpr(meta[i], pattern = ':')
    
    #get name of metadata (before ':')
    name[i] <- substr(meta[i], 1, loc[[1]] - 1)
    
    #get metadata value (after ':')
    val[i] <- substr(meta[i], loc[[1]] +1, nchar(meta[i]))
    
    remove(loc)
  }
  
  #de duplicate metadata names
  
  if( any(duplicated(name))){
    #find which names are duplicated
    dups <- which(duplicated(name))
    
    #append with numeric
    for (i in 1:length(dups)){
      name[dups[i]] <- paste0(name[dups[i]], '_0', i+1)
    }
  }
  
  
  #pull out flags
  
  q_ind <- grep(names(dat), pattern = 'Q')
  
  flags <- dat[q_ind]
  
  
  #find var names which relate to flags (previous variable)
  #does not apply for QCFF_01 (general quality flag) last flag
  q_ind2 <- q_ind[-length(q_ind)] #removes QCFF index
  
  name_ind <- q_ind2 - 1
  flag_vars <- names(dat[name_ind])
  
  # flag_names <- paste0(flag_vars, '_Q') #oce convention suggests flags g=having same names as data objects
  flag_names <- flag_vars
  
  for (i in 1:length(flag_names)){
    names(flags)[[i]] <- flag_names[[i]]
  }
  
  #remove flags from orifginal data
  
  dat <- dat[-q_ind]
  ###STEP 2: COMBINE DATA INTO OCE FORMAT####
  
  #set data into oce object
  oce_dat <- as.oce(dat, note = NULL)
  
  #set metadata
  for (i in 1:length(name)){
    oce_dat <- oceSetMetadata(oce_dat, name = name[[i]], value = val[[i]], note = NULL)
  }
  
  #add flags
  oce_dat[['flags']] <- flags
  
  
  return(oce_dat)
  
} #end of function



#Plotting


interpPlot <- function(data, variable){
  require(oce)
  require(ocedata)
  data("coastlineWorldFine")
  clwf <- as.data.frame(coastlineWorldFine@data)
  
  pal <- cmocean::cmocean('balance')
  ppal <- pal(n = 100)
  eval(parse(text = paste0('dat <- data %>% dplyr::filter(., !is.na(',variable,'))')))
  
  var_interp <- interp::interp(x = dat$longitude, y = dat$latitude, z = dat[[variable]], duplicate = 'mean')
  var_int_xyz <- akima::interp2xyz(var_interp, data.frame = TRUE)
  
  p <- ggplot(var_int_xyz) +
    geom_tile(aes(x = x, y = y, fill = z)) +
    geom_point(data = dat, aes(x = longitude, y = latitude), alpha = 0.5, shape = 7, size = 6)+
    theme_classic() +
    scale_x_continuous(name = 'Longitude', limits = c(-67, -57)) +
    scale_y_continuous(name = 'Latitude', limits = c(43, 50) ) +
    geom_polygon(data = clwf, aes(x = longitude, y = latitude ),  fill = 'darkolivegreen')+
    labs(fill = variable) +
    scale_fill_gradientn(colors = ppal, na.value = 'white') +
    coord_map(xlim = c(-66, -60), ylim = c(47, 49))
  
  return(p)
}

getBaseMap <- function(extent, bathy, bathy_levels, projection) {
  
  library(tidyverse)
  library(sf)
  library(rnaturalearth)
  library(rnaturalearthhires)
  library(mapdata)
  library(marmap)
  library(isoband)
  
  #' @author K. Sorochan
  #' @details produces a ggplot object of a basemap with bathymetry contours using rnaturalearth sf and isoband packages
  #' 
  #' @param extent a named vector with coordinates of mapping extent. The names must be 'xmin', 'xmax', 'ymin', 'ymax'
  #' @param bathy an object specifying bathymetry. It must be the same type of object as a "fortified" noaa bathy
  #' @param bathy_levels a vector of contours for isobaths here. They must be negative for contours below sealevel
  #' @param projection an r projection for the map
  #' 
  #' @export
  
  #Get bathymetry
  bathy1 <- bathy %>%
    dplyr::filter(., y > extent[1] & y < extent[2] & x > extent[3] & x < extent[4])
  
  bathy_mat <- data.matrix(spread(bathy1, x, z))
  
  bathy_isolines <- isolines(as.numeric(colnames(bathy_mat)), 
                             bathy_mat[,1], 
                             bathy_mat, 
                             levels = c(bathy_levels))
  
  bathy_lines <- iso_to_sfg(bathy_isolines)
  
  bathy_line_data <- st_sf(
    level = 2:(length(bathy_lines)+1),
    geometry = st_sfc(bathy_lines), crs = proj_wgs1984)
  
  #Get sf objects
  canada <- ne_countries(country = "canada", scale = "large", returnclass = "sf")
  
  can_sub <- canada %>%
    st_as_sf(., coords = c("long","lat"), crs=proj_wgs1984) %>%
    st_crop(., extent)
  
  #Plot
  #Get plots of all observations over cruise domain
  
  gg_map <- ggplot() + 
    
    #Get polygons
    geom_sf(data = can_sub) +
    coord_sf(expand=FALSE) +
    
    #Get bathymetry
    geom_sf(data = bathy_line_data) +
    
    theme_bw()
  
  return(gg_map)
  
}



#' Plots CTD profiles using IML QC'd data format standards
#'
#'
#' 
#' Facet wrap is used to create distinct panels for each station provided
#'
#' @param dat a datafram containing QC'd CTD data, named using IML conventions
#' 
#' 
#' 
#' @return A gridded object of at least 3 ggplot objects
#' @export

profile_plot <- function(dat){
  # plot temp
  p <- ggplot(dat) +
    geom_point(aes(x = TE90_01, y = PRES_01), col = 'red') +
    scale_y_reverse(name = 'Pressure [db]')
  # plot salinity
  p_TS <- p + geom_point(aes(x = (PSAL_01 -25), y = PRES_01), col = 'blue') +
    scale_x_continuous(name = expression(paste("Temperature [",degree,"C]")),sec.axis = sec_axis(~ . +25, name = 'Salinity [PSU]')) +
    theme(axis.line.x.bottom = element_line(colour = 'red'),
          axis.ticks.x.bottom = element_line(colour = 'red'),
          panel.background = element_blank(),
          panel.grid = element_blank(),
          axis.line.y = element_line(linetype = 'solid'),
          axis.line.x.top = element_line(colour = 'blue'),
          axis.ticks.x.top = element_line(colour = 'blue'),
          axis.title = element_text(size = 20)
    ) +
    facet_wrap(~station)
  # have to force rescale for multi axes ggplot
  
  # plot fluorescence
  p <- ggplot(dat) +
    geom_point(aes(x = FLOR_01, y = PRES_01), col = 'green') +
    scale_y_reverse(name = 'Pressure [db]')
  
  # plot density
  p_FD <- p + geom_point(aes(x = (SIGT_01  -22), y = PRES_01)) +
    scale_x_continuous(name = 'Fluorescence [mv]',sec.axis = sec_axis(~.   +22, name = 'Density')) +
    theme(axis.line.x.bottom = element_line(colour = 'green'),
          axis.ticks.x.bottom = element_line(colour = 'green'),
          panel.background = element_blank(),
          panel.grid = element_blank(),
          axis.line.y = element_line(linetype = 'solid'),
          axis.line.x.top = element_line(colour = 'black'),
          axis.title = element_text(size = 20)
    ) +
    facet_wrap(~station)
  # manual rescale
  
  

  
  p <- grid.arrange(p_TS, p_FD, widths = c(1, 1), heights = c(2), nrow = 1, ncol = 2)
  # for 4.85
  # grid.arrange(p_TS, p_FD, pp , widths = c(1, 1, 1), heights = c(2), nrow = 1, ncol = 3)
  
  return(p)
}


plotTS_balloon <- function(x, reference.p = 0, var, salinity = 'salinity', temperature = 'temperature'){
  
  #' Make a balloon plot against a TS plot
  #' includes isopycnal line calculations
  #' @author E. Chisholm
  #' 
  #' @param x dataframe with temperature, salinity
  #' @param reference.p reference pressure (default at 0 for surface)- used to calculate ispycnals
  #' @param var variable on which size of points will be based
  #' @example 
  #' p <- plotTS_balloon(x)
  #' p + ggtitle('Concentration by taxa')
  #'
  #'  @export
  #' 
  #' @note modified from source: https://github.com/Davidatlarge/ggTS/blob/master/ggTS_DK.R
  #' 
  #' 
  
  #require(ggplot2)
  #require(gsw)
  #require(metR)
  
  
  #get isopycnal lines
  sal <-  x[[salinity]]
  pot.temp <-  x[[temperature]]
  
  # make TS long table
  TS <- expand.grid(
    sal = seq(floor(min(sal, na.rm = TRUE)), ceiling(max(sal, na.rm = TRUE)), length.out = 100),
    pot.temp = seq(floor(min(pot.temp, na.rm = TRUE)), ceiling(max(pot.temp, na.rm = TRUE)), length.out = 100)
  )
  TS$dens <- gsw_rho_t_exact(SA = TS$sal, t = TS$pot.temp, p = reference.p) - 1000 # the function calculates in-situ density, but because potential temperature and a single reference pressure is used the result equals potential density at reference pressure
  
  
  #plot
  eval(parse(text = paste0("p <- ggplot()+
                           #isopycnal lines
                           geom_contour(data = TS, aes(x = sal, y = pot.temp, z = dens), col = 'grey', linetype = 'solid',
                           breaks = seq(min(round(TS$dens*2)/2, na.rm = TRUE), # taking density times 2, rounding and dividing by 2 rounds it to the neares 0.5
                           max(round(TS$dens*2)/2, na.rm = TRUE), 
                           by = .5)) +
                           geom_text_contour(data = TS, aes(x = sal, y =pot.temp, z= dens),binwidth = 0.5, col = 'grey', nudge_x = 0.1)+ #CONTOUR LABELS
                           #roi data sorted by number of rois and taxa
                           geom_point(data = x, aes(x = ", salinity, ", y = ", temperature, ", size = ", var, "), shape = 21) +
                           #scale_size_area(max_size=10)+ #make balloons bigger
                           #label legends
                           labs(size = 'Dissolved Oxygen') +
                           labs(col = 'Taxa')+
                           #set x axis (ensure scaling to data)
                           scale_x_continuous(name = 'Salinity [PSU]', expand = c(0,0), 
                           limits = c(floor(min(x[[salinity]], na.rm = TRUE)), ceiling(max(x[[salinity]], na.rm = TRUE)))) + # use range of 'sal' for x axis
                           #set y axis (esure scaled to data)
                           scale_y_continuous(name = expression(paste('potential temperature [ ',degree,' C]')), 
                           limits = c(floor(min(x[[temperature]], na.rm = TRUE)), ceiling(max(x[[temperature]], na.rm = TRUE)))) +
                           #get rid of grid lines and text formatting
                           theme_classic() + theme(text = element_text(size=14)) "
  )))
  
  
  return(p)
}



plotTS_EC <- function(x, reference.p = 0,  salinity = 'salinity', temperature = 'temperature', bin = FALSE){
  
  #' Make  a TS plot
  #' includes isopycnal line calculations
  #' @author E. Chisholm
  #' 
  #' @param x dataframe with temperature, salinity
  #' @param reference.p reference pressure (default at 0 for surface)- used to calculate ispycnals
  #' @param salinity name of salinity variable
  #' @param temperature name of temperature variable
  #' 
  #' 
  #' 
  #' @example 
  #'  @export
  #' 
  #' @note modified from source: https://github.com/Davidatlarge/ggTS/blob/master/ggTS_DK.R
  #' 
  #' 
  
  #require(ggplot2)
  #require(gsw)
  #require(metR)
  
  
  #get isopycnal lines
  sal <-  x[[salinity]]
  pot.temp <-  x[[temperature]]
  
  # make TS long table
  TS <- expand.grid(
    sal = seq(floor(min(sal, na.rm = TRUE)), ceiling(max(sal, na.rm = TRUE)), length.out = 100),
    pot.temp = seq(floor(min(pot.temp, na.rm = TRUE)), ceiling(max(pot.temp, na.rm = TRUE)), length.out = 100)
  )
  TS$dens <- gsw_rho_t_exact(SA = TS$sal, t = TS$pot.temp, p = reference.p) - 1000 # the function calculates in-situ density, but because potential temperature and a single reference pressure is used the result equals potential density at reference pressure
  
  
  #plot
  if(bin == FALSE){
  eval(parse(text = paste0(" p <- ggplot()+
                           #isopycnal lines
                           geom_contour(data = TS, aes(x = sal, y = pot.temp, z = dens), col = 'grey', linetype = 'solid',
                           breaks = seq(min(round(TS$dens*2)/2, na.rm = TRUE), # taking density times 2, rounding and dividing by 2 rounds it to the neares 0.5
                           max(round(TS$dens*2)/2, na.rm = TRUE), 
                           by = .5)) +
                           geom_text_contour(data = TS, aes(x = sal, y =pot.temp, z= dens),binwidth = 0.5, col = 'grey', nudge_x = 0.1)+ #CONTOUR LABELS
                           #roi data sorted by number of rois and taxa
                           geom_point(data = x, aes(x = ",salinity,", y =", temperature,"), shape = 21) +
                           #scale_size_area(max_size=10)+ #make balloons bigger
                           #label legends
                           #labs(size = 'Dissolved Oxygen') +
                           labs(col = 'Taxa')+
                           #set x axis (ensure scaling to data)
                           scale_x_continuous(name = 'Salinity [PSU]', expand = c(0,0), 
                           limits = c(floor(min(x[[salinity]], na.rm = TRUE)), ceiling(max(x[[salinity]], na.rm = TRUE)))) + # use range of 'sal' for x axis
                           #set y axis (esure scaled to data)
                           scale_y_continuous(name = expression(paste('potential temperature [ ',degree,' C]')), 
                           limits = c(floor(min(x[[temperature]], na.rm = TRUE)), ceiling(max(x[[temperature]], na.rm = TRUE)))) +
                           #get rid of grid lines and text formatting
                           theme_classic() + theme(text = element_text(size=14)) "
  )))
  
  }else{
    #plot binned data
    #not tested yet
    eval(parse(text = paste0(" p <- ggplot()+
                           #isopycnal lines
                             geom_contour(data = TS, aes(x = sal, y = pot.temp, z = dens), col = 'grey', linetype = 'solid',
                             breaks = seq(min(round(TS$dens*2)/2, na.rm = TRUE), # taking density times 2, rounding and dividing by 2 rounds it to the neares 0.5
                             max(round(TS$dens*2)/2, na.rm = TRUE), 
                             by = .5)) +
                             geom_text_contour(data = TS, aes(x = sal, y =pot.temp, z= dens),binwidth = 0.5, col = 'grey', nudge_x = 0.1)+ #CONTOUR LABELS
                             #roi data sorted by number of rois and taxa
                             geom_point(data = x, aes(x = ",salinity,", y =", temperature,", col = as.factor(bin)), shape = 21) +
                             #scale_size_area(max_size=10)+ #make balloons bigger
                             #label legends
                             #labs(size = 'Dissolved Oxygen') +
                             labs(col = 'Taxa')+
                             #set x axis (ensure scaling to data)
                             scale_x_continuous(name = 'Salinity [PSU]', expand = c(0,0), 
                             limits = c(floor(min(x[[salinity]], na.rm = TRUE)), ceiling(max(x[[salinity]], na.rm = TRUE)))) + # use range of 'sal' for x axis
                             #set y axis (esure scaled to data)
                             scale_y_continuous(name = expression(paste('potential temperature [ ',degree,' C]')), 
                             limits = c(floor(min(x[[temperature]], na.rm = TRUE)), ceiling(max(x[[temperature]], na.rm = TRUE)))) +
                             #get rid of grid lines and text formatting
                             theme_classic() + theme(text = element_text(size=14)) "
    )))
    
  }
  
  return(p)
}