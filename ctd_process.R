####SGSL HYDRO PROJECT####


#E. Chisholm
#November 8, 2019


#Read in CTD data

library(oce)
library(dplyr)
library(ggplot2)


source('functions.R')
#CTD files processed from .hex files with configuration file through SeaBird software
#read in as .cnv files

#ctd file basepath

##COR2019002 - Process#####
bp <- '~/sGSL_hydro/COR2019002_data/'


##IML2018051
#bp <- '~/sGSL_hydro/IML2018051_data/'

#read all cnv files
ctd_files <- list.files(bp, pattern = '*.cnv$')

#pull only downcasts

ctd_dn <- grep(ctd_files, pattern = '^d', value = TRUE)

ctd_up <- grep(ctd_files, pattern = '^u', value = TRUE)



ctd <- lapply(paste0(bp,ctd_dn), read.oce)
ctd <- lapply(ctd, oceSetMetadata,  'Cast', 'Down')
#results in warnings about IPTS-68 scale being converted to ITS-90


#remove casts with errors
ctd <- ctd[-20]
ctd <- ctd[-34]
ctd <- ctd[-40]
ctd <- ctd[-60]

#sub upcast for W5 - W32 (CTD event 1-7)
#where downcast is erroneous
#find files for W5 - W32
ctd_sub <- ctd_up[c(1:8)]

for (i in 1:8){
  ctd[[i]] <- oce::read.oce(paste0(bp, ctd_sub[[i]]))
  ctd[[i]] <- oce::oceSetMetadata(ctd[[i]], 'Cast', 'Up')
}

ctd_cor <- ctd
####IML2018051 - Process####


##IML2018051
bp <- '~/sGSL_hydro/IML2018051_data/'

#read all cnv files
ctd_files <- list.files(bp, pattern = '*.cnv$')



ctd <- lapply(paste0(bp,ctd_files), read.oce)
ctd <- lapply(ctd, oceSetMetadata,  'Cast', 'Down')

ctd_hud <- ctd


ctd <- c(ctd_hud, ctd_cor)

remove(bp, ctd_dn, ctd_files, ctd_sub, ctd_up, i)
###combined ctd casts from both cruises###

save(ctd, file = 'combined_ctd_data.RData')
