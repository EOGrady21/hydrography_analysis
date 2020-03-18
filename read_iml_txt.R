##read in IML text format files

##E. Chisholm
##December 20th 2019

library(tidyverse)

##EXAMPLE
# #find text files
# txt_fn <- list.files('IML2018051_txt',full.names = TRUE )
# 
# #first file is description of each variable with units
# var_fn <- txt_fn[1]
# 
# txt_fn <- txt_fn[-1]
# 
# oce_dat <- list()
# for (i in 1:length(txt_fn)){
#   oce_dat[[i]] <- read_iml_txt(file = txt_fn[i])
# }


#function
read_iml_txt <- function(file){
  require(readr)
  require(oce)
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
  
  #remove flags from original data
  
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