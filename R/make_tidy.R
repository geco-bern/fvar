# script to prepare dataset for fET

# rm(list = ls())

# install packages
library(bigleaf)
library(tidyverse)
library(lubridate)
library(visdat)

# load function not included in fvar
source('~/Desktop/Coding/fvar/R/mct.R')

LBr <- FALSE; # TRUE = Le Bray, FALSE = Puechabon

if (LBr) {
  # LE BRAY
  
  # load csv data
  raw_data <- read.csv("./new_data/Euler/FLX_FR-LBr_FLUXNET2015_FULLSET_DD_1996-2008_1-3.csv")
  raw_vpd <- read.csv("./new_data/Euler/FLX_FR-LBr_FLUXNET2015_FULLSET_HH_1996-2008_1-3_VPD_DAY.csv")
  
  # extract VPD first
  dd_vpd <- raw_vpd %>%
    dplyr::select(date,VPD_F_DAY, VPD_F_DAY_QC) %>%
    mutate(date = lubridate::ymd(date))
  
  ddf <- raw_data %>% 
    
    # select only relevant data
    dplyr::select(
      TIMESTAMP, 
      TA_F_DAY, #temperature
      TA_F_DAY_QC,
      SWC_F_MDS_1, #soil moisture
      SWC_F_MDS_1_QC,
      SWC_F_MDS_2, #soil moisture
      SWC_F_MDS_2_QC,
      SWC_F_MDS_3, #soil moisture
      SWC_F_MDS_3_QC,
      SWC_F_MDS_4, #soil moisture
      SWC_F_MDS_4_QC,
      SWC_F_MDS_5, #soil moisture
      SWC_F_MDS_5_QC,
      SWC_F_MDS_6, #soil moisture
      SWC_F_MDS_6_QC,
      NETRAD, #Net radiation
      NETRAD_QC,
      WS_F, #windspeed
      WS_F_QC, 
      LE_F_MDS, #latent heat
      LE_F_MDS_QC,
      P_F,
      P_F_QC #Precipitation
      # consider also: longwave incoming radiation; 
    ) %>%
    
    # time conversion 
    mutate(TIMESTAMP = lubridate::ymd(TIMESTAMP)) %>%
    
    rename(date = TIMESTAMP) %>% 
    
    # add VPD
    left_join(dd_vpd, by = c("date" = "date")) %>%
    
    # correctly represent missing data
    na_if(-9999) %>%
    
    # filter data based on quality check 
    mutate(
      TA_F_DAY = ifelse(TA_F_DAY_QC > 0.8, TA_F_DAY, NA),
      NETRAD = ifelse(NETRAD_QC > 0.8, NETRAD, NA),
      WS_F = ifelse(WS_F_QC > 0.8, WS_F, NA),
      VPD_F_DAY = ifelse(VPD_F_DAY_QC > 0.8, VPD_F_DAY, NA),
      SWC_F_MDS_1 = ifelse(SWC_F_MDS_1_QC > 0.8, SWC_F_MDS_1, NA),
      SWC_F_MDS_2 = ifelse(SWC_F_MDS_2_QC > 0.8, SWC_F_MDS_2, NA),
      SWC_F_MDS_3 = ifelse(SWC_F_MDS_3_QC > 0.8, SWC_F_MDS_3, NA),
      SWC_F_MDS_4 = ifelse(SWC_F_MDS_4_QC > 0.8, SWC_F_MDS_4, NA),
      SWC_F_MDS_5 = ifelse(SWC_F_MDS_5_QC > 0.8, SWC_F_MDS_5, NA),
      SWC_F_MDS_6 = ifelse(SWC_F_MDS_6_QC > 0.8, SWC_F_MDS_6, NA),
      LE_F_MDS = ifelse(LE_F_MDS_QC > 0.8, LE_F_MDS, NA),
      P_F = ifelse(P_F_QC > 0.8, P_F, NA)
    ) %>%
    
    # erase QC variables 
    #dplyr::select(-ends_with("_QC"), NEE_VUT_REF_NIGHT_QC, NEE_VUT_REF_DAY_QC) %>%
    dplyr::select(-ends_with("_QC")) %>%
    
    # calculate ET
    mutate(ET = LE.to.ET(LE_F_MDS, TA_F_DAY)) %>% #get ET with bigleaf function (mm/s)
    mutate(ET = ET*60*60*24) #ET units conversion (mm/day)
  # mutate(ET_rbeni = convert_et(LE_F_MDS, TA_F_DAY, 61)) #with rbeni
  
  # filter rainy days+2; threshold: 1mm
  for (i in 1:nrow(ddf)) {
    precip_day <- ddf$P_F[i]
    if (is.na(precip_day)) {
      next
    } else if(precip_day > 1){
      ddf$P_F[i] <- NA
      ddf$P_F[i+1] <- NA
      ddf$P_F[i+2] <- NA
    }
  }
  
  # See missing data (max 25% should be missing)
  ddf %>%
    summarise_all(funs(100*sum(is.na(.))/length(.))) %>%
    t()
  
  # only take years with soil moisture
  ddf_try <- ddf %>% 
    filter(year(date) %in% c(2005,2006,2007,2008))
  
  # save as RData
  save(ddf, file = "./new_data/ddf_whole_FR-LBr.RData")
  save(ddf_try, file = "./new_data/ddf_FR-LBr.RData") 
  
  
  
} else {
  # PUECHABON
  raw_data <- read.csv("./new_data/Euler/FLX_FR-Pue_FLUXNET2015_FULLSET_DD_2000-2014_2-3.csv")
  
  ddf <- raw_data %>% 
    
    # select only relevant data
    dplyr::select(
      TIMESTAMP,
      TA_F_DAY, #temperature
      TA_F_DAY_QC,
      WS_F, #windspeed
      WS_F_QC, 
      LE_F_MDS, #latent heat
      LE_F_MDS_QC,
      P_F, #Precipitation
      P_F_QC,
      G_F_MDS, #Soil heat flux
      G_F_MDS_QC,
      USTAR, #friction velocity
      USTAR_QC
      # consider also: longwave incoming radiation; 
    ) %>%
    
    # time conversion 
    mutate(TIMESTAMP = lubridate::ymd(TIMESTAMP)) %>%
    
    rename(date = TIMESTAMP) %>% 
    
    # correctly represent missing data
    na_if(-9999) %>%
    
    # filter data based on quality check 
    mutate(
      TA_F_DAY = ifelse(TA_F_DAY_QC > 0.8, TA_F_DAY, NA),
      WS_F = ifelse(WS_F_QC > 0.8, WS_F, NA),
      LE_F_MDS = ifelse(LE_F_MDS_QC > 0.8, LE_F_MDS, NA),
      P_F = ifelse(P_F_QC > 0.8, P_F, NA),
      G_F_MDS = ifelse(G_F_MDS_QC > 0.8, G_F_MDS, NA),
      USTAR = ifelse(USTAR_QC > 0.8, USTAR, NA)
    ) %>%
    
    # erase QC variables 
    #dplyr::select(-ends_with("_QC"), NEE_VUT_REF_NIGHT_QC, NEE_VUT_REF_DAY_QC) %>%
    dplyr::select(-ends_with("_QC")) %>%
    
    # calculate ET
    mutate(ET = LE.to.ET(LE_F_MDS, TA_F_DAY)) %>% #get ET with bigleaf function (mm/s)
    mutate(ET = ET*60*60*24) %>%  #ET units conversion (mm/day)
    #mutate(ET_rbeni = convert_et(LE_F_MDS, TA_F_DAY, 61)) #with rbeni
  
    # #calculate water balance
    mutate(water_balance = P_F - ET) 

  # get CWD
ddf_CWD <- ddf %>% 
    na.omit() %>% # remove NA otherwise mct() won't run 
    mct("water_balance", "date")

ddf <- ddf_CWD$df
  
  # # filter rainy days+2; threshold: 1mm
  # for (i in 1:nrow(ddf)) {
  #   precip_day <- ddf$P_F[i]
  #   if (is.na(precip_day)) {
  #     next
  #   } else if(precip_day > 1){
  #     ddf$P_F[i] <- NA
  #     ddf$P_F[i+1] <- NA
  #     ddf$P_F[i+2] <- NA
  #   }
  # }
  
  # See missing data (max 25% should be missing)
  ddf %>%
    summarise_all(funs(100*sum(is.na(.))/length(.))) %>%
    t()
  
  # # only take years with soil moisture
  # ddf_try <- ddf %>% 
  #   filter(year(date) %in% c(2005,2006,2007,2008))
  
  # save as RData
  save(ddf, file = "./new_data/ddf_FR_Pue.RData")
}










  