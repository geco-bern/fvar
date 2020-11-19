## compare observations ET with PET from Splash 

# install packages
library(bigleaf)
library(tidyverse)
library(lubridate)
library(visdat)

# read Splash PET from csv 
PET_splash <- read.csv("/Users/fgiardina/Desktop/Data/fvar/Data/Splash/PET_splash_Pue.csv")

#convert time
df_PET <- PET_splash %>% 
  mutate(date = lubridate::dmy(date))

# plot time series of PET splash
ggplot(data = df_PET, aes(x=date)) +
  geom_line(aes(y = pet_splash))
# save plot
dev.copy(png,'time_series-PET_splash.png', units="in", width=5, height=3, res=300)
dev.off()

# merge with df_nn_soilm_obs
df_nn_soilm_obs <- df_nn_soilm_obs %>% left_join(df_PET, by = "date")

# plot ET vs PETsplash (during moist days)
out_modobs <- df_nn_soilm_obs %>% 
  mutate(pet_splash = pet_splash/1.5) %>% 
  dplyr::filter(moist) %>% 
  rbeni::analyse_modobs2("ET", "pet_splash", type = "heat")
out_modobs$gg
# save plot
dev.copy(png,'scatter_ET-PET_splash.png', units="in", width=5, height=3, res=300)
dev.off()

# compare with ET vs nn_pot
out_modobs1 <- df_nn_soilm_obs %>% 
  dplyr::filter(moist) %>% 
  rbeni::analyse_modobs2("ET", "nn_pot", type = "heat")
out_modobs1$gg
# save plot
dev.copy(png,'scatter_ET-nn_pot.png', units="in", width=5, height=3, res=300)
dev.off()

