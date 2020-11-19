# plot fET vs CWD

# # select needed variables
# ddf_plot <- df_nn_soilm_obs %>% 
#   dplyr::select(date, fvar, ET, P_F) %>% 
#   mutate(water_balance = P_F - ET)
# 
# # calculate CWD
# ddf_plot <- ddf_plot %>% 
#   mct("water_balance", "date")
# 
# ddf_plot <- ddf_plot$df

library(ggplot2)

# merge back CWD
ddf_plot <- df_nn_soilm_obs %>% left_join(ddf, by = "date")

# plot time series of CWD
ggplot(data = ddf_plot, aes(x=date, y = deficit)) +
  geom_line() +
  labs(x = "time", y = "CWD")

#plot time series of fET
ggplot(data = ddf_plot, aes(x=date, y = fvar)) +
  geom_line() +
  labs(x = "time", y = "fET")


# plot fET as a function of CWD
ggplot(data = ddf_plot, aes(x=deficit, y = fvar)) +
  geom_point() +
  labs(x = "CWD", y = "fET")




