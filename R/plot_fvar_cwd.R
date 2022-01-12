library(tidyverse)

load("~/Downloads/plot_allsites_raw.RData")

all_sites <- plot_allsites_raw$name_site %>% 
  unique()

agg_plot_by_site <- function(df, sitename, nbin = 30){
  
  ## Bin data along deficit and make regression with upper 90% quantile
  df <- df %>%
    dplyr::filter(name_site == sitename) %>% 
    ungroup() %>% 
    mutate(bin = cut(deficit, breaks = nbin)) %>% 
    mutate(cwd_lower = as.numeric( sub("\\((.+),.*", "\\1", bin)),
           cwd_upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", bin) )
    ) %>% 
    mutate(cwd_mid = (cwd_lower + cwd_upper)/2) %>% 
    dplyr::select(-bin, -cwd_lower, -cwd_upper)
  
  df_agg <- df %>%
    ungroup() %>%
    group_by(cwd_mid) %>%
    summarise(fvar_q25 = quantile(fvar, probs = 0.25, na.rm = TRUE),
              fvar_q50 = quantile(fvar, probs = 0.5, na.rm = TRUE),
              fvar_q75 = quantile(fvar, probs = 0.75, na.rm = TRUE),
              fvar_mean = mean(fvar, na.rm = TRUE),
              fvar_sd = sd(fvar, na.rm = TRUE)) %>% 
    mutate(sitename = sitename)
  
  # ggplot() +
  #   geom_ribbon(aes(cwd_mid, ymin = fvar_q25, ymax = fvar_q75), data = df_agg, fill = "grey70") +
  #   geom_line(aes(cwd_mid, fvar_q50), data = df_agg) +
  #   ylim(0, 1.2)
  
  gg <- ggplot() +
    geom_ribbon(aes(cwd_mid, ymin = fvar_mean - fvar_sd, ymax = fvar_mean + fvar_sd), data = df_agg, fill = "grey70") +
    geom_line(aes(cwd_mid, fvar_mean), data = df_agg) +
    ylim(0, 1.2) +
    geom_hline(yintercept = 1.0, linetype = "dotted") +
    theme_classic() +
    labs(title = sitename, x = "CWD (mm)", y = "fET")
  
  return(list(df_agg = df_agg, gg = gg))
}

## apply this to all sites
list_agg_plot <- purrr::map(
  as.list(all_sites),
  ~agg_plot_by_site(plot_allsites_raw, ., nbin = 20)
)

## plot with all lines
df_agg <- purrr::map_dfr(
  list_agg_plot,
  "df_agg"
  )

df_agg %>% 
  ggplot() +
  geom_line(aes(cwd_mid, fvar_mean, group = sitename, color = sitename)) +
  ylim(0, 1.2) +
  geom_hline(yintercept = 1.0, linetype = "dotted") +
  theme_classic() +
  labs(title = "All sites", x = "CWD (mm)", y = "fET")

## individual plots accessible as:
list_agg_plot[[1]]$gg
list_agg_plot[[2]]$gg
list_agg_plot[[3]]$gg
list_agg_plot[[4]]$gg
list_agg_plot[[5]]$gg
list_agg_plot[[6]]$gg
