library(dplyr)
library(lubridate)
library(readr)

usesite <- "FR-Pue"

getvars <- c( 
  "GPP_NT_VUT_REF", 
  "GPP_DT_VUT_REF",                
  "LE_F_MDS", 
  "LE_F_MDS_QC", 
  "NEE_VUT_REF_NIGHT_QC", 
  "NEE_VUT_REF_DAY_QC", # quality flag that goes with GPP 
  "TA_F_DAY",
  "VPD_F_DAY",
  "P_F",
  "SW_IN_F",
  "NETRAD"
)

df_fluxnet <- rsofun::get_obs_bysite_fluxnet2015( 
  sitename=usesite, 
  path_fluxnet2015="./inst/extdata/", 
  path_fluxnet2015_hh ="./inst/extdata/", 
  timescale="d", 
  getvars=getvars,
  threshold_GPP = 0.3
  ) %>% 
  mutate(sitename = usesite)

## get some mo' (modelled soil moisture)
load("~/data/nn_fluxnet2015/modobs_fluxnet2015_s11_s12_s13_with_SWC_v3.Rdata")
df_wcont_splash  <- fluxnet[[ usesite ]]$ddf$s11 %>%
  as_tibble() %>%
  mutate( date=ymd( paste0( as.character(year), "-01-01") ) + days(doy-1) ) %>% 
  mutate( sitename = usesite ) %>% 
  select(sitename, date, wcont_splash = wcont)
df_wcont_splash %>% write_csv( path = "./inst/extdata/df_wcont_splash.csv")

df_wcont_swbm    <- fluxnet[[ usesite ]]$ddf$s12 %>%
  as_tibble() %>%
  mutate( date=ymd( paste0( as.character(year), "-01-01") ) + days(doy-1) ) %>% 
  mutate( sitename = usesite ) %>% 
  select(sitename, date, wcont_swbm = wcont)
df_wcont_swbm %>% write_csv( path = "./inst/extdata/df_wcont_swbm.csv")

df_wcont_et      <- fluxnet[[ usesite ]]$ddf$swc_by_etobs %>%
  as_tibble() %>%
  mutate( date=ymd( paste0(as.character(year), "-01-01") ) + months(moy-1) + days(dom-1) ) %>% 
  mutate( sitename = usesite ) %>% 
  select(sitename, date, wcont_et = soilm_from_et)
df_wcont_et %>% write_csv( path = "./inst/extdata/df_wcont_et.csv")

df_wcont_et_orth <- fluxnet[[ usesite ]]$ddf$swc_by_etobs %>%
  as_tibble() %>%
  mutate( date=ymd( paste0(as.character(year), "-01-01") ) + months(moy-1) + days(dom-1) ) %>% 
  mutate( sitename = usesite ) %>% 
  select(sitename, date, wcont_et_orth = soilm_from_et_orthbucket)
df_wcont_et_orth %>% write_csv( path = "./inst/extdata/df_wcont_et_orth.csv")

df_fpar <- fluxnet[[ usesite ]]$ddf$inp %>% 
  as_tibble() %>% 
  mutate( date=ymd( paste0( as.character(year), "-", as.character(moy), "-", as.character(dom)) ),
          sitename = usesite) %>% 
  select(sitename, date, fpar)
df_fpar %>% write_csv( path = "./inst/extdata/df_fpar.csv")

# df_wcont_et_orth <- readr::read_csv( file = "./inst/extdata/df_wcont_et_orth.csv")
# df_wcont_et <- readr::read_csv( file = "./inst/extdata/df_wcont_et.csv")
# df_wcont_swbm <- readr::read_csv( file = "./inst/extdata/df_wcont_swbm.csv")
# df_wcont_splash <- readr::read_csv( file = "./inst/extdata/df_wcont_splash.csv")

df_fluxnet <- df_fluxnet %>% 
  left_join(df_wcont_splash, by = c("sitename", "date")) %>% 
  left_join(df_wcont_swbm, by = c("sitename", "date")) %>% 
  left_join(df_wcont_et, by = c("sitename", "date")) %>% 
  left_join(df_wcont_et_orth, by = c("sitename", "date")) %>% 
  left_join(df_fpar, by = c("sitename", "date"))

save(df_fluxnet, file = "./data/df_fluxnet.Rdata")



