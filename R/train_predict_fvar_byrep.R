train_predict_fvar_byrep <- function( irep, df, idxs_moist, settings, weights=NA, plot=FALSE, verbose ){
  
  ## Do training only if sufficient data is available
  if ( length(idxs_moist)>30 && (nrow(df) - length(idxs_moist))>30 ){
    
    if (verbose) print(paste("NN repetition", irep))
    
    ##------------------------------------------------
    ## Train model on moist days' data, not using soil moisture as predictor ("_moist")
    ##------------------------------------------------
    ## Remove all variables starting with "soilm_" from predictors
    # settings$varnams_soilm <- df %>% dplyr::select( starts_with("soilm") ) %>% names()
    settings$predictors_without_soilm <- settings$predictors[ !(settings$predictors %in% settings$varnams_soilm) ]
    
    out_nn_moist <- predict_nn( 
      data       = df[ idxs_moist, ],
      weights    = weights[ idxs_moist ],
      predictors = settings$predictors_without_soilm,
      nam_target = settings$target,
      do_predict = TRUE,
      package    = settings$package,
      lifesign   = "full",
      seed       = irep,
      hidden     = settings$nnodes
    )
    
    # ## Evaluate predictions of good days model
    # stats_nn_moist <- rsofun::analyse_modobs(
    #   out_nn_moist$vals,
    #   df[[settings$target]][idxs_moist],
    #   plot.title="NN pot, moist only",
    #   do.plot=plot
    # )
    
    ##------------------------------------------------
    ## Get potential values (fvar_pot) with moist-days model, predicting all days
    ##------------------------------------------------
    ## Note: model is passed on as argument 'nn', therefore not training is done. 
    out_nn_pot <- predict_nn( 
      data       = df, 
      weights    = weights,
      predictors = settings$predictors,
      nam_target = settings$target, 
      nn         = out_nn_moist$nn, 
      do_predict = TRUE, 
      package    = settings$package
    )
    
    # ## Evaluate predictions of moist days model
    # stats_nn_pot <- rsofun::analyse_modobs(
    #   out_nn_pot$vals,
    #   df[[settings$target]],
    #   plot.title="NN pot",
    #   do.plot=plot
    # )
    
    ##------------------------------------------------
    ## Train full model, including soil moisture as a predictor ("act")
    ##------------------------------------------------
    out_nn_act <- predict_nn( 
      data       = df, 
      weights    = weights,
      predictors = settings$predictors, 
      nam_target = settings$target, 
      do_predict = TRUE, 
      package    = settings$package,
      seed       = irep,
      hidden     = settings$nnodes
    )
    
    # ## get statistics of mod vs. obs of all-days full model
    # stats_nn_act <- rsofun::analyse_modobs( 
    #   out_nn_act$vals, 
    #   df[[settings$target]],
    #   plot.title = "NN act", 
    #   do.plot    = plot
    # )
    
    df_out <- tibble(
      nn_pot = as.vector(out_nn_pot$vals),
      nn_act = as.vector(out_nn_act$vals),
      nn_fxx = as.vector(out_nn_act$vals) / as.vector(out_nn_pot$vals)
    ) %>%
      dplyr::mutate(irep = irep) %>%
      dplyr::mutate(idx = 1:nrow(.))
    
  } else {
    
    rlang::warn(paste("Too few data points with soil moisture threshold", isoilm_trh))
    
    df_out <- tibble(
      nn_pot = NA,
      nn_act = NA,
      nn_fxx = NA
    ) %>%
      dplyr::mutate(irep = irep) %>%
      dplyr::mutate(idx = 1:nrow(.))
    
  }
  
  return(df_out)	
  
}
