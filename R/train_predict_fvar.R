#' Trains models for potential and actual fluxes. 
#' 
#' @param df A data frame containing observational data for all the predictors and training variables with all NAs removed.
#' @param settings A character string defining which variable (column name in \code{df}) is to be used as target variable.
#' 
train_predict_fvar <- function( df, settings, soilm_threshold, weights=NA, plot=FALSE, verbose = FALSE ){

	##------------------------------------------------
	## Determine "moist days", i.e. where soil moisture is above threshold.
	## Get respective indices.
	##------------------------------------------------
	## If multiple layer's soil moisture data is available, do subset w.r.t. soil layer with highest value
	# df <- df %>% mutate( maxsoilm = apply( dplyr::select( df, one_of(settings$varnams_soilm) ), 1, FUN = max, na.rm = TRUE ) )
	# idxs_moist <- which( df$maxsoilm > soilm_threshold )
  
  ## simple
  idxs_moist <- which( df[[settings$varnams_soilm]] > soilm_threshold )

  ## identify and record moist days for this threshold
  vec_moist <- rep( NA, nrow(df) )
  vec_moist[idxs_moist]  <- TRUE
  vec_moist[-idxs_moist] <- FALSE

	df <- df %>% 
	  dplyr::select( one_of(settings$target), one_of(settings$predictors), one_of(settings$varnams_soilm) )

	## If no weights are specified, use 1 for all data points
	if (is.na(weights)) weights <- rep( 1.0, nrow(df) )

	##------------------------------------------------
	## Loop and aggregate over repetitions
	##------------------------------------------------
	df <- purrr::map(
		as.list(seq(settings$nrep)),
		~train_predict_fvar_byrep( 
			., 
			df,
			idxs_moist,
			settings, 
			weights = weights, 
			plot = FALSE,
			verbose = verbose 
			)
		)	%>%
		dplyr::bind_rows() %>%
		dplyr::group_by(idx) %>%
		dplyr::summarize(
			nn_pot = mean(nn_pot, na.rm = TRUE),
			nn_act = mean(nn_act, na.rm = TRUE),
			fvar   = mean(nn_fxx, na.rm = TRUE)
			) %>%
		dplyr::mutate(moist = vec_moist) %>%
		dplyr::mutate(fvar = remove_outliers_fXX(fvar, coef=3.0 )) %>%
		dplyr::bind_rows(df)

	return( df )
}

train_predict_fvar_byrep <- function( irep, df, idxs_moist, settings, weights=NA, plot=FALSE, verbose ){
  
  ## Do training only if sufficient data is available
  if ( length(idxs_moist)>30 && (nrow(df) - length(idxs_moist))>30 ){
    
    if (verbose) rlang::inform(paste("NN repetition", irep))
    
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
      hidden     = NULL # settings$nnodes
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
      hidden     = NULL # settings$nnodes
    )
    
    # ## get statistics of mod vs. obs of all-days full model
    # stats_nn_act <- rsofun::analyse_modobs( 
    #   out_nn_act$vals, 
    #   df[[settings$target]],
    #   plot.title = "NN act", 
    #   do.plot    = plot
    # )
    
    df_out <- tibble(
      nn_pot_cv = as.vector(out_nn_pot$vals_cv),  # for validation
      nn_act_cv = as.vector(out_nn_act$vals_cv),  # for validation
      nn_pot = as.vector(out_nn_pot$vals),
      nn_act = as.vector(out_nn_act$vals),
      nn_fxx = as.vector(out_nn_act$vals) / as.vector(out_nn_pot$vals)
      ) %>%
      dplyr::mutate(irep = irep) %>%
      dplyr::mutate(idx = 1:nrow(.))
    
  } else {
    
    rlang::warn("Too few data points with current soil moisture threshold")
    
    df_out <- tibble(
      nn_pot = NA,
      nn_act = NA,
      nn_fxx = NA) %>%
      dplyr::mutate(irep = irep) %>%
      dplyr::mutate(idx = 1:nrow(.))
    
  }
  
  return(df_out)	
  
}



