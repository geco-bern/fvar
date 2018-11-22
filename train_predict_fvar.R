#' Trains models for potential and actual fluxes. 
#' 
#' @param df A data frame containing observational data for all the predictors and training variables with all NAs removed.
#' @param target A character string defining which variable (column name in \code{df}) is to be used as target variable.
#' 
train_predict_fvar <- function( df, target, varnams_soilm, predictors, hidden_good, hidden_all, soilm_threshold, nrep, weights=NA, package="nnet", plot=FALSE ){

	##------------------------------------------------
	## Determine "good days", i.e. where soil moisture is abover threshold.
	## Get respective indices.
	##------------------------------------------------
	## If multiple layer's soil moisture data is available, do subset w.r.t. soil layer with highest value
	df <- df %>% mutate( maxsoilm = apply( dplyr::select( df, one_of(settings$varnams_soilm) ), 1, FUN = max, na.rm = TRUE ) )
	idxs_moist <- which( df$maxsoilm > soilm_threshold )

	## save and remove date column (not needed for training)
	dates <- df$date
	df <- df %>% dplyr::select( one_of(settings$target), one_of(settings$predictors), one_of(settings$varnams_soilm) )

	## If no weights are specified, use 1 for all data points
	if (is.na(weights)) weights <- rep( 1.0, nrow(df) )

	## Do training only if sufficient data is available
	if ( length(idxs_moist)>30 && (nrow(df) - length(idxs_moist))>30 ){

	  ## Initialise
	  nn_act_vals <- array( NA, dim=c(nrow(df),nrep) )
	  nn_vpd_vals <- array( NA, dim=c(nrow(df),nrep) )
	  nn_pot_vals <- array( NA, dim=c(nrow(df),nrep) )
	  nn_fxx_vals <- array( NA, dim=c(nrow(df),nrep) )

	  for (irep in 1:nrep){

	    print(paste("NN repetition", irep))
	    ##------------------------------------------------
	    ## Train model on moist days' data, not using soil moisture as predictor ("pot")
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
	      package    = package,
	      lifesign   = "full",
	      seed       = irep,
	      hidden     = hidden_good
	      )

	    ## Evaluate predictions of good days model
	    stats_nn_moist <- rsofun::analyse_modobs(
						                                    out_nn_moist$vals,
						                                    df[[settings$target]][idxs_moist],
						                                    plot.title="NN pot, moist only",
						                                    do.plot=plot
						                                    )

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
	      package    = package
	      )

	    ## Evaluate predictions of good days model
	    stats_nn_pot <- rsofun::analyse_modobs(
					                                    out_nn_pot$vals,
					                                    df[[settings$target]],
					                                    plot.title="NN pot",
					                                    do.plot=plot
					                                    )

	    # print( paste( "R2 of NN pot with (soil moisture threshold =", soilm_threshold, "):", stats_nn_good$rsq ) )

	    ##------------------------------------------------
	    ## Train full model, including soil moisture as a predictor ("act")
	    ##------------------------------------------------
	    out_nn_act <- predict_nn( 
	      data       = df, 
	      weights    = weights,
	      predictors = settings$predictors, 
	      nam_target = settings$target, 
	      do_predict = TRUE, 
	      package    = package,
	      seed       = irep,
	      hidden     = hidden_all
	      )

	    ## get statistics of mod vs. obs of all-days full model
	    stats_nn_act <- rsofun::analyse_modobs( 
					                                    out_nn_act$vals, 
					                                    df[[settings$target]],
					                                    plot.title="NN act", 
					                                    do.plot=plot
					                                    )

	    ## keep only values
	    nn_pot_vals[,irep] <- as.vector( out_nn_pot$vals )
	    nn_act_vals[,irep] <- as.vector( out_nn_act$vals )
	    nn_fxx_vals[,irep] <- as.vector( out_nn_act$vals ) / as.vector( out_nn_pot$vals )

	  }

	} else {

	  print(paste("too few data points with soil moisture threshold", isoilm_trh))

	  # print(paste("too few data points with soil moisture threshold", isoilm))
	  nn_pot_vals[,] <- NA
	  nn_act_vals[,] <- NA
	  nn_fxx_vals[,] <- NA

	}

	##------------------------------------------------
	## Aggregate over repetitions
	##------------------------------------------------
	if (nrep>1){

	  df$var_nn_pot    <- apply( nn_pot_vals[,], 1, FUN = mean )
	  df$var_nn_act    <- apply( nn_act_vals[,], 1, FUN = mean )

	  df$fvar          <- apply( nn_fxx_vals[,], 1, FUN = mean )

	  df$moist              <- rep( NA, nrow(df) )
	  df$moist[idxs_moist]  <- TRUE
	  df$moist[-idxs_moist] <- FALSE
	
	} else {

	  df$var_nn_pot    <- nn_pot_vals[,1]
	  df$var_nn_act    <- nn_act_vals[,1]

	  df$fvar          <- nn_fxx_vals[,1]

	  df$moist         <- rep( NA, nrow(df) )
	  df$moist[idxs_moist]      <- TRUE
	  df$moist[-idxs_moist]     <- FALSE
	  
	}

	## Remove outliers
	df$fvar <- remove_outliers_fXX( df$fvar, coef=3.0 )    

	## Add date column back
	df <- df %>% mutate( date=dates ) %>% 
							 dplyr::select( -one_of(settings$target), -one_of(settings$predictors), -one_of(settings$varnams_soilm) )

	return( df )
}





