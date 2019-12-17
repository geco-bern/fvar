#' Trains models for potential and actual fluxes. 
#' 
#' @param df A data frame containing observational data for all the predictors and training variables with all NAs removed.
#' @param settings A character string defining which variable (column name in \code{df}) is to be used as target variable.
#' 
train_predict_fvar <- function( df, settings, soilm_threshold, weights=NA, plot=FALSE, verbose = FALSE ){

	##------------------------------------------------
	## Determine "moist days", i.e. where soil moisture is abover threshold.
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

	## save and remove date column (not needed for training)
	dates <- df$date
	df <- df %>% 
	  dplyr::select( date, one_of(settings$target), one_of(settings$predictors), one_of(settings$varnams_soilm) )

	## If no weights are specified, use 1 for all data points
	if (is.na(weights)) weights <- rep( 1.0, nrow(df) )

	##------------------------------------------------
	## Loop and ggregate over repetitions
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
			fvar        = mean(nn_fxx, na.rm = TRUE)
			) %>%
		dplyr::mutate(moist = vec_moist) %>%
		dplyr::mutate(fvar = remove_outliers_fXX(fvar, coef=3.0 )) %>%
		dplyr::mutate(date = dates) %>% 
		dplyr::right_join(df, by = "date")

	return( df )
}




