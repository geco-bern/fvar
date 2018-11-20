#' Trains models for potential and actual fluxes. 
#' 
#' @param df A data frame containing observational data for all the predictors and training variables with all NAs removed.
#' @param varnam_target A character string defining which variable (column name in \code{df}) is to be used as target variable.
#' 
get_fvar <- function( df, varnam_target, varnams_soilm, nhidden_good, nhidden_all, soilm_threshold ){

	##------------------------------------------------
	## Determine "good days", i.e. where soil moisture is abover threshold
	##------------------------------------------------
	if ( length(varnams_soilm)>1 ){
	  ## for observational data, do subset w.r.t. soil layer with highest value
	  tmp <- df %>% dplyr::select( vars( one_of(varnams_soilm)) )
	  tmp[ is.na(tmp) ] <- 0.0
	  vec <- apply( tmp, 1, FUN = max, na.rm=TRUE )
	} else {
	  vec <- df_nona[[ isoilm_data ]]
	}

	idxs_good <- which( vec > isoilm_trh )


	return( df_fvar )
}




#' Prepares data for training
#' 
#' @param df A data frame containing observational data for all the predictors and training variables. One variable among predictors has to be soil moisture \code{"soilm"}. 
#' @param varnam_target A character string defining which variable (column name in \code{df}) is to be used as target variable.
#' 
prepare_data <- function( df, varnam_target, varnams_soilm ){

	##------------------------------------------------
	## Remove outliers
	##------------------------------------------------
	df <- df %>% dplyr::mutate_( varnam_target  = remove_outliers( varnam_target, coef=3.0 ) )

	##------------------------------------------------
	## Get observational soil moisture data (variable availability!)
	##------------------------------------------------
	## normalise to within zero and one
	df <- df %>%  mutate_at( vars( one_of(varnams_soilm)), funs(norm_to_max(.)) ) %>%
	
                ## get mean observational soil moisture across different depths (if available)
                mutate( soilm_mean = apply( select( ., one_of(varnams_soilm) ), 1, FUN = mean, na.rm = TRUE ) ) %>%
                mutate( soilm_mean = ifelse( is.nan(soilm_mean), NA, soilm_mean ) ) %>%

  ##------------------------------------------------
  ## Temperature filter
  ##------------------------------------------------
	## Use only days where temperature is above 5 degrees
  df <- dplyr::filter( df, temp > 5.0 )

  ##------------------------------------------------
  ## do additional data cleaning, removing NA in target variable, necessary for NN training
  ##------------------------------------------------
  df <- dplyr::filter_( df, !is.na( varnam_target ) )
  
  ## use obs soilm data only if of sufficient length
  lengths <- apply( dplyr::select( df, vars(one_of(varnams_soilm)) ), 2, function(x) sum(!is.na(x)) )

  ## drop layer swc obs data if length of data is less than 75% of legth of maximum
  idx <- 0
  drop_idx <- c()
  for (ivar in varnams_soilm){
    idx <- idx + 1
    if (lengths[ivar]<0.75*max(lengths)){
      df[[ ivar ]] <- NULL
      drop_idx <- c(drop_idx, idx)
    }
  }
  if ( length(drop_idx)>0 ) { varnams_soilm <- varnams_soilm[-drop_idx] }
  
  ## remove NAs in observed soil moisture data
  for (ivar in varnams_soilm){
    df <- df[ which(!is.na(df[[ivar]])), ]  ## XXX THIS CAUSES THE PROBLEM OF REMOVING THE ENTIRE DATA WHEN SOILM_OBS DATA IS MISSING XXX
  }

  return( df )

}


norm_to_max <- function( vec ){
  vec <- ( vec - min( vec, na.rm=TRUE ) ) / ( max( vec, na.rm=TRUE ) - min( vec, na.rm=TRUE ) )
  return( vec )
}
