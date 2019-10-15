get_droughts_fvar <- function( df_nn, nam_target, df_soilm=NA, df_par=NA, leng_threshold=5, plot=TRUE, makepdf=FALSE, par_runmed=5 ){

  ##------------------------------------------------
  ## Get cutoff and plot histogram on non-filled fGPP
  ##------------------------------------------------
  ## define drought by finding the lower 5% quantile of the hypothetical symetrical distribution
  positive <- df_nn$fvar[ which(df_nn$fvar>=1.0) ]
  even <- c( positive, 1.0-(positive-1.0) )
  cutoff <- quantile( even, 0.05, na.rm=TRUE )

  ## consider only instances where fvar falls below 0.97 as drought
  cutoff <- min( 0.97, cutoff )


  if (length(positive)>0){

    if (plot){
      if (makepdf) pdf( "./fig/hist_fvar.pdf", width=8, height=6 )
      par( xaxs="i", las=1 )
      hist( even,      
            breaks=c(min(even),seq(0, 1.5, 0.01),max(even)),           
            xlim=c(0,1.5), 
            col=add_alpha("blue", 0.4), 
            freq=FALSE, 
            xlab="fLUE", 
            main="histogram of fvar"
            )
      hist( df_nn$fvar, 
            breaks=c(min(df_nn$fvar, na.rm=TRUE),seq(0, 1.5, 0.01),max(df_nn$fvar, na.rm=TRUE)), 
            xlim=c(0,1.5), 
            col=add_alpha("red", 0.4),  
            freq=FALSE, 
            add=TRUE 
            )
      abline( v=cutoff, col="red" )
      if (makepdf) dev.off()
    }

    ##------------------------------------------------
    ## Fill and smooth fGPP (fLUE) in case gaps are smaller than 'fill_threshold' days
    ##------------------------------------------------
    fill_threshold <- 20

    ## identify and store data gaps bigger than 'fill_threshold'
    na_instances <- get_consecutive( is.na(df_nn$fvar), leng_threshold=fill_threshold, do_merge=FALSE )

    ## "fill" data: interpolate to missing values
    # xxx can approx take ymd dates? xxx
    df_nn$fvar_filled <- approx( df_nn$date, df_nn$fvar, xout=df_nn$date )$y

    ## take running median to smooth data, event identification is based on smoothed data
    idxs <- which( !is.na(df_nn$fvar_filled) )
    tmp  <- tibble( date=df_nn$date[idxs], fvar_smooth=runmed( df_nn$fvar_filled[idxs], par_runmed ) )
    df_nn <- df_nn %>% left_join( tmp, by="date" )

    ## re-create gaps defined by 'na_instances' so that they are not counted towards drought events
    if (nrow(na_instances)>0){
      for (idx in 1:nrow(na_instances)){
        df_nn$fvar_filled[ na_instances$idx_start[idx]:(na_instances$idx_start[idx]+na_instances$len[idx]-1) ] <- NA
        df_nn$fvar_smooth[ na_instances$idx_start[idx]:(na_instances$idx_start[idx]+na_instances$len[idx]-1) ] <- NA
      }
    }

    # XXX todo: this is just for plotting, but should be added to output! 
    ## "fill" NAs and create 'minmax' data frame for minimum and maximum fvar by soil moisture
    df_nn$fvar_min_filled <- approx( df_nn$date, df_nn$fvar_min, xout=df_nn$date )$y
    df_nn$fvar_max_filled <- approx( df_nn$date, df_nn$fvar_max, xout=df_nn$date )$y
    df_nn$fvar_med_filled <- approx( df_nn$date, df_nn$fvar_med, xout=df_nn$date )$y

    # minmax <- tibble( 
    #   date            = df_nn$date     [ which(!is.na(fvar_min_filled ) ) ],
    #   fvar_min_filled = fvar_min_filled[ which(!is.na(fvar_min_filled ) ) ],
    #   fvar_max_filled = fvar_max_filled[ which(!is.na(fvar_max_filled ) ) ],
    #   fvar_med_filled = fvar_med_filled[ which(!is.na(fvar_med_filled ) ) ]
    #   )

    # ## Average over soil moisture datasets; all and obs-only
    # if (length(varnams_swc_obs)>0){
    #   df_nn$soilm_obs <- apply( dplyr::select( df_nn, one_of(varnams_swc_obs)), 1, FUN=mean, na.rm=TRUE )
    #   df_nn$soilm_obs[ is.nan( df_nn$soilm_obs ) ] <- NA
    # }

    # df_nn$soilm_mean <- apply( dplyr::select( df_nn, one_of(varnams_swc)), 1, FUN=mean, na.rm=TRUE )
    # df_nn$soilm_mean[ is.nan( df_nn$soilm_mean ) ] <- NA

    ##------------------------------------------------
    ## Get GPP droughts
    ##------------------------------------------------
    print("get drought events ...")
    df_nn$fvar_smooth_filled <- approx( df_nn$date, df_nn$fvar_smooth, xout=df_nn$date )$y
    df_nn$is_drought_byvar <- ( df_nn$fvar_smooth_filled < cutoff )
    droughts <- get_consecutive( 
                                df_nn$is_drought_byvar, 
                                leng_threshold = leng_threshold, 
                                do_merge       = FALSE
                                )


    ##------------------------------------------------
    ## Prune identified drought events and update is_drought_byvar
    ##------------------------------------------------
    if (!identical(NA, df_soilm)) df_nn <- df_nn %>% left_join( df_soilm, by="date" )
    if (!identical(NA, df_par))   df_nn <- df_nn %>% left_join( df_par,   by="date" )

    out <- prune_droughts(  
                          droughts,
                          leng_threshold = leng_threshold,
                          df_nn$is_drought_byvar,
                          df_nn$fvar_smooth,
                          df_nn$fvar_smooth_filled,                                  
                          mod_pot   = df_nn$var_nn_pot, 
                          mod_act   = df_nn$var_nn_act, 
                          obs       = df_nn[[ nam_target ]],
                          soilm     = df_nn$soilm,
                          par       = df_nn$par,
                          verbose   = FALSE
                          )

    droughts <- out$instances
    df_nn$is_drought_byvar_recalc <- out$is_drought_byvar

    # # ##------------------------------------------------
    # # ## get EVI anomalies
    # # ## XXX: DE-ACTIVATED FOR PUBLIC REPOSITORY. 
    # # ## USES ORIGINAL EVI DATA WHICH IS NOT DISTRIBUTED HERE.
    # # ##------------------------------------------------
    # # out_evianomalies <- get_evianomalies_bysite( sitename, quantile=0.03, do.plot=FALSE ) 

    # ##------------------------------------------------
    # ## Determine wether algorithm failed
    # ## Classify as failed depending on the following criteria:
    # ## * during droughts NNgood is lower than NNall
    # ## * R2 of NNall vs. observed
    # ## * R2 of NNgood vs. observed during good days
    # ## * RMSE of NNgood vs. NNall during good days
    # ##------------------------------------------------
    # # failed <- FALSE

    # droughtdays <- df_nn %>% dplyr::filter(  is_drought_byvar )
    # nondrgtdays <- df_nn %>% dplyr::filter( !is_drought_byvar )

    # # ## * during droughts NNgood is lower than NNall
    # # if ( nrow(droughtdays) > 0 ){
    # #   if ( mean( droughtdays$var_nn_pot, na.rm=TRUE ) < mean( droughtdays$var_nn_act, na.rm=TRUE ) ) failed <- TRUE 
    # # }

    # ## * R2 of NNall
    # mod <- df_nn$var_nn_act
    # obs <- df_nn[[ nam_target ]]
    # if (makepdf){
    #   plot.fil <- "fig/modobs_nn_act_.pdf"
    # } else {
    #   plot.fil <- NA
    # }
    # out_NNall <- analyse_modobs( 
    #                             mod, 
    #                             obs, 
    #                             do.plot=TRUE, 
    #                             plot.title="GPP from NN_act, all days", 
    #                             nrcol=1, 
    #                             plot.fil=plot.fil 
    #                             )
    # # if ( out_NNall$prmse > 60 || out_NNall$rsq < 0.3 ) failed <- TRUE

    # ## * R2 of NNgood vs. observed during good days
    # mod <- nondrgtdays$var_nn_pot
    # obs <- nondrgtdays[[ nam_target ]]
    # if (makepdf){
    #   plot.fil <- "fig/modobs_nn_pot_moistdays.pdf"
    # } else {
    #   plot.fil <- NA
    # }
    # out_NNgood <- analyse_modobs( 
    #                               mod, 
    #                               obs, 
    #                               do.plot=TRUE, 
    #                               plot.title="GPP from NN_pot, moist days", 
    #                               nrcol=1, 
    #                               plot.fil=plot.fil
    #                               )
    # # if ( out_NNgood$prmse > 60 || out_NNgood$rsq < 0.3 ) failed <- TRUE

    # ## * R2 of NNall during bad days has no big bias
    # if ( nrow(droughtdays) > 0 && nrow(droughtdays)>0.05*nrow(nondrgtdays) ){
    #   mod <- droughtdays$var_nn_act
    #   obs <- droughtdays[[ nam_target ]]
    #   out_NNallbad <- analyse_modobs( 
    #                                   mod, 
    #                                   obs, 
    #                                   do.plot=FALSE, 
    #                                   plot.title="GPP from NN_act, dry days", 
    #                                   nrcol=1
    #                                   )
    #   # if ( out_NNallbad$prmse > 70 || out_NNallbad$rsq < 0.3 ) failed <- TRUE
    # } else {
    #   out_NNallbad <- NA
    # }

    # ## * RMSE of NNgood vs. NNall during good days
    # if (makepdf){
    #   plot.fil <- "fig/pot_vs_act_NN.pdf" 
    # } else {
    #   plot.fil <- NA
    # }
    # out_NNgoodall <- analyse_modobs( 
    #                                 nondrgtdays$var_nn_act, 
    #                                 nondrgtdays$var_nn_pot, 
    #                                 do.plot=TRUE, 
    #                                 plot.title="LUE of NN_pot vs. NN_act, moist days", 
    #                                 nrcol=1, 
    #                                 plot.fil=plot.fil
    #                                 )
    # # if ( out_NNgoodall$prmse > 15 ) failed <- TRUE

    # stats <- list( out_NNall=out_NNall, out_NNgood=out_NNgood, out_NNgoodall=out_NNgoodall, out_NNallbad=out_NNallbad )

  }
  
  ##-------------------------------------------------------------------------------
  ## Complement date object
  ##-------------------------------------------------------------------------------
  droughts <- droughts %>%
    as_tibble() %>% 
    mutate( date_start = df_nn$date[.$idx_start],
            date_end   = df_nn$date[.$idx_start+.$len-1] ) %>% 
    mutate( date_start = ymd(date_start), date_end = ymd(date_end) )
  
  return( list( droughts=droughts, df_nn=df_nn ) )
}

prune_droughts <- function( 
  instances, 
  leng_threshold, 
  is_drought_byvar, 
  fvar_smooth, 
  fvar_smooth_filled, 
  mod_pot     = NULL, 
  mod_act     = NULL, 
  obs         = NULL, 
  soilm       = NULL, 
  soilm_mod   = NULL, 
  par         = NULL, 
  rmse_cutoff = NA,
  verbose     = FALSE ){

  if (nrow(instances)>0){

    print(paste("number of events before pruning:", nrow(instances)))

    ##-------------------------------------------------------------------------------
    ## Trim drought event based on missing values in fvar_smooth
    ##-------------------------------------------------------------------------------
    idx_drop <- c()
    for ( idx in 1:nrow(instances) ){

      ## Case 1: NAs at the end of the drought period. Find first non-NA starting from end of drought. Define this as the new end of drought
      if ( is.na( fvar_smooth[ instances$idx_start[idx]+instances$len[idx]-1 ] ) ){
        jdx_look <- instances$idx_start[idx]+instances$len[idx]-2
        while ( is.na( fvar_smooth[ jdx_look ] ) ) jdx_look <- jdx_look - 1
        instances$len[idx] <- jdx_look - instances$idx_start[idx] + 1
      }

      ## Case 2: NAs at the beginning of the drought period (identified based on the interpolated, gap-filled fvar)
      ## ==> drop the event
      if ( is.na( fvar_smooth[ instances$idx_start[idx] ] ) ) idx_drop <- c( idx_drop, idx )

    }

    if (length(idx_drop)>0) instances <- instances[ -idx_drop, ]

    ## keep only droughts longer than <leng_threshold> days after this trimming
    instances <- instances[ which( instances$len >= leng_threshold ), ]

    print(paste("number of events after trimming:", nrow(instances)))

    ##-------------------------------------------------------------------------------
    ## Prune drought events by whether soil moisture is (relatively) low
    ##-------------------------------------------------------------------------------
    idx_drop <- c()
    if (!is.null(soilm) && nrow(instances)>0 ){

      soilm_threshold <- quantile( soilm, probs=0.75, na.rm=TRUE )
      # soilm_threshold <- 0.5

      idx_drop <- c()

      for ( idx in 1:nrow(instances) ){

        soilm_sub <- soilm[ instances$idx_start[idx]:(instances$idx_start[idx]+instances$len[idx]-1) ]
        # if ( !any( !is.na(soilm_sub) ) ) soilm_sub <- soilm_mod[ instances$idx_start[idx]:(instances$idx_start[idx]+instances$len[idx]-1) ]
        if ( mean( soilm_sub, na.rm=TRUE ) > soilm_threshold ) idx_drop <- c( idx_drop, idx )

      }

      if (length(idx_drop)>0) instances <- instances[ -idx_drop, ]
      print(paste("number of events after pruning by soil moisture:", nrow(instances)))

    }


    ##-------------------------------------------------------------------------------
    ## Prune drought events by quality of modelled vs. observed during drought period
    ##-------------------------------------------------------------------------------
    idx_drop <- c()
    if (!is.null(mod_act) && !is.null(mod_pot) && !is.null(obs)  && nrow(instances)>0 ){

      idx_drop <- c()

      for ( idx in 1:nrow(instances) ){

        ## extract data for this event
        mod_act_sub <- mod_act[ instances$idx_start[idx]:(instances$idx_start[idx]+instances$len[idx]-1) ]
        mod_pot_sub <- mod_pot[ instances$idx_start[idx]:(instances$idx_start[idx]+instances$len[idx]-1) ]
        obs_sub     <- obs[ instances$idx_start[idx]:(instances$idx_start[idx]+instances$len[idx]-1) ]
        
        ## remove NAs
        tmp <- mod_act_sub + mod_pot_sub + obs_sub
        mod_act_sub <- mod_act_sub[ which(!is.na(tmp)) ]
        mod_pot_sub <- mod_pot_sub[ which(!is.na(tmp)) ]
        obs_sub     <- obs_sub[ which(!is.na(tmp)) ]

        ## get RMSE
        rmse_act_sub    <- rmse( mod_act_sub, obs_sub )
        rmse_pot_sub    <- rmse( mod_pot_sub, obs_sub )

        rmse_act    <- rmse( mod_act, obs )
        rmse_pot    <- rmse( mod_pot, obs )

        # ## drop event (instance) if RMSE of NN-pot is smaller than RMSE of NN-act
        # print(paste("rmse_pot_sub", rmse_pot_sub))
        # print(paste("rmse_act_sub", rmse_act_sub))

        # boxplot( mod_act_sub - obs_sub, at=1, xlim=c(0,3) )
        # boxplot( mod_pot_sub - obs_sub, at=2, xlim=c(0,3), add=TRUE )

        if ( rmse_pot_sub < 0.9 * rmse_act_sub ) idx_drop <- c( idx_drop, idx )

        # # print(paste("RMSE during event", idx, "=", rmse))
        # # print("--------------------------------")
        # if (!is.na(rmse_cutoff)){
        #   if (is.nan(rmse) || rmse>rmse_cutoff) { idx_drop <- c( idx_drop, idx ) }
        # }

      }

      if (length(idx_drop)>0) instances <- instances[ -idx_drop, ]
      print(paste("number of events after pruning by mod vs obs:", nrow(instances)))

    }

    ##-------------------------------------------------------------------------------
    ## Prune drought events if light is very low (was GPP in Stocker et al. 2018)
    ##-------------------------------------------------------------------------------
    idx_drop <- c()
    if ( !is.null(par) && nrow(instances)>0  && nrow(instances)>0 ){

      for ( idx in 1:nrow(instances) ){

        ## extract data for this event
        par_sub <- par[ instances$idx_start[idx]:(instances$idx_start[idx]+instances$len[idx]-1) ]
        
        if ( mean( par_sub, na.rm=TRUE ) < 0.05 * max( par, na.rm=TRUE ) ){
          ## insignificant "drought" - probably just noise under low light levels
          idx_drop <- c( idx_drop, idx )
        }

      }

      if (length(idx_drop)>0) instances <- instances[ -idx_drop, ]
      print(paste("number of events after pruning by PAR level:", nrow(instances)))

    }

    ##-------------------------------------------------------------------------------
    ## Update 'is_drought_byvar'
    ##-------------------------------------------------------------------------------
    print(paste("number of events after pruning:", nrow(instances)))

    is_drought_byvar[] <- FALSE
    if (nrow(instances)>0){
      for ( idx in 1:nrow(instances) ){
        is_drought_byvar[ instances$idx_start[idx]:(instances$idx_start[idx]+instances$len[idx]-1) ] <- TRUE
      }
    }

    if (verbose){
      test_instances <- get_consecutive( is_drought_byvar )
      print("original:")
      print( instances )
      print("recalculated from is_drought_byvar:")
      print(test_instances)
    }

  }

  out <- list( instances=instances, is_drought_byvar=is_drought_byvar )
  return( out )

}

rmse <- function(mod, obs){
  tibble(mod=mod, obs=obs) %>%
    tidyr::drop_na() %>%
    yardstick::rmse(obs, mod) %>% 
    select(.estimate) %>%  
    unlist() %>% 
    unname()
}