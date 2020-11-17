predict_nn <- function( data, predictors, nam_target, weights=NULL, nn=NULL, do_predict=TRUE, 
                        do_modobs=FALSE, lifesign="none", package="nnet", seed=1, hidden=NULL ){
  
  if (package=="nnet"){

    require( nnet )
    require( caret )
    require( recipes )

    downscale <- FALSE
    if (is.null(nn)){

      forml  <- as.formula( paste( nam_target, "~", paste( predictors, collapse=" + " ) ) )

      ## this has caused a problem before due to values being too hight -> weird
      if (mean(data[[ nam_target ]], na.rm = TRUE)>1e6){ 
        data[[ nam_target ]] <- data[[ nam_target ]] * 1e-6
        downscale <- TRUE
      }

      ## this doesn't work
      # myrecipe <- recipe(forml, data = data) %>%
      #   step_center(all_numeric(), -all_outcomes()) %>%
      #   step_scale(all_numeric(), -all_outcomes()) %>%
      #   step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE)
      #   # step_medianimpute(all_numeric(), -all_outcomes())
            
      traincotrlParams <- caret::trainControl( 
        method="repeatedcv", 
        number=5, 
        repeats=5, 
        verboseIter=FALSE,
        savePredictions = "final"
        )
      
      if (is.null(hidden)){
        tune_grid <- expand.grid( .decay = c(0.05, 0.01, 0.005), .size = seq(4,20,2) )
      } else {
        tune_grid <- expand.grid( .decay = c(0.01), .size = c(hidden) )
      }
      
      set.seed(seed)
      
      nn <- caret::train(
        forml,
        data      = data,
        metric    = "RMSE",
        weights   = weights,
        method    = "nnet",
        linout    = TRUE,
        tuneGrid  = tune_grid,
        preProc   = "range", # preprocessParams,
        trControl = traincotrlParams,
        trace     = FALSE,
        na.action = na.omit
        )
      
    }
    
    if (do_predict){
      ## make predictions (on same data as used for training)
      vals <- as.vector( predict( nn, data ) )
      
    } else {
      vals <- rep( NA, nrow(data) )
    }
    
    ## get predicted values from cross-validation resamples, take mean across repetitions
    vals_cv <- nn$pred %>% 
      as_tibble() %>% 
      dplyr::filter(size == nn$bestTune$size, decay == nn$bestTune$decay) %>% 
      separate(Resample, into = c("fold", "rep")) %>% 
      group_by(size, decay, rowIndex) %>% 
      summarise(pred = mean(pred)) %>% 
      pull(pred)
    
    if (downscale){ 
      vals_cv <- vals_cv * 1e6
      vals <- vals * 1e6
      }

    # ## test 
    # data <- data %>% mutate(pred = vals)
    # data <- data %>% mutate(pred_resamples = nn$resample)
    # data %>% rbeni::analyse_modobs2("pred", "GPP_NT_VUT_REF")
    # 
    # df_test <- nn$pred %>% 
    #   as_tibble() %>% 
    #   separate(Resample, into = c("fold", "rep")) %>% 
    #   group_by(size, decay, rowIndex) %>% 
    #   summarise(obs = mean(obs), pred = mean(pred))
    # 
    # df_test %>% rbeni::analyse_modobs2("pred", "obs")
    
  } else {

    rlang::abort("predict_nn(): No other training package implemented than nnet.")

  }

  return( list( nn = nn, vals = vals, vals_cv = vals_cv, hidden_best = nn$bestTune$size ) )

}
