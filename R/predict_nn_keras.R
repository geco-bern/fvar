predict_nn_keras <- function(df, 
                             nam_target, 
                             predictors, 
                             prop,   # prop = proportion train and test splits 
                             num_epochs, 
                             batch_size, 
                             val_size,
                             learning_rate,
                             num_layers,
                             num_units_per_layer, # limit: only layers with same number of units
                             print_model_summary = F){
  
  # maybe simpler to define just one function? (not _byfold)


  ## First shuffle the dataset
  df_shffld <- df[sample(nrow(df)),]

  ## get indexes for train_data into train and test splits (Hint: sample() )
  ind <- sample(2, nrow(df_shffld), replace=TRUE, prob = c(prop,(1-prop))) 
  idx <- which(ind == 1) #get the positions of the train points 

  ##---------------------------------------
  ## Train, given indices for training
  ##---------------------------------------
  out_train <- predict_nn_keras_byfold(df_shffld, nam_target, predictors, idx = idx, num_epochs, batch_size, val_size)

  ## add predictions to original data frame
  nam_joinvars <- out_train$df_cv %>%
    dplyr::select(-pred) %>%
    names()
  out_train$df_compl <- df %>%
    select(-one_of(c(predictors))) %>%
    left_join(out_train$df_cv, by = nam_joinvars)

  return(out_train)
}

predict_nn_keras_byfold <- function(df, 
                                    nam_target, 
                                    predictors, 
                                    idx, 
                                    num_epochs, 
                                    batch_size, 
                                    val_size,
                                    learning_rate,
                                    num_layers,
                                    num_units_per_layer,
                                    print_model_summary = F){

  ## Use the indexes to get test and train splits
  df_train <- df[idx, ] ## include all columns
  df_test <- df[-idx, ]  ## include all columns
  
  # ## Save time stamps for the test and train splits, as a separate data frame for time-series plots 
  # train_data_time <- train_split$TIMESTAMP_START
  # test_data_time <- test_split$TIMESTAMP_START

  ## Separate the splits to get train_data, train_target, test_data and test_target. After this you should have 4 
  # corresponding dataframes. Not including time stamp columns from the train data and test data as we treat 
  # the observations as IID. (we have stored them separately for plots)
  train_target <-  df_train %>% dplyr::select(nam_target)
  df_train <- df_train %>% dplyr::select(one_of(predictors)) # "one_of" take only variables specified in string file (here: predictors)

  # test_target <- df_test %>% dplyr::select(nam_target)
  # df_test <- df_test %>% dplyr::select(one_of(predictors))
  
  ## scale and center each of the columns
  pp_stats <- preProcess(df_train, method = c("center","scale"))    # get the statistics (mean, variance, etc) of numeric cols
  df_train_pp <- predict(pp_stats, df_train)  # transform the train data to center and scale it
  # df_test_pp <- predict(pp_stats, df_test)  # transform the test data to center and scale it (using train data stats, to avoid leakage)
  
  ## define and compile model 
  model <- keras_model_sequential()
  #build model
  model = model %>% 
    layer_dense(units=num_units_per_layer, activation="relu", input_shape = ncol(df_train_pp))
  #Add the hidden layers. Note this requires just a simple for loop that calls the function "layer_dense"
  if (num_layers>1){
    for (i in 1:(num_layers-1)){
      model = model %>% layer_dense(units=num_units_per_layer, activation="relu")
    }
  }
  #Add the output layer. Note that it uses a sigmoid activation function. 
  model = model %>% layer_dense(units=1, activation="sigmoid")
  #Print the model description
  if (print_model_summary){
    summary(model)
  }

  #Compile the model, defining loss (e.g. binary cross-entropy) and metric to measure during training (e.g. accuracy).
  model %>% compile(optimizer=optimizer_adam(lr = learning_rate), #learning rate for stochastic gradient descent
                    loss='mse', #'binary_crossentropy'
                    metrics= list('mae')) #list('accuracy'))
  
  set.seed(1982)
  #Fit the model
  history = model %>% fit(
    x                = df_train_pp %>% as.matrix(), # convert df to matrix to feed the model 
    y                = train_target %>% as.matrix(),
    batch_size       = batch_size, 
    validation_split = val_size, 
    epochs = num_epochs,
    shuffle = TRUE, 
    callbacks = list(callback_early_stopping(monitor = "val_loss",  patience = 0))
  )

  ##---------------------------------------
  ## Predict with trained model on test set only
  ##---------------------------------------
  df_test <- df_test %>% select(one_of(predictors))
  df_test <- predict(pp_stats, df_test)    # scale test data
  df_test <- as.matrix(df_test)
  vec_pred <- predict(model, df_test) # same function as for applying scaling transform? 

  ## construct data frame with test results
  df_cv <- df[-idx, ] %>%    #confusing to call CV here: there is no validation split 
    select(-one_of(c(predictors))) %>% 
    mutate(pred = as.vector(vec_pred))

  # not sure about this part, isn't it a repetition of the above? 
  ##---------------------------------------
  ## Predict with trained model on all data - UNTESTED   
  ##---------------------------------------
  df_all <- df %>% select(one_of(predictors))
  df_all <- predict(pp, df_all)    # transform the test data to center and scale it
  df_all <- as.matrix(df_all)
  vec_pred_all <- predict(model, df_all)
  
  ## construct data frame with test results
  df_all <- df %>%
    select(-one_of(c(predictors))) %>%
    mutate(pred = as.vector(vec_pred_all))
  
  return(list(nn = model, pp = pp_stats, df_cv = df_cv, df_all = df_all))

}
