predict_nn_keras <- function(df, nam_target, predictors, prop){

  ## First shuffle the dataset
  df_shffld <- df[sample(nrow(df)),]

  ## get indices for train_data into train and test splits (Hint: sample() )
  ind <- sample(2, nrow(df_shffld), replace=TRUE, prob = c(prop,(1-prop)))
  idx <- which(ind == 1)

  ##---------------------------------------
  ## Train, given indices for training
  ##---------------------------------------
  out_train <- predict_nn_keras_byfold(df_shffld, nam_target, predictors, idx = idx)

  ## add predictions to original data frame
  nam_joinvars <- out_train$df_test %>%
    dplyr::select(-pred) %>%
    names()
  out_train$df_compl <- df %>%
    select(-one_of(c(predictors))) %>%
    left_join(out_train$df_test, by = nam_joinvars)

  return(out_train)
}

predict_nn_keras_byfold <- function(df, nam_target, predictors, idx){

  ## Use the  indicies to get test and train splits
  df_train <- df[idx, ] ## include all columns
  df_test <- df[-idx, ]  ## include all columns

  ## Separate the splits to get train_data, train_target, test_data and test_target. After this you should have 4 corresponding dataframes. Also drop the time stamp columns from the train data and test data as we treat the observations as IID. ( we have stored them separately for plots )
  train_target <-  df_train %>% select(nam_target)
  df_train <- df_train %>% select(one_of(predictors))

  ## Make use of recipe() or any other function you wish, to scale and center each of the columns
  pp <- preProcess(df_train, method = c("center","scale"))    # get the statistics (mean, variance, etc) of numeric cols
  df_pp <- predict(pp, df_train)  # transform the train data to center and scale it

  model <- keras_model_sequential()
  model %>% 
    layer_dense(units = 20, activation = 'relu', input_shape = ncol(df_pp)) %>%
    layer_dense(units = 1) %>%
    compile( optimizer = optimizer_adam(lr=0.001),
             loss = 'mse',
             metrics = list('mae'))

  set.seed(1982)
  history <- fit(
    object           = model, 
    x                = df_pp %>% as.matrix(), 
    y                = train_target %>% as.matrix(),
    batch_size       = 128, 
    validation_split = 0.15, 
    epochs = 50,
    shuffle = TRUE, 
    callbacks = list(callback_early_stopping( monitor = "val_loss",  patience = 0))
  )

  ##---------------------------------------
  ## Predict with trained model
  ##---------------------------------------
  vec_target <- df_test %>% pull(nam_target)
  df_test <- df_test %>% select(one_of(predictors))
  df_test <- predict(pp, df_test)    # transform the test data to center and scale it
  df_test <- as.matrix(df_test)
  vec_pred <- predict(model, df_test)

  ## construct data frame with test results
  df_test <- df[-idx, ] %>%
    select(-one_of(c(nam_target, predictors))) %>%
    mutate(pred = as.vector(vec_pred))

  return(list(model = model, pp = pp, df_test = df_test))

}
