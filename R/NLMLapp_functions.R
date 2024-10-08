elastic_net_wrapper <- function(data, outcome=NULL, predictors_con=NULL,predictors_cat=NULL, split=80, outer_cv=NULL, stratified=T,scaling=T,
                                repeated_cv=1,ensr_cv=10,ensr_alphas=seq(0, 1, length = 10),ensr_lambdas=100,seed=404,shuffle=T,
                                stop_train=NULL,stop_test=NULL,family='binary',pred_min=NULL,pred_max=NULL,prefer_sensitivity=T){
  # required packages
  `%!in%` = Negate(`%in%`)

  # combine predictors
  predictors = c(predictors_con,predictors_cat)

  # shuffle dataset to lose time contingency for CV
  if (shuffle==T){
    set.seed(seed)
    data = data[sample(nrow(data)),]
  }

  # split data into y and x
  y = data[outcome]
  x = data[predictors]

  # create list to store the predictions
  predictions_all = rep('Train',length(unlist(y)))

  # create list for test indices
  test_indices = list()

  # create list of x and y datasets to be analyzed
  analysis_list = list()

  # split x and y into training and testing data
  if (is.null(outer_cv)==T){
    # performing stratified split
    if(stratified==T){
      if (family=='binary'){
        # get indices
        set.seed(seed)
        my_train_ind_no_y =  sample(which(y==0), size = split/100*length(which(y==0)))
        set.seed(seed)
        my_train_ind_y =  sample(which(y==1), size = split/100*length(which(y==1)))
        # store test indices
        test_indices[[1]] = c(1:length(unlist(y)))[c(1:length(unlist(y)))%!in%c(my_train_ind_no_y,my_train_ind_y)]
        # split data
        y_train = y[c(my_train_ind_no_y,my_train_ind_y),]
        y_test =y[-c(my_train_ind_no_y,my_train_ind_y),]
        x_train = x[c(my_train_ind_no_y,my_train_ind_y),]
        x_test = x[-c(my_train_ind_no_y,my_train_ind_y),]
        # add to analysis list
        analysis_list[[1]] = list(y_train,y_test,x_train,x_test)
      }
      else if (family=='continuous'){
        # get indices
        set.seed(seed)
        my_train_ind =  createDataPartition(as.matrix(y), p = split/100, list = T,groups=min(3,nrow(y)))
        # store test indices
        test_indices[[1]] = c(1:length(unlist(y)))[c(1:length(unlist(y)))%!in%unlist(my_train_ind)]
        # split data
        y_train = y[c(unlist(my_train_ind)),]
        y_test =y[-c(unlist(my_train_ind)),]
        x_train = x[c(unlist(my_train_ind)),]
        x_test = x[-c(unlist(my_train_ind)),]
        # add to analysis list
        analysis_list[[1]] = list(y_train,y_test,x_train,x_test)
      }
    }
    # performing non-stratified split
    else if(stratified==F){
      set.seed(seed)
      my_train_ind =  sample(c(1:nrow(y)), size = split/100*nrow(y))
      # store test indices
      test_indices[[1]] = c(1:length(unlist(y)))[c(1:length(unlist(y)))%!in%c(my_train_ind)]
      # split data
      y_train = y[c(my_train_ind),]
      y_test =y[-c(my_train_ind),]
      x_train = x[c(my_train_ind),]
      x_test = x[-c(my_train_ind),]
      # add to analysis list
      analysis_list[[1]] = list(y_train,y_test,x_train,x_test)
    }
  }
  # creating datasets for cross-validation
  else {
    if(stratified==T){
      # creating folds
      set.seed(seed)
      folds <- create_folds(as.numeric(unlist(y)),k = outer_cv,type='stratified')
      # creating datasets
      for(nfold in 1:length(folds)){
        y_train <- y[c(folds[[nfold]]), ]
        y_test <- y[-c(folds[[nfold]]), ]
        x_train <- x[c(folds[[nfold]]), ]
        x_test <- x[-c(folds[[nfold]]), ]
        analysis_list[[nfold]] = list(y_train,y_test,x_train,x_test)
        # store indices
        test_indices[[nfold]] = c(1:length(unlist(y)))[c(1:length(unlist(y)))%!in%c(folds[[nfold]])]
      }
    }
    else if(stratified==F){
      set.seed(seed)
      folds <- create_folds(as.numeric(unlist(y)),k = outer_cv,type='basic')
      for(nfold in 1:length(folds)){
        y_train <- y[c(folds[[nfold]]), ]
        y_test <- y[-c(folds[[nfold]]), ]
        x_train <- x[c(folds[[nfold]]), ]
        x_test <- x[-c(folds[[nfold]]), ]
        analysis_list[[nfold]] = list(y_train,y_test,x_train,x_test)
        # store indices
        test_indices[[nfold]] = c(1:length(unlist(y)))[c(1:length(unlist(y)))%!in%c(folds[[nfold]])]
      }
    }
  }

  # creating the results dataframe
  if (family==('binary')){
    results_df = data.frame(matrix(ncol = (11+length(predictors))))
    colnames(results_df) = c('fold','nrow_train','nrow_test','ny_train','ny_test','AUC','sensitivity','specificity',
                             'accuracy','PPV','NPV',predictors)
  }
  if (family==('continuous')){
    results_df = data.frame(matrix(ncol = (10+length(predictors))))
    colnames(results_df) = c('fold','nrow_train','nrow_test','mean_y_train','mean_y_test','R2','R2_adjusted','RMSE','MSE',
                             'MAE',predictors)
  }

  # Creating list for models
  models = c()

  # Training and testing the elastic net
  for (entry in 1:length(analysis_list)){
    # getting the training and testing data
    y_train_entry = analysis_list[[entry]][[1]]
    y_test_entry= analysis_list[[entry]][[2]]
    x_train_entry= analysis_list[[entry]][[3]]
    x_test_entry= analysis_list[[entry]][[4]]

    # Stopping if there aren't enough observations in the training data
    if (is.null(stop_train)==F){
      if (sum(as.numeric(as.character(unlist(y_train_entry))))<stop_train){next}
    }

    #scaling numeric data
    if (scaling==T){
      for(variable in predictors_con){
        mean_variable = mean(as.numeric(unlist(x_train_entry[,variable])),na.rm=T)
        sd_variable = sd(as.numeric(unlist(x_train_entry[,variable])),na.rm=T)
        x_train_entry[,variable] = (as.numeric(unlist(x_train_entry[,variable]))-mean_variable)/sd_variable
        x_test_entry[,variable] = (as.numeric(unlist(x_test_entry[,variable]))-mean_variable)/sd_variable
      }
    }

    # removing variables with no variance from the training data
    for (name in colnames(x_train_entry)){
      if (length(unique(unlist(x_train_entry[,name])))<2){
        x_train_entry = x_train_entry[, !colnames(x_train_entry) %in% c(name)]
        x_test_entry = x_test_entry[, !colnames(x_test_entry) %in% c(name)]
      }
    }

    # identify binary data
    binary_predictors = colnames(x_train_entry)[which(apply(x_train_entry,2,function(x) { all(x %in% 0:1) })==T)]
    binary_predictors = subset(binary_predictors,binary_predictors%!in%colnames(x_train_entry)[grepl('numeric',sapply(x_train_entry,class))])

    # transforming to a data matrix
    x_train_entry = data.matrix(x_train_entry)
    x_test_entry = data.matrix(x_test_entry)

    # correcting dummy coded variables
    x_train_entry[,c(binary_predictors)]<- x_train_entry[,c(binary_predictors)]-1
    x_test_entry[,c(binary_predictors)]<- x_test_entry[,c(binary_predictors)]-1

    # finding best lambda and alpha

    # creating a variable for storing the crossvalidation results for the alphas and the lambdas
    MSEs = NULL

    # store variables for  ensr
    x_train_entry <<- x_train_entry
    y_train_entry <<- y_train_entry
    ensr_lambdas <<- ensr_lambdas
    ensr_cv <<- ensr_cv
    ensr_alphas <<- ensr_alphas

    # get ensr family
    ensr_family <<- ifelse(family=='binary','binomial','gaussian')

    for (repeated_cv_number in 1:repeated_cv){

      # setting the seed
      set.seed(repeated_cv_number)
      # selecting the best alpha and lambda for this seed
      ensr_obj = ensr(y =data.matrix(y_train_entry), x = x_train_entry,nlambda=ensr_lambdas,nfolds = ensr_cv,
                      alphas = ensr_alphas,family=ensr_family,standardize = F)
      ensr_obj_summary = summary(object = ensr_obj)

      # storing the results
      MSEs = cbind(MSEs,ensr_obj_summary$cvm)
    }

    # converting the cross validation results to a dataframe
    MSEs = as.data.frame(MSEs)
    MSEs$rowMeans = rowMeans(MSEs)

    # adding the alphas and lambdas that we used
    # these are the same for every seed!
    MSEs$lambdas = ensr_obj_summary$lambda
    MSEs$alphas= ensr_obj_summary$alpha
    MSEs = MSEs[order(MSEs$rowMeans,decreasing = F), ]

    # Selecting the  alpha and the lambda of the best model
    alpha.min = MSEs$alphas[1]
    lambda.min = MSEs$lambdas[1]

    # fitting the elastic net model and getting the estimates for the variables
    elastic_model = glmnet(y =data.matrix(y_train_entry), x = x_train_entry, family = ensr_family, alpha = alpha.min,
                           lambda=lambda.min,standardize = F)
    estimates = elastic_model$beta

    # having at least one parameter
    while (length(which(estimates[,1]!=0))<1){
      MSEs = MSEs[-1,]
      lambda.min = MSEs$lambdas[1]
      alpha.min = MSEs$alphas[1]
      elastic_model = glmnet(y =data.matrix(y_train_entry), x = x_train_entry, family = ensr_family,
                             alpha = alpha.min,lambda=lambda.min,standardize = F)
      estimates = elastic_model$beta
    }

    # calculate metrics

    # Stopping if there aren't enough observations in the training data
    if (is.null(stop_test)==F){
      if (sum(as.numeric(as.character(unlist(y_test_entry))))<stop_test){next}
    }

    if (family=='binary'){
      # AUC
      predictions = predict(elastic_model, newx=x_test_entry,type = "response")
      model_roc =  roc(unlist(y_test_entry),as.numeric(predictions),direction="<",quiet=T)
      model_coords = coords(model_roc,"best", ret=c("threshold", "specificity", "sensitivity"), transpose=FALSE)
      model_auc = auc(model_roc)

      # Sensitivity and specificity
      if (prefer_sensitivity==T){
        coords_to_pick = which(model_coords$sensitivity==max(model_coords$sensitivity))
      }
      else {
        coords_to_pick = which(model_coords$specificity==max(model_coords$specificity))
      }
      model_spec <- model_coords[coords_to_pick,2]
      model_sens <- model_coords[coords_to_pick,3]

      # store predictions
      predictions_all[test_indices[[entry]]] = predictions

      # accuracy, PPV, NPV
      predictions_bin = ifelse(predictions>model_coords$threshold[coords_to_pick],1,0)
      confmatrix <- confusionMatrix(as.factor(predictions_bin),as.factor(unlist(y_test_entry)),positive='1')

      # storing metrics
      results_df[entry,'fold']=entry
      results_df[entry,'nrow_train']=nrow(x_train_entry)
      results_df[entry,'nrow_test']=nrow(x_test_entry)
      results_df[entry,'ny_train']=sum(as.numeric(as.character(unlist(y_train_entry))))
      results_df[entry,'ny_test']=sum(as.numeric(as.character(unlist(y_test_entry))))
      results_df[entry,'AUC']=model_auc
      results_df[entry,'sensitivity']=model_sens
      results_df[entry,'specificity']=model_spec
      results_df[entry,'accuracy']=confmatrix$overall[1]
      results_df[entry,'PPV']=confmatrix$byClass[3]
      results_df[entry,'NPV']=confmatrix$byClass[4]

    }
    else if (family=='continuous'){

      # Getting the predictions
      predictions = predict(elastic_model, newx=x_test_entry,type = "response")
      if (is.null(pred_min)==F){predictions[predictions<pred_min]=pred_min}
      if (is.null(pred_max)==F){predictions[predictions>pred_max]=pred_max}

      # store predictions
      predictions_all[test_indices[[entry]]] = predictions

      # Getting R2, adjusted R2, RMSE, MSE, MAE
      R2 = 1-(sum((y_test_entry-predictions)^2)/length(unlist(y_test_entry)))/(sum((y_test_entry-mean(unlist(y_train_entry)))^2)/length(unlist(y_test_entry)))
      R2_adjusted = 1 - (1-R2)*(length(unlist(y_test_entry))-1)/(length(unlist(y_test_entry))-length(which(estimates!=0))-1)
      RMSE = RMSE(unlist(y_test_entry),predictions)
      MSE = RMSE^2
      MAE = MAE(unlist(y_test_entry),predictions)

      # storing metrics
      results_df[entry,'fold']=entry
      results_df[entry,'nrow_train']=nrow(x_train_entry)
      results_df[entry,'nrow_test']=nrow(x_test_entry)
      results_df[entry,'mean_y_train']=mean(unlist(y_train_entry))
      results_df[entry,'mean_y_test']=mean(unlist(y_test_entry))
      results_df[entry,'R2']=R2
      results_df[entry,'R2_adjusted']=R2_adjusted
      results_df[entry,'RMSE']=RMSE
      results_df[entry,'MSE']=MSE
      results_df[entry,'MAE']=MAE
    }

    # storing estimates
    for (predictor in predictors){
      index = which(rownames(estimates)==predictor)
      if (length(index)==0){
        results_df[entry,predictor]<- NA
      }
      else{
        results_df[entry,predictor]<- estimates[index]
      }
    }

    # Storing model
    models[[entry]]=elastic_model

  }

  # remove stored variables
  rm(x_train_entry,envir = .GlobalEnv)
  rm(y_train_entry,envir = .GlobalEnv)
  rm(ensr_lambdas,envir = .GlobalEnv)
  rm(ensr_cv,envir = .GlobalEnv)
  rm(ensr_alphas,envir = .GlobalEnv)

  # Create final results
  results = list()
  results$models = models
  results$metrics = results_df
  results$predictions = predictions_all

  # return results
  return(results)
}
elastic_net_wrapper_pooled <- function(data, outcome=NULL, by=NULL,predictors_con=NULL,predictors_cat=NULL,
                                       between_predictors_con=NULL,between_predictors_cat=NULL,
                                       split=80, outer_cv=NULL,stratified=T,scaling=T,repeated_cv=1,ensr_cv=10,
                                       ensr_alphas=seq(0, 1, length = 10),ensr_lambdas=100,seed=404,
                                       stop_test=NULL,shuffle=F,family='binary',pred_min=NULL,pred_max=NULL,prefer_sensitivity=T){
  # required packages
  `%!in%` = Negate(`%in%`)

  # combine predictors
  predictors = c(predictors_con,predictors_cat,between_predictors_con,between_predictors_cat)

  # create list of x and y datasets to be analyzed
  analysis_list = list()
  if (is.null(outer_cv)==T){
    analysis_list[[1]] = list()
    analysis_list[[1]][[1]] = data.frame()
    analysis_list[[1]][[2]] = data.frame()
    analysis_list[[1]][[3]] = list()
    analysis_list[[1]][[4]] = list()
  }
  else{
    for(nfold in 1:outer_cv){
      analysis_list[[nfold]] = list()
      analysis_list[[nfold]][[1]] = data.frame()
      analysis_list[[nfold]][[2]] = data.frame()
      analysis_list[[nfold]][[3]] = list()
      analysis_list[[nfold]][[4]] = list()
    }
  }

  # Loop over by
  for (by_index in 1:length(unique(unlist(data[by])))){
    by_entry = unique(unlist(data[by]))[by_index]
    data_by = subset(data,unlist(data[by])==by_entry)

    # shuffle dataset to lose time contingency for CV
    if (shuffle==T){
      set.seed(seed)
      data_by = data_by[sample(nrow(data_by)),]
    }

    # split data into y and x
    y_by = data_by[outcome]
    x_by = data_by[predictors]

    # split x and y into training and testing data
    if (is.null(outer_cv)==T){
      # performing stratified split
      if(stratified==T){
        if (family=='binary'){
          # get indices
          set.seed(seed)
          my_train_ind_no_y =  sample(which(y_by==0), size = split/100*length(which(y_by==0)))
          set.seed(seed)
          my_train_ind_y =  sample(which(y_by==1), size = split/100*length(which(y_by==1)))
          # split data
          y_train_by = y_by[c(my_train_ind_no_y,my_train_ind_y),]
          y_test_by =y_by[-c(my_train_ind_no_y,my_train_ind_y),]
          x_train_by = x_by[c(my_train_ind_no_y,my_train_ind_y),]
          x_test_by = x_by[-c(my_train_ind_no_y,my_train_ind_y),]
        }
        else if (family=='continuous'){
          # get indices
          set.seed(seed)
          my_train_ind =  createDataPartition(as.matrix(y_by), p = split/100, list = T,groups=min(3,nrow(y_by)))
          # split data
          y_train_by = y_by[c(unlist(my_train_ind)),]
          y_test_by =y_by[-c(unlist(my_train_ind)),]
          x_train_by = x_by[c(unlist(my_train_ind)),]
          x_test_by = x_by[-c(unlist(my_train_ind)),]
        }
        #scaling numeric data on the within level
        if (scaling==T){
          for(variable in predictors_con){
            mean_variable = mean(as.numeric(unlist(x_train_by[,variable])),na.rm=T)
            sd_variable = sd(as.numeric(unlist(x_train_by[,variable])),na.rm=T)
            if(sd_variable==0){
              x_train_by[,variable] = as.numeric(unlist(x_train_by[,variable]))-mean_variable
              x_test_by[,variable] = as.numeric(unlist(x_test_by[,variable]))-mean_variable
              next}
            x_train_by[,variable] = (as.numeric(unlist(x_train_by[,variable]))-mean_variable)/sd_variable
            x_test_by[,variable] = (as.numeric(unlist(x_test_by[,variable]))-mean_variable)/sd_variable
          }
        }
        # add to analysis list
        analysis_list[[1]][[1]][(nrow(analysis_list[[1]][[1]])+1):(nrow(analysis_list[[1]][[1]])+length(y_train_by)),1] = y_train_by
        analysis_list[[1]][[2]][(nrow(analysis_list[[1]][[2]])+1):(nrow(analysis_list[[1]][[2]])+nrow(x_train_by)),1:ncol(x_train_by)] = x_train_by
        analysis_list[[1]][[3]][[by_index]] = y_test_by
        analysis_list[[1]][[4]][[by_index]] = x_test_by
      }
      # performing non-stratified split
      else if(stratified==F){
        set.seed(seed)
        my_train_ind =  sample(c(1:nrow(y_by)), size = split/100*nrow(y_by))
        y_train_by = y_by[c(my_train_ind),]
        y_test_by =y_by[-c(my_train_ind),]
        x_train_by = x_by[c(my_train_ind),]
        x_test_by = x_by[-c(my_train_ind),]

        #scaling numeric data on the within level
        if (scaling==T){
          for(variable in predictors_con){
            mean_variable = mean(as.numeric(unlist(x_train_by[,variable])),na.rm=T)
            sd_variable = sd(as.numeric(unlist(x_train_by[,variable])),na.rm=T)
            if(sd_variable==0){
              x_train_by[,variable] = as.numeric(unlist(x_train_by[,variable]))-mean_variable
              x_test_by[,variable] = as.numeric(unlist(x_test_by[,variable]))-mean_variable
              next}
            x_train_by[,variable] = (as.numeric(unlist(x_train_by[,variable]))-mean_variable)/sd_variable
            x_test_by[,variable] = (as.numeric(unlist(x_test_by[,variable]))-mean_variable)/sd_variable
          }
        }
        # add to analysis list
        analysis_list[[1]][[1]][(nrow(analysis_list[[1]][[1]])+1):(nrow(analysis_list[[1]][[1]])+length(y_train_by)),1] = y_train_by
        analysis_list[[1]][[2]][(nrow(analysis_list[[1]][[2]])+1):(nrow(analysis_list[[1]][[2]])+nrow(x_train_by)),1:ncol(x_train_by)] = x_train_by
        analysis_list[[1]][[3]][[by_index]] = y_test_by
        analysis_list[[1]][[4]][[by_index]] = x_test_by
      }
    }
    # creating datasets for cross-validation
    else {
      if(stratified==T){
        # creating folds
        set.seed(seed)
        folds <- create_folds(as.numeric(unlist(y_by)),k = outer_cv,type='stratified')
        # creating datasets
        for(nfold in 1:length(folds)){
          y_train_by <- y_by[c(folds[[nfold]]), ]
          y_test_by <- y_by[-c(folds[[nfold]]), ]
          x_train_by <- x_by[c(folds[[nfold]]), ]
          x_test_by <- x_by[-c(folds[[nfold]]), ]

          #scaling numeric data on the within level
          if (scaling==T){
            for(variable in predictors_con){
              mean_variable = mean(as.numeric(unlist(x_train_by[,variable])),na.rm=T)
              sd_variable = sd(as.numeric(unlist(x_train_by[,variable])),na.rm=T)
              if(sd_variable==0){
                x_train_by[,variable] = as.numeric(unlist(x_train_by[,variable]))-mean_variable
                x_test_by[,variable] = as.numeric(unlist(x_test_by[,variable]))-mean_variable
                next}
              x_train_by[,variable] = (as.numeric(unlist(x_train_by[,variable]))-mean_variable)/sd_variable
              x_test_by[,variable] = (as.numeric(unlist(x_test_by[,variable]))-mean_variable)/sd_variable
            }
          }
          # add to analysis list
          analysis_list[[nfold]][[1]][(nrow(analysis_list[[nfold]][[1]])+1):(nrow(analysis_list[[nfold]][[1]])+length(y_train_by)),1] = y_train_by
          analysis_list[[nfold]][[2]][(nrow(analysis_list[[nfold]][[2]])+1):(nrow(analysis_list[[nfold]][[2]])+nrow(x_train_by)),1:ncol(x_train_by)] = x_train_by
          analysis_list[[nfold]][[3]][[by_index]] = y_test_by
          analysis_list[[nfold]][[4]][[by_index]] = x_test_by
        }
      }
      else if(stratified==F){
        set.seed(seed)
        folds <- create_folds(as.numeric(unlist(y_by)),k = outer_cv,type='basic')
        for(nfold in 1:length(folds)){
          y_train_by <- y_by[c(folds[[nfold]]), ]
          y_test_by <- y_by[-c(folds[[nfold]]), ]
          x_train_by <- x_by[c(folds[[nfold]]), ]
          x_test_by <- x_by[-c(folds[[nfold]]), ]

          #scaling numeric data on the within level
          if (scaling==T){
            for(variable in predictors_con){
              mean_variable = mean(as.numeric(unlist(x_train_by[,variable])),na.rm=T)
              sd_variable = sd(as.numeric(unlist(x_train_by[,variable])),na.rm=T)
              if(sd_variable==0){
                x_train_by[,variable] = as.numeric(unlist(x_train_by[,variable]))-mean_variable
                x_test_by[,variable] = as.numeric(unlist(x_test_by[,variable]))-mean_variable
                next}
              x_train_by[,variable] = (as.numeric(unlist(x_train_by[,variable]))-mean_variable)/sd_variable
              x_test_by[,variable] = (as.numeric(unlist(x_test_by[,variable]))-mean_variable)/sd_variable
            }
          }
          # add to analysis list
          analysis_list[[nfold]][[1]][(nrow(analysis_list[[nfold]][[1]])+1):(nrow(analysis_list[[nfold]][[1]])+length(y_train_by)),1] = y_train_by
          analysis_list[[nfold]][[2]][(nrow(analysis_list[[nfold]][[2]])+1):(nrow(analysis_list[[nfold]][[2]])+nrow(x_train_by)),1:ncol(x_train_by)] = x_train_by
          analysis_list[[nfold]][[3]][[by_index]] = y_test_by
          analysis_list[[nfold]][[4]][[by_index]] = x_test_by
        }
      }
    }
  }

  # Scaling numeric data on the between level
  if (scaling==T){
    for (entry in 1:length(analysis_list)){
      for(variable in between_predictors_con){
        mean_variable = mean(as.numeric(unlist(analysis_list[[entry]][[2]][,variable])),na.rm=T)
        sd_variable = sd(as.numeric(unlist(analysis_list[[entry]][[2]][,variable])),na.rm=T)
        if(sd_variable==0){
          analysis_list[[entry]][[2]][,variable] = as.numeric(unlist(analysis_list[[entry]][[2]][,variable]))-mean_variable
          analysis_list[[entry]][[4]] = lapply(analysis_list[[entry]][[4]], function (x){x[,variable]=(x[,variable]-mean_variable)/sd_variable
          return(x)})
          next}
        analysis_list[[entry]][[2]][,variable] = (as.numeric(unlist(analysis_list[[entry]][[2]][,variable]))-mean_variable)/sd_variable
        analysis_list[[entry]][[4]] = lapply(analysis_list[[entry]][[4]], function (x){x[,variable]=(x[,variable]-mean_variable)/sd_variable
        return(x)})
      }
    }
  }

  # creating the results dataframe
  results_list = list()

  if (family==('binary')){
    results_df_model = data.frame(matrix(ncol = (3+length(predictors))))
    colnames(results_df_model) = c('fold','nrow_train','ny_train',predictors)
    results_df_pooled = data.frame(matrix(ncol = (10)))
    colnames(results_df_pooled) = c('by','fold','nrow_test','ny_test','AUC','sensitivity','specificity',
                                    'accuracy','PPV','NPV')
  }
  else if (family==('continuous')){
    results_df_model = data.frame(matrix(ncol = (3+length(predictors))))
    colnames(results_df_model) = c('fold','nrow_train','mean_y_train',predictors)
    results_df_pooled = data.frame(matrix(ncol = (9)))
    colnames(results_df_pooled) = c('by','fold','nrow_test','mean_y_test','R2','R2_adjusted','RMSE','MSE',
                                    'MAE')
  }

  # Training and testing the elastic net
  for (entry in 1:length(analysis_list)){

    # getting the training and testing data
    y_train_entry = analysis_list[[entry]][[1]]
    x_train_entry= analysis_list[[entry]][[2]]

    # identify binary data
    binary_predictors = colnames(x_train_entry)[which(apply(x_train_entry,2,function(x) { all(x %in% 0:1) })==T)]
    binary_predictors = subset(binary_predictors,binary_predictors%!in%colnames(x_train_entry)[grepl('numeric',sapply(x_train_entry,class))])

    # transforming to a data matrix
    x_train_entry = data.matrix(x_train_entry)

    # correcting dummy coded variables
    x_train_entry[,c(binary_predictors)]<- x_train_entry[,c(binary_predictors)]-1

    # removing variables with no variance from the training data
    removed_names = c()
    for (name in colnames(x_train_entry)){
      if (length(unique(x_train_entry[,name]))<2){
        x_train_entry = x_train_entry[, !colnames(x_train_entry) %in% c(name)]
        removed_names[(length(removed_names)+1)]=name
      }
    }

    # finding best lambda and alpha

    # creating a variable for storing the crossvalidation results for the alphas and the lambdas
    MSEs <- NULL

    # store variables for  ensr
    x_train_entry <<- x_train_entry
    y_train_entry <<- y_train_entry
    ensr_lambdas <<- ensr_lambdas
    ensr_cv <<- ensr_cv
    ensr_alphas <<- ensr_alphas

    # get ensr family
    ensr_family <<- ifelse(family=='binary','binomial','gaussian')

    for (repeated_cv_number in 1:repeated_cv){

      # setting the seed
      set.seed(repeated_cv_number)
      # selecting the best alpha and lambda for this seed
      ensr_obj <- ensr(y =data.matrix(y_train_entry), x = x_train_entry,nlambda=ensr_lambdas,nfolds = ensr_cv,
                       alphas = ensr_alphas,family=ensr_family,standardize = F)
      ensr_obj_summary <- summary(object = ensr_obj)

      # storing the results
      MSEs <- cbind(MSEs,ensr_obj_summary$cvm)
    }

    # converting the cross validation results to a dataframe
    MSEs <- as.data.frame(MSEs)
    MSEs$rowMeans <- rowMeans(MSEs)

    # adding the alphas and lambdas that we used
    # these are the same for every seed!
    MSEs$lambdas <- ensr_obj_summary$lambda
    MSEs$alphas<- ensr_obj_summary$alpha
    MSEs <- MSEs[order(MSEs$rowMeans,decreasing = F), ]

    # Selecting the  alpha and the lambda of the best model
    alpha.min <- MSEs$alphas[1]
    lambda.min <- MSEs$lambdas[1]

    # fitting the elastic net model and getting the estimates for the variables
    elastic_model <- glmnet(y =data.matrix(y_train_entry), x = x_train_entry, family = ensr_family, alpha = alpha.min,
                            lambda=lambda.min,standardize = F)
    estimates <- elastic_model$beta

    # having at least one parameter
    while (length(which(estimates[,1]!=0))<1){
      MSEs <- MSEs[-1,]
      lambda.min <- MSEs$lambdas[1]
      alpha.min <- MSEs$alphas[1]
      elastic_model <- glmnet(y =data.matrix(y_train_entry), x = x_train_entry, family = ensr_family,
                              alpha = alpha.min,lambda=lambda.min,standardize = F)
      estimates <- elastic_model$beta
    }

    # Store predictors model
    if (family==('binary')){
      results_df_model[entry,'fold'] = entry
      results_df_model[entry,'nrow_train']=nrow(x_train_entry)
      results_df_model[entry,'ny_train']=sum(as.numeric(as.character(unlist(y_train_entry))))
    }
    else if (family==('continuous')){
      results_df_model[entry,'fold'] = entry
      results_df_model[entry,'nrow_train']=nrow(x_train_entry)
      results_df_model[entry,'mean_y_train']=mean(as.numeric(as.character(unlist(y_train_entry))))
    }

    # storing estimates
    for (predictor in predictors){
      index = which(rownames(estimates)==predictor)
      if (length(index)==0){
        results_df_model[entry,predictor]<- NA
      }
      else{
        results_df_model[entry,predictor]<- estimates[index]
      }
    }

    # calculate metrics model
    for (by_index in 1:length(unique(unlist(data[by])))){
      by_entry = unique(unlist(data[by]))[by_index]
      # getting test data
      y_test_by = analysis_list[[entry]][[3]][[by_index]]
      x_test_by = analysis_list[[entry]][[4]][[by_index]]

      # Stopping if there aren't enough observations in the training data
      if (is.null(stop_test)==F){
        if (family == 'binary'){if (sum(as.numeric(as.character(unlist(y_test_by))))<stop_test){next}}
        else if (family=='continuous'){if (length(unlist(y_test_by))<stop_test){next}}
      }

      # having at least two levels
      if(length(unique(unlist(y_test_by)))<2){next}

      # transforming to a data matrix
      x_test_by = data.matrix(x_test_by)

      # correcting dummy coded variables
      x_test_by[,c(binary_predictors)]<- x_test_by[,c(binary_predictors)]-1

      # remove variables which were removed before
      for (name in removed_names){
        x_test_by = x_test_by[, !colnames(x_test_by) %in% c(name)]
      }

      if (family=='binary'){
        # AUC
        predictions = predict(elastic_model, newx=x_test_by,type = "response")
        model_roc =  roc(unlist(y_test_by),as.numeric(predictions),direction="<",quiet=T)
        model_coords = coords(model_roc,"best", ret=c("threshold", "specificity", "sensitivity"), transpose=FALSE)
        model_auc = auc(model_roc)

        # Sensitivity and specificity
        if (prefer_sensitivity==T){
          coords_to_pick = which(model_coords$sensitivity==max(model_coords$sensitivity))
        }
        else {
          coords_to_pick = which(model_coords$specificity==max(model_coords$specificity))
        }
        model_spec <- model_coords[coords_to_pick,2]
        model_sens <- model_coords[coords_to_pick,3]

        # accuracy, PPV, NPV
        predictions_bin = ifelse(predictions>model_coords$threshold[coords_to_pick],1,0)
        confmatrix <- confusionMatrix(as.factor(predictions_bin),as.factor(unlist(y_test_by)),positive='1')

        # storing metrics
        results_df_pooled[((entry-1)*length(unique(unlist(data[by])))+by_index),'by']=by_entry
        results_df_pooled[((entry-1)*length(unique(unlist(data[by])))+by_index),'fold']=entry
        results_df_pooled[((entry-1)*length(unique(unlist(data[by])))+by_index),'nrow_test']=nrow(x_test_by)
        results_df_pooled[((entry-1)*length(unique(unlist(data[by])))+by_index),'ny_test']=sum(as.numeric(as.character(unlist(y_test_by))))
        results_df_pooled[((entry-1)*length(unique(unlist(data[by])))+by_index),'AUC']=model_auc
        results_df_pooled[((entry-1)*length(unique(unlist(data[by])))+by_index),'sensitivity']=model_sens
        results_df_pooled[((entry-1)*length(unique(unlist(data[by])))+by_index),'specificity']=model_spec
        results_df_pooled[((entry-1)*length(unique(unlist(data[by])))+by_index),'accuracy']=confmatrix$overall[1]
        results_df_pooled[((entry-1)*length(unique(unlist(data[by])))+by_index),'PPV']=confmatrix$byClass[3]
        results_df_pooled[((entry-1)*length(unique(unlist(data[by])))+by_index),'NPV']=confmatrix$byClass[4]
      }
      else if (family=='continuous'){

        # Making predictions
        predictions = predict(elastic_model, newx=x_test_by,type = "response")

        if (is.null(pred_min)==F){predictions[predictions<pred_min]=pred_min}
        if (is.null(pred_max)==F){predictions[predictions>pred_max]=pred_max}

        # Getting R2, adjusted R2, RMSE, MSE, MAE
        R2 = 1-(sum((y_test_by-predictions)^2)/length(y_test_by))/(sum((y_test_by-mean(unlist(y_train_entry)))^2)/length(y_test_by))
        R2_adjusted = 1 - (1-R2)*(length(y_test_by)-1)/(length(y_test_by)-length(which(estimates!=0))-1)
        RMSE = RMSE(y_test_by,predictions)
        MSE = RMSE^2
        MAE = MAE(y_test_by,predictions)

        # storing metrics
        results_df_pooled[((entry-1)*length(unique(unlist(data[by])))+by_index),'by']=by_entry
        results_df_pooled[((entry-1)*length(unique(unlist(data[by])))+by_index),'fold']=entry
        results_df_pooled[((entry-1)*length(unique(unlist(data[by])))+by_index),'nrow_test']=nrow(x_test_by)
        results_df_pooled[((entry-1)*length(unique(unlist(data[by])))+by_index),'mean_y_test']=mean(y_test_by)
        results_df_pooled[((entry-1)*length(unique(unlist(data[by])))+by_index),'R2']=R2
        results_df_pooled[((entry-1)*length(unique(unlist(data[by])))+by_index),'R2_adjusted']=R2_adjusted
        results_df_pooled[((entry-1)*length(unique(unlist(data[by])))+by_index),'RMSE']=RMSE
        results_df_pooled[((entry-1)*length(unique(unlist(data[by])))+by_index),'MSE']=MSE
        results_df_pooled[((entry-1)*length(unique(unlist(data[by])))+by_index),'MAE']=MAE
      }
    }

    # remove stored variables
    rm(x_train_entry,envir = .GlobalEnv)
    rm(y_train_entry,envir = .GlobalEnv)
    rm(ensr_lambdas,envir = .GlobalEnv)
    rm(ensr_cv,envir = .GlobalEnv)
    rm(ensr_alphas,envir = .GlobalEnv)
  }

  results_list$results_pooled_model=results_df_model
  results_list$results_by=results_df_pooled

  # return df
  return(results_list)
}

NLML_plot <- function(results_estimates,percentile=0.90,range=TRUE,title=NULL,subtitle=NULL,xlim=NULL,ylab='Predictor',xlab='Estimate', gradient_values= c(-1.5,-1,-0.5,0,0.25,0.5,0.75,1,1.5,2,2.5)){

  # Summarize results
  results_estimates_long <- gather(results_estimates, variable, estimate, colnames(results_estimates)[1]:colnames(results_estimates)[ncol(results_estimates)], factor_key=TRUE)
  summary_estimates <- results_estimates_long %>%
    group_by(variable) %>%
    summarise(mean_estimate = mean(estimate,na.rm=T),P025 = quantile(estimate, 0.025,na.rm=TRUE),P975 = quantile(estimate, 0.975,na.rm=TRUE))

  # Set colors
  green_red_palette <- brewer_pal(type = "div", palette = "RdYlGn")(11)
  green_red_palette_dark <- colorspace::darken(green_red_palette, amount = 0.3)
  green_red_palette_dark_less <- colorspace::darken(green_red_palette, amount = 0.075)

  # Set limits
  if (is.null(xlim)==T){
    xlim = c(min(summary_estimates$P025),max(summary_estimates$P975))
  }

  # Make base plot
  base_plot <- ggplot(summary_estimates[abs(summary_estimates$mean_estimate)>=
                                          quantile(abs(summary_estimates$mean_estimate),percentile,na.rm=TRUE),],
                      aes(x=mean_estimate,y=fct_reorder(variable,mean_estimate)))

  # Add 95% segments
  if (range==TRUE){
    base_plot <- base_plot + geom_segment(aes(x=P025,xend=P975,yend=variable,color=mean_estimate),linewidth=3)
  }

  # Finish plot
  plot <- base_plot +

    # Add 0 line
    geom_vline(xintercept = 0, lty = 2, linewidth = 0.2) +

    # Add means
    geom_point(aes(fill = mean_estimate),pch = 21,size=3) +

    # Change colors
    scale_fill_gradientn(colours = green_red_palette_dark,values = gradient_values)+
    scale_color_gradientn(colours = green_red_palette_dark_less,values = gradient_values)+

    # Remove background
    theme_bw()+

    # Set names of the x and y axis
    xlab(xlab)+
    ylab(ylab)+

    # Set the x-axis at the top
    scale_x_continuous(position = "top")+

    # Change themes
    theme(legend.position="none",
          axis.text.y = element_text(size = 8,face = "bold"),
          axis.ticks.y = element_blank(),
          axis.line = element_blank(),
          plot.title = element_text(hjust = 0.5,face = "bold"),
          plot.subtitle = element_text(hjust = 0.5,face = "bold"))+

    # Correct zoom x
    coord_cartesian(xlim=xlim)+

    # Title
    ggtitle(title,subtitle = subtitle)

  # Return plot
  return(plot)
}

coefficient_ranker <- function(data){
  data_ranked = as.data.frame(t(apply(data,1,function(x){
    x[x==0]=NA
    x[x<0&is.na(x)==F]=-order(x[x<0&is.na(x)==F])
    x[x>0&is.na(x)==F]=order(x[x>0&is.na(x)==F])
    x
  })))
  return(data_ranked)
}

NLML_plot_ranked_split <- function(results_estimates,percentile=0.90,range=TRUE,title=NULL,subtitle=NULL,xlim=NULL,ylab='Predictor',xlab='Estimate', gradient_values= c(-1.5,-1,-0.5,0,0.25,0.5,0.75,1,1.5,2,2.5)){

  # Summarize results
  results_estimates_long <- gather(results_estimates, variable, estimate, colnames(results_estimates)[1]:colnames(results_estimates)[ncol(results_estimates)], factor_key=TRUE)
  summary_estimates_positive <- subset(results_estimates_long,estimate>0) %>%
    group_by(variable) %>%
    summarise(mean_estimate = mean(estimate,na.rm=T),P025 = quantile(estimate, 0.025,na.rm=TRUE),P975 = quantile(estimate, 0.975,na.rm=TRUE))
  summary_estimates_negative <- subset(results_estimates_long,estimate<0) %>%
    group_by(variable) %>%
    summarise(mean_estimate = mean(estimate,na.rm=T),P025 = quantile(estimate, 0.025,na.rm=TRUE),P975 = quantile(estimate, 0.975,na.rm=TRUE))
  summary_estimates <- rbind(summary_estimates_positive,summary_estimates_negative)

  # Set colors
  green_red_palette <- brewer_pal(type = "div", palette = "RdYlGn")(11)
  green_red_palette_dark <- colorspace::darken(green_red_palette, amount = 0.3)
  green_red_palette_dark_less <- colorspace::darken(green_red_palette, amount = 0.075)

  # Set limits
  if (is.null(xlim)==T){
    xlim = c(min(summary_estimates$P025),max(summary_estimates$P975))
  }

  # Make base plot
  base_plot <- ggplot(summary_estimates[abs(summary_estimates$mean_estimate)<=
                                          quantile(abs(summary_estimates$mean_estimate),(1-percentile),na.rm=TRUE),],
                      aes(x=mean_estimate,y=fct_reorder(variable,mean_estimate)))

  # Add 95% segments
  if (range==TRUE){
    base_plot <- base_plot + geom_segment(aes(x=P025,xend=P975,yend=variable,color=mean_estimate),linewidth=3)
  }

  # Finish plot
  plot <- base_plot +

    # Add 0 line
    geom_vline(xintercept = 0, lty = 2, linewidth = 0.2) +

    # Add means
    geom_point(aes(fill = mean_estimate),pch = 21,size=3) +

    # Change colors
    scale_fill_gradientn(colours = green_red_palette_dark,values = gradient_values)+
    scale_color_gradientn(colours = green_red_palette_dark_less,values = gradient_values)+

    # Remove background
    theme_bw()+

    # Set names of the x and y axis
    xlab(xlab)+
    ylab(ylab)+

    # Set the x-axis at the top
    scale_x_continuous(position = "top")+

    # Change themes
    theme(legend.position="none",
          axis.text.y = element_text(size = 8,face = "bold"),
          axis.ticks.y = element_blank(),
          axis.line = element_blank(),
          plot.title = element_text(hjust = 0.5,face = "bold"),
          plot.subtitle = element_text(hjust = 0.5,face = "bold"))+

    # Correct zoom x
    coord_cartesian(xlim=xlim)+

    # Title
    ggtitle(title,subtitle = subtitle)

  # Return plot
  return(plot)
}
