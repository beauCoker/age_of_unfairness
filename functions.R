compute_features = function(person_id,
                            screening_date,
                            first_offense_date,
                            current_offense_date,
                            offenses_within_30, 
                            before_cutoff_date,
                            arrest,
                            charge,
                            jail,
                            prison,
                            prob,
                            people) {
  ### Computes features (e.g., number of priors) for each person_id/screening_date.

  # pmap coerces dates to numbers so convert back to date.
  first_offense_date = as_date(first_offense_date)
  screening_date = as_date(screening_date)
  current_offense_date = as_date(current_offense_date) 
  
  out = list()
  
  ### ID information
  out$person_id = person_id
  out$screening_date = screening_date
  
  ### Other features
  
  # Number of felonies
  out$p_felony_count_person = ifelse(is.null(charge), 0, sum(charge$is_felony, na.rm = TRUE))
  
  # Number of misdemeanors
  out$p_misdem_count_person  = ifelse(is.null(charge), 0, sum(charge$is_misdem, na.rm = TRUE))
  
  # Number of violent charges
  out$p_charge_violent  = ifelse(is.null(charge), 0, sum(charge$is_violent, na.rm = TRUE))
  
  #p_current_age: Age at screening date
  out$p_current_age = floor(as.numeric(as.period(interval(people$dob,screening_date)), "years"))
  
  #p_age_first_offense: Age at first offense 
  out$p_age_first_offense = floor(as.numeric(as.period(interval(people$dob,first_offense_date)), "years"))
  
  ### History of Violence
  
  #p_juv_fel_count
  # out$p_juv_fel_count = ifelse(is.null(charge), 0, sum(charge$is_felony & charge$is_juv,na.rm=TRUE))
  if(is.null(charge)){
    out$p_juv_fel_count = 0 
    out$p_felprop_violarrest = 0
    out$p_murder_arrest = 0
    out$p_felassault_arrest = 0 
    out$p_misdemassault_arrest = 0
    out$p_famviol_arrest = 0 
    out$p_sex_arrest = 0
    out$p_weapons_arrest = 0
  }
  else{
    # p_juv_fel_count
    juv_fel_arrests = charge %>% 
      group_by(date_charge_filed) %>% 
      summarize(juv_charges = sum(is_juv, na.rm=True), 
                fel_charges = sum(is_felony), na.rm = True) 

    out$p_juv_fel_count = sum(juv_fel_arrests$juv_charges > 0 & juv_fel_arrests$fel_charges > 0,na.rm=TRUE)
   
    #p_felprop_violarrest
    out$p_felprop_violarrest = sum(charge$is_felprop_violarrest, na.rm = TRUE)
    
    #p_murder_arrest
    out$p_murder_arrest = sum(charge$is_murder, na.rm = TRUE)
    
    #p_felassault_arrest
    out$p_felassault_arrest = sum(charge$is_felassault_arrest, na.rm = TRUE)
    
    #p_misdemassault_arrest
    out$p_misdemassault_arrest = sum(charge$is_misdemassault_arrest, na.rm = TRUE)
    
    #p_famviol_arrest
    out$p_famviol_arrest = sum(charge$is_family_violence, na.rm = TRUE)
    
    #p_sex_arrest
    out$p_sex_arrest = sum(charge$is_sex_offense, na.rm = TRUE)
    
    #p_weapons_arrest
    out$p_weapons_arrest =  sum(charge$is_weapons, na.rm = TRUE)
    
  }
  

  # #p_felprop_violarrest
  # out$p_felprop_violarrest = ifelse(is.null(charge), 0,sum(charge$is_felprop_violarrest, na.rm = TRUE))
  # 
  # #p_murder_arrest
  # out$p_murder_arrest = ifelse(is.null(charge), 0, sum(charge$is_murder, na.rm = TRUE))
  # 
  # #p_felassault_arrest
  # out$p_felassault_arrest = ifelse(is.null(charge), 0, sum(charge$is_felassault_arrest, na.rm = TRUE))
  # 
  # #p_misdemassault_arrest
  # out$p_misdemassault_arrest = ifelse(is.null(charge), 0, sum(charge$is_misdemassault_arrest, na.rm = TRUE))
  # 
  # #p_famviol_arrest
  # out$p_famviol_arrest = ifelse(is.null(charge), 0, sum(charge$is_family_violence, na.rm = TRUE))
  # 
  # #p_sex_arrest
  # out$p_sex_arrest = ifelse(is.null(charge), 0, sum(charge$is_sex_offense, na.rm = TRUE))
  # 
  # #p_weapons_arrest
  # out$p_weapons_arrest =  ifelse(is.null(charge), 0, sum(charge$is_weapons, na.rm = TRUE))
  
  ### History of Non-Compliance
  
  # Number of offenses while on probation
  out$p_n_on_probation = ifelse(is.null(charge) | is.null(prob), 0, count_on_probation(charge,prob))
  
  # Whether or not current offense was while on probation (two ways)
  if(is.null(prob)){
    out$p_current_on_probation = 0
  } else if(is.na(current_offense_date)) {
    out$p_current_on_probation = NA
  } else {
    out$p_current_on_probation = if_else(count_on_probation(data.frame(offense_date=current_offense_date),prob)>0,1,0)
  }
  
  # Number of times provation was violated or revoked
  out$p_prob_revoke =  ifelse(is.null(prob), 0, sum(prob$is_revoke==1 & prob$EventDate < current_offense_date))
  
  ### Criminal Involvement
  
  # Number of charges / arrests
  out$p_charge = ifelse(is.null(charge), 0, nrow(charge))
  out$p_arrest = ifelse(is.null(arrest), 0, length(unique(arrest$arrest_date)))
  
  # Number of times sentenced to jail/prison 30 days or more
  out$p_jail30 = ifelse(is.null(prison), 0, sum(jail$sentence_days >= 30, na.rm=TRUE))
  out$p_prison30 = ifelse(is.null(prison), 0, sum(prison$sentence_days >= 30, na.rm=TRUE))
  
  # Number of prison sentences
  out$p_prison =  ifelse(is.null(prison), 0, nrow(prison))
  
  # Number of times on probation
  out$p_probation =  ifelse(is.null(prob), 0, sum(prob$prob_event=="On" & prob$EventDate < current_offense_date, na.rm = TRUE))
  
  return(out)
}

compute_features_on = function(person_id,
                               screening_date,
                               first_offense_date,
                               current_offense_date,
                               offenses_within_30,
                               before_cutoff_date,
                               arrest,
                               charge,
                               jail,
                               prison,
                               prob,
                               people) {
  ### Computes features related to current offense
  
  # pmap coerces dates to numbers so convert back to date.
  first_offense_date = as_date(first_offense_date)
  screening_date = as_date(screening_date)
  current_offense_date = as_date(current_offense_date) 
  
  out = list()
  
  ### ID information
  out$person_id = person_id
  out$screening_date = screening_date
  
  out$is_misdem = ifelse(is.null(charge), NA, if_else(any(charge$is_misdem==1) & all(charge$is_felony==0),1,0))
  
  return(out)
}

compute_outcomes = function(person_id,
                            screening_date,
                            first_offense_date,
                            current_offense_date,
                            before_cutoff_date,
                            arrest,
                            charge,
                            jail,
                            prison,
                            prob,
                            people){
  
  out = list()
  
  # pmap coerces dates to numbers so convert back to date.
  first_offense_date = as_date(first_offense_date)
  screening_date = as_date(screening_date)
  current_offense_date = as_date(current_offense_date)
  
  ### ID information
  out$person_id = person_id
  out$screening_date = screening_date
  
  if(is.null(charge)) {
    out$recid = 0
    out$recid_violent = 0
    
  } else {
    
    # Sort charges in ascending order
    charge = charge %>% dplyr::arrange(offense_date)
    
    # General recidivism
    date_next_offense = charge$offense_date[1]
    years_next_offense = as.numeric(as.period(interval(screening_date,date_next_offense)), "years")
    out$recid = if_else(years_next_offense <= 2, 1, 0)
    
    # Violent recidivism
    date_next_offense_violent = filter(charge,is_violent==1)$offense_date[1]
    if(is.na(date_next_offense_violent)) {
      out$recid_violent = 0
    } else {
      years_next_offense_violent = as.numeric(as.period(interval(screening_date,date_next_offense_violent)), "years")
      out$recid_violent = if_else(years_next_offense_violent <= 2, 1, 0)
    }
  }
  
  return(out)
}

count_on_probation = function(charge, prob){
  
  # Make sure prob is sorted in ascending order of EventDate
  
  u_charge = charge %>%
    group_by(offense_date) %>%
    summarize(count = n()) %>%
    mutate(rank = findInterval(as.numeric(offense_date), as.numeric(prob$EventDate)))  %>%
    group_by(rank) %>%
    mutate(
      event_before = ifelse(rank==0, NA, prob$prob_event[rank]),
      days_before = ifelse(rank==0, NA, floor(as.numeric(as.period(interval(prob$EventDate[rank],offense_date)), "days"))),
      event_after = ifelse(rank==nrow(prob), NA, prob$prob_event[rank+1]),
      days_after = ifelse(rank==nrow(prob),NA, floor(as.numeric(as.period(interval(offense_date, prob$EventDate[rank+1])), "days")))
    ) %>%
    mutate(is_on_probation = pmap(list(event_before, days_before, event_after, days_after), .f=classify_charge)) %>%
    unnest()
  
  return(sum(u_charge$count[u_charge$is_on_probation]))
}



classify_charge = function(event_before, days_before, event_after, days_after,
                           thresh_days_before=365, thresh_days_after=30) {
  
  if (is.na(event_before)) {
    # No events before
    if (event_after == "Off" & days_after <= thresh_days_after) {
      return(TRUE)
    }
    
  } else if (is.na(event_after)) {
    # No events after
    if (event_before == "On" & days_before <= thresh_days_before) {
      return(TRUE)
    }
  }
  
  else { # Neither event is NA
    
    if (event_before=="On" & event_after=="Off") {
      return(TRUE)
      
    } else if (event_before=="On" & days_before <= thresh_days_before & event_after=="On") {
      return(TRUE)
      
    } else if (event_before=="Off" & event_after=="Off" & days_after <= thresh_days_after) {
      return(TRUE)
    } 
  }
  return(FALSE)
}


rmse = function(y, yhat) {
  sqrt(mean((y-yhat)^2))
}

fit_xgboost <- function(train, param) {
  ###
  # Cross validates each combination of parameters in param and returns best model
  # param is a list of xgboost parameters as vectors
  # train is formatted for xgboost input
  ###
  
  param_df = expand.grid(param) # Each row is a set of parameters to be cross validated
  n_param = nrow(param_df)
  
  ## Allocate space for performance statistics (and set seeds)
  performance = data.frame(
    i_param = 1:n_param,
    seed = sample.int(10000, n_param),
    matrix(NA,nrow=2,ncol=5,
           dimnames=list(NULL,
                         c("iter","train_rmse_mean","train_rmse_std","test_rmse_mean","test_rmse_std"))))
  col_eval_log = 3:7 # Adjust manually. Column index in performance of evaluation_log output from xgb.cv
  
  cat("Training on",n_param,"sets of parameters.\n")
  
  ## Loop through the different parameters sets
  for (i_param in 1:n_param) {
    
    set.seed(performance$seed[i_param])
    
    mdcv = xgb.cv(data=train, 
                  params = list(param_df[i_param,])[[1]], 
                  nthread=6, 
                  nfold=5, 
                  nrounds=10000,
                  verbose = FALSE, 
                  early_stopping_rounds=50, 
                  maximize=FALSE)
    
    performance[i_param,col_eval_log] = mdcv$evaluation_log[mdcv$best_iteration,]
  }
  
  ## Train on best parameters using best number of rounds
  i_param_best = performance$i_param[which.min(performance$test_rmse_mean)]
  print(t(param_df[i_param_best,])) #Prints the best parameters
  
  set.seed(performance$seed[i_param_best])
  
  mdl_best = xgb.train(data=train, 
                       params=list(param_df[i_param_best,])[[1]], 
                       nrounds=performance$iter[i_param_best], 
                       nthread=6)
  
  return(mdl_best)
}


fit_svm <- function(formula, train, param) {
  ###
  # Cross validates each combination of parameters in param and returns best model
  # param is a list of svm parameters as vectors
  # svm parameters are cost, epsilon, and gamma_scale (a scaling factor on the default gamma value)
  # train is formatted for xgboost input
  ###
  
  
  param_df = expand.grid(param) # Each row is a set of parameters to be cross validated
  n_param = nrow(param_df)
  
  ## Compute default gamma parameter
  gamma_default = 1/ncol(train)
  param_df = param_df %>%
    mutate(gamma = gamma_scale * gamma_default)
  
  ## Make sure only one type parameter
  if(length(param$type) == 1){
    
    if(str_detect(param$type,'regression')){
      reg_or_class = 'reg'
    } else if (str_detect(param$type,'classification')) {
      reg_or_class = 'class'
    }
  } else{
    stop('Can only handle one type parameter')
  }
  
  ## Allocate space for performance statistics (and set seeds)
  performance = rep(NA,n_param)
  
  cat("Training on",n_param,"sets of parameters.\n")
  
  ## Loop through the different parameters sets
  for (i_param in 1:n_param) {
    
    mdcv = suppressWarnings(e1071::svm(formula = formula, 
                      data = train, 
                      type = param$type,
                      kernel = 'radial',
                      gamma = param_df$gamma[i_param],
                      epsilon = param_df$epsilon[i_param],
                      cost = param_df$cost[i_param],
                      cross = 5,
                      scale = TRUE))
    
    if(reg_or_class == "reg"){
      performance[i_param] = mdcv$tot.MSE
    } else if (reg_or_class == "class") {
      performance[i_param] = mdcv$tot.accuracy
    }
    
  }
  
  ## Train on best parameters using best number of rounds
  if(reg_or_class == "reg"){
    i_param_best = which.min(performance)
  } else if (reg_or_class == "class") {
    i_param_best = which.max(performance)
  }
  
  print("Best parameters:")
  print(t(param_df[i_param_best,]))
  
  mdl_best = suppressWarnings(e1071::svm(formula = formula, 
                        data = train, 
                        type = param$type,
                        kernel = 'radial',
                        gamma = param_df$gamma[i_param_best],
                        epsilon = param_df$epsilon[i_param_best],
                        cost = param_df$cost[i_param_best],
                        cross = 5,
                        scale = TRUE))
  
  return(mdl_best)
}
