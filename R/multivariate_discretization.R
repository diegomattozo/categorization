#' Multivariate Discretization.
#'
#' @param train Train Dataframe.
#' @param test Test Dataframe.
#'
#' @return Discretized Dataframe.
#' @export
# v02 - added m = 3 times in a row
multdiscretization = function(train, test, alpha=0.005) {
  cutpoints = quantile_discretization(train)$cuts
  varNames = names(train)[-length(train)]
  respName = names(train)[length(train)]
  alpha = 1 + alpha
  k = 0
  stopCriteria = 0
  old_giniCoef = 0
  best_giniCoef = 0
  cuts_so_far = initialize_cutpoints(varNames)
  while(stopCriteria < 3 && k < 1000) {
    new_cutp = list(); new_giniCoef = list()

    for (i in varNames) {
      best = find_best_cutpoint(train, test, i, cutpoints[[i]], cuts_so_far) #retorna maior gini e novo vetor de pontos de corte.
      new_cutp[[i]] = best$cuts
      new_giniCoef[[i]] = best$ginicoef

    }
    old_giniCoef = best_giniCoef
    best_gini_varName = calc_best_gini(new_giniCoef, varNames)
    best_giniCoef = new_giniCoef[[best_gini_varName]]

    if ((old_giniCoef*alpha) > best_giniCoef) {
      stopCriteria = stopCriteria + 1
    }

    cuts_so_far[[best_gini_varName]] = new_cutp[[best_gini_varName]]
    k = k + 1
  }


  list('train' = cutpoint_discretization(train, cuts_so_far), 'test' = cutpoint_discretization(test, cuts_so_far), 'cuts'=cuts_so_far,
       'gini' = best_giniCoef)
}

# Find best new cutpoint in a vector o cutpoints.
#
# @param train Train Dataframe.
# @param test Test Dataframe.
# @param varName Names of dataframe covariates.
# @param cutpoints Vector with all possible boundary cutpoints for the covariate in question.
# @param cuts_so_far List of cutpoints used so far all covariates.
#
# @return List with best cutpoint and your gini coeficient.
find_best_cutpoint = function(train, test, varName, cutpoints, cuts_so_far) {
  respName = names(train)[length(test)]
  ginicoefs = numeric()
  cutp_so_far = cuts_so_far[[varName]]
  cutpoints = cutpoints[!(cutpoints %in% cutp_so_far)]
  cutsList = list()
  k = 1
  for (i in cutpoints) {
    if (is.nan(cutp_so_far[1])) {
      cut_p = c(i)
    } else {
      cut_p = c(i, cutp_so_far)
    }

    cut_p = cut_p[order(cut_p)]
    cuts_so_far[[varName]] = cut_p

    train_disc = cutpoint_discretization(train, cuts_so_far)
    test_disc = cutpoint_discretization(test, cuts_so_far)
    cutsList[[k]] = cut_p
    ginicoefs = append(ginicoefs, logistic_reg_giniCoef(train_disc, test_disc, respName))
    k = k + 1
  }
  best_coef_index = which.max(ginicoefs)[1]
  best_coef = ginicoefs[best_coef_index]
  best_cutp = cutsList[[best_coef_index]]

  return(list('cuts'=best_cutp, 'ginicoef'=best_coef))
}

# Calculate which gini is the best for all covariates.
#
# @param giniList List containing all the ginis.
# @param varNames Name of the dataframe covariates.
#
# @return Return the name of the covariate with the best ginicoef.
calc_best_gini = function(giniList, varNames) {
  giniVec = numeric()
  for (i in varNames) {
      giniVec = append(giniVec, giniList[[i]])
  }
  index = which.max(giniVec)

  varNames[index]
}


#' Fit logistic regression in the train dataframe and calculates the gini coef. in the test dataframe.
#'
#' @param train Train DataFrame
#' @param test Test DataFrame
#' @param respName Name of response variable. Necessary to the formula of glm object.
#'
#' @return Returns the gini coef. of the test dataframe.
#' @export
logistic_reg_giniCoef = function(train, test, respName) {
  my_formula = as.formula(paste(respName, '.', sep="~"))

  fit <- glm(my_formula, family = binomial, data = train)

  #Calculo gini na base de validacao
  predValid <- predict(fit, test, type="response")
  giniValid = optiRum::giniCoef(predValid, test[, length(test)])
  giniValid
}
