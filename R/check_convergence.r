#' @title check_convergence
#' 
#' @description checks convergence of model (hesssian, gradient) and tells which parameters have high gradients or poor estimation
#' Based on Check_Identifiable in TMBhelper
#'
#' @param obj_fn objective function (MakeADFun object)
#' @param model_res model results (nlminb results)
#' 
#' @return Return convergence messages and estimated parameters and uncertainty
#' 

check_convergence = function(
    obj_fn,
    model_res) {
  res = list()
  res$convergence = model_res$convergence
  res$max_gradient = max(abs(obj_fn$gr(opt$par)))

  if (res$convergence == 1) {
    print(res$convergence)
    stop("Model did not converge!")
  }
  if (res$max_gradient > 0.001) {
    print(obj_fn$gr(obj_fn$env$last.par.best))
    stop("Gradients are high, please improve optimization!")
  }
  # look at fixed estimated parameters
  if (length(obj_fn$env$random) == 0) {
    fixed_obj = obj_fn$env$last.par.best
  } else {
    fixed_obj = obj_fn$env$last.par.best[-c(obj_fn$env$random)]
  }
  # extract parameters and uncertainty
  res$Hess = optimHess(par = fixed_obj, fn = obj_fn$fn, gr = obj_fn$gr)
  if (is.nan(max(res$Hess))) {
    print("The hessian was not invertible")
  } else {
    res$eigen = eigen(res$Hess)
    res$which_bad = which(res$eigen$values < sqrt(.Machine$double.eps))
    # check for parameters
    if (length(res$eigen$vectors[, res$which_bad]) > 0) {
      rowmax = apply(as.matrix(res$eigen$vectors[, res$which_bad]),
        MARGIN = 1, FUN = function(x){max(abs(x))})
    } else {
      rowmax = rep(0, length(res$eigen$values))
    }
    res$bad_params = data.frame(
      "Parameters" = names(obj_fn$par),
      "MLE" = fixed_obj,
      "Parameter_check" = ifelse(rowmax > 0.001, "Bad", "OK")
    )
    # message for parameters
    if (length(res$which_bad) == 0) {
      message("All parameters are identifiable")
    } else {
      message("There are some parameters that were not identifiable: check parameter list!")
    }
  }
  if(res$convergence == 0 & res$max_gradient < 0.001 & length(res$which_bad) == 0){
    message("Model diagnostics consistent with convergence.")
    res <- list()
    res$convergence = model_res$convergence
    res$max_gradient = max(abs(obj_fn$gr(opt$par)))
    res$message = "Good to go!"
  }
    return(res)
}