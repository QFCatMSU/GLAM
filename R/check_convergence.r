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
  res$max_gradient = max(abs(obj_fn$gr(model_res$par)))
  final_gradient = obj_fn$gr(model_res$par)

  if (res$convergence == 1) {
    message("Model did not converge!")
  }
  if (res$max_gradient > 0.001) {
    res$whichbad_params = data.frame(
      parameters = names(model_res$par),
      MLE = as.numeric(model_res$par),
      gradient = as.numeric(final_gradient),
      parameter_check = c(ifelse(as.numeric(final_gradient) > 0.001, "Bad", "OK"))
    )
    message("Gradients are high, please improve optimization!")
    return(res)
  } else {
    res$whichbad_params = "All parameter gradients look good!"
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
    res$hess_status = "The hessian was not invertible"
  } else {
    res$eigen = eigen(res$Hess)
    res$whichbad_eigen = which(res$eigen$values < sqrt(.Machine$double.eps))
    # check for parameters
    if (length(res$eigen$vectors[, res$whichbad_eigen]) > 0) {
      rowmax = apply(as.matrix(res$eigen$vectors[, res$whichbad_eigen]),
        MARGIN = 1, FUN = function(x){ max(abs(x)) })

      res$whichbad_eigen = data.frame(
          "Parameters" = names(obj_fn$par),
          "MLE" = fixed_obj,
          "Parameter_check" = ifelse(rowmax > 0.001, "Bad", "OK")
          )
    } else {
      res$whichbad_eigen = "All parameters are identifiable"
    }
  }
  if(res$convergence == 0 & res$max_gradient < 0.001 & class(res$whichbad_eigen) == "character"){
    message("Model diagnostics consistent with convergence.")
    res <- list()
    res$convergence = model_res$convergence
    res$max_gradient = max(abs(obj_fn$gr(model_res$par)))
    res$message = "Good to go!"
  }
    return(res)
}