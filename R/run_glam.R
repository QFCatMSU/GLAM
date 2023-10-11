#' @title run_glam
#' 
#' @description run GLAM with nlminb and Newton steps
#'
#' @param nlminb_control
#' @param fixed_names names of fixed parameters that will go in map argument of MakeADFun
#' @param rand_names ## ** not used right now
#' @param bound_list ## ** not used right now
#' @param report_sdrep use sdreport from TMB, get standard errors for parameters
#' @param n_newton number of Newton steps (recommended max = 3)

#' 
#' @return list of model diagnostics related to convergence and gradients, model results, and parameter estimates
#' 

run_glam = function(nlminb_control = list(
                      eval.max = 1e3,
                      iter.max = 1e3,
                      trace = 0
                    ),
                    fixed_names = NULL,
                    # rand_names = NULL,
                    # bound_list = NULL,
                    report_sdrep = TRUE,
                    n_newton = 3) {
  # mapping fixed parameters and removing bounds if parameter is fixed
  if (!is.null(fixed_names)) {
    fixed_list = list()
    for (i in 1:length(fixed_names)) {
      fixed_list[[paste(fixed_names[i])]] = as.factor(rep(NA, length(pars[[which(names(pars) %in% fixed_names[i])]])))
    }
  } else {
    fixed_list = NULL
  }


  ## MakeADFun ####
  model = MakeADFun(func = glam, parameters = pars, map = fixed_list, hessian = TRUE, silent = TRUE)

  ## ** - will need to incorporate bounds and random effects later

  ## Run model ####
  res = try(nlminb(model$par, model$fn, model$gr,
    control = nlminb_control
  ))


  ## Check model convergence and gradients ####
  # rerun model with Newton steps if gradients are bad
  final_gradient = model$gr(res$par)
  max_gradient = max(abs(final_gradient))
  if (max_gradient < 0.001) {
    n_newton_steps = 0
  } else {
    n_newton_steps = 1
  }
  if (n_newton_steps) {
    tryCatch(
      for (n in 1:n_newton) {
        g = as.numeric(model$gr(res$par))
        h = stats::optimHess(res$par, model$fn, model$gr) # Hessian matrix
        new_par = res$par - solve(h, g)
        # rewrite results
        res = nlminb(new_par, model$fn, model$gr,
          control = list(eval.max = 1e4, iter.max = 1e4)
        )
      }, error = function(e) { err = conditionMessage(e) }
    )
  }

  # look at convergence, gradients, Hessian
  check = check_convergence(obj_fn = model, model_res = res)


  ## Report ####
  # sdreport
  if (report_sdrep) {
    sdrep = try(sdreport(model))
    sdrep = summary(sdrep)
  } else {
    sdrep = NULL
  }

  # saved parameters
  df = tryCatch(
    data.frame(
      "parameter" = names(res$par),
      "estimate" = res$par,
      "gradient" = as.vector(final_gradient)
    ),
    error = function(e) {
      return(NULL)
    }
  )

  # export
  output = list()
    output$check = check # has all model convergence, gradient, and Hessian checks and messages, check this after model run
    output$report = model$report(res$par) # list of output from RTMB model
    output$params = df # parameter list with parameter names, estimates, and gradients
    output$sdrep = sdrep # sdreport output - parameter estimates and standard errors
    output$model = model # MakeADFun model/output

  return(output)
}