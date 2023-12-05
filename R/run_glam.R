#' @title run_glam
#' 
#' @description run GLAM with nlminb and Newton steps
#'
#' @param nlminb_control
#' @param fixed_names names of fixed parameters that will go in map argument of MakeADFun
#' @param rand_names names of random parameters that will go into random argument of MakeADFun
#' @param bound_list ## ** not used right now
#' @param hessian_run run nlminb with hessian
#' @param report_sdrep use sdreport from TMB, get standard errors for parameters
#' @param run_newton run Newton steps?
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
                    rand_names = NULL,
                    # bound_list = NULL,
                    hessian_run = FALSE,
                    report_sdrep = TRUE,
                    run_newton = TRUE,
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

  if(!is.null(rand_names)){
    if(hessian_run) message("Hessian not yet implemented for models with random effects, will not run with Hessian")
    hessian_run = FALSE
  }

  ## MakeADFun ####
  obj = MakeADFun(func = glam, random = rand_names, parameters = pars, map = fixed_list, hessian = TRUE, silent = TRUE)
  ## ** - will need to incorporate bounds and random effects later

  ## Run model ####
  res = try(nlminb(obj$par, obj$fn, obj$gr,
    control = nlminb_control
  ))

  # rerun with hessian
  if(hessian_run) {
    res = try(nlminb(res$par, obj$fn, obj$gr, obj$he,
      control = list(abs.tol = 1e4)
    ))
  }


  ## Check model convergence and gradients ####
  # rerun model with Newton steps if gradients are bad
  final_gradient = obj$gr(res$par)
  max_gradient = max(abs(final_gradient))

  if (run_newton) {
    tryCatch(
      for (n in 1:n_newton) {
        g = as.numeric(obj$gr(res$par))
        h = stats::optimHess(res$par, obj$fn, obj$gr) # Hessian matrix
        new_par = res$par - solve(h, g)
        # rewrite results
        res = nlminb(new_par, obj$fn, obj$gr,
          control = list(eval.max = 1e4, iter.max = 1e4)
        )
      }, error = function(e) { err = conditionMessage(e) }
    )
  }


  # look at convergence, gradients, Hessian
  check = check_convergence(obj_fn = obj, model_res = res)
  check$message = res$message


  ## Report ####
  # sdreport
  if (report_sdrep) {
    sdrep = try(sdreport(obj))
    sdrep = summary(sdrep)
    sdcheck = sdrep[which(abs(sdrep[,1])*2 < sdrep[,2]),]
    if(!is.null(sdcheck)) {
      print("Standard errors for some parameter estimates are high, consider checking this!")
      check$sdcheck = sdcheck
    }
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
    output$model_name = data$model_name
    output$check = check # has all model convergence, gradient, Hessian checks and messages, and SE messages, check this after model run
    output$report = obj$report() # list of output from RTMB model
    output$params = df # parameter list with parameter names, estimates, and gradients
    output$sdrep = sdrep # sdreport output - parameter estimates and standard errors
    output$obj = obj # MakeADFun obj/output

  return(output)
}