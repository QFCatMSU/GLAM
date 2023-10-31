#' @title run_retro
#'
#' @description Run retrospective analysis
#'
#' @param n_peel number of peels to use in retrospective analysis
#' @param nlminb_control
#' @param report_sdrep use sdreport from TMB, get standard errors for parameters
#' @param n_newton number of Newton steps (recommended max = 3)
#'
run_retro = function(n_peel,
                     nlminb_control = list(
                       eval.max = 1e3,
                       iter.max = 1e3,
                       trace = 0
                     ),
                     report_sdrep = FALSE,
                     n_newton = 3) {
  output = list()
  temp = list(data = data, pars = pars)
  out = list(data = data, pars = pars)

  if (n_peel > 0) {
    output = list(run_peel(
      peel = 1, input = temp,
      nlminb_control = nlminb_control,
      report_sdrep = report_sdrep,
      n_newton = n_newton
    ))
  }
  if (n_peel > 1) {
    for (p in 2:n_peel) {
      output[[p]] = run_peel(p,
        input = temp,
        nlminb_control = nlminb_control,
        report_sdrep = report_sdrep,
        n_newton = n_newton
      )
    }
  }
  # revert to original data and parameters
  assign("data", out$data, env = .GlobalEnv)
  assign("pars", out$pars, env = .GlobalEnv)
  return(output)
}