#' @title run_peel
#'
#' @description One run of retrospective analysis (N peels)
#'
#' @param peel number of years to take off (defined in run_retro.r)
#' @param input list of data and parameters
#' @param nlminb_control
#' @param report_sdrep use sdreport from TMB, get standard errors for parameters
#' @param n_newton number of Newton steps for each peel (recommended max = 3)
#'

run_peel = function(peel,
                    input,
                    nlminb_control = list(
                      eval.max = 1e3,
                      iter.max = 1e3,
                      trace = 0
                    ),
                    report_sdrep = FALSE,
                    n_newton = 3) {
  # set up data and parameters (minus peel years)
  message("Minus ", peel, " year(s)")
  input$data$n_years = input$data$n_years - peel
  input$data$years = input$data$years[1:input$data$n_years]

  for (r in 1:length(input$data)) {
    if (length(input$data[[r]]) > 1) {
      input_years_m = nrow(data[[r]])
      input_years_m[2] = nrow(input$data[[r]]) == (input$data$n_years + peel)
      input_years_v = length(input$data[[r]]) == (input$data$n_years + peel)
      if (!is.null(input_years_m)) {
        input$data[[r]] = as.matrix(input$data[[r]][1:(input$data$n_years), ])
      } else if (input_years_v) {
        input$data[[r]] = input$data[[r]][1:(input$data$n_years)]
      }
    }
  }

  for (r in 1:length(input$pars)) {
    if (length(input$pars[[r]]) == input$data$n_years) {
      input$pars[[r]] = input$pars[[r]][1:(length(input$pars[[r]]) - peel)]
    }
  }

  assign("data", input$data, env = .GlobalEnv)
  assign("pars", input$pars, env = .GlobalEnv)

  res = suppressWarnings(run_glam(
    nlminb_control = nlminb_control,
    fixed_names = NULL,
    report_sdrep = report_sdrep,
    n_newton = n_newton
  ))
  output = list(
    model_name = paste0(input$data$model_name, "_peel_", peel),
    peel_years = input$data$years,
    report = res$report$out,
    params = res$params
  )

  return(output)
}