#' @title prep_glam_data
#' 
#' @description converts data from Excel to R, needs to be in certain format (followed B. Rook's data sheet - MI4_LWF_DAT_10_23_2023)
#' 
#' @param model_name name of model run
#' @param data_file_name name of Excel sheet with data (must include all sheets - in_singles, in_watage, in_latage, in_mat, in_other, in_obs_pat/g/r)
#' @param sel_type_trap selectivity function ("logistic" or "lognormal") for trapnet fleet
#' @param gill_fleet TRUE if gillnet fleet in model
#' @param rec_fleet TRUE if recreational fleet in model
#' @param pauly_M calculate M from Pauly equation (needs parameters h2o_t, linf, and vbk - incorporate in in_singles Excel sheet)
#' @param M_init initial M estimate if not using Pauly's M
#' @param recruit_model recruitment function ("AR1" - autoregressive)
#' 

prep_glam_data = function(model_name = "GLAM", 
                          data_file_name,
                          # fleet_num = 1,
                          sel_type_trap = "logistic",
                          gill_fleet = FALSE,
                          rec_fleet = FALSE,
                          pauly_M = TRUE,
                          M_init = NULL,
                          recruit_model = "AR1"
                          # YSPG_switch = FALSE
) {
  # read in excel file for data
  if (!"readxl" %in% rownames(installed.packages())) {
    install.packages("tidyverse")
    install.packages("readxl")
    install.packages("janitor")
  }
  suppressMessages(require("tidyverse"))
  suppressMessages(require("readxl"))
  suppressMessages(require("janitor"))

  excel_path = tryCatch(paste0("data/", data_file_name, ".xlsx"), error = function(e) { return(NA) })
  if (is.na(excel_path)) stop("Please put the data excel sheet in the 'data' folder")
  excel_data_names = excel_sheets(excel_path)
  excel_data = suppressMessages(lapply(1:length(excel_data_names), function(x) read_excel(excel_path, sheet = excel_data_names[x])))
  # remove additional NAs and any notes from Excel sheet
  excel_data = lapply(excel_data, function(x) remove_empty(x, which = c("rows", "cols")))
  excel_data = lapply(excel_data, function(x) x[, which(!grepl("//*", colnames(x)))])
  excel_data = lapply(excel_data, function(x) x[complete.cases(x), ])
  names(excel_data) = excel_data_names
  if ("notes" %in% names(excel_data)) excel_data = excel_data[!names(excel_data) == "notes"]

  data = rename_data(data = excel_data)

  # number of years of assessment and number of ages of stock
  data$n_years = length(data$fyear:data$lyear)
  data$years = data$fyear:data$lyear
  data$n_ages = length(data$fage:data$lage)
  data$ages = data$fage:data$lage

  # natural mortality
  if (pauly_M) {
    log_M_init = tryCatch((0.465 * log(data$h2o_t) - 0.277 *
      log(data$linf) + 0.655 *
      log(data$vbk)), error = function(e) { return(NA) })
    if (is.na(log_M_init)) stop("Please give values of temperature (h2o_t), asymptotic length (linf), and the Brody growth coefficient (vbk) to estimate Pauly's M!")
  } else {
    log_M_init = tryCatch(log(M_init), error = function(e) { return(NA) })
    if (is.na(log_M_init)) stop("Please give an initial estimate of natural mortality (M_init)!")
  }
  if (pauly_M & !is.null(M_init)) warning("You are calculating Pauly's M and giving an initial estimate of natural mortality. 
    Consider changing either pauly_M = FALSE or M_init = NULL.
    Natural mortality will only be calculated based on Pauly's M.")
  data$log_M_init = log_M_init

  # fishery
  if (gill_fleet) {
    if (is.null(data$obs_eff_gill)) stop("You indicated there is a gillnet fleet (gill_fleet = TRUE), please add data for gillnet!")
  }
  if (rec_fleet) {
    if (is.null(data$obs_eff_rec)) stop("You indicated there is a recreational fleet (rec_fleet = TRUE), please add data for the recreational fleet!")
  }

  data$model_name = model_name
  data$recruit_model = recruit_model
  data$sel_type_trap = sel_type_trap
  data$gill_fleet = gill_fleet
  data$rec_fleet = rec_fleet

  return(data)
}