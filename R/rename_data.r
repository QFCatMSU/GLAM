#' @title rename_data
#'
#' @description converts names of data vectors/matrices from ADMB to RTMB version (followed B. Rook's data sheet - MI4_LWF_DAT_10_23_2023)
#'
#' @param data data from excel that includes all the sheets (this is extracted in prep_glam_input.r)
#'

rename_data = function(data) {
  # separate excel sheets into separate matrices
  # new parameter names that match the RTMB model
  new_pars = c(
    "fyear", "lyear", "retroyear",
    "fage", "lage", "targ_age",
    "rho_recr", "rho_ct_trap", "rho_ct_gill", "rho_ct_rec",
    "rho_q_trap", "rho_q_gill", "rho_q_rec", "rho_sel",
    "sp_time", "h2o_t", "linf", "vbk", "sd_M",
    "per_fem", "eggs_per_kg",
    "ref_len_trap", "ref_len_gill", "ref_len_rec",
    "wa", "la", "mat",
    "n_samp_trap", "n_samp_gill", "n_samp_rec",
    "ess_trap", "ess_gill", "ess_rec",
    "biomass_trap", "biomass_gill", "biomass_rec",
    "obs_eff_trap", "obs_eff_gill", "obs_eff_rec",
    "eff_trap_adj", "eff_gill_adj", "eff_rec_adj",
    "mn_wt_trap", "mn_wt_gill", "mn_wt_rec",
    "harv_trap_adj", "harv_gill_adj", "harv_rec_adj",
    "obs_pa_trap", "obs_pa_gill", "obs_pa_rec",
    "plus_grp"
  )
  old_pars = c(
    "fyear", "ldyear", "lyear",
    "fage", "lage", "targ_age",
    "rho_sr", "rho_ct", "rho_cg", "rho_cr",
    "rho_et", "rho_eg", "rho_er", "rho_sel",
    "sp_time", "h2o_t", "linf", "vb_k", "sd_M",
    "percent_female", "eggs_per_kg",
    "reflen_t", "reflen_g", "reflen_r",
    "in_watage", "in_latage", "in_mat",
    "in_n_samp_t", "in_n_samp_g", "in_n_samp_r",
    "in_eff_samp_t", "in_eff_samp_g", "in_eff_samp_r",
    "in_harv_wgt_t", "in_harv_wgt_g", "in_harv_wgt_r",
    "in_effort_t", "in_effort_g", "in_effort_r",
    "in_effort_t_adjust", "in_effort_g_adjust", "in_effort_r_adjust",
    "in_mnwgt_t", "in_mnwgt_g", "in_mnwgt_r",
    "in_t_harv_adjust", "in_g_harv_adjust", "in_r_harv_adjust",
    "in_obs_pat", "in_obs_pag", "in_obs_par",
    "in_plusgrp"
  )

  params_list = data.frame(new = new_pars, old = old_pars)
  output = list()

  for (j in 1:length(data)) {
    assign(names(data[j]), get(names(data[j]), data))
  }
  if (!"in_singles" %in% ls(environment())) stop("Sheet 'in_singles' is missing in data excel sheet")
  if (!"in_watage" %in% ls(environment())) stop("Sheet 'in_watage' is missing in data excel sheet")
  if (!"in_latage" %in% ls(environment())) stop("Sheet 'in_latage' is missing in data excel sheet")
  if (!"in_mat" %in% ls(environment())) stop("Sheet 'in_mat' is missing in data excel sheet")
  if (!"in_other" %in% ls(environment())) stop("Sheet 'in_other' is missing in data excel sheet")

  names_temp = params_list |>
    filter(old %in% colnames(in_singles))
  names_temp2 <- NULL
  in_singles = in_singles |>
    select_if(colnames(in_singles) %in% names_temp$old)
  for(j in 1:ncol(in_singles)){
    if(names(in_singles[,j]) %in%  names_temp$old) names_temp2[j] = names_temp$new[names_temp$old %in% names(in_singles[,j])]
  }
  in_singles = in_singles |>
    rename_if(colnames(in_singles) %in% names_temp$old, ~ names_temp2) 
  for (j in 1:ncol(in_singles)) {
    output[[colnames(in_singles[j])]] = assign(colnames(in_singles[j]), get(colnames(in_singles[j]), in_singles))
  }

  output$wa = as.matrix(in_watage[, -which(colnames(in_watage) == "year")])
  output$la = as.matrix(in_latage[, -which(colnames(in_watage) == "year")])
  output$mat = as.matrix(in_mat[, -which(colnames(in_watage) == "year")])

  names_temp = params_list |>
    filter(old %in% colnames(in_other))
  names_temp2 <- NULL
  in_other = in_other |>
    select_if(colnames(in_other) %in% names_temp$old)
  for(j in 1:ncol(in_other)){
    if(names(in_other[,j]) %in%  names_temp$old) names_temp2[j] = names_temp$new[names_temp$old %in% names(in_other[,j])]
  }
  in_other = in_other |>
    rename_if(colnames(in_other) %in% names_temp$old, ~ names_temp2) 
  for (j in 1:ncol(in_other)) {
    output[[colnames(in_other[j])]] = assign(colnames(in_other[j]), get(colnames(in_other[j]), in_other))
  }

  output$obs_pa_trap = as.matrix(in_obs_pat[, -which(colnames(in_watage) == "year")])
  output$obs_pa_gill = tryCatch(as.matrix(in_obs_pag[, -which(colnames(in_watage) == "year")]), error = function(e) { return(NULL) })
  output$obs_pa_rec = tryCatch(as.matrix(in_obs_par[, -which(colnames(in_watage) == "year")]), error = function(e) { return(NULL) })

  return(output)
}
