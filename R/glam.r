#'@title glam
#' 
#' @description Great Lakes Assessment Model
#' 
#' @param pars list of parameters
#' 

glam = function(pars) {
  ## Table of Contents 
  # 0. Data inputs and parameters
  # 1. Growth and Eggs
  # 2. Selectivity and Catchability
  # 3. Mortalities (natural, fishing, and total mortality)
  # 4. Time-dynamic calculations (recruitment, numbers at age, predicted outputs)
  # 5. Projected values
  # 6. Reference points
  # 7. Objective functions
  # 8. Report section

  ## 0. Data inputs and parameters ####
  getAll(data, pars)
  ages = fage:lage
  years = fyear:lyear
  # adjustments to effort and catch
  obs_ct_trap = (biomass_trap / mn_wt_trap) / harv_trap_adj
  # gillnet fleet
  if (gill_fleet) {
    if ("eff_gill_adj" %in% names(data)) obs_eff_gill = obs_eff_gill * eff_gill_adj
    obs_ct_gill = (biomass_gill / mn_wt_gill) / harv_gill_adj
  } else {
    obs_eff_gill = 0
    obs_ct_gill = 0
  }
  # recreational fleet
  if (rec_fleet) {
    obs_ct_rec = obs_ct_rec / harv_rec_adj
  } else {
    obs_eff_rec = 0
    obs_ct_rec = 0
  }

  ## 1. Growth and Eggs ####
  # instantaneous growth rates from weight at age matrix
  g_mat = matrix(NA, nrow = n_years, ncol = n_ages)
  for (y in 1:(n_years - 1)) {
    for (a in 1:(n_ages - 1)) {
      g_mat[y, a] = log(wa[y + 1, a + 1]) - log(wa[y, a])
    }
  }
  g_mat[g_mat < 0] = 0 # replace negative values with 0, assume instantaneous growth rate can't be negative
  g_mat[, n_ages] = g_mat[, n_ages - 1] # last age equivalent to second to last age
  g_mat[n_years, ] = g_mat[n_years - 1, ] # last year equivalent to second to last year

  # Weight at age calculations
  # population weight at age in the beginning of the year at the time of spawning, and time of harvest (assumes harvest occur halfway through year)
  pop_wa = spawn_wa = matrix(0, nrow = n_years, ncol = n_ages)
  pop_wa[1, ] = wa[1, 2:(n_ages + 1)] * exp(-0.5 * g_mat[1, ])
  for (y in 1:(n_years - 1)) {
    for (a in 1:n_ages) {
      pop_wa[y + 1, a] = wa[y + 1, a + 1] * exp(-0.5 * g_mat[y, a])
    }
  }

  for (y in 1:n_years) {
    for (a in 1:(n_ages - 1)) {
      spawn_wa[y, a] = wa[y, a + 1] * exp((sp_time - 0.5) * g_mat[y, a + 1])
    }
  }
  pop_wa[, n_ages] = wa[, (n_ages + 1)]
  spawn_wa[, n_ages] = wa[, (n_ages + 1)]

  # Eggs per female
  # eggs_fem = eggs_per_kg * spawn_wa[n_years, ]
  # Weighting factor for calculating spawning biomass
  wt_fac = mat * spawn_wa
  # # Percent of mature female as vector
  per_fem_v = mat[n_years, ] * per_fem
  # Egg factor (used to calculate annual egg production)
  egg_fac = wt_fac * eggs_per_kg


  ## 2. Selectivity and Catchability ####
  # Selectivity
  sel_trap_dev = exp(log_sel_trap_p1)
  sel_trap_p2 = exp(log_sel_trap_p2)
  if ("log_sel_trap_dev" %in% names(pars)) {
    for (y in 2:n_years) {
      sel_trap_dev[y] = exp(log(sel_trap_dev[y - 1]) + log_sel_trap_dev[y - 1])
    }
  }
  # Gillnet fleet
  if (gill_fleet) {
    sel_gill_dev = exp(log_sel_gill_p1)
    sel_gill_p2 = exp(log_sel_gill_p2)
    if ("log_sel_gill_dev" %in% names(pars)) {
      for (y in 2:n_years) {
        sel_gill_dev[y] = exp(log(sel_gill_dev[y - 1]) + log_sel_gill_dev[y - 1])
      }
    }
  }
  # recreational fleet
  if (rec_fleet) {
    sel_rec_dev = exp(log_sel_gill_p1)
    sel_rec_p2 = exp(log_sel_gill_p2)
  }

  sel_trap = matrix(0, nrow = n_years, ncol = n_ages)
  sel_gill = matrix(0, nrow = n_years, ncol = n_ages)
  sel_rec = matrix(0, nrow = n_years, ncol = n_ages)
  for (y in 1:n_years) {
    # trapnet
    if (sel_type_trap == "lognormal") {
      if (length(sel_trap_dev) > 1) {
        sel_trap[y, ] = 1 / (sqrt(2 * pi) * sel_trap_dev[y] * la[y, ]) *
          exp(-((log(la[y, ]) - sel_trap_p2)^2) / (2 * sel_trap_dev[y]^2)) /
          (1 / (sqrt(2 * pi) * sel_trap_dev[y] * ref_len_trap) *
            exp(-((log(ref_len_trap) - sel_trap_p2)^2) / (2 * sel_trap_dev[y]^2)))
      } else {
        sel_trap[y, ] = 1 / (sqrt(2 * pi) * sel_trap_dev * la[y, ]) *
          exp(-((log(la[y, ]) - sel_trap_p2)^2) / (2 * sel_trap_dev^2)) /
          (1 / (sqrt(2 * pi) * sel_trap_dev * ref_len_trap) *
            exp(-((log(ref_len_trap) - sel_trap_p2)^2) / (2 * sel_trap_dev^2)))
      }
    } else {
      if (length(sel_trap_dev) > 1) {
        sel_trap[y, ] = 1 / (1 + exp(-sel_trap_p2 * (la[y, ] - sel_trap_dev[y]))) /
          (1 / (1 + exp(-sel_trap_p2 * (ref_len_trap - sel_trap_dev[y]))))
      } else {
        sel_trap[y, ] = 1 / (1 + exp(-sel_trap_p2 * (la[y, ] - sel_trap_dev))) /
          (1 / (1 + exp(-sel_trap_p2 * (ref_len_trap - sel_trap_dev))))
      }
    }
    # gill net
    if (gill_fleet) {
      if (length(sel_gill_dev) > 1) {
        sel_gill[y, ] = 1 / (sqrt(2 * pi) * sel_gill_dev[y] * la[y, ]) *
          exp(-((log(la[y, ]) - sel_gill_p2)^2) / (2 * sel_gill_dev[y]^2)) /
          (1 / (sqrt(2 * pi) * sel_gill_dev[y] * ref_len_gill) *
            exp(-((log(ref_len_gill) - sel_gill_p2)^2) / (2 * sel_gill_dev[y]^2)))
      } else {
        sel_gill[y, ] = 1 / (sqrt(2 * pi) * sel_gill_dev * la[y, ]) *
          exp(-((log(la[y, ]) - sel_gill_p2)^2) / (2 * sel_gill_dev^2)) /
          (1 / (sqrt(2 * pi) * sel_gill_dev * ref_len_gill) *
            exp(-((log(ref_len_gill) - sel_gill_p2)^2) / (2 * sel_gill_dev^2)))
      }
    }
    # rec fleet
    if (rec_fleet) {
      sel_rec[y, ] = 1 / (1 + exp(-log_sel_rec_p2 * (la[y, ] - log_sel_rec_dev))) /
        (1 / (1 + exp(-log_sel_rec_p2 * (ref_len_rec - log_sel_rec_dev))))
    }
  }

  # Catchability
  q_trap = exp(log_q_trap)
  for (y in 2:n_years) {
    q_trap[y] = exp(log(q_trap[y - 1]) + log_q_trap_dev[y - 1])
  }
  if (gill_fleet) {
    q_gill = exp(log_q_gill)
    for (y in 2:n_years) {
      q_gill[y] = exp(log(q_gill[y - 1]) + log_q_gill_dev[y - 1])
    }
  } else {
    q_gill = 0
  }
  if (rec_fleet) {
    q_rec = exp(log_q_rec)
    for (y in 2:n_years) {
      q_rec[y] = exp(log(q_rec[y - 1]) + log_q_rec_dev[y - 1])
    }
  } else {
    q_rec = 0
  }


  ## 3. Mortalities ####
  # Mortalities
  M = exp(log_M) # natural mortality
  # natural mortality from sea lamprey
  if ("M_lamprey" %in% names(data)) {
    M_lamprey = M_lamprey * M_lamprey_mult
  } else {
    M_lamprey = 0
  }
  FM_trap = q_trap * obs_eff_trap * sel_trap # fishing mortality for trap nets
  FM_gill = q_gill * obs_eff_gill * sel_gill # fishing mortality for gill nets
  FM_rec = q_rec * obs_eff_rec * sel_rec
  FM_tot = FM_trap + FM_gill + FM_rec # total fishing mortality
  Z = FM_tot + M + M_lamprey # total mortality
  MD = Z + (-FM_tot) + (-M_lamprey)
  S = exp(-Z)
  S0 = matrix(exp(-M), nrow = n_years, ncol = n_ages) #  survival
  A = 1 - S
  S_spawn = exp(-sp_time * Z)
  # FM_max = apply(FM_tot, 1, max) # annual maximum fishing mortality ** can't get this to work with advector


  ## 4. Time-dynamic calculations ####
  log_recr = numeric(n_years)
  log_recr[1] = log_recr_init
  nage = matrix(0, nrow = n_years, ncol = n_ages)
  nage[1, 1] = exp(log_recr_init)
  nage[1, 2:(length(log_pop_init) + 1)] = exp(log_pop_init)

  for (y in 2:n_years) {
    log_recr[y] = log_recr_avg + tanh(acor) * (log_recr[y - 1] - log_recr_avg) + log_recr_dev[y - 1]
    log_recr[n_years] = log_recr[n_years - 1]
    nage[y, 1] = exp(log_recr[y])
    for (a in 2:n_ages) {
      nage[y, a] = nage[y - 1, a - 1] * exp(-Z[y - 1, a - 1])
    }
    nage[y, n_ages] = nage[y, n_ages] + nage[y - 1, n_ages] * exp(-Z[y - 1, n_ages])
  }

  # Out of loop calculations
  nage_spawn = per_fem * nage * S_spawn # number of spawners at time of spawning
  biomass = nage * pop_wa # predicted biomass
  sp_biomass = nage_spawn * wt_fac # predicted spawning biomass
  # trapnet fleet
  ct_trap = (FM_trap / Z) * (nage * (1 - S)) # Baranov catch equation - trap net
  ct_trap_tot = rowSums(ct_trap) # catch summed across ages - trap net
  pa_trap = ct_trap / (ct_trap_tot + 0.001) # proportions at age - trap net
  biomass_trap = mn_wt_trap * ct_trap
  # gillnet fleet
  if (gill_fleet) {
    ct_gill = (FM_gill / Z) * (nage * (1 - S)) # Baranov catch equation - gill net
    ct_gill_tot = rowSums(ct_gill) # catch summed across ages - gill net
    pa_gill = ct_gill / (ct_gill_tot + 0.001) # proportions at age - gill net
    biomass_gill = mn_wt_gill * ct_gill
  } else {
    ct_gill = ct_gill_tot = pa_gill = biomass_gill = 0
  }
  # recreational fleet
  if (rec_fleet) {
    ct_rec = (FM_rec / Z) * (nage * (1 - S)) # Baranov catch equation - rec net
    ct_rec_tot = rowSums(ct_rec) # catch summed across ages - rec net
    pa_rec = ct_rec / (ct_rec_tot + 0.001) # proportions at age - rec net
    biomass_rec = mn_wt_rec * ct_rec
  } else {
    ct_rec = ct_rec_tot = pa_rec = biomass_rec = 0
  }
  mdead = (MD / Z) * (nage * (1 - exp(-Z))) # total number dead due to natural causes
  sldead = (M_lamprey / Z) * (nage * (1 - exp(-Z))) # total number dead due to sea lamprey
  tdead = mdead + sldead + ct_trap + ct_gill # total numbers killed
  # Standardized residuals
  # resid_ct_trap = (log(obs_ct_trap+0.001) + log(ct_trap+0.001))


  ## 5. Projected Values ####
  A_proj = rowSums(sapply(0:2, function(x) A[n_years - x, ])) / 3 # calculate average annual mortality over last three years
  age_5 = n_ages:which(ages == 5) # calculate average annual mortality - ages 5-19
  avg_A = sum(sapply(age_5, function(x) A_proj[x])) / length(age_5)
  FM_proj = rowSums(sapply(0:2, function(x) FM_tot[n_years - x, ])) / 3
  FM_proj_trap = rowSums(sapply(0:2, function(x) FM_trap[n_years - x, ])) / 3
  FM_proj_gill = rowSums(sapply(0:2, function(x) FM_gill[n_years - x, ])) / 3
  Z_proj = rowSums(sapply(0:2, function(x) Z[n_years - x, ])) / 3
  S_proj = exp(-Z_proj)
  M_proj = sapply(n_years, function(x) MD[x, ])
  S_proj0 = exp(-M_proj)
  S_proj_t = rep(0, length(S_proj0))
  if (targ_age > ages[1]) {
    S_proj_t[1:(targ_age - 1)] = S_proj0[1:(targ_age - 1)]
  }
  age_targ = which(ages == targ_age):n_ages
  S_proj_t[age_targ] = 0.35
  w_fac_proj = wt_fac[n_years, ] # extracts maturity*weight at spawn from last year
  wa_proj = wa[n_years, ] # extracts weight at age vector from last year


  ## 6. Reference Points ####
  nsurv = matrix(0, nrow = n_years, ncol = n_ages)
  nsurv[, 1] = per_fem
  nsurv0 = matrix(0, nrow = n_years, ncol = n_ages)
  nsurv0[, 1] = per_fem
  psurv = 1
  psurv0 = 1
  psurv_t = 1

  for (a in 2:n_ages) {
    # annual
    nsurv[, a] = nsurv[, a - 1] * S[, a - 1]
    nsurv0[, a] = nsurv0[, a - 1] * S0[, a - 1]
    # projected
    psurv[a] = psurv[a - 1] * S_proj[a - 1]
    psurv0[a] = psurv0[a - 1] * S_proj0[a - 1]
    psurv_t[a] = psurv_t[a - 1] * S_proj_t[a - 1]
  }

  # Annual estimates of SSBR and SPR
  ssbr = mat * spawn_wa * nsurv * exp(-sp_time * Z)
  ssbr[, n_ages] = mat[, n_ages] * spawn_wa[, n_ages] * nsurv[, n_ages] *
    exp(-sp_time * Z[, n_ages]) * (1 / (1 - S[, n_ages]) - 1)
  ssbr0 = mat * spawn_wa * nsurv0 * exp(-sp_time * M)
  ssbr0[, n_ages] = mat[, n_ages] * spawn_wa[, n_ages] * nsurv0[, n_ages] *
    exp(-sp_time * M) * (1 / (1 - S0[, n_ages]) - 1)
  ssbr_annual = rowSums(ssbr)
  ssbr_annual0 = rowSums(ssbr0)
  spr_annual = ssbr_annual / ssbr_annual0

  # Projected estimates of YPR, SSBR, and SPR
  ssbr_proj = 0
  ssbr_proj0 = 0
  ssbr_proj_t = 0
  ypr_proj = 0
  for (a in 1:n_ages) {
    ssbr_proj = ssbr_proj + per_fem * w_fac_proj[a] * psurv[a] * exp(-sp_time * Z_proj[a])
    ssbr_proj0 = ssbr_proj0 + per_fem * w_fac_proj[a] * psurv0[a] * exp(-sp_time * M_proj[a])
    if (a < targ_age) {
      ssbr_proj_t = ssbr_proj_t + per_fem * w_fac_proj[a] * psurv_t[a] * exp(-sp_time * M_proj[a])
    }
    if (a >= targ_age) {
      ssbr_proj_t = ssbr_proj_t + per_fem * w_fac_proj[a] * psurv_t[a] * exp(log(S_proj_t[n_ages]) * sp_time)
    }
    ypr_proj = ypr_proj + (FM_proj[a] / Z_proj[a]) * ((1 - S_proj[a]) * psurv[a] * wa_proj[a])
  }
  spr_proj = ssbr_proj / ssbr_proj0
  spr_proj_t = ssbr_proj_t / ssbr_proj0
  ssbr_ratio_t = ssbr_proj / ssbr_proj_t

  # TAC (calculate average recruitment over last 10 years)
  avg_range = (n_years - 9):n_years
  avg_recr = sum(nage[avg_range, 1]) / length(avg_range)

  #
  recr = nage[, 1]
  avg_FM_trap = sum(FM_proj_trap) / n_years
  avg_FM_gill = sum(FM_proj_gill) / n_years
  avg_Z = sum(Z_proj) / n_years
  # avg_biomass_lbs = kg2lbs(sum(biomass[avg_range, ]) / length(avg_range))
  # avg_sp_biomass_lbs = kg2lbs(sum(sp_biomass[avg_range, ]) / length(avg_range))
  avg_FM_tot = sum(FM_tot) / length(FM_tot)


  ## 7. Objective Function ####
  # Rho approach for variances
  sig = exp(log_sig) # back transform sigma
  sd_recr = sig * rho_recr
  sd_ct_trap = sig * rho_ct_trap
  sd_q_trap = sig * rho_q_trap
  if ("rho_sel" %in% names(data)) sd_sel = sig * rho_sel
  if (gill_fleet) {
    sd_ct_gill = sig * rho_ct_gill
    sd_q_gill = sig * rho_q_gill
  }
  if (rec_fleet) {
    sd_ct_rec = sig * rho_ct_rec
    sd_q_rec = sig * rho_q_rec
  }
  # Objective functions
  nlp = 0 # components related to priors and process error
  nll = 0 # components related to data and observation error

  # penalty on fishing mortality
  nlp = nlp + 1000 * log((avg_FM_tot + 0.000001) / 0.2)^2
  # penalty on natural mortality
  nlp = nlp + (0.5 / sd_M^2 * (log_M_init - log_M)^2 + log(sd_M))

  # likelihood on observed catch - trap net
  nll = nll + 0.5 / sd_ct_trap^2 * sum(log((0.01 + obs_ct_trap) / (0.01 + ct_trap_tot))^2) + (years[n_years] - years[1] + 1) * log(sd_ct_trap)
  # likelihood on catchability - trap net 
  nlp = nlp + (0.5 / sd_q_trap^2 * sum(log_q_trap_dev^2) + length(log_q_trap_dev) * log(sd_q_trap))
  # likelihood on age composition - trap net - multinomial
  nll = nll - sum(ess_trap * obs_pa_trap * log(0.0001 + pa_trap))
  # random walk for selectivity - trap net
  if ("log_sel_trap_dev" %in% names(pars)) nlp = nlp + (0.5 / sd_sel^2 * sum(log_sel_trap_dev^2) + (length(log_sel_trap_dev) - 1) * log(sd_sel))
  # gillnet
  if (gill_fleet) {
    # likelihood on observed catch - gill net
    nll = nll + (0.5 / sd_ct_gill^2 * sum(log((0.01 + obs_ct_gill) / (0.01 + ct_gill_tot))^2) + (years[n_years] - years[1] + 1) * log(sd_ct_gill))
    # likelihood on catchability - gill net
    nlp = nlp + (0.5 / sd_q_gill^2 * sum(log_q_gill_dev^2) + (length(log_q_gill_dev) - 1) * log(sd_q_gill))
    # likelihood on age composition - gill net - multinomial
    nll = nll - sum(ess_gill * obs_pa_gill * log(0.0001 + pa_gill))
    # random walk for selectivity - gill net
    if ("log_sel_gill_dev" %in% names(pars)) nlp = nlp + (0.5 / sd_sel^2 * sum(log_sel_gill_dev^2) + (length(log_sel_gill_dev) - 1) * log(sd_sel))
  }
  # rec fleet
  if (rec_fleet) {
    # likelihood on observed catch - rec fleet
    nll = nll + (0.5 / sd_ct_rec^2 * sum(log((0.01 + obs_ct_rec) / (0.01 + ct_rec_tot))^2) + (years[n_years] - years[1] + 1) * log(sd_ct_rec))
    # likelihood on catchability - rec fleet
    nlp = nlp + (0.5 / sd_q_rec^2 * sum(log_q_rec_dev^2) + (length(log_q_rec_dev) - 1) * log(sd_q_rec))
    # likelihood on age composition - rec fleet - multinomial
    nll = nll - sum(ess_rec * obs_pa_rec * log(0.0001 + pa_rec))
    # random walk for selectivity - rec fleet
    if ("log_sel_rec_dev" %in% names(pars)) nlp = nlp + (0.5 / sd_sel^2 * sum(log_sel_rec_dev^2) + (length(log_sel_rec_dev) - 1) * log(sd_sel))
  }
  # recruitment deviations
  # AR(1)
  nlp = nlp + (0.5 / sd_recr^2 * sum(log_recr_dev^2) + (length(log_recr_dev) - 1) * log(sd_recr) +
    0.5 * (1 - tanh(acor)^2) / sd_recr^2 * (log_recr_init - log_recr_avg)^2 +
    log(sqrt(sd_recr^2 / (1 - tanh(acor)^2))))

  jnll = nlp + nll


  ## 8. Report Section ####
  out = list(
    ct_trap = rowSums(ct_trap),
    ct_gill = rowSums(ct_gill),
    biomass_trap = rowSums(biomass_trap),
    biomass_gill = rowSums(biomass_gill),
    recr = recr,
    sp_biomass = rowSums(sp_biomass),
    sel_trap = sel_trap,
    sel_gill = sel_gill,
    pa_trap = pa_trap,
    pa_gill = pa_gill,    
    M = M,
    FM_trap = FM_trap,
    FM_gill = FM_gill,
    q_trap = q_trap,
    q_gill = q_gill
  )
  singles = list(
    ssbr_proj = ssbr_proj,
    ssbr_proj0 = ssbr_proj0,
    ssbr_proj_t = ssbr_proj_t,
    spr_proj_t = spr_proj_t,
    spr_proj = spr_proj,
    spr_annual = spr_annual,
    ypr_proj = ypr_proj,
    avg_FM_trap = avg_FM_trap,
    avg_FM_gill = avg_FM_gill,
    avg_FM_tot = avg_FM_tot,
    avg_A = avg_A
  )

  # Export
  REPORT(out)
  REPORT(singles)
  REPORT(jnll)
  REPORT(nlp)
  REPORT(nll)

  jnll
}