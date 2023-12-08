#' @title sci_transl
#' 
#' @description converts metric labels, from Z. Siders
#' 
#' @param range
#' 
sci_transl <- function(range) {
  x <- gsub("^[[:graph:]]+e\\+", "", formatC(max(range), format = "g"))
  v <- switch(x,
    "01" = "10's",
    "02" = "100's",
    "03" = "1000's",
    "04" = "10's of Thousands",
    "05" = "100's of Thousands",
    "06" = "Millions",
    "07" = "10's of Millions",
    "08" = "100's of Millions",
    "09" = "Billions"
  )
  return(list(
    scale = as.numeric(paste0("1e", x)),
    label = v,
    r = range / as.numeric(paste0("1e", x))
  ))
}


#' @title plot_list
#' 
#' @description 
#' 
#' @param type
#' 
plot_list = function(type, model_res = NULL) {
  if(type == "data") {
    # weight at age, length at age, maturity at age
    # biomass, effort 
    plot_list = list()
      plot_list$wa = lh_plot(ptype = "wa")
      plot_list$la = lh_plot(ptype = "la")
      plot_list$mat = lh_plot(ptype = "mat")
      plot_list$biomass = N_plot(ptype = "biomass_dat")
      plot_list$eff = eff_plot()
  } else if(type == "results") {
    # numbers at age, recruitment
    model_res = model_res$report$out
    
    plot_list = list()
      plot_list$nage = N_plot(ptype = "nage", model_res = model_res)
      plot_list$recruit = N_plot(ptype = "recruit", model_res = model_res)
      plot_list$biomass = N_plot(ptype = "biomass_est", model_res = model_res)
      plot_list$sp_biomass = N_plot(ptype = "sp_biomass", model_res = model_res)
      plot_list$mort = mort_plot(model_res = model_res)
      plot_list$ct_trap = ct_plot(ptype = "trap", model_res = model_res)
      if(data$gill_fleet) plot_list$ct_gill = ct_plot(ptype = "gill", model_res = model_res)
      plot_list$pa_trap = pa_plot(ptype = "trap", model_res = model_res)
      if(data$gill_fleet) plot_list$pa_gill = pa_plot(ptype = "gill", model_res = model_res)
      plot_list$sel_trap = sel_plot(ptype = "trap", model_res = model_res)
      if(data$gill_fleet) plot_list$sel_gill = sel_plot(ptype = "gill", model_res = model_res)
      plot_list$q = q_plot(model_res = model_res)
      plot_list$resid_pa_trap = resid_pa_plot(ptype = "trap", model_res = model_res)
      if(data$gill_fleet) plot_list$resid_pa_gill = resid_pa_plot(ptype = "gill", model_res = model_res)
  }
  return(plot_list)
}


#' @title plot_pdf
#' 
#' @description 
#' 
#' @param type
#' @param file_path
#' 
plot_pdf = function(type, model_res = NULL, file_path = "") {
  # name of pdf
  export_name = switch(type,
    "data" = paste0(file_path, data$model_name, "_data_plots.pdf"),
    "results" = paste0(file_path, data$model_name, "_results_plots.pdf")
  )
  plots = plot_list(type = type, model_res = model_res)
  # export
  pdf(file = export_name, width = 9, height = 7)
    for(i in 1:length(plots)) print(plots[[i]])
  dev.off()
}

#' @title plot_ind
#' 
#' @description 
#' 
#' @param type
#' @param file_path
#' 
plot_ind = function() {

}


#' @title lh_plot
#' 
#' @description 
#' 
#' @param ptype
#' 
lh_plot = function(ptype) {
  # colors by year
  ramp = grDevices::colorRamp(c("darkorange", "purple4"))
  col_vec = grDevices::rgb(ramp(seq(0, 1, length = data$n_years)), max = 255)
  # Weight-at-age
  if (ptype == "wa") {
    if (!is.null(data$model_name)) {
      my_title = paste0("Population Weight-at-age for ", data$model_name)
    } else {
      my_title = "Population Weight-at-age"
    }
    wa = data$wa
    rownames(wa) = data$years
    colnames(wa) = (data$ages[1] - 1):data$ages[data$n_ages]
    wa = wa |>
      as_tibble(rownames = "year") |>
      gather(colnames(wa), key = "age", value = "value") |>
      mutate_at(c("age", "value"), as.numeric)

    p = ggplot(wa, aes(x = age, y = value, color = year)) +
      geom_line() +
      scale_color_manual(name = "Years", values = col_vec) +
      labs(x = "Ages", y = "", title = my_title) +
      theme_qfc() +
      theme(
        text = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 14)
      )
  }
  # Length-at-age
  if (ptype == "la") {
    if (!is.null(data$model_name)) {
      my_title = paste0("Population Length-at-age for ", data$model_name)
    } else {
      my_title = "Population Length-at-age"
    }
    la = data$la
    rownames(la) = data$years
    colnames(la) = data$ages
    la = la |>
      as_tibble(rownames = "year") |>
      gather(colnames(la), key = "age", value = "value") |>
      mutate_at(c("age", "value"), as.numeric)

    p = ggplot(la, aes(x = age, y = value, color = year)) +
      geom_line() +
      scale_color_manual(name = "Years", values = col_vec) +
      labs(x = "Ages", y = "", title = my_title) +
      theme_qfc() +
      theme(
        text = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 14)
      )
  }
  # Maturity-at-age
  if (ptype == "mat") {
    if (!is.null(data$model_name)) {
      my_title = paste0("Population Maturity-at-age for ", data$model_name)
    } else {
      my_title = "Population Maturity-at-age"
    }
    mat = data$mat
    rownames(mat) = data$years
    colnames(mat) = data$ages
    mat = mat |>
      as_tibble(rownames = "year") |>
      gather(colnames(mat), key = "age", value = "value") |>
      mutate_at(c("age", "value"), as.numeric)

    p = ggplot(mat, aes(x = age, y = value, color = year)) +
      geom_line() +
      scale_color_manual(name = "Years", values = col_vec) +
      labs(x = "Ages", y = "", title = my_title) +
      theme_qfc() +
      theme(
        text = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 14)
      )
  }

  return(p)
}


#' @title N_plot
#' 
#' @description 
#' 
#' @param ptype
#' 
N_plot = function(ptype, model_res = NULL) {
 if(ptype == "biomass_dat") {
  if (!is.null(data$model_name)) {
      my_title = paste0("Population Biomass for ", data$model_name)
  } else {
    my_title = "Population Biomass"
  }
  biomass_trap = as_tibble(data$biomass_trap) |>
                  mutate(year = data$years,
                        fishery = "Trap net") 
  if(data$gill_fleet) {
    biomass_gill = as_tibble(data$biomass_gill) |>
                  mutate(year = data$years,
                        fishery = "Gill net")
    biomass = bind_rows(biomass_trap, biomass_gill)
    biomass_scale = sci_transl(biomass$value)
    biomass$value = biomass_scale$r 
  } else {
    biomass = biomass_trap
    biomass_scale = sci_transl(biomass$value)
    biomass$value = biomass_scale$r
  }  

  p = ggplot(biomass, aes(x = year, y = value, color = fishery)) +
      geom_line() +
      scale_color_manual(name = "Fishery", values = c("#1B9E77", "#7570B3")) +
      labs(y = paste0("Biomass (", biomass_scale$label, ")"), x = "Years", 
          title = my_title) +
      theme_qfc() +
      theme(
        text = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 14)
      )
 }
 if(ptype == "biomass_est") {
  if (!is.null(data$model_name)) {
      my_title = paste0("Predicted Biomass for ", data$model_name)
  } else {
    my_title = "Predicted Biomass"
  }
  biomass_trap = as_tibble(model_res$biomass_trap) |>
                  mutate(year = data$years,
                        fishery = "Trap net") 
  if(data$gill_fleet) {
    biomass_gill = as_tibble(model_res$biomass_gill) |>
                  mutate(year = data$years,
                        fishery = "Gill net")
    biomass = bind_rows(biomass_trap, biomass_gill)
    biomass_scale = sci_transl(biomass$value)
    biomass$value = biomass_scale$r 
  } else {
    biomass = biomass_trap
    biomass_scale = sci_transl(biomass$value)
    biomass$value = biomass_scale$r
  }  

  p = ggplot(biomass, aes(x = year, y = value, color = fishery)) +
      geom_line() +
      scale_color_manual(name = "Fishery", values = c("#1B9E77", "#7570B3")) +
      labs(y = paste0("Biomass (", biomass_scale$label, ")"), x = "Years", 
          title = my_title) +
      theme_qfc() +
      theme(
        text = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 14)
      )
 }
 if(ptype == "nage") {
  if (!is.null(data$model_name)) {
      my_title = paste0("Estimated Abundance for ", data$model_name)
  } else {
    my_title = "Predicted Biomass"
  }
  nage = as_tibble(model_res$nage) |>
                  mutate(year = data$years) 
  nage_scale = sci_transl(nage$value)
  nage$value = nage_scale$r

  p = ggplot(nage, aes(x = year, y = value)) +
      geom_line() +
      labs(y = paste0("Number of Fish (", nage_scale$label, ")"), x = "Years", 
          title = my_title) +
      theme_qfc() +
      theme(
        text = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 14)
      )
 }
 if(ptype == "recruit") {
  if (!is.null(data$model_name)) {
      my_title = paste0("Recruitment of Age-", data$fage, "for ", data$model_name)
  } else {
    my_title = paste0("Recruitment of Age-", data$fage)
  }
  recr = as_tibble(model_res$recr) |>
                  mutate(year = data$years) 
  recr_scale = sci_transl(recr$value)
  recr$value = recr_scale$r

  p = ggplot(recr, aes(x = year, y = value)) +
      geom_line() +
      labs(y = paste0("Recruitment (", recr_scale$label, ")"), x = "Years", 
          title = my_title) +
      theme_qfc() +
      theme(
        text = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 14)
      )
 }
 if(ptype == "sp_biomass") {
  if (!is.null(data$model_name)) {
      my_title = paste0("Spawning Biomass for ", data$model_name)
  } else {
    my_title = paste0("Spawning Biomass")
  }
  sp_biomass = as_tibble(model_res$sp_biomass) |>
                  mutate(year = data$years) 
  sp_biomass_scale = sci_transl(sp_biomass$value)
  sp_biomass$value = sp_biomass_scale$r

  p = ggplot(sp_biomass, aes(x = year, y = value)) +
      geom_line() +
      labs(y = paste0("Spawning Biomass (", sp_biomass_scale$label, ")"), x = "Years", 
          title = my_title) +
      theme_qfc() +
      theme(
        text = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 14)
      )
 }

 return(p)
}


#' @title eff_plot
#' 
#' @description 
#' 
#' 
eff_plot = function() {
  if (!is.null(data$model_name)) {
    my_title = paste0("Observed Effort for ", data$model_name)
  } else {
    my_title = "Observed Effort"
  }
  eff_trap = as_tibble(data$obs_eff_trap) |>
            mutate(year = data$years,
                  fishery = "Trap net (Lifts)")
  if(data$gill_fleet) {
    eff_gill = as_tibble(data$obs_eff_gill * data$eff_gill_adj) |>
                mutate(year = data$years,
                fishery = "Gill net (Feet)")
    eff = bind_rows(eff_trap, eff_gill)
  } else {
    eff = eff_trap
  }

  p = ggplot(eff, aes(x = year, y = value, color = fishery)) +
      geom_line() +
      scale_color_manual(values = c("#1B9E77", "#7570B3")) +
      facet_wrap( ~ fishery, 
          strip.position = "left", 
          scale = "free_y", 
          ncol = 1) +
      labs(y = "", x = "Years", title = my_title) +
      theme_qfc() +
      theme(
        text = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        strip.placement="outside",
        strip.text=element_text(size=14),
        legend.position = "none"
      )

    return(p)
}


#' @title mort_plot
#' 
#' @description 
#' 
#' 
mort_plot = function(model_res) {
  if (!is.null(data$model_name)) {
    my_title = paste0("Mortality Rates for ", data$model_name)
  } else {
    my_title = "Mortality Rates"
  }
  df = data.frame(matrix(nrow = data$n_years, ncol = 0))
  # natural
  M = rep(model_res$M, data$n_years) |>
        as_tibble() |>
        mutate(year = data$years,
              fishery = "3_Natural")
  df = rbind(df, M)
  # trap
  FM_trap = model_res$FM_trap |>
        rowSums() |>
        as_tibble() |>
        mutate(year = data$years,
              fishery = "1_Trap net")
  df = rbind(df, FM_trap)
  # gill
  if(data$gill_fleet) {
    FM_gill = model_res$FM_gill |>
          rowSums() |>
          as_tibble() |>
          mutate(year = data$years,
                fishery = "2_Gill net")
    df = rbind(df, FM_gill)
  }

  p = ggplot(df, aes(x = year, y = value, fill = fishery)) +
      geom_bar(stat = "identity") +
      labs(y = "", x = "Years", title = my_title) +
      scale_fill_manual(values = grDevices::colors()[c(23,89,12)],
                        label = c("Nautural", "Gill net", "Trap net")) +
      theme_qfc() +
      theme(legend.position = "top",
            legend.spacing.x = unit(0.3, 'cm'),
            legend.direction = "horizontal",
            legend.title = element_blank(),
            text = element_text(size = 14),
            axis.text = element_text(size = 14))

  return(p)
}


#' @title ct_plot
#' 
#' @description 
#' 
#' 
ct_plot = function(ptype, model_res) {
  if(ptype == "trap") {
    if (!is.null(data$model_name)) {
      my_title = paste0("Trap Net Harvest for ", data$model_name)
    } else {
      my_title = "Trap Net Harvest"
    }
    ct = as_tibble(model_res$ct_trap) |>
            mutate(year = data$years,
                  type = "Predicted")
    obs = as_tibble((data$biomass_trap / data$mn_wt_trap) / data$harv_trap_adj) |>
            mutate(year = data$years,
                  type = "Observed")
    ct = rbind(ct, obs)
    ct_scale = sci_transl(ct$value)
    ct$value = ct_scale$r

    p = ggplot(ct, aes(x = year, y = value, color = type, shape = type)) +
          geom_line(data = filter(ct, type == "Predicted")) +
          geom_point(data = filter(ct, type == "Observed")) +
          scale_colour_manual(values =  c("red", "black"),
                       guide = guide_legend(override.aes = list(
                          linetype = c("blank","solid"),
                          shape = c(19, NA)))) +
          scale_shape_manual(values = c(19, NA)) +
          labs(y = paste0("Harvest (", ct_scale$label, ")"), x = "Years", title = my_title) +
          theme_qfc() +
          theme(legend.position = "top",
                legend.spacing.x = unit(0.3, 'cm'),
                legend.direction = "horizontal",
                legend.title = element_blank(),
                text = element_text(size = 14),
                axis.text = element_text(size = 14))
  }
  if(ptype == "gill") {
    if (!is.null(data$model_name)) {
      my_title = paste0("Gill Net Harvest for ", data$model_name)
    } else {
      my_title = "Gill Net Harvest"
    }
    ct = as_tibble(model_res$ct_gill) |>
            mutate(year = data$years,
                  type = "Predicted")
    obs = as_tibble((data$biomass_gill / data$mn_wt_gill) / data$harv_gill_adj) |>
            mutate(year = data$years,
                  type = "Observed")
    ct = rbind(ct, obs)
    ct_scale = sci_transl(ct$value)
    ct$value = ct_scale$r

    p = ggplot(ct, aes(x = year, y = value, color = type, shape = type)) +
          geom_line(data = filter(ct, type == "Predicted")) +
          geom_point(data = filter(ct, type == "Observed")) +
          scale_colour_manual(values =  c("red", "black"),
                       guide = guide_legend(override.aes = list(
                          linetype = c("blank","solid"),
                          shape = c(19, NA)))) +
          scale_shape_manual(values = c(19, NA)) +
          labs(y = paste0("Harvest (", ct_scale$label, ")"), x = "Years", title = my_title) +
          theme_qfc() +
          theme(legend.position = "top",
                legend.spacing.x = unit(0.3, 'cm'),
                legend.direction = "horizontal",
                legend.title = element_blank(),
                text = element_text(size = 14),
                axis.text = element_text(size = 14))
  }

  return(p)
}


#' @title pa_plot
#' 
#' @description 
#' 
#' 
pa_plot = function(ptype, model_res) {
  if(ptype == "trap") {
    if (!is.null(data$model_name)) {
      my_title = paste0("Trap Net Proportion at Age for ", data$model_name)
    } else {
      my_title = "Trap Net Proportion at Age"
    }
    obs_pa_trap = data$obs_pa_trap
    colnames(obs_pa_trap) = data$fage:data$lage
    rownames(obs_pa_trap) = data$fyear:data$lyear
    pa_trap = model_res$pa_trap
    colnames(pa_trap) = data$fage:data$lage
    rownames(pa_trap) = data$fyear:data$lyear
    obs_pa_trap = obs_pa_trap |>
                as_tibble(rownames = "year") |>
                gather(colnames(obs_pa_trap), key = "age", value = "value") 
    obs_pa_trap$age = as.numeric(obs_pa_trap$age)
    pa_trap = pa_trap |>
                as_tibble(rownames = "year") |>
                gather(colnames(pa_trap), key = "age", value = "value") 
    pa_trap$age = as.numeric(pa_trap$age)
    ym = round(max(pa_trap$value),1)

    p <- obs_pa_trap |> 
            ggplot(aes(x = age, y = value, group = year)) +
            geom_area() + 
            geom_line() + geom_point() +
            facet_wrap(~year, strip.position = "right") +
            scale_x_continuous(breaks = c(4,8,12,16)) +
            scale_y_continuous(breaks=seq(0,ym,ym/2)) +
            geom_line(data= pa_trap, color = "red") +
            labs(y = "", x = "Ages", title = my_title) +
            theme_qfc() +
            theme(legend.position = "top",
                legend.spacing.x = unit(0.3, 'cm'),
                legend.direction = "horizontal",
                legend.title = element_blank(),
                text = element_text(size = 14),
                axis.text = element_text(size = 14))
  }
  if(ptype == "gill") {
    if (!is.null(data$model_name)) {
      my_title = paste0("Gill Net Proportion at Age for ", data$model_name)
    } else {
      my_title = "Gill Net Proportion at Age"
    }
    obs_pa_gill = data$obs_pa_gill
    colnames(obs_pa_gill) = data$fage:data$lage
    rownames(obs_pa_gill) = data$fyear:data$lyear
    pa_gill = model_res$pa_gill
    colnames(pa_gill) = data$fage:data$lage
    rownames(pa_gill) = data$fyear:data$lyear
    obs_pa_gill = obs_pa_gill |>
                as_tibble(rownames = "year") |>
                gather(colnames(obs_pa_gill), key = "age", value = "value") 
    obs_pa_gill$age = as.numeric(obs_pa_gill$age)
    pa_gill = pa_gill |>
                as_tibble(rownames = "year") |>
                gather(colnames(pa_gill), key = "age", value = "value") 
    pa_gill$age = as.numeric(pa_gill$age)
    ym = round(max(pa_gill$value),1)

    p <- obs_pa_gill |> 
            ggplot(aes(x = age, y = value, group = year)) +
            geom_area() + 
            geom_line() + geom_point() +
            facet_wrap(~year, strip.position = "right") +
            scale_x_continuous(breaks = c(4,8,12,16)) +
            scale_y_continuous(breaks=seq(0,ym,ym/2)) +
            geom_line(data= pa_gill, color = "red") +
            labs(y = "", x = "Ages", title = my_title) +
            theme_qfc() +
            theme(legend.position = "top",
                legend.spacing.x = unit(0.3, 'cm'),
                legend.direction = "horizontal",
                legend.title = element_blank(),
                text = element_text(size = 14),
                axis.text = element_text(size = 14))
  }

  return(p)
}


#' @title sel_plot
#' 
#' @description 
#' 
#' 
sel_plot = function(ptype, model_res) {
  # colors by year
  ramp = grDevices::colorRamp(c("darkorange", "purple4"))
  col_vec = grDevices::rgb(ramp(seq(0, 1, length = data$n_years)), max = 255)

  if(ptype == "trap") {
    if (!is.null(data$model_name)) {
      my_title = paste0("Trap Net Selectivity for ", data$model_name)
    } else {
      my_title = "Trap Net Selectivity"
    }
    sel = model_res$sel_trap
    rownames(sel) = data$years
    colnames(sel) = data$ages
    sel = sel |>
      as_tibble(rownames = "year") |>
      gather(colnames(sel), key = "age", value = "value") |>
      mutate_at(c("age", "value"), as.numeric)

    p = ggplot(sel, aes(x = age, y = value, color = year)) +
      geom_line() +
      scale_color_manual(name = "Years", values = col_vec) +
      labs(x = "Ages", y = "", title = my_title) +
      theme_qfc() +
      theme(
        text = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 14)
      )
  }
  if(ptype == "gill") {
    if (!is.null(data$model_name)) {
      my_title = paste0("Gill Net Selectivity for ", data$model_name)
    } else {
      my_title = "Gill Net Selectivity"
    }
    sel = model_res$sel_gill
    rownames(sel) = data$years
    colnames(sel) = data$ages
    sel = sel |>
      as_tibble(rownames = "year") |>
      gather(colnames(sel), key = "age", value = "value") |>
      mutate_at(c("age", "value"), as.numeric)

    p = ggplot(sel, aes(x = age, y = value, color = year)) +
      geom_line() +
      scale_color_manual(name = "Years", values = col_vec) +
      labs(x = "Ages", y = "", title = my_title) +
      theme_qfc() +
      theme(
        text = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 14)
      )
  }

  return(p)
}


#' @title q_plot
#' 
#' @description 
#' 
#' 
q_plot = function(model_res) {
  if (!is.null(data$model_name)) {
    my_title = paste0("Catchability for ", data$model_name)
  } else {
    my_title = "Catchability"
  }
  q_trap = as_tibble(model_res$q_trap) |>
                  mutate(year = data$years,
                        fishery = "Trap net") 
  if(data$gill_fleet) {
    q_gill = as_tibble(model_res$q_gill) |>
                  mutate(year = data$years,
                        fishery = "Gill net")
    q = bind_rows(q_trap, q_gill)
  } else {
    q = q_trap
  }  

  p = ggplot(q, aes(x = year, y = value, color = fishery)) +
      geom_line() +
      scale_color_manual(name = "Fishery", values = c("#1B9E77", "#7570B3")) +
      labs(y = "", x = "Years", 
          title = my_title) +
      theme_qfc() +
      theme(
        text = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 14)
      )

  return(p)
}


#' @title resid_pa_plot
#' 
#' @description 
#' 
#' 
resid_pa_plot = function(ptype, model_res) {

  if(ptype == "trap") {
    if (!is.null(data$model_name)) {
      my_title = paste0("Trap Net Fishery Age Residuals for ", data$model_name)
    } else {
      my_title = "Trap Net Fishery Age Residuals"
    }
    comp_residt = model_res$resid_pa_trap
    colnames(comp_residt) = data$fage:data$lage
    rownames(comp_residt) = data$fyear:data$lyear
    comp_residt = comp_residt |>
                as_tibble(rownames = "year") |>
                gather(colnames(comp_residt), key = "age", value = "value") 
    comp_residt$age = as.numeric(comp_residt$age)
    comp_residt$year = as.numeric(comp_residt$year)
    comp_residt = comp_residt |>
                    mutate(direct = ifelse(value > 0, "pos", "neg"))
    comp_residt1 = comp_residt |> filter(direct == "neg")
    comp_residt1$value = abs(comp_residt1$value)
    comp_residt2 = comp_residt |> filter(direct == "pos")
    comp_residt2$value = abs(comp_residt2$value)

    p <- ggplot() +
            geom_point(data = comp_residt1, aes(x = year, y = age, size = value), alpha = 0.75, colour = "red") +
            geom_point(data = comp_residt2, aes(x = year, y = age, size = value), alpha = 0.75, colour = "blue") +
            ylab("Age") +
            labs(title = my_title) +
            scale_x_continuous("Year", seq(1985, 2020, 5)) +
            scale_fill_identity(name = "Proportion at age", guide = "legend") +
            theme_qfc() +
            theme(
              text = element_text(size = 14),
              axis.text = element_text(size = 14),
              legend.position = "none"
            )
  }
  if(ptype == "gill") {
    if (!is.null(data$model_name)) {
      my_title = paste0("Gill Net Fishery Age Residuals for ", data$model_name)
    } else {
      my_title = "Gill Net Fishery Age Residuals"
    }
    comp_residt = model_res$resid_pa_gill
    colnames(comp_residt) = data$fage:data$lage
    rownames(comp_residt) = data$fyear:data$lyear
    comp_residt = comp_residt |>
                as_tibble(rownames = "year") |>
                gather(colnames(comp_residt), key = "age", value = "value") 
    comp_residt$age = as.numeric(comp_residt$age)
    comp_residt$year = as.numeric(comp_residt$year)
    comp_residt = comp_residt |>
                    mutate(direct = ifelse(value > 0, "pos", "neg"))
    comp_residt1 = comp_residt |> filter(direct == "neg")
    comp_residt1$value = abs(comp_residt1$value)
    comp_residt2 = comp_residt |> filter(direct == "pos")
    comp_residt2$value = abs(comp_residt2$value)

    p <- ggplot() +
            geom_point(data = comp_residt1, aes(x = year, y = age, size = value), alpha = 0.75, colour = "red") +
            geom_point(data = comp_residt2, aes(x = year, y = age, size = value), alpha = 0.75, colour = "blue") +
            ylab("Age") +
            labs(title = my_title) +
            scale_x_continuous("Year", seq(1985, 2020, 5)) +
            scale_fill_identity(name = "Proportion at age", guide = "legend") +
            theme_qfc() +
            theme(
              text = element_text(size = 14),
              axis.text = element_text(size = 14),
              legend.position = "none"
            )
  }

  return(p)
}