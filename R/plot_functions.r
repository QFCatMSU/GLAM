library(ggplot2)

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
plot_list = function(type) {
  if(type == "data") {
    # weight at age, length at age, maturity at age, biomass, effort 
    plot_list = suppressWarnings(list(
        lh_plot(ptype = "wa"), lh_plot(ptype = "la"), lh_plot(ptype = "mat"),
        N_plot(ptype = "biomass_dat"), eff_plot()
    ))
  } else if(type == "results") {
    #
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
plot_pdf = function(type, file_path = "") {
  # name of pdf
  export_name = switch(type,
    "data" = paste0(file_path, data$model_name, "_data_plots.pdf"),
    "results" = paste0(data$model_name, "_results_plots.pdf")
  )
  plots = plot_list(type = type)
  # export
  pdf(file = export_name, width = 9, height = 7)
    for(i in 1:length(plots)) print(plots[[i]])
  dev.off()
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
N_plot = function(ptype) {
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

 return(p)
}


#' @title eff_plot
#' 
#' @description 
#' 
#' 
eff_plot = function() {
  if (!is.null(data$model_name)) {
    my_title = paste0(" Observed Effort for ", data$model_name)
  } else {
    my_title = "Observed Effort"
  }
  browser()
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
