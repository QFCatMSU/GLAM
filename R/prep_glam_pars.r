#' @title prep_glam_pars
#' 
#' @description put parameters in list to run for RTMB
#' 
#' @param log_sig log scale sigma value used to convert rhos to SD; for now all errors
#' @param log_M log scale natural mortality
#' @param log_q_trap log scale catchability of trapnet fleet
#' @param log_q_gill log scale catchability of gillnet fleet
#' @param log_q_rec log scale catchability of recreational fleet
#' @param log_q_trap_dev log scale catchability deviations of trapnet fleet
#' @param log_q_gill_dev log scale catchability deviations of gillnet fleet
#' @param log_q_rec_dev log scale catchability deviations of recreational fleet
#' @param log_sel_trap_p1 log scale selectivity parameter 1 for trapnet
#' @param log_sel_trap_p2 log scale selectivity parameter 2 for trapnet
#' @param log_sel_gill_p1 log scale selectivity parameter 1 for gillnet
#' @param log_sel_gill_p2 log scale selectivity parameter 2 for gillnet
#' @param log_sel_rec_p1 log scale selectivity parameter 1 for recreational
#' @param log_sel_rec_p2 log scale selectivity parameter 2 for recreational
#' @param log_sel_trap_dev log scale selectivity (random walk) deviations of trapnet fleet
#' @param log_sel_gill_dev log scale selectivity (random walk) deviations of gillnet fleet
#' @param log_sel_rec_dev log scale selectivity (random walk) deviations of recreational fleet
#' @param log_pop_init log scale initial population size scalar
#' @param log_recr_init log scale initial recruitment
#' @param log_recr_avg log scale average recruitment
#' @param log_recr_dev log scale recruitment deviations
#' @param acor autocorrelation for recruitment deviations
#' 

prep_glam_pars = function(log_sig,
                        log_M,
                        log_q_trap,
                        log_q_gill = NULL,
                        log_q_rec = NULL,
                        log_q_trap_dev = NULL,
                        log_q_gill_dev = NULL,
                        log_q_rec_dev = NULL,
                        log_sel_trap_p1,
                        log_sel_trap_p2,
                        log_sel_gill_p1 = NULL,
                        log_sel_gill_p2 = NULL,
                        log_sel_rec_p1 = NULL,
                        log_sel_rec_p2 = NULL,
                        log_sel_trap_dev = NULL,
                        log_sel_gill_dev = NULL,
                        log_sel_rec_dev = NULL,
                        log_pop_init,
                        log_recr_init,
                        log_recr_avg,
                        log_recr_dev,
                        acor
                        ){
    if (!"tidyverse" %in% rownames(installed.packages())) {
        install.packages("tidyverse")
    }
    require(tidyverse)
    
    pars = as.list(match.call())
    pars = pars |> discard(is.null)
    pars = pars[!names(pars) == ""]
    pars = lapply(pars, function(x) eval.parent(x))
    return(pars)
}
