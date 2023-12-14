# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#
# 	Demo of GLAM and RTMB
#
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#

## Setup ####
# install.packages('RTMB', repos = c('https://kaskr.r-universe.dev', 'https://cloud.r-project.org'))
library(RTMB)
# for plotting
# devtools::install_github("QFCatMSU/gg-qfc")
library(ggqfc)
library(tidyverse)
library(ggplot2)

## R scripts for running GLAM 
source("R/glam.R")
source("R/run_glam.R")
source("R/check_convergence.R")
source("R/read_pars.R")
source("R/run_retro.R")
source("R/run_peel.R")
source("R/ess_calc.R")


## Set up data and parameters
# demo dataset
load("data/WF_sim_data.Rdata")
# parameters
pars = read_pars(log_sig = -2,
                        log_M = data$log_M_init,
                        log_q_trap = -5,
                        log_q_gill = -5,
                        log_q_rec = NULL,
                        log_q_trap_dev = numeric(data$n_years - 1),
                        log_q_gill_dev = numeric(data$n_years - 1),
                        log_q_rec_dev = NULL,
                        log_sel_trap_p1 = 6.06,
                        log_sel_trap_p2 = -2.9,
                        log_sel_gill_p1 = -2.1,
                        log_sel_gill_p2 = 1.82,
                        log_sel_rec_p1 = NULL,
                        log_sel_rec_p2 = NULL,
                        log_sel_trap_dev = numeric(data$n_years - 1),
                        log_sel_gill_dev = numeric(data$n_years - 1),
                        log_sel_rec_dev = NULL,
                        log_pop_init = rep(9, 4),
                        log_recr_init = 12,
                        log_recr_avg = 12,
                        log_recr_dev = numeric(data$n_years - 1),
                        acor = 0.5
                        )


## Plot data ####

## Run GLAM ####
fixed_names = NULL
rand_names = NULL
res = run_glam(nlminb_control = list(
                      eval.max = 1e4,
                      iter.max = 1e4
                    ),
                    hessian_run = FALSE,
                    run_newton = TRUE,
                    n_newton = 3,
                    fixed_names = fixed_names,
                    rand_names = rand_names)


## Plot results ####