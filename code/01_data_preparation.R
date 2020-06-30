# Data pre-processing.

source('./source/libraries.R')
source('./source/functions.R')

studies <- readRDS('./data/studies.Rds')

#Dividing the data-base into multiple small data-table

study_area <- studies[, .(id, study_area, study_area_type, country, continent, 
                          lat_mean, lon_mean, area, variable, surface)]

run_type <- studies[, .(id, imerg_type)]
run_type <- split_tidy(run_type)

alg_vers <- studies[, .(id, imerg_vers)]
alg_vers<- split_tidy(alg_vers)



gpm_record <- studies[, .(id, record_start, record_end, record_length)]

study_tempscale <- studies[, .(id, temporal_scale)]
study_tempscale <- split_tidy(study_tempscale)

study_gridscale <- studies[, .(id, grid_scale)]
study_gridscale <- split_tidy(study_gridscale)

study_compscale <- studies[, .(id, comparison_scale)]
study_compscale <- split_tidy(study_compscale)

study_compmthod <- studies[, .(id, comparison_method)]
study_compmthod <- split_tidy(study_compmthod)


study_stats_time <- studies[, .(id, timeseries_eval)]
study_stats_time <- split_tidy(study_stats_time)

study_stats_cat <- studies[, .(id, categ_eval)]
study_stats_cat <- split_tidy(study_stats_cat)

study_perfm <- studies[, .(id, best_perform, worst_perform, limitations, reference, year, journal)]

