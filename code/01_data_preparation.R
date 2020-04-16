# Data pre-processing.

source('./source/libraries.R')
source('./source/functions.R')

studies <- readRDS('./data/studies.Rds')

#Dividing the data-base into multiple small data-table

study_area <- studies[, .(id, study_area, study_area_type, country, continent, 
                          lat_mean, lon_mean, area, variable, surface)]

gpm_vers <- studies[, .(id, gpm_type)]
gpm_vers <- split_tidy(gpm_vers)

gpm_alg <- studies[, .(id, gpm_algorithm)]
gpm_alg <- split_tidy(gpm_alg)

gpm_record <- studies[, .(id, record_start, record_end, record_length)]

study_tempscale <- studies[, .(id, temporal_scale)]
study_tempscale <- split_tidy(study_tempsacle)

study_gridscale <- studies[, .(id, grid_scale)]
study_gridscale <- split_tidy(study_gridscale)

study_compscale <- studies[, .(comparison_scale)]
study_compscale <- split_tidy(study_compscale)

study_compmthod <- studies[, .(comprison_method)]
study_compmthod <- split_tidy(study_compmthod)

study_stats_time <- studies[, .(id, timeseries_eval)]
study_stats_time <- split_tidy(study_stats)

study_stats_cat <- studies[, .(id, categ_eval)]
study_stats_cat <- split_tidy(study_stats_cat)

study_perfm <- studies[, .(id, best_perform, worst_perform, limitations, reference, year, journal)]



