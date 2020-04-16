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

study_scale <- studies[, .(id, temporal_scale, grid_scale, comparison_scale, comprison_method)]

study_stats <- studies[, .(id, timeseries_eval)]
study_stats2 <- studies[, .(id, categ_eval)]

study_perfm <- studies[, .(id, best_perform, worst_perform, limitations, reference, year, journal)]



