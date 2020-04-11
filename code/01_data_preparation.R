# Data pre-processing.

source('./source/libraries.R')


studies <- readRDS('./data/studies.Rds')

#Dividing the data-base into multiple small data-table

study_area <- studies[, .(id, study_area, study_area_type, country, continent, lat_mean, lon_mean, area)]
gpm_types <- studies[, .(id, gpm_type, gpm_algorithm, record_start, record_end, record_length)]
study_scale <- studies[, .(id, temporal_scale, grid_scale, comparison_scale, comprison_method)]
study_stats <- studies[, .(id, timeseries_eval, categ_eval)]
study_perfm <- studies[, .(id, best_perform, worst_perform, limitations, reference, year, journal)]

#Example
study_stats_split <- separate(study_stat, col = "timeseries_eval", into = c("v1", "v2", "v3", "v4", "v5", "v6", "v7" ))



