# Data pre-processing.

source('./source/libraries.R')


studies <- readRDS('./data/studies.Rds')

#Dividing the data-base into multiple small data-table

study_area <- studies[, .(id, study_area, study_area_type, country, continent, 
                          lat_mean, lon_mean, area, variable, surface)]
gpm_vers <- studies[, .(id, gpm_type)]
gpm_alg <- studies[, .(id, gpm_algorithm)]
gpm_record <- studies[, .(id, record_start, record_end, record_length)]

study_scale <- studies[, .(id, temporal_scale, grid_scale, comparison_scale, comprison_method)]
study_stats <- studies[, .(id, timeseries_eval)]
study_stats2 <- studies[, .(id, categ_eval)]

study_perfm <- studies[, .(id, best_perform, worst_perform, limitations, reference, year, journal)]




test <- studies[, .(id, gpm_algorithm)]
study_stats_split <- separate(test, 2, sep = ',', into = names)
study_stats_tidy <- melt(study_stats_split, id.vars = 'id')
study_stats_tidy <- study_stats_tidy[complete.cases(study_stats_tidy)]



split_tidy <- function(x) {
  names <- c("v1", "v2", "v3", "v4", "v5", "v6", "v7")
  study_split <- separate(x, 2, sep = ',', into = names)
  study_tidy <- melt(study_split, id.vars = 'id') 
  study_tidy <- study_tidy[complete.cases(study_tidy)] 
  return(study_tidy)
}

test3 <- split_tidy(test2)



