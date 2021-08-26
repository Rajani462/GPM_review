
source('./source/libs.R')
source('./source/functions.R')

# read the datasets -------------------------------------------------------

studies <- readRDS('./data/studies.Rds')
hydrological <- readRDS('./data/hydrological.Rds')


# Pre-processing ----------------------------------------------------------


#Dividing the data-base into multiple small data-table

study_area <- studies[, .(id, study_area, study_area_type, country, continent, 
                          lat_mean, lon_mean, area, variable, surface)]

continent_type <- studies[, .(id, continent)]

study_country <- studies[, .(id, country)]
study_country <- split_tidy(study_country)
study_country <- subset(study_country, !is.na(country))

run_type <- studies[, .(id, imerg_type)]
run_type <- split_tidy(run_type)
run_type <- subset(run_type, !is.na(imerg_type))

alg_vers <- studies[, .(id, imerg_vers)]
alg_vers<- split_tidy(alg_vers)
alg_vers <- subset(alg_vers, !is.na(imerg_vers))

recordlength <- studies[, .(id, record_length)]


study_tempscale <- studies[, .(id, temporal_scale)]
study_tempscale <- split_tidy(study_tempscale)
study_tempscale <- subset(study_tempscale, !is.na(temporal_scale))

study_gridscale <- studies[, .(id, grid_scale)]
study_gridscale <- split_tidy(study_gridscale)
study_gridscale <- subset(study_gridscale, !is.na(grid_scale))

study_compscale <- studies[, .(id, comparison_scale)]
study_compscale <- split_tidy(study_compscale)
study_compscale <- subset(study_compscale, !is.na(comparison_scale))


study_compmthod <- studies[, .(id, comparison_method)]
study_compmthod <- split_tidy(study_compmthod)
study_compmthod <- subset(study_compmthod, !is.na(comparison_method))

study_stats_time <- studies[, .(id, timeseries_eval)]
study_stats_time <- split_tidy(study_stats_time)
study_gridscale <- subset(study_gridscale, !is.na(grid_scale))


study_stats_cat <- studies[, .(id, categ_eval)]
study_stats_cat <- split_tidy(study_stats_cat)
study_stats_cat <- subset(study_stats_cat, !is.na(categ_eval))


refr_type <- studies[, .(id, ref_type)]
refr_type <- split_tidy(refr_type)
refr_type <- subset(refr_type, !is.na(ref_type))

#refr_type <- study_plot[refr_type, on = 'id']

reference_type <-  studies[, .(id, gauge_eval, radar_eval, model_eval,
                               satellite_eval)]

refre_type <-  studies[, .(id, ref_type)]

vol_indices <- studies[, .(id, timeseries_eval)]
vol_indices <- split_tidy(vol_indices)
vol_indices <- subset(vol_indices, !is.na(timeseries_eval))

cat_indices <- studies[, .(id, categ_eval)]
cat_indices <- split_tidy(cat_indices)
cat_indices <- subset(cat_indices, !is.na(categ_eval))


#indices <- cat_indices[vol_indices, on = 'id']

study_perfm <- studies[, .(id, best_perform, worst_perform, limitations, reference, year, journal)]

#### Hydrological
hydro_metrics <- hydrological[, .(Ref, Location, Basin, Data_type, Model, NSE, Bias)]


# Saving ------------------------------------------------------------------
saveRDS(study_country, './data/study_country.rds')
saveRDS(continent_type, './data/continent_type.rds')
saveRDS(alg_vers, './data/alg_vers.rds')
saveRDS(run_type, './data/run_type.rds')
saveRDS(study_tempscale, './data/study_tempscale.rds')
saveRDS(study_gridscale, './data/study_gridscale.rds')
saveRDS(study_compscale, './data/study_compscale.rds')
saveRDS(study_compmthod, './data/study_compmthod.rds')
saveRDS(recordlength, './data/recordlength.rds')
saveRDS(refr_type, './data/refr_type.rds')
saveRDS(vol_indices, './data/vol_indices.rds')
saveRDS(cat_indices, './data/cat_indices.rds')
saveRDS(hydro_metrics, './data/hydro_metrics.rds')
