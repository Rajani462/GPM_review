###splitting and tidy function

split_tidy <- function(x) {
col_names <- c("v1", "v2", "v3", "v4", "v5", "v6", "v7")
study_split <- separate(x, 2, sep = ',', into = col_names) #'2' select the second column
study_tidy <- melt(study_split, id.vars = 'id',
                   na.rm = TRUE, value.factor = TRUE)
#study_tidy <- study_tidy[complete.cases(study_tidy)]
study_tidy <- study_tidy[, .(id, variable_name = as.factor(value))]
study_tidy[variable_name == '', variable_name := NA]
study_tidy[, variable_name := droplevels(variable_name)]
}
#----------------------------------------------------------









