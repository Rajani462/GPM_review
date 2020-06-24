###splitting and tidy function

split_tidy <- function(x) {
  name <- colnames(x[, 2])
col_names <- c("v1", "v2", "v3", "v4", "v5", "v6", "v7")
study_split <- separate(x, 2, sep = ',', into = col_names) #'2' select the second column
study_tidy <- melt(study_split, id.vars = 'id')
#study_tidy <- study_tidy[complete.cases(study_tidy)]
#study_tidy <- study_tidy[, .(id, variable_name = as.factor(value))]
}
#----------------------------------------------------------

namme <- colnames(gpm_alg[, 2])


col_names <- c("v1", "v2", "v3", "v4", "v5", "v6", "v7")
name <- colnames(study_tempscale[, 2])
study_split <- separate(study_tempscale, 2, sep = ',', into = col_names)

study_tidy <- melt(study_split, id.vars = 'id', measure = 2:7,
                   value.name = name, na.rm = TRUE, value.factor = TRUE, )
study_tidy

study_tidy <- study_tidy[complete.cases(study_tidy)]
study_tidy <- study_tidy[, .(id, variable_name = as.factor(value.name))]
