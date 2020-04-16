###spliting and tidy function

split_tidy <- function(x) {
names <- c("v1", "v2", "v3", "v4", "v5", "v6", "v7")
value_name <- names(x[, 2])
study_split <- separate(x, 2, sep = ',', into = names)
study_tidy <- melt(study_split, id.vars = 'id', value.name = value_name) 
study_tidy <- study_tidy[complete.cases(study_tidy)]
study_tidy <- study_tidy[, variable := NULL]
return(study_tidy)
}
#----------------------------------------------------------
