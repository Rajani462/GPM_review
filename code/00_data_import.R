#Import raw data 

source('./source/libraries.R')

studies <- fread('./data/raw/studies.csv', 
                 header = T, 
                 stringsAsFactors = T) #All strings should be factors (categorical variables)

hydrological <- fread('./data/raw/Hydrological_extremes.csv',
                    header = T,
                    stringsAsFactors = T)

#testing that are imported correctly

studies$id
str(studies)

saveRDS(studies, file = './data/studies.Rds')
saveRDS(hydrological, file = './data/hydrological.Rds')
