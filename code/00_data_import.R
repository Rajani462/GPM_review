#Import raw data 

source('./source/libraries.R')

studies <- fread('./data/raw/studies.csv', 
                 header = T, 
                 stringsAsFactors = T) #All strings should be factors (categorical variables)

#testing that are imported correctly

studies$id
str(studies)

saveRDS(studies, file = './data/studies.Rds') #Not any more

