#Import data from probes

source('./source/libraries.R')

gpm_run_01 <- fread('./data/raw/GPM_review', header = T, 
                      col.names = 'value')

save(gpm_run_01, file = './data/gpm_review_01.Rdata')