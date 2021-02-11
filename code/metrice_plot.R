library(rgdal)
library(ggplot2)
library(tidyverse)
library(data.table)
library(sp)
library(viridis)

studies_met <- fread('C:/Users/rkpra/OneDrive/Documents/R_projects/GPM_review/data/Metrices.csv', 
                     header = T, 
                     stringsAsFactors = T) #All strings should be factors (categorical variables)

#testing that are imported correctly

studies_met$id
str(studies_met)

saveRDS(studies_met, file = 'C:/Users/rkpra/OneDrive/Documents/R_projects/GPM_review/data/studies_metrics.Rds')
##############

study_met <- readRDS('C:/Users/rkpra/OneDrive/Documents/R_projects/GPM_review/data/studies_metrics.Rds')

#Dividing the data-base into multiple small data-table

study_met <- study_met[, .(id, study_area, study_area_type, country, continent, 
                           lat_mean, lon_mean, COR, RMSE, POD, FAR, CSI)]




countries <- readOGR("C:/Users/rkpra/OneDrive/Documents/R_projects/GPM_review/data/ne_110m_admin_0_countries", layer="ne_110m_admin_0_countries") 
#countries_robin <- spTransform(countries, CRS("+init=ESRI:54030"))
countries_robin_df <- fortify(countries, region = "NAME")

names(study_met)[1] <- "id2"
names(study_met)[4] <- "NAME"

countries_join_df <- merge(countries_robin_df, study_met, by.x = "id", by.y = "NAME", all = TRUE)

theme_opts <- list(theme(panel.grid.minor = element_blank(),
                         panel.grid.major = element_blank(),
                         panel.background = element_blank(),
                         plot.background = element_rect(fill="#e6e8ed"),
                         panel.border = element_blank(),
                         axis.line = element_blank(),
                         axis.text.x = element_blank(),
                         axis.text.y = element_blank(),
                         axis.ticks = element_blank(),
                         axis.title.x = element_blank(),
                         axis.title.y = element_blank(),
                         plot.title = element_text(size=22)))

countries_join_df2 <- countries_join_df[order(countries_join_df$order),]

countries_join_df3 <- as.data.table(countries_join_df2)

countries_join_df4 <- countries_join_df3[, ':='(COR_med = median(COR, na.rm = TRUE), 
                                                POD_med = median(POD, na.rm = TRUE)), by = id]

# plot map
ggplot(countries_join_df4, aes(long,lat, group=group, fill=POD_med)) + 
  geom_polygon() + 
  geom_path(data=countries_join_df4, aes(long,lat, group=group), color="white", size=0.3) + 
  scale_fill_viridis(breaks=c(0.2, 0.4, 0.6, 0.8), name="POD", guide = guide_legend( keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1)) + 
  labs(title="IMERG Probability Of Detection (POD)") + 
  theme(legend.position = c(0.7, 0.25)) + 
  coord_equal()





path <- "C:/Users/rkpra/OneDrive/Documents/R_projects/GPM_review/data/"
write.csv(countries_join_df4, "C:/Users/rkpra/OneDrive/Documents/R_projects/GPM_review/data/countries_join_df4.csv")

###########################################################################################
