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




wmap <- readOGR("C:/Users/rkpra/OneDrive/Documents/R_projects/GPM_review/data/ne_110m_admin_0_countries", layer="ne_110m_admin_0_countries") 
wmap_robin <- spTransform(wmap, CRS("+proj=robin"))
wmap_robin_df <- fortify(wmap_robin, region = "NAME")

names(study_met)[1] <- "id2"
names(study_met)[4] <- "NAME"

wmap_join_df <- merge(wmap_robin_df, study_met, by.x = "id", by.y = "NAME", all = TRUE)

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

wmap_join_df2 <- wmap_join_df[order(wmap_join_df$order),]

wmap_join_df3 <- as.data.table(wmap_join_df2)

wmap_join_df4 <- wmap_join_df3[, ':='(COR_med = median(COR, na.rm = TRUE), 
                                                POD_med = median(POD, na.rm = TRUE)), by = id]

# plot map
ggplot(wmap_join_df4, aes(long,lat, group=group, fill=POD_med)) + 
  geom_polygon(data=wmap_join_df4, aes(long,lat, group=group), color="white", size=0.3) + 
  geom_path(data=wmap_join_df4, aes(long,lat, group=group), color="white", size=0.3) + 
  scale_fill_viridis(breaks=c(0.2, 0.4, 0.6, 0.8, 0.9), name="POD", 
                     guide = guide_legend( keyheight = unit(3, units = "mm"), 
                                           keywidth=unit(12, units = "mm"), 
                                           label.position = "bottom", 
                                           title.position = 'top', nrow=1)) + 
  labs(title="IMERG Probability Of Detection (POD)") + 
  theme(legend.position = c(0.6, 0.22)) + 
  coord_equal()



ggplot(wmap_join_df4, aes(long,lat, group=group, fill=POD_med)) + 
  scale_x_continuous(limits = c(-16810131,16810131 )) + 
  scale_y_continuous(limits = c(-6825155, 8243004)) + 
  geom_polygon(data=wmap_join_df4, aes(long,lat, group=group), color="white", size=0.3) + 
  geom_path(data=wmap_join_df4, aes(long,lat, group=group), color="white", size=0.3) + 
  scale_fill_viridis(breaks=c(0.2, 0.4, 0.6, 0.8, 0.9), name="POD", 
                     guide = guide_legend( keyheight = unit(3, units = "mm"), 
                                           keywidth=unit(12, units = "mm"), 
                                           label.position = "bottom", 
                                           title.position = 'top', nrow=1)) + 
  #labs(title="IMERG Probability Of Detection (POD)") + 
  theme(legend.position = c(0.6, 0.13)) + 
  coord_equal() + 
  theme_opts

  

#coord_equal() + 
#coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90)) + 
#theme_opts



ggplot(wmap_join_df4, aes(long,lat, group=group, fill = COR_med)) + 
  geom_polygon() + 
  scale_x_continuous(limits = c(-16810131,16810131 )) + 
  scale_y_continuous(limits = c(-6825155, 8243004)) +
  geom_path(data=wmap_join_df4, aes(long,lat, group=group, fill = COR_med), color="white", size=0.3) + 
  #scale_fill_viridis(breaks=c(0.2, 0.4, 0.6, 0.8, 1.0), name="COR", guide = guide_legend( keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1)) + 
  #labs(title="IMERG Coefficient Of Correlation (COR)") + 
  scale_fill_viridis("COR" , direction = 1) + 
  coord_equal() + 
  theme_small + 
  theme(legend.position = c(0.95, 0.6)) + 
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())



path <- "C:/Users/rkpra/OneDrive/Documents/R_projects/GPM_review/data/"
write.csv(wmap_join_df4, "C:/Users/rkpra/OneDrive/Documents/R_projects/GPM_review/data/wmap_join_df4.csv")

###########################################################################################
