source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')
##############################

study_met <- readRDS('./data/studies_metrics.Rds')

#############################

study_met <- study_met[, .(id, study_area, study_area_type, country, continent, 
                           lat_mean, lon_mean, COR, RMSE, POD, FAR, CSI)]


wmap <- readOGR("./data/ne_110m_admin_0_countries", layer="ne_110m_admin_0_countries") 
wmap_robin <- spTransform(wmap, CRS("+proj=robin"))
wmap_robin_df <- fortify(wmap_robin, region = "NAME")


names(study_met)[1] <- "id2"
names(study_met)[4] <- "NAME"

wmap_join_df <- merge(wmap_robin_df, study_met, by.x = "id", by.y = "NAME", all = TRUE)


wmap_join_df2 <- wmap_join_df[order(wmap_join_df$order),]

wmap_join_df3 <- as.data.table(wmap_join_df2)

wmap_join_df4 <- wmap_join_df3[, ':='(COR_med = median(COR, na.rm = TRUE), 
                                      POD_med = median(POD, na.rm = TRUE), 
                                      FAR_med = median(FAR, na.rm = TRUE)), by = id]


  
######################

# add graticule and bounding box (longlat)
grat <- readOGR("./data/ne_110m_graticules_all", layer="ne_110m_graticules_30") 
grat_df <- fortify(grat)

bbox <- readOGR("./data/ne_110m_graticules_all", layer="ne_110m_wgs84_bounding_box") 
bbox_df<- fortify(bbox)

# graticule (Robin)
grat_robin <- spTransform(grat, CRS("+proj=robin"))  # reproject graticule
grat_df_robin <- fortify(grat_robin)

bbox_robin <- spTransform(bbox, CRS("+proj=robin"))  # reproject bounding box
bbox_robin_df <- fortify(bbox_robin)


# plot --------------------------------------------------------------------


#### POD

plot_POD <- ggplot() + 
  geom_polygon(data=bbox_robin_df, aes(x=long, y=lat), colour="black", fill="white", size = 0.3) + 
  #geom_polygon(fill="white") +
  geom_polygon(data=wmap_join_df4, aes(long,lat, group=group, fill=POD_med), color="white", size=0.3) + 
  geom_path(data=grat_df_robin, aes(long, lat, group=group, fill=NULL), linetype="dashed", color="grey50", size=0.2) +
  scale_fill_viridis("POD") + 
  scale_x_continuous("Longitude", labels = label_number(scale = 1/100000)) + 
  scale_y_continuous("Latitude", labels = label_number(scale = 1/100000)) +
  coord_fixed(ratio = 1) + 
  #theme_void()
  theme_small + 
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank())

ggsave("results/POD_global.png",
       width = 7.2, height = 5.3, units = "in", dpi = 600)

#### FAR

plot_FAR <- ggplot() + 
  geom_polygon(data=bbox_robin_df, aes(x=long, y=lat), colour="black", fill="white", size = 0.3) +
  geom_polygon(data=wmap_join_df4, aes(long,lat, group=group, fill = FAR_med), color="white", size=0.3) + 
  geom_path(data=grat_df_robin, aes(long, lat, group=group, fill=NULL), linetype="dashed", color="grey50", size=0.2) +
  scale_fill_viridis("FAR") + 
  scale_x_continuous("Longitude", labels = label_number(scale = 1/100000)) + 
  scale_y_continuous("Latitude", labels = label_number(scale = 1/100000)) +
  coord_equal() + 
  theme_small + 
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank())

ggsave("results/FAR_global.png",
       width = 7.2, height = 5.3, units = "in", dpi = 600)

#### COR

plot_COR <- ggplot() + 
  geom_polygon(data=bbox_robin_df, aes(x=long, y=lat), colour="black", fill="white", size = 0.3) + 
  geom_polygon(data=wmap_join_df4, aes(long,lat, group=group, fill=COR_med), color="white", size=0.3) + 
  geom_path(data=grat_df_robin, aes(long, lat, group=group, fill=NULL), linetype="dashed", color="grey50", size=0.2) +
  scale_fill_viridis("COR") + 
  scale_x_continuous("Longitude", labels = label_number(scale = 1/100000)) + 
  scale_y_continuous("Latitude", labels = label_number(scale = 1/100000)) +
  coord_equal() + 
  theme_small

ggsave("results/COR_global.png",
       width = 7.2, height = 5.3, units = "in", dpi = 600)


#### overlay the plots in single panel
g2 <- ggplotGrob(plot_POD)
g3 <- ggplotGrob(plot_FAR)
g4 <- ggplotGrob(plot_COR)

g <- rbind(g2, g3, g4, size = "first")
g$widths <- unit.pmax(g2$widths, g3$widths)
grid.newpage()
grid.draw(g)



ggsave("results/plots_paper/COR_POD_30_rev1.png", g,
       width = 7.2, height = 6.5, units = "in", dpi = 600)
