#detailed description of the code is in https://stackoverflow.com/questions/43984614/rggplot2geom-points-how-to-swap-points-with-pie-charts

source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')
##############################


# reading data-sets and define colours  --------------------------------------------------------
study_tempscale <- readRDS('./data/study_tempscale.rds')
study_gridscale <- readRDS('./data/study_gridscale.rds')
continent_type <- readRDS('./data/continent_type.rds')

mycol_gridscale7 <- c( "#69bdd2", "#739F3D", "#e07b39")


# pre-process before ploting ----------------------------------------------


ref_spat_tempo <- study_tempscale[study_gridscale, on = 'id']
ref_spat_tempo2<- ref_spat_tempo[continent_type, on = 'id']


ref_spat_tempo2$grid_scale<- unclass(ref_spat_tempo2$grid_scale)

ref_spat_tempo2$grid_scale[which(ref_spat_tempo2$grid_scale >= 3)]<-10 #change the grid scales from >0.25--3 into 10


ref_spat_tempo2$continent <- factor(ref_spat_tempo2$continent, 
                                    levels = c("Africa", "Asia", "Europe", "North America", "South America", "Global"))

ref_spat_tempo2$temporal_scale <- factor(ref_spat_tempo2$temporal_scale, 
                                         levels = c("0.5h", "1h", "3h", "6h", "12h", 
                                                    "daily",  "monthly",  "seasonal", "annual"))

#ref_spat_tempo5$grid_scale <- as.factor(ref_spat_tempo5$grid_scale, 
                                     #levels = c("0.1", "0.25", ">0.25"))


ref_spat_tempo2 <- subset(ref_spat_tempo2, !is.na(temporal_scale))
ref_spat_tempo2 <- subset(ref_spat_tempo2, !is.na(grid_scale))
ref_spat_tempo2 <- subset(ref_spat_tempo2, !is.na(continent))


subset_tempo <- ref_spat_tempo2[, .(count = .N), by = .(continent, temporal_scale, grid_scale)]


df_plot <- as.data.frame(subset_tempo) %>% 
  group_by(continent, temporal_scale) %>%
  mutate(total = sum(count)) %>%
  group_by(continent, temporal_scale, total)

which(is.na(df_plot))

#total_studies <- sum(df_plot$total)
#df_plot <- mutate(df_plot, percent = (total/total_studies)*100)

df_plot$continent <- unclass(df_plot$continent)
df_plot$temporal_scale <- unclass(df_plot$temporal_scale)
df_plot$grid_scale <- as.factor(df_plot$grid_scale)


df_plot$grid_scale <- factor(df_plot$grid_scale, levels = c("1", "2", "10"), ordered = TRUE)

df.grobs <-  df_plot%>% 
  do(subplots = ggplot(., aes(x = "", y = count, fill = grid_scale)) + 
       geom_col(position = "fill", alpha = 0.75, colour = "white") + 
       coord_polar(theta = "y") + 
       scale_fill_manual(labels = c("0.1", "0.25", ">0.25"), 
                         values=setNames(mycol_gridscale7, c(1, 2, 10))) + 
       theme_void()+ guides(fill = F)) %>% 
  mutate(subgrobs = list(annotation_custom(ggplotGrob(subplots),
                                           x = continent-12/20, y = temporal_scale-12/20, 
                                           xmax = continent+12/20, ymax = temporal_scale+12/20))) #size of the pie charts



final_plot <- df.grobs %>%
  {ggplot(data = ., aes(factor(continent), factor(temporal_scale))) + 
      scale_x_discrete("Continents", labels = c("1" = "Africa", "2" = "Asia", "3"  = "Europe", 
                                                "4"= "North America", "5" = "South America",
                                                "6" = "Golbal")) + 
      scale_y_discrete("Temporal scale", labels = c("1" = "0.5h", "2" = "1h", "3" = "3h",
                                                    "4" = "6h", "5" = "12h", "6" = "daily",
                                                    "7" = "monthly", "8" = "seasonal",
                                                    "9" = "annual")) + 
      .$subgrobs + 
      geom_text(aes(label = round(total, 2)), size = 3) + 
      geom_col(data = df_plot,
               aes(0,0, fill = grid_scale), 
               colour = "white") + 
      scale_fill_manual("Spatial scale", labels = c("0.1", "0.25", ">0.25"),
                        values=mycol_gridscale7)}

final_plot + theme_small + 
  theme(axis.text.x = element_text(angle = 40, hjust = 0.8, vjust = 0.9))


ggsave("results/plots_paper/spatio_temporal_continents2_rev1.png",
       width = 7.2, height = 5.3, units = "in", dpi = 600)

