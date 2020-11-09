refr_type



ref_spat_tempo <- refr_type[study_tempscale, on = 'id']
ref_spat_tempo2 <- ref_spat_tempo[study_gridscale, on = 'id']
ref_spat_tempo2<- ref_spat_tempo2[continent_type, on = 'id']

library(writexl)
library(readxl)
write_xlsx(refr_type,  "Reference_type.xlsx")
write_xlsx(study_tempscale,  "Temporal_scale.xlsx")
write_xlsx(study_gridscale,  "Grid_scale.xlsx")
write_xlsx(ref_spat_tempo,  "Ref_tempo.xlsx")
write_xlsx(ref_spat_tempo2,  "Ref_tempo_grid.xlsx")



spat_temp <- study_tempscale[study_gridscale, on = 'id']
write_xlsx(spat_temp,  "Spat_temp.xlsx")


X = data.table(x = c(1,1,1,2,2,5,6), y = 1:7, key = "x")
Y = data.table(x = c(2,6), z = letters[2:1], key = "x")



ref_spat_tempo3 <- subset(ref_spat_tempo2, !is.na(temporal_scale))
ref_spat_tempo4 <- subset(ref_spat_tempo3, !is.na( grid_scale))
ref_spat_tempo5 <- subset(ref_spat_tempo4, !is.na(continent))
#spat_tempo <- study_tempscale[study_gridscale, on = 'id']
#spat_tempo_ref <- spat_tempo[refr_type, on = 'id']



dx <- data.table(a = c(1,1,1,1,2,2), b = 3:8)
dy <- data.table(a = c(1,1,2), c = 7:9)

dz <- dx[dy, on = 'a', by=.EACHI]


#subset_tempo <- imerg_combi[, .(continent, temporal_scale, grid_scale)]
#ref_spat_tempo2$grid_scale <- factor(ref_spat_tempo2$grid_scale, 
                                     #levels = c("0.1", "0.25"))
ref_spat_tempo5$continent <- factor(ref_spat_tempo5$continent, 
                                    levels = c("Africa", "Asia", "Europe", "North America",
                                               "South America", "Global"))
ref_spat_tempo5$temporal_scale <- factor(ref_spat_tempo5$temporal_scale, 
                                         levels = c("0.5h", "1h", "3h", "6h", "12h", 
                                                    "daily",  "monthly",  "seasonal", "annual"))
ref_spat_tempo5$grid_scale <- factor(ref_spat_tempo5$grid_scale, 
                                     levels = c("0.1", "0.25", "0.5", "1", "2", "2.5", "3"))
subset_tempo <- ref_spat_tempo5[, .(count = .N), by = .(grid_scale, temporal_scale, continent)]


#subset_tempo[, x2 := unclass(continent)]
#subset_tempo[, y2 := unclass(temporal_scale)]

df_plot <- as.data.frame(subset_tempo) %>% 
  group_by(grid_scale, temporal_scale) %>%
  #do(data_frame(component = LETTERS[1:3], value = runif(3))) %>% 
  mutate(total = sum(count)) %>%
  group_by(grid_scale, temporal_scale, total)


df_plot
#total_studies <- sum(df_plot$total)
#df_plot <- mutate(df_plot, percent = (total/total_studies)*100)

df_plot$grid_scale <- unclass(df_plot$grid_scale)
df_plot$temporal_scale <- unclass(df_plot$temporal_scale)

colset_bright2 <- c("#6a3d9a", "#375E97")

df.grobs <-  df_plot%>% 
  do(subplots = ggplot(., aes(1, count, fill = continent)) + 
       geom_col(position = "fill", alpha = 0.75, colour = "white") + 
       coord_polar(theta = "y") + 
       scale_fill_manual(labels = c("Africa", "Asia", "Europe", "North America",
       "South America", "Global"), values=mycol_continent6) + 
       theme_void()+ guides(fill = F)) %>% 
  mutate(subgrobs = list(annotation_custom(ggplotGrob(subplots),
                                           x = grid_scale-10/18, y = temporal_scale-10/18, 
                                           xmax = grid_scale+10/18, ymax = temporal_scale+10/18))) #size of the pie charts

df.grobs  <- subset(df.grobs , !is.na(temporal_scale))
df.grobs  <- subset(df.grobs, !is.na( grid_scale))
df.grobs  <- subset(df.grobs, !is.na(continent))




final_plot <- df.grobs %>%
  {ggplot(data = ., aes(factor(grid_scale), factor(temporal_scale))) + 
      #scale_x_continuous(expand=c(0,0),"Validation lenghth",breaks=c(1,2,3,4,5),
      # labels=c("0-12","13-24","25-36","37-48","49-60"), limits=c(0.5,6)) + 
      #scale_x_discrete("Continent", labels = c("1" = "Africa", "2" = "Asia", "3" = "Europe", 
      scale_x_discrete("spatial_scale", labels = c("1" = "0.1", "2" = "0.25", "3"  = "0.5", 
                                                   "4"= "1", "5" = "2", "6" = "2.5", "7" = "3")) + 
      scale_y_discrete("Temporal scale", labels = c("1" = "0.5h", "2" = "1h", "3" = "3h",
                                                    "4" = "6h", "5" = "12h", "6" = "daily",
                                                    "7" = "monthly", "8" = "seasonal",
                                                    "9" = "annual")) + 
      .$subgrobs + 
      geom_text(aes(label = round(total, 2)), size = 3) + 
      geom_col(data = df_plot,
               aes(0,0, fill = continent), 
               colour = "white") + 
    scale_fill_manual("Continent", labels = c("Africa", "Asia", "Europe", "North America",
                                                  "South America", "Global"), values=mycol_continent6)}

final_plot + theme_small + theme(axis.text.x = element_text(angle = 40, hjust = 0.8, vjust = 0.9))
