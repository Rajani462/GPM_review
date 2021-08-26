source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')
##################################
refr_type <- readRDS('./data/refr_type.rds')
study_tempscale <- readRDS('./data/study_tempscale.rds')
recordlength <- readRDS('./data/recordlength.rds')

refr_type <- refr_type[study_tempscale, on = 'id']

refr_type <- refr_type[recordlength, on = 'id']


saveRDS(refr_type, file = './data/refr_record_length.Rds')
#########################import data----
refr_type <- readRDS('./data/refr_record_length.Rds')


refr_type$group = cut(refr_type$record_length,c(0,12,24,36,48,60))
levels(refr_type$group) = c("0-12","13-24","25-36","37-48", "49-60")


refr_type$temporal_scale <- factor(refr_type$temporal_scale, 
                                   levels = c("0.5h", "1h", "3h", "6h", "12h", 
                                              "daily",  "monthly",  "seasonal", "annual"))
refr_type$ref_type <- factor(refr_type$ref_type, 
                             levels = c("g", "s", "r", "m"))


refr_type <- subset(refr_type, !is.na(group))
refr_type <- subset(refr_type, !is.na(temporal_scale))
refr_type <- subset(refr_type, !is.na(ref_type))



subset_df4 <- refr_type[, .(group, temporal_scale, ref_type)]

subset_df5 <- subset_df4[, .(count = .N), by = .(group, temporal_scale, ref_type)]


subset_df6 <- subset_df5[, x2 := unclass(group)]
subset_df7 <- subset_df6[, y2 := unclass(temporal_scale)]

df8 <- as.data.frame(subset_df7) %>% 
  group_by(group, temporal_scale) %>%
  #do(data_frame(component = LETTERS[1:3], value = runif(3))) %>% 
  #mutate(total = (sum(count)/362)*100) %>% 
  mutate(total = sum(count)) %>% 
  group_by(group, temporal_scale, total)

df8

total_count <- sum(df8$count)


df8$group <- unclass(df8$group)
df8$temporal_scale <- unclass(df8$temporal_scale)

refr_type$ref_type <- factor(refr_type$ref_type, 
                             levels = c("g", "s", "r", "m"), ordered = TRUE)

df.grobs8 <-  df8%>% 
  do(subplots = ggplot(., aes(x = "", y = count, fill = ref_type)) + 
       geom_col(position = "fill", alpha = 0.75, colour = "white") + 
       coord_polar(theta = "y") + 
       scale_fill_manual(labels = c("Gauge", "Satellite", "Radar", "Model"), 
                         values= setNames(palettes_bright$colset_cheer_brights, c("g", "s", "r", "m"))) + 
       theme_void()+ guides(fill = F)) %>% 
  mutate(subgrobs = list(annotation_custom(ggplotGrob(subplots),
                                           x = group-12/20, y = temporal_scale-12/20, 
                                           xmax = group+12/20, ymax = temporal_scale+12/20))) #size of the pie charts


final_plot <- df.grobs8 %>%
  {ggplot(data = ., aes(factor(group), factor(temporal_scale))) + 
      #scale_x_continuous(expand=c(0,0),"Validation lenghth",breaks=c(1,2,3,4,5),
      # labels=c("0-12","13-24","25-36","37-48","49-60"), limits=c(0.5,6)) + 
      scale_x_discrete("Validation length (months)", labels = c("1" = "0 - 12", "2" = "13 - 24" ,
                                                       "3" = "25 - 36",  "4" = "37 - 48",
                                                       "5" = "49 - 60")) +
      scale_y_discrete("Temporal scale", labels = c("1" = "0.5h",  "2" = "1h", "3" = "3h",
                                                    "4" = "6h", "5" = "12h", "6" = "daily",
                                                    "7" = "monthly", "8" = "seasonal",
                                                    "9" = "annual")) +
      .$subgrobs + 
      geom_text(aes(label = round(total, 2)), size = 3) + 
      geom_col(data = df8,
               aes(0,0, fill = ref_type), 
               colour = "white") + scale_fill_manual("Reference type", labels = c("Gauge", "Satellite", "Radar", "Model"), values=palettes_bright$colset_cheer_brights)}

final_plot + theme_small



ggsave("results/plots_paper/Validation_length_Piechart_rev1.png",
       width = 7.2, height = 5.3, units = "in", dpi = 600)
