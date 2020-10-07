n2 <- 4                                               # Higher amount of hex colors
hex_codes2 <- hue_pal()(n2)                             # Identify hex codes
show_col(hex_codes2)  
hex_codes2


mycol_continent5 <- c( "#69bdd2", "#739F3D", "#1979a9", "#e07b39", 
                      "#80391e")  

mycol_continent6 <- c( "#69bdd2", "#739F3D", "#1979a9", "#edb879", "#e07b39", 
                      "#80391e")
scale_color_manual(labels = c("Gauge", "satellite", "Radar", "Model"), values = c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF"))

Piecol <- c("#69bdd2", "#AF4425", "#662E1C", "#EBDCB2")


ggplot(study_plot) + 
  geom_bar(aes(x = factor(year), fill = continent)) + 
  scale_fill_manual(values = mycol_continent_glob) + 
  labs(x = "Year", y = "Number of papers") + 
  facet_grid(~continent, space = "free", scales = "free_y") + 
  theme_very_small + 
  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(angle = 40, hjust = 0.8, vjust = 0.9))



##################################

###box plot
ggplot(na.omit(imerg_combi), aes(x=imerg_vers, 
                                 y=record_length,
                                 fill= downscale)) +
  geom_boxplot(alpha=0.4) 

ggplot(na.omit(imerg_combi), aes(x=imerg_vers, 
                                 y=record_length,
                                 fill = imerg_type)) +
  geom_boxplot(alpha=0.4) + 
  theme_classic()


ggplot(na.omit(imerg_combi), aes(x=reorder(imerg_type, record_length), 
                                 y=record_length)) + 
  facet_wrap(~year) + 
  geom_boxplot(alpha=0.4) + 
  theme_classic()

ggplot(na.omit(imerg_combi), aes(x=reorder(imerg_type, record_length), 
                                 y=record_length, color = year)) + 
  geom_jitter() + 
  theme_classic()



ggplot(study_plot, aes(record_length, continent)) + 
  geom_boxplot()

ggplot(na.omit(imerg_combi), aes(record_length, continent)) + 
  geom_boxplot()



ggplot(na.omit(imerg_combi), aes(factor(temporal_scale), factor(grid_scale), color = temporal_scale)) + 
  geom_jitter()

ggplot(na.omit(imerg_combi), aes(temporal_scale)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=percent) +
  facet_wrap(~continent) + 
  labs(x = "Temporal scale", y = "Papers") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 0.9))
####################

###IMERG_Vers_ vs_year

imerg_vers_count <- imerg_combi[, .('vers_count' = .N),
                                by = .(imerg_vers, year)]

ggplot(imerg_vers_count, aes(year, vers_count, color = imerg_vers)) + 
  geom_line()

###Reference_type_vs_number

reftype_count <- studies[, .(id, 'count_pap' = .N),
                         by = ref_type]

ggplot(reftype_count, aes(x = reorder(ref_type, count_pap))) + 
  geom_bar() + 
  labs(x = "Reference type", y = "Number of papers") + 
  geom_text(aes(x = ref_type, 
                y = count_pap, label = count_pap, hjust = -0.2)) + 
  coord_flip() + 
  theme_generic

ggsave("results/plots/Ref_type_vs_papers.png", width = 7.2, 
       height = 5.3, units = "in", dpi = 600)


ref_imergcombi <- reftype_count[imerg_combi, on = 'id']

imerg_combi <- study_plot[alg_vers, on = 'id']
imerg_combi <- imerg_combi[run_type, on = 'id']

###spatial_vs_temporal_scales_scatter_plot and reference types
ggplot(ref_imergcombi, aes(grid_scale, temporal_scale, color = ref_type)) + 
  geom_jitter(width = 0.20, height = 0.5)+ 
  facet_wrap(~ref_type) + 
  labs(x = "Spatial scale", y = "Temporal scale") + 
  scale_fill_manual(values = c("#F0810F", "#739F3D", "#ACBD78")) + 
  #scale_color_manual(values = colset_bright)
  theme_small + 
  labs(col = "Reference_type")
#theme(legend.title = IMERG_TYPE)
#facet_grid(~, scales="free", space="free_x")

ggsave("results/plots/Temporal_vs_Spatial_Reference.png", width = 7.2, 
       height = 5.3, units = "in", dpi = 600)

###scatter plot

ggplot(na.omit(imerg_combi), aes(x=lon_mean, 
                                 y=lat_mean, 
                                 col = imerg_vers)) + 
  geom_point() + 
  theme_classic()

ggplot(na.omit(imerg_combi), aes(x=lon_mean, y=lat_mean,
                                 size = record_length,
                                 col = imerg_type)) + 
  geom_jitter() + 
  theme_classic()

ggplot(na.omit(imerg_combi), aes(x=lat_mean, y=lon_mean,
                                 #size = record_length,
                                 col = imerg_type)) + 
  geom_point() + 
  theme_classic()

ggplot(na.omit(imerg_combi), aes(imerg_vers, year)) + 
  geom_line()

ggplot(na.omit(imerg_combi), aes(comparison_method, downscale)) + 
  geom_jitter()

ggplot(na.omit(imerg_combi), aes(imerg_vers, imerg_type, color = imerg_type)) + 
  geom_jitter()

ggplot(na.omit(imerg_combi), aes(imerg_type, record_length)) + 
  geom_jitter()


###spatio_temporal_vs_papers_bar_plot----

g1 <- ggplot(imerg_combi, aes(grid_scale, fill = imerg_type)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), position=position_dodge()) + 
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(x = "Spatial scale", y = "studies") + 
  scale_fill_manual(values = c("#F8766D", "#00BA38", "#619CFF")) + 
  facet_grid(~continent) + 
  theme_small + 
  labs(fill = "IMERG_TYPE") + 
  theme(legend.position = "none") + 
  theme(axis.title.y = element_blank())

g2 <- ggplot(imerg_combi, aes(temporal_scale, fill = imerg_type)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), position=position_dodge()) + 
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(x = "Temporalal scale", y = "studies") + 
  scale_fill_manual(values = c("#F8766D", "#00BA38", "#619CFF")) + 
  facet_grid(~continent, scales="free") + 
  theme_small + 
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.9)) + 
  labs(fill = "IMERG_TYPE") + 
  theme(legend.position = "none") + 
  theme(axis.title.y = element_blank())

g3 <- ggplot(imerg_combi, aes(grid_scale, temporal_scale, color = imerg_type)) + 
  geom_jitter(width = 0.20, height = 0.2)+ 
  labs(x = "Spatial scale", y = "Temporal scale") + 
  scale_fill_manual(values = palettes_bright$colset_cheer_brights) + 
  facet_grid(~continent, scales = "free") + 
  theme_small + 
  labs(col = "IMERG_TYPE") + 
  theme(legend.position = "bottom")


g4 <- grid.arrange(arrangeGrob(g1, g2, nrow=2), g3, nrow = 1)

ggsave("results/plots/Rajani.png", g4, width = 8.7, 
       height = 5.3, units = "in", dpi = 600)

#https://www.r-graph-gallery.com/261-multiple-graphs-on-same-page.html####


###Validation_length----
ggplot(refr_type, aes(factor(year), record_length)) + 
  geom_boxplot()

ggplot(study_plot, aes(factor(year), record_length, color = continent)) + 
  geom_jitter(width = 0.2) + 
  theme_small

ggplot(study_plot, aes(x = record_length, y = temporal_scale)) + 
  geom_point() + 
  theme_small
  
ggplot(study_plot, aes(factor(record_length))) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), position=position_dodge()) + 
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(x = "Spatial scale", y = "studies") + 
  #scale_fill_manual(values = c("#F8766D", "#00BA38", "#619CFF")) + 
  facet_grid(~continent, scales="free") + 
  theme_small + 
  labs(fill = "IMERG_TYPE") + 
  theme(legend.position = "none") + 
  theme(axis.title.y = element_blank())

ggplot(refr_type, aes(ref_type, record_length)) + 
  geom_jitter(width = 0.2) + 
  theme_small
####################################
refr_type[, ref_count := .N, by = ref_type]

refr_type <- refr_type[study_tempscale, on = 'id']

refr_type[, tempo_count := .N, by = temporal_scale]

refr_type[, reclength_count := .N, by = record_length]


refr_type$temporal_scale <- factor(refr_type$temporal_scale, 
                                     levels = c("0.5h", "1h", "3h", "6h", "12h", 
                                                "daily",  "monthly",  "seasonal", "annual"))
refr_type$ref_type <- factor(refr_type$ref_type, 
                                   levels = c("g", "s", "r", "m"))

saveRDS(refr_type, file = './data/refr_record_length.Rds')
#########################import data----
refr_type <- readRDS('./data/refr_record_length.Rds')


refr_type$group = cut(refr_type$record_length,c(0,12,24,36,48,60))
levels(refr_type$group) = c("0-12","13-24","25-36","37-48", "49-60")

refr_type <- subset(refr_type, !is.na(group))
refr_type <- subset(refr_type, !is.na(temporal_scale))


ggplot(refr_type, aes(factor(group))) +
  geom_bar() + 
  facet_grid(~ref_type) + 
  labs(x = "record_length") + 
  theme_generic + 
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.9))
ggsave("results/1.png",
       width = 7.2, height = 5.3, units = "in", dpi = 600)

ggplot(refr_type, aes(group, temporal_scale)) + 
  geom_jitter(width = 0.2, height = 0.2) + 
  labs(x = "record_length") + 
  theme_small

ggsave("results/2.png",
       width = 7.2, height = 5.3, units = "in", dpi = 600)

ggplot(refr_type, aes(ref_type, group, col = continent)) + 
  geom_jitter(height = 0.2) + 
  labs(y = "record_length") + 
  theme_generic
ggsave("results/3.png",
       width = 7.2, height = 5.3, units = "in", dpi = 600)


ggplot(refr_type, aes(temporal_scale, record_length)) + 
  #facet_grid(~ref_type, scales="free") +
  geom_bar(position="dodge", stat="identity") + 
  theme_generic + 
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.9))

ggsave("results/4.png",
       width = 7.2, height = 5.3, units = "in", dpi = 600)


ggplot(refr_type, aes(temporal_scale, record_length)) + 
  facet_grid(~ref_type, scales="free") +
  geom_bar(position="dodge", stat="identity") + 
  theme_small + 
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.9))

ggsave("results/5.png",
       width = 7.2, height = 5.3, units = "in", dpi = 600)

ggplot(refr_type, aes(country, ref_count)) + 
  #facet_grid(~continent, scales="free") +
  geom_bar(position="dodge", stat="identity") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.9))


#Relative_frequencies_Reference types####

ggplot(refr_type, aes(ref_type, group = continent)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  #geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales::percent) + 
  facet_grid(~continent) + 
  ylab("Relative frequencies") + 
  xlab("Reference types") + 
  scale_x_discrete(labels = c("Gauge", "satellite", "Radar", "Model")) + 
  theme_very_small + 
  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.9)) 
  
ggsave("results/plots/Relative_freq_Reftypes.png",
       width = 7.2, height = 5.3, units = "in", dpi = 600)

ggplot(refr_type, aes(ref_type, group = continent)) + 
  #geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales::percent) + 
  facet_grid(~year) + 
  ylab("Relative frequencies") + 
  xlab("Reference types") + 
  scale_x_discrete(labels = c("Gauge", "satellite", "Radar", "Model")) + 
  theme_very_small + 
  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.9))

ggsave("results/6.png",
       width = 7.2, height = 5.3, units = "in", dpi = 600)


ggplot(refr_type, aes(temporal_scale, record_length)) + 
  facet_wrap(~continent) +
  geom_bar(position="dodge", stat="identity") + 
  theme_small + 
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.9))

ggsave("results/7.png",
       width = 7.2, height = 5.3, units = "in", dpi = 600)
  


ggplot(refr_type, aes(temporal_scale, record_length, col = ref_type, shape = ref_type)) + 
  #facet_wrap(~continent) +
  geom_jitter(width = 0.3) + 
  theme_generic

ggsave("results/8.png",
       width = 7.2, height = 5.3, units = "in", dpi = 600)

ggplot(refr_type, aes(temporal_scale, ref_type, size = record_length, col = record_length)) + 
  #facet_wrap(~continent) +
  geom_jitter(height = 0.3, width = 0.3) + 
  theme_generic

ggsave("results/9.png",
       width = 7.2, height = 5.3, units = "in", dpi = 600)

ggplot(refr_type, aes(ref_type, record_length)) + 
  facet_grid(~year) +
  geom_boxplot() + 
  theme_small

ggsave("results/10.png",
       width = 7.2, height = 5.3, units = "in", dpi = 600)

ggplot(refr_type, aes(continent, record_length)) + 
  facet_grid(~year) +
  geom_bar(position="dodge", stat="identity") + 
  #geom_boxplot() + 
  theme_small + 
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.9))

ggsave("results/plots/record_length_vs_year.png",
       width = 7.2, height = 5.3, units = "in", dpi = 600)

ggplot(refr_type, aes(factor(year), record_length)) + 
  facet_grid(~continent) +
  geom_bar(position="dodge", stat="identity") + 
  #geom_boxplot() + 
  theme_small + 
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.9))

ggsave("results/11.png",
       width = 7.2, height = 5.3, units = "in", dpi = 600)


ggplot(refr_type, aes(factor(year), record_length)) + 
  #_grid(~ref_type) +
  geom_boxplot()+ 
  theme_generic

ggsave("results/12.png",
       width = 7.2, height = 5.3, units = "in", dpi = 600)

ggplot(refr_type, aes(factor(year), record_length)) + 
  facet_grid(~ref_type) +
  geom_boxplot()+ 
  theme_generic + 
  theme(axis.text.x = element_text(angle = 30, hjust = 0.8, vjust = 0.9))

ggsave("results/13.png",
       width = 7.2, height = 5.3, units = "in", dpi = 600)

ggplot(refr_type, aes(temporal_scale, ref_type)) + 
  facet_wrap(~continent) +
  geom_jitter(width = 0.2, height = 0.2) + 
  theme_generic + 
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.9))

ggsave("results/14.png",
       width = 7.2, height = 5.3, units = "in", dpi = 600)

#With Countries####
ggplot(refr_type, aes(reorder(country, record_length), record_length)) + 
  #facet_wrap(~continent, scales = 'free') +
  geom_bar(stat="identity") + 
  coord_flip() + 
  theme(axis.text.y = element_text(angle = 20, hjust = 0.8, vjust = 0.9))
  
ggplot(refr_type, aes(country, reclength_count)) + 
    facet_wrap(~group, scales = 'free') +
    geom_bar(stat="identity") + 
  theme_small + 
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.9))
  

ggplot(refr_type, aes(country, group)) + 
  #facet_wrap(~continent, scales = 'free') +
  geom_jitter(height = 0.2, width = 0.2) + 
  labs(y = "record_length") + 
  theme_small + 
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.9))

ggsave("results/15.png",
       width = 7.2, height = 5.3, units = "in", dpi = 600)


ggplot(refr_type, aes(country, temporal_scale)) + 
  geom_jitter(height = 0.2) + 
  theme_generic + 
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.9)) 

ggsave("results/16.png",
       width = 7.2, height = 5.3, units = "in", dpi = 600)

ggplot(refr_type, aes(group, temporal_scale, col = ref_type)) + 
  geom_jitter(height = 0.2, width = 0.2) + 
  labs(x = "Validation length", y = "Temporal scale", col = "Reference type") + 
  theme_small + 
  scale_color_manual(labels = c("Gauge", "satellite", "Radar", "Model"), values = c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF")) + 
  theme(axis.text.y = element_text(angle = 30, hjust = 0.8, vjust = 0.9))

ggsave("results/17.png",
       width = 7.2, height = 5.3, units = "in", dpi = 600)






#sample example----
ggplot(refr_type, aes(temporal_scale, group, col = ref_type)) + 
  geom_jitter(height = 0.2, width = 0.2) + 
  labs(x = "Validation length", y = "Temporal scale", col = "Reference type") + 
  theme_small + 
  scale_color_manual(labels = c("Gauge", "satellite", "Radar", "Model"), values = c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF")) + 
  theme(axis.text.y = element_text(angle = 30, hjust = 0.8, vjust = 0.9))

ggsave("results/17.png",
       width = 7.2, height = 5.3, units = "in", dpi = 600)

df <- data_frame(x1 = c(5, 7, 9, 11), y1 = c(10, 14, 18, 22)) %>% 
  group_by(x1, y1) %>%
  do(data_frame(component = LETTERS[1:3], value = runif(3))) %>% 
  mutate(total = sum(value)) %>% 
  group_by(x1, y1, total) 


######################################Pie_chart----
subset_df4 <- refr_type[, .(group, temporal_scale, ref_type)]

subset_df5 <- subset_df4[, .(count = .N), by = .(group, temporal_scale, ref_type)]


subset_df6 <- subset_df5[, x2 := unclass(group)]
subset_df7 <- subset_df6[, y2 := unclass(temporal_scale)]

df8 <- as.data.frame(subset_df7) %>% 
  group_by(group, temporal_scale) %>%
  #do(data_frame(component = LETTERS[1:3], value = runif(3))) %>% 
  mutate(total = sum(count)) %>% 
  group_by(group, temporal_scale, total)

df8

df8$group <- unclass(df8$group)
df8$temporal_scale <- unclass(df8$temporal_scale)


df.grobs8 <-  df8%>% 
  do(subplots = ggplot(., aes(1, count, fill = ref_type)) + 
       geom_col(position = "fill", alpha = 0.75, colour = "white") + 
       coord_polar(theta = "y") + 
       scale_fill_manual(labels = c("Gauge", "Satellite", "Radar", "Model"), values=palettes_bright$colset_cheer_brights) + 
       theme_void()+ guides(fill = F)) %>% 
  mutate(subgrobs = list(annotation_custom(ggplotGrob(subplots),
                                           x = group-10/18, y = temporal_scale-10/18, 
                                           xmax = group+10/18, ymax = temporal_scale+10/18))) #size of the pie charts


final_plot <- df.grobs8 %>%
  {ggplot(data = ., aes(factor(group), factor(temporal_scale))) + 
      #scale_x_continuous(expand=c(0,0),"Validation lenghth",breaks=c(1,2,3,4,5),
                        # labels=c("0-12","13-24","25-36","37-48","49-60"), limits=c(0.5,6)) + 
      scale_x_discrete("Validation lenghth", labels = c("1" = "0-12", "2" = "13-24" ,
                                                        "3" = "25-36",  "4" = "37-48",
                                                        "5" = "49-60")) +
      scale_y_discrete("Temporal scale", labels = c("1" = "0.5h",  "2" = "1h", "3" = "3h",
                                                    "4" = "6h", "5" = "12h", "6" = "daily",
                                                    "7" = "monthly", "8" = "seasonal",
                                                    "9" = "annual")) +
      .$subgrobs + 
      geom_text(aes(label = round(total, 2)), size = 3) + 
      geom_col(data = df8,
               aes(0,0, fill = ref_type), 
               colour = "white") + scale_fill_manual("Reference type", labels = c("Gauge", "satellite", "Radar", "Model"), values=palettes_bright$colset_cheer_brights)}

final_plot + theme_generic



ggsave("results/19.png",
       width = 7.2, height = 5.3, units = "in", dpi = 600)
