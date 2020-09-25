n2 <- 3                                               # Higher amount of hex colors
hex_codes2 <- hue_pal()(n2)                             # Identify hex codes
show_col(hex_codes2)  
hex_codes2


mycol_continent5 <- c( "#69bdd2", "#739F3D", "#1979a9", "#e07b39", 
                      "#80391e")  

mycol_continent6 <- c( "#69bdd2", "#739F3D", "#1979a9", "#edb879", "#e07b39", 
                      "#80391e")

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
  facet_grid(~continent, scales="free") + 
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

##https://www.r-graph-gallery.com/261-multiple-graphs-on-same-page.html


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

refr_type[, tempo_count := .N, by = temporal_scale]

refr_type[, reclength_count := .N, by = record_length]


refr_type <- refr_type[study_tempscale, on = 'id']

refr_type$temporal_scale <- factor(refr_type$temporal_scale, 
                                     levels = c("0.5h", "1h", "3h", "6h", "12h",
                                                "18h", 
                                                "daily",  "monthly",  "seasonal", "annual"))
refr_type$ref_type <- factor(refr_type$ref_type, 
                                   levels = c("g", "s", "r", "m"))

saveRDS(refr_type, file = './data/refr_record_length.Rds')
########################
refr_type <- readRDS('./data/refr_record_length.Rds')


ggplot(refr_type, aes(record_length, group = ref_type)) + 
  geom_bar()

ggplot(refr_type, aes(fill= ref_type, y=ref_count, x=record_length)) + 
  geom_bar(position="dodge", stat="identity")

ggplot(refr_type, aes(record_length, temporal_scale)) + 
  geom_jitter() + 
  theme_small

ggplot(refr_type, aes(ref_type, record_length)) + 
  geom_jitter() + 
  theme_small

ggplot(refr_type, aes(temporal_scale, record_length)) + 
  facet_grid(~ref_type, scales="free") +
  geom_bar(position="dodge", stat="identity")

ggplot(refr_type, aes(temporal_scale, tempo_count)) + 
  facet_wrap(~continent, scales="free") +
  geom_bar(position="dodge", stat="identity")

ggplot(refr_type, aes(temporal_scale, record_length)) + 
  facet_wrap(~continent) +
  geom_bar(position="dodge", stat="identity")

ggplot(refr_type, aes(temporal_scale, record_length, col = ref_type, shape = ref_type)) + 
  #facet_wrap(~continent) +
  geom_jitter(width = 0.3) + 
  theme_generic

ggplot(refr_type, aes(temporal_scale, ref_type, size = record_length, col = record_length)) + 
  #facet_wrap(~continent) +
  geom_jitter() + 
  theme_generic


ggplot(refr_type, aes(ref_type, record_length)) + 
  #facet_wrap(~continent) +
  geom_boxplot()



ggplot(refr_type, aes(continent, record_length)) + 
  facet_grid(~year) +
  geom_bar(position="dodge", stat="identity") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.9))

ggplot(refr_type, aes(factor(year), record_length)) + 
  facet_grid(~ref_type) +
  geom_boxplot()+ 
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.9))


ggplot(refr_type, aes(temporal_scale, ref_type)) + 
  facet_wrap(~continent) +
  geom_jitter(width = 0.2)
