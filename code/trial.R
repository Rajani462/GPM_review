n2 <- 5                                               # Higher amount of hex colors
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