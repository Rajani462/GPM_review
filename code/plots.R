source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')
##############################
#data preparation for plots----

study_plot <- studies[, .(id, study_area, study_area_type,
                          country, continent, 
                          lat_mean, lon_mean, area,
                          variable, surface, 
                          downscale, record_length,
                          year, journal)]


imerg_combi <- study_plot[alg_vers, on = 'id']
imerg_combi <- imerg_combi[run_type, on = 'id']
imerg_combi <- imerg_combi[study_gridscale, on = 'id']
imerg_combi <- imerg_combi[study_tempscale, on = 'id']
imerg_combi <- imerg_combi[study_compmthod, on  = 'id']
imerg_combi <- imerg_combi[study_compscale, on  = 'id']
imerg_combi <- imerg_combi[ref_type, on  = 'id']


study_plot$continent <- factor(study_plot$continent, 
                                levels = c("Africa", "Asia", "Europe", "North America",
                                           "South America", "Global"))
imerg_combi$continent <- factor(imerg_combi$continent, 
                                levels = c("Africa", "Asia", "Europe", "North America",
                                           "South America", "Global"))

#write.xlsx(imerg_combi, 'imerg_combi2.xlsx')

#trial2 <- study_plot[, .(alg_vers, run_type, study_gridscale, study_tempscale, 
                         #study_compmthod, study_compscale, ref_type), on = 'id']

#write.xlsx(trial2, 'trial2.xlsx')
###before plotting remove NA's from imerg_combi

trial3 <- subset(imerg_combi, !is.na .(imerg_type, imerg_vers))

imerg_combi <- subset(imerg_combi, !is.na(imerg_type))
imerg_combi <- subset(imerg_combi, !is.na(imerg_vers))
imerg_combi <- subset(imerg_combi, !is.na(continent))
imerg_combi <- subset(imerg_combi, !is.na(comparison_scale))
imerg_combi <- subset(imerg_combi, !is.na(downscale))

#reorder the levels of temporal_scale of imerg_combi
imerg_combi$temporal_scale <- factor(imerg_combi$temporal_scale, 
                                     levels = c("0.5h", "1h", "3h", "6h", "12h",
                                                "18h", "1d to 60d", 
                                                "daily",  "monthly",  "seasonal", "annual"))

imerg_combi$imerg_type <- factor(imerg_combi$imerg_type, 
                                     levels = c("IMERG_E", "IMERG_L", "IMERG_F"))
#reorder the levels of continents



###########Spatial distribution of studies----

study_plot2 <- subset(study_plot,continent!="Global")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

global_dist <- world_map <- ggplot(data = world) + 
  geom_sf(fill = "white") + 
  coord_sf(xlim = c(-170, 170), ylim = c(-58, 90)) + 
  geom_point(data = study_plot2, aes(lon_mean, lat_mean, 
                                     color = continent)) + 
  scale_color_manual(values = mycol_continent5) + 
  theme_generic + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        legend.position = "none")

######################################

#prepare data for continent and country wise bar plot----


continents <- study_plot[, .(id, continent)]
country_continents <- study_country[continents, on = 'id']
country_continents <- subset(country_continents, !is.na(country))

plot_country <- country_continents[, .('paper_count' = .N),
                         by = .(country, continent)][, prop := round(paper_count / sum(paper_count) * 100, 2)]


imerg_verscount <- alg_vers[, .('vers_count' = .N),
                               by = imerg_vers]

###country wise papers reordered

country_plot <- ggplot(plot_country) + 
  geom_bar(aes(x = reorder(country, -prop),
               y = prop,
               #color = continent, 
               fill = continent),
           stat = "identity") + 
  labs(x = "Country", y = "Studies (%)") + 
  scale_fill_manual(values = mycol_continent5) + 
  
  theme_small + 
  theme(legend.position = "bottom") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.9)) + 
  labs(fill = "Continents") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


global_dist <- ggplotGrob(global_dist)

Country_barplot + annotation_custom(grob = global_dist, xmin = 3, xmax = Inf, 
                                    ymin = 5, ymax = Inf)

ggsave("results/plots/Global_dist.png",
       width = 7.2, height = 5.3, units = "in", dpi = 600)


#continent wise papers reordered

plot_continents <- study_plot[, .('paper_count' = .N),
                              by = continent][, prop := round(paper_count / sum(paper_count), 2)]

###Line_plot
plot_continents2 <- study_plot[, .('paper_count' = .N),
                              by = .(continent, year)]

ggplot(plot_continents2, aes(x = year, y = paper_count, group = continent)) + 
  geom_line(aes(color = continent))
################  

ggplot(plot_continents, aes(x = reorder(continent, prop),
                       y = prop, fill = continent, label = scales::percent(prop))) + 
  geom_bar(stat = "identity") + 
  labs(x = "Continent", y = "Number of stuides") + 
  geom_text(aes(label = scales::percent(prop), hjust = 0.5), size = 3.2) + 
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#F0810F", "#337BAE",
                               "#97B8C2",  "#739F3D",
                               "#ACBD78",  
                               "#F4CC70", "#EBB582")) + 
  coord_flip() + 
  theme_generic + 
  theme(legend.position = "none") + 
  theme(axis.title.y = element_blank())

ggsave("results/plots/paperfraction_per_continent.png", width = 7.2,
       height = 5.3, units = "in", dpi = 600)

###continent wise papers per year----
ggplot(study_plot) + 
  geom_bar(aes(x = factor(year), fill = continent)) + 
  scale_fill_manual(values = mycol_continent6) + 
  labs(x = "Year", y = "Number of studies") + 
  facet_grid(~continent, space = "free", scales = "free_y") + 
  theme_very_small + 
  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(angle = 40, hjust = 0.8, vjust = 0.9))

ggsave("results/plots/papers_per_year_continents.png", width = 7.2,
       height = 4.3, units = "in", dpi = 600)


ggplot(study_plot) + 
  geom_bar(aes(x = factor(year), fill = continent)) + 
  scale_fill_manual(values = mycol_continent6) + 
  labs(x = "Year", y = "Number of studies") + 
  #facet_grid(~continent, space = "free", scales = "free_x") + 
  theme_generic + 
  theme(legend.position = "right") + 
  theme(legend.direction = "vertical") + 
  theme(legend.title = element_blank()) + 
  theme(axis.text.x = element_text(angle = 40, hjust = 0.8, vjust = 0.9))

ggsave("results/plots/papers_per_year_continents2.png", width = 7.2,
       height = 5.3, units = "in", dpi = 600)

#line plot
Papers_year <- study_plot[, .('paper_count' = .N),
                                by = .(continent, year)]

ggplot(Papers_year, aes(x = year, y = paper_count, group = continent)) + 
  geom_line(aes(col = continent)) + 
  labs(x = "Year", y = "Number of studies") + 
  theme_very_small
  
#data_continent wise
continent_period <- study_plot[, .(id, continent, record_length, year)]

count_pap <- study_plot[, .(id, continent, record_length)]

asia <- study_plot[continent == "Asia"]
africa <- study_plot[continent == "Africa"]
europe <- study_plot[continent == "Europe"]
south_america <- study_plot[continent == "South America"]
north_america <- study_plot[continent == "North America"]
global <- study_plot[continent == "Global"]

#data for histogram or barplot
plot_asia <- asia[, .('count' = .N),
                   by = .(record_length, continent)][, percent := round(count / sum(count) * 100, 2)]

#bar plots continent wise
trial <- study_plot[, .( 'count' = .N),
              by = .(record_length, continent)]

ggplot(trial, aes(record_length, count)) + 
  geom_point()

#divide the record_lengths into ranges (10 months each)
record_length_asia <-as.data.table(table(cut(asia$record_length, breaks = seq(0, 60, by = 12))))
record_length_asia[, continent_name := factor('Asia')]

record_length_africa <-as.data.table(table(cut(africa$record_length, breaks = seq(0, 60, by = 12))))
record_length_africa[, continent_name := factor('Africa')]

record_length_europe <-as.data.table(table(cut(europe$record_length, breaks = seq(0, 60, by = 12))))
record_length_europe[, continent_name := factor('Europe')]

record_length_s_america <-as.data.table(table(cut(south_america$record_length, breaks = seq(0, 60, by = 12))))
record_length_s_america[, continent_name := factor('South_America')]

record_length_n_america <-as.data.table(table(cut(north_america$record_length, breaks = seq(0, 60, by = 12))))
record_length_n_america[, continent_name := factor('North_America')]

record_length_global <-as.data.table(table(cut(global$record_length, breaks = seq(0, 60, by = 12))))
record_length_global[, continent_name := factor('Global')]

#merge all the continents to a single data table
merge_continents <- merge(record_length_asia, record_length_africa, all = TRUE)
AFE <- merge(merge_continents, record_length_europe, all = TRUE)
AFES <- merge(AFE, record_length_s_america, all = TRUE)
AFESN <- merge(AFES, record_length_n_america, all = TRUE)
AFESNG <- merge(AFESN, record_length_global, all = TRUE)


####Validation_length plots----
ggplot(AFESNG) + 
  geom_bar(aes(x= factor(V1),
      y = N,
      group = continent_name,
      fill = continent_name,
      color = continent_name),
      stat = "identity",
      position = position_dodge()) + 
  labs(x = "Validation length (months)", y = "studies") + 
  scale_fill_manual(values = mycol_continent6) + 
  scale_x_discrete(labels = c("0-12", "13-24", "25-36", "37-48", "48-60")) + 
  theme_generic + 
  theme(legend.title = element_blank())

ggsave("results/plots/validation_lengths_continent.png", width = 7.2,
       height = 5.3, units = "in", dpi = 600)
  
##########################################
######Temporal_scale_vs_papers_bar_plot----

ggplot(imerg_combi, aes(temporal_scale, fill = imerg_type)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), position=position_dodge()) + 
  scale_y_continuous(labels=percent) + 
  #facet_wrap(~imerg_type) + 
  labs(x = "Temporalal scale", y = "studies") + 
  #scale_fill_manual(values = palettes_bright$colset_cheer_brights) + 
  scale_fill_manual(values = c("#F8766D", "#00BA38", "#619CFF")) + 
  facet_grid(~continent, scales="free", space="free_x") + 
  theme_generic + 
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.9)) + 
  labs(fill = "IMERG_TYPE")

ggsave("results/plots/Temporal_scale_vs_papers.png", width = 9.5,
       height = 5.3, units = "in", dpi = 600)

###temporal_scale_vs_IMERG_version

ggplot(imerg_combi, aes(temporal_scale)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=percent) + 
  facet_wrap(~imerg_vers) + 
  labs(x = "Temporalal scale", y = "studies") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.9)) + 
  theme_generic

ggsave("results/plots/Temp_scale_IMERG_vers.png", p6_1)

###Temporal_scale_vs_IMERG_type

ggplot(imerg_combi, aes(temporal_scale)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=percent) + 
  facet_wrap(~imerg_type) + 
  labs(x = "Temporalal scale", y = "studies") + 
  theme_small + 
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.9))

ggsave("results/plots/Temp_scale_IMERG_type.png", p6_2)

###IMERG_type vs IMERG_version----

ggplot(imerg_combi, aes(imerg_type, imerg_vers, color = imerg_type)) + 
  geom_jitter()+ 
  facet_wrap(~continent) + 
  labs(x = "IMERG RUN", y = "IMERG version") + 
  scale_fill_manual(values = c("#F0810F", "#739F3D", "#ACBD78")) + 
  #scale_color_manual(values = colset_bright)
  theme_small + 
  #labs(col = "IMERG_TYPE")
  theme(legend.position = "none")
#facet_grid(~, scales="free", space="free_x")

  
ggsave("results/plots/IMERG_TYPE_vs_VERSION.png", width = 7.2, 
       height = 5.3, units = "in", dpi = 600)


### Temporal_scale_vs_Year----

ggplot(imerg_combi, aes(temporal_scale)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=percent) + 
  facet_wrap(~year) + 
  labs(x = "Temporalal scale", y = "studies") + 
  theme_small + 
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.9)) + 
 

ggsave("results/plots/Temp_scale_Year.png", width = 7.2, 
       height = 5.3, units = "in", dpi = 600)

###spatial_scale_vs_papers_bar_plot----

ggplot(imerg_combi, aes(grid_scale, fill = imerg_type)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), position=position_dodge()) + 
  scale_y_continuous(labels=percent) + 
  labs(x = "Spatial scale", y = "studies") + 
  scale_fill_manual(values = c("#F8766D", "#00BA38", "#619CFF")) + 
  facet_grid(~continent, scales="free", space="free_x") + 
  #facet_wrap(~continent) + 
  #theme(axis.text.x = element_text(angle = 50, hjust = 1, vjust = 0.9))
  theme_generic + 
  labs(fill = "IMERG_TYPE")

ggsave("results/plots/Spatial_scale_vs_papers.png", width = 9.5, 
       height = 5.3, units = "in", dpi = 600)

###spatial_vs_temporal_scales_scatter_plot----
ggplot(imerg_combi, aes(grid_scale, temporal_scale, color = imerg_type)) + 
  geom_jitter(width = 0.20, height = 0.2)+ 
  #facet_wrap(~continent) + 
  labs(x = "Spatial scale", y = "Temporal scale") + 
  #scale_fill_manual(values = c("#F0810F", "#739F3D", "#ACBD78")) + 
  scale_fill_manual(values = palettes_bright$colset_cheer_brights) + 
  facet_grid(~continent, scales = "free") + 
  #scale_color_manual(values = colset_bright)
  theme_generic + 
  labs(col = "IMERG_TYPE")
  #theme(legend.title = IMERG_TYPE)
  #facet_grid(~, scales="free", space="free_x")

ggsave("results/plots/Temporal_vs_Spatial_scales.png", width = 7.2, 
       height = 5.3, units = "in", dpi = 600)

spatio_tempo <- imerg_combi[, .('count' = .N),
                               by = .(temporal_scale, grid_scale)]


ggplot(spatio_tempo, aes(grid_scale, temporal_scale, color = count)) + 
  geom_point(aes(size = count)) + 
  theme_generic




ggsave("results/plots/Temporal_vs_Spatial_scales.png", width = 7.2, 
       height = 5.3, units = "in", dpi = 600)


###comparison_method----
ggplot(imerg_combi, aes(ref_type, comparison_method)) + 
  geom_jitter()


  labs(x = "Spatial scale", y = "Temporal scale") + 
  scale_fill_manual(values = c("#F0810F", "#739F3D", "#ACBD78")) + 
  #scale_color_manual(values = colset_bright)


ggplot(imerg_combi, aes(comparison_method)) + 
  geom_bar()+ 
  facet_wrap(~year) + 
  labs(x = "comparison_method") + 
  scale_fill_manual(values = c("#4D648D", "#337BAE",
                               "#97B8C2",  "#739F3D",
                               "#ACBD78",  
                               "#F4CC70", "#EBB582")) + 
  theme_small + 
  facet_grid(~continent, scales="free", space="free_x") + 
  theme(axis.text.x = element_text(angle = 50, hjust = 1, vjust = 0.9))


ggsave("results/plots/Temporal_vs_Spatial_scales.png", )

###comparison_scale----
ggplot(imerg_combi, aes(continent)) + 
  geom_bar()+ 
  facet_wrap(~year) + 
  labs(x = "comparison_method") + 
  scale_fill_manual(values = c("#4D648D", "#337BAE",
                               "#97B8C2",  "#739F3D",
                               "#ACBD78",  
                               "#F4CC70", "#EBB582")) + 
  facet_grid(~comparison_scale, scales="free", space="free_x") + 
  theme(axis.text.x = element_text(angle = 50, hjust = 1, vjust = 0.9))

ggsave("results/plots/Temporal_vs_Spatial_scales.png", )

###downscaled----
ggplot(imerg_combi, aes(continent)) + 
  geom_bar()+ 
  facet_wrap(~year) + 
  labs(x = "comparison_method") + 
  scale_fill_manual(values = c("#4D648D", "#337BAE",
                               "#97B8C2",  "#739F3D",
                               "#ACBD78",  
                               "#F4CC70", "#EBB582")) + 
  facet_grid(~downscale, scales="free", space="free_x") + 
  theme(axis.text.x = element_text(angle = 50, hjust = 1, vjust = 0.9))

ggsave("results/plots/Temporal_vs_Spatial_scales.png", )


############################################

ggplot(imerg_combi, aes(grid_scale)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  labs(x = "Spatial scale", y = "studies") + 
  scale_fill_manual(values = c("#4D648D", "#337BAE",
                               "#97B8C2",  "#739F3D",
                               "#ACBD78",  
                               "#F4CC70", "#EBB582")) + 
  theme_small + 
  facet_wrap(~comparison_method) + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))
