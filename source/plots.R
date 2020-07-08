source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')
###############################
#data preparation for plots

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

###########Spatial distribution of publication
library("ggmap")
library("choroplethrMaps")
data(country.map, package = "choroplethrMaps")
head(unique(country.map$region), 12)





library("rworldmap")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

global_dist <- ggplot(data = world) + 
  geom_sf() + 
  coord_sf(xlim = c(160, -160), ylim = c(80, -80)) + 
  geom_point(data = study_plot, aes(lon_mean, lat_mean, 
                                    color = continent)) + 
  theme(axis.title.x = element_text(vjust = -3), 
  axis.title.y = element_text(vjust = 3)) + # move away for axis
  #labs(x = "Longitude", y = "Latitude")
  xlab(label = "Longitude") +
  ylab(label = "Latitude")

global_dist + theme_bw()

######################################

#prepare data for continent and country wise bar plot


plot_continents <- study_plot[, .('paper_count' = .N),
                         by = continent][, prop := round(paper_count / sum(paper_count), 2)]


plot_country <- study_plot[, .('paper_count' = .N),
                         by = .(country, continent)][, prop := round(paper_count / sum(paper_count), 2)]


imerg_verscount <- alg_vers[, .('vers_count' = .N),
                               by = imerg_vers]

#continent wise papers reordered
p1 <- ggplot(plot_continents, aes(x = reorder(continent, prop),
                       y = prop, fill = continent)) + 
  geom_bar(stat = "identity") + 
  labs(x = "Continent", y = "Papers fraction") + 
  scale_fill_manual(values = c("#4D648D", "#337BAE",
                               "#97B8C2",  "#739F3D",
                               "#ACBD78",  
                               "#F4CC70", "#EBB582")) + 
  coord_flip() + 
  theme_bw()
ggsave("results/plots/paperfraction_per_continent.png", p1,
       dpi = 300, width = 170, height = 100, units = "mm")

#continent wise papers per year
p2 <- ggplot(study_plot) + 
  geom_bar(aes(x = factor(year), fill = continent)) + 
  scale_fill_manual(values = c("#4D648D", "#337BAE",
                               "#97B8C2",  "#739F3D",
                               "#ACBD78",  
                               "#F4CC70", "#EBB582")) + 
  labs(x = "Year", y = "Number of papers") +
  theme_bw()
ggsave("results/plots/papers_per_year_continents.png", p2,
       dpi = 300, width = 170, height = 100, units = "mm")

#country wise papers reoodered
p3 <- ggplot(plot_country, aes(x = reorder(country, prop),
                       y = prop)) +
  geom_bar(stat = "identity") + 
  labs(x = "Country", y = "Papers fraction") + 
  coord_flip() + 
  theme_bw()
ggsave("results/plots/paperfraction_per_country.png", p3,
       dpi = 300, width = 150, height = 120, units = "mm")

#################################################

#data_continent wise
continent_period <- study_plot[, .(id, continent, record_length)]

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

#divide the record_lengths into ranges (10 months each)
record_length_asia <-as.data.table(table(cut(asia$record_length, breaks = seq(0, 60, by = 10))))
record_length_asia[, continent_name := factor('Asia')]

record_length_africa <-as.data.table(table(cut(africa$record_length, breaks = seq(0, 60, by = 10))))
record_length_africa[, continent_name := factor('Africa')]

record_length_europe <-as.data.table(table(cut(europe$record_length, breaks = seq(0, 60, by = 10))))
record_length_europe[, continent_name := factor('Europe')]

record_length_s_america <-as.data.table(table(cut(south_america$record_length, breaks = seq(0, 60, by = 10))))
record_length_s_america[, continent_name := factor('South_America')]

record_length_n_america <-as.data.table(table(cut(north_america$record_length, breaks = seq(0, 60, by = 10))))
record_length_n_america[, continent_name := factor('North_America')]

record_length_global <-as.data.table(table(cut(global$record_length, breaks = seq(0, 60, by = 10))))
record_length_global[, continent_name := factor('global')]

#merge all the continents to a single data table
merge_continents <- merge(record_length_asia, record_length_africa, all = TRUE)
asia_afi_ero <- merge(merge_continents, record_length_europe, all = TRUE)
asia_afi_ero_samer <- merge(asia_afi_ero, record_length_s_america, all = TRUE)
asia_afi_ero_s_n_ame <- merge(asia_afi_ero_samer, record_length_n_america, all = TRUE)
asia_afi_ero_s_n_ame_glob <- merge(asia_afi_ero_s_n_ame, record_length_global, all = TRUE)

##and now the plot
bar_plot <- ggplot(asia_afi_ero_s_n_ame_glob) + 
  geom_bar(aes(x= factor(V1),
      y = N,
      group = continent_name,
      fill = continent_name,
      color = continent_name),
      stat = "identity",
      position = position_dodge()) + 
  labs(x = "Validation length (months)", y = "Papers") + 
  scale_fill_manual(values = c("#4D648D", "#337BAE", "#97B8C2",
                               "#739F3D","#ACBD78",  "#F4CC70",
                               "#EBB582"))
p4 <- bar_plot +  scale_x_discrete(labels = c("0-10", "10-20",
                                              "20-30", "30-40", "40-50", "50-60"))

ggsave("results/plots/validation_lengths_continent.png",
       p4, dpi = 280, width = 200, height = 120, units = "mm")
  
##############################
#before plotting remove NA's from imerg_combi

imerg_combi <- subset(imerg_combi, !is.na(imerg_type))
imerg_combi <- subset(imerg_combi, !is.na(imerg_vers))
imerg_combi <- subset(imerg_combi, !is.na(continent))
imerg_combi <- subset(imerg_combi, !is.na(comparison_scale))
imerg_combi <- subset(imerg_combi, !is.na(downscale))
#reorder the levels of temporal_sclae of imerg_combi
imerg_combi$temporal_scale <- factor(imerg_combi$temporal_scale, 
                                     levels = c("0.5h", "1h", "3h", "6h", "12h",
                                                "18h",  "1h to 168h", "1d to 60d", 
                                                "daily",  "monthly",  "seasonal", "annual"))

imerg_combi$imerg_type <- factor(imerg_combi$imerg_type, 
                                     levels = c("IMERG_E", "IMERG_L", "IMERG_F"))
###temporal_scale_plot

p6 <- ggplot(imerg_combi, aes(temporal_scale, fill = continent)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=percent) + 
  #facet_wrap(~imerg_type) + 
  labs(x = "Temporalal scale", y = "Papers") + 
  scale_fill_manual(values = c("#4D648D", "#337BAE",
                               "#97B8C2",  "#739F3D",
                               "#ACBD78",  
                               "#F4CC70", "#EBB582")) + 
  facet_grid(imerg_type~continent, scales="free", space="free_x") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.9))

ggsave("results/plots/Temporal_scale_vs_papers.png", p6)

#temporal_scale_vs_IMERG_version

p6_1 <- ggplot(imerg_combi, aes(temporal_scale)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=percent) + 
  facet_wrap(~imerg_vers) + 
  labs(x = "Temporalal scale", y = "Papers") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.9))

ggsave("results/plots/Temp_scale_IMERG_vers.png", p6_1)

#Temporal_scale_vs_IMERG_type

p6_2 <- ggplot(imerg_combi, aes(temporal_scale)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=percent) + 
  facet_wrap(~imerg_type) + 
  labs(x = "Temporalal scale", y = "Papers") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.9))

ggsave("results/plots/Temp_scale_IMERG_type.png", p6_2)

#Temporal_scale_vs_Year
p6_3 <- ggplot(imerg_combi, aes(temporal_scale)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=percent) + 
  facet_wrap(~year) + 
  labs(x = "Temporalal scale", y = "Papers") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.9))

ggsave("results/plots/Temp_scale_Year.png", p6_3)

###spatial_scale_vs_papers_bar_plot

p7 <- ggplot(imerg_combi, aes(grid_scale, fill = continent)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=percent) + 
  labs(x = "Spatial scale", y = "Papers") + 
  scale_fill_manual(values = c("#4D648D", "#337BAE",
                               "#97B8C2",  "#739F3D",
                               "#ACBD78",  
                               "#F4CC70", "#EBB582")) + 
  facet_grid(imerg_type~continent, scales="free", space="free_x") + 
  theme(axis.text.x = element_text(angle = 50, hjust = 1, vjust = 0.9))

ggsave("results/plots/Spatial_scale_vs_papers.png", p7)


#spatial_vs_temporal_scales_scatter_plot
p8 <- ggplot(imerg_combi, aes(grid_scale, temporal_scale, color = imerg_type)) + 
  geom_jitter()+ 
  facet_wrap(~continent) + 
  labs(x = "Spatial scale", y = "Temporal scale") + 
  scale_fill_manual(values = c("#4D648D", "#337BAE",
                               "#97B8C2",  "#739F3D",
                               "#ACBD78",  
                               "#F4CC70", "#EBB582")) + 
  theme_light()
  #facet_grid(~, scales="free", space="free_x")

ggsave("results/plots/Temporal_vs_Spatial_scales.png", p8)

##comparison_method

ggplot(imerg_combi, aes(comparison_method)) + 
  geom_bar()+ 
  facet_wrap(~year) + 
  labs(x = "comparison_method") + 
  scale_fill_manual(values = c("#4D648D", "#337BAE",
                               "#97B8C2",  "#739F3D",
                               "#ACBD78",  
                               "#F4CC70", "#EBB582")) + 
  facet_grid(downscale~continent, scales="free", space="free_x") + 
  theme(axis.text.x = element_text(angle = 50, hjust = 1, vjust = 0.9))

ggsave("results/plots/Temporal_vs_Spatial_scales.png", )

##comparison_scale
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

##downscaled
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










############################################3

ggplot(imerg_combi, aes(grid_scale)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  labs(x = "Spatial scale", y = "Papers") + 
  scale_fill_manual(values = c("#4D648D", "#337BAE",
                               "#97B8C2",  "#739F3D",
                               "#ACBD78",  
                               "#F4CC70", "#EBB582")) + 
  facet_wrap(~comparison_method)
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))



#scatter plot

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



#box plot
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

ggplot(imerg_combi, aes(factor(year), temporal_scale)) + 
  geom_jitter()
  
  
  facet_wrap(~year) + 
  labs(x = "Spatial scale", y = "Temporal scale") + 
  scale_fill_manual(values = c("#4D648D", "#337BAE",
                               "#97B8C2",  "#739F3D",
                               "#ACBD78",  
                               "#F4CC70", "#EBB582")) + 
  facet_grid(imerg_type~continent, scales="free", space="free_x")

