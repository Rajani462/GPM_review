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
imerg_combi <- imerg_combi[run_tpe, on = 'id']
imerg_combi <- imerg_combi[study_gridscale, on = 'id']
imerg_combi <- imerg_combi[study_tempscale, on = 'id']
imerg_combi <- imerg_combi[study_compmthod, on  = 'id']

###########Spatial distribution of publication

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

#data for continent and country wise bar plot


plot_data2 <- study_plot[, .('publication_count' = .N),
                         by = continent]

plot_data3 <- study_plot[, .('publication_count' = .N),
                         by = .(country, continent)]

#continent wise_reordered
ggplot(plot_data2, aes(x = reorder(continent, publication_count),
                       y = publication_count, fill = continent)) + 
  geom_bar(stat = "identity") + 
  labs(x = "Continent", y = "Number of puplications") + 
  scale_fill_manual(values = c("#4D648D", "#337BAE",
                               "#97B8C2",  "#739F3D",
                               "#ACBD78",  
                               "#F4CC70", "#EBB582")) + 
  coord_flip() + 
  theme_bw()

#continent wise publications per year
ggplot(study_plot) + 
  geom_bar(aes(x = factor(year), fill = continent)) + 
  scale_fill_manual(values = c("#4D648D", "#337BAE",
                               "#97B8C2",  "#739F3D",
                               "#ACBD78",  
                               "#F4CC70", "#EBB582")) + 
  labs(x = "Year", y = "Number of papers") +
  theme_classic()

#country wise_reoodered
ggplot(plot_data3, aes(x = reorder(country, publication_count),
                       y = publication_count, fill = country)) +
  geom_bar(stat = "identity") + 
  labs(x = "Country", y = "Number of puplications") + 
  coord_flip() + 
  theme_bw()

#################################################



#bar plot of IMERG_alg and version

ggplot(na.omit(imerg_combi), aes(variable_name)) + 
  geom_bar() + 
  scale_x_discrete(drop = TRUE)
  
#scatter plot
ggplot(na.omit(imerg_combi), aes(x=lat_mean, 
                                 y=lon_mean, 
                                 col = gpm_algorithm)) + 
  geom_point() + 
  theme_bw()

ggplot(na.omit(imerg_combi), aes(x=lat_mean, y=lon_mean,
                                 size = record_length,
                                 col = gpm_algorithm)) + 
  geom_point() + 
  theme_classic()

ggplot(imerg_combi, aes(conti, gpm_type)) + 
  geom_line()

ggplot(na.omit(imerg_combi), aes(record_length, year)) + 
  geom_point()

ggplot(na.omit(imerg_combi), aes(comparison_method, downscale)) + 
  geom_point()

#box plot
ggplot(na.omit(imerg_combi), aes(x=gpm_algorithm, 
                                 y=record_length,
                               fill= downscale)) +
  geom_boxplot(alpha=0.4) 
#
  