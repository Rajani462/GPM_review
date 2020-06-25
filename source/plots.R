source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')
###############################
#data preparation for plots

study_plot <- studies[, .(id, study_area, study_area_type, country, continent, 
                          lat_mean, lon_mean, area, variable, surface, record_length, year)]


imerg_types <- alg_vers[run_type, on = 'id']

imerg_combi <- study_plot[alg_vers, on = 'id']
imerg_combi <- imerg_combi[run_tpe, on = 'id']
imerg_combi <- imerg_combi[study_gridscale, on = 'id']
imerg_combi <- imerg_combi[study_tempscale, on = 'id']


Paper_num<- study_plot[,.(Papers_count = sum(year)),by=.(journal)]

###########

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



#bar plot
ggplot(study_plot) + 
  geom_bar(aes(x = factor(year), fill = continent)) + 
  scale_fill_manual(values = c("#4D648D", "#337BAE", "#97B8C2",  "#739F3D", "#ACBD78",  
                               "#F4CC70", "#EBB582")) + 
  labs(x = "Year", y = "Number of papers") +
  theme_classic()


####continent wise bar plot
continent_wise <- ggplot(study_plot) +
  aes(x = continent, fill = continent) +
  geom_bar() + 
  geom_text(stat = 'count', aes(label = stat(count), hjust = -0.1), size=3.1)

continent_wise + coord_flip() + 
  labs(x = "Continent", y = "Number of papers") + 
  scale_fill_manual(values = c("#4D648D", "#337BAE", "#97B8C2",  "#739F3D", "#ACBD78",  
                               "#F4CC70", "#EBB582")) + 
  theme_bw()

#bar plot of IMERG_alg and versions
gpm_vers <- gpm_alg[gpm_vers, on = 'id']


#plot

ggplot(na.omit(gpm_alg), aes(variable_name)) + 
  geom_bar() + 
  scale_x_discrete(drop = TRUE)
  
#scatter plot
ggplot(na.omit(imerg_combi), aes(x=lat_mean, y=lon_mean, col = alg_vers)) + 
  geom_point()

ggplot(na.omit(imerg_combi), aes(x=lat_mean, y=lon_mean, size = record_length, col = gpm_algorithm)) + 
  geom_point()

ggplot(imerg_combi, aes(gpm_algorithm, gpm_type)) + 
  geom_point()

ggplot(na.omit(imerg_combi), aes(record_length, area)) + 
  geom_point()
