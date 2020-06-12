study_plot <- studies[, .(id, study_area, study_area_type, country, continent, 
                          lat_mean, lon_mean, area, variable, surface, record_length, year, journal)]



Paper_num<- study_plot[,.(Papers_count = sum(year)),by=.(journal)]

############
library("rnaturalearth")
library("rnaturalearthdata")


world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

global_dist <- ggplot(data = world) + 
  geom_sf() + 
  coord_sf(xlim = c(180, -180), ylim = c(90, -90)) + 
  geom_point(data = study_plot, aes(lon_mean, lat_mean,  color = continent)) + 
  labs(x = "Longitude", y = "Latitude")

global_dist + theme_classic()



#bar plot
ggplot(study_plot) + 
  geom_bar(aes(x = factor(year), fill = journal)) + 
  theme_classic()

#scatter plot
country_wise <- ggplot(study_plot) + 
  geom_bar(aes(x = country))

country_wise + coord_flip() + 
  theme_bw()
  
