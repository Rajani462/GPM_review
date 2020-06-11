study_plot <- studies[, .(id, study_area, study_area_type, country, continent, 
                          lat_mean, lon_mean, area, variable, surface, year, journal)]



Paper_num<- study_plot[,.(Papers_count = sum(year)),by=.(journal)]

############
library("rnaturalearth")
library("rnaturalearthdata")

theme_harvard <- theme_minimal() + # start with a minimal theme and add what we need
  theme(text = element_text(color = "gray20"),
        legend.position = c("top"), # position the legend in the upper left 
        legend.direction = "horizontal",
        legend.justification = 0.1, # anchor point for legend.position.
        legend.text = element_text(size = 11, color = "gray10"),
        axis.text = element_text(face = "italic"),
        axis.title.x = element_text(vjust = -1), # move title away from axis
        axis.title.y = element_text(vjust = 2), # move away for axis
        axis.ticks.y = element_blank(), # element_blank() is how we remove elements
        axis.line = element_line(color = "gray40", size = 0.5),
        axis.line.y = element_blank(),
        panel.grid.major = element_line(color = "gray50", size = 0.5),
        panel.grid.major.x = element_blank())

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

global_dist <- ggplot(data = world) + 
  geom_sf() + 
  coord_sf(xlim = c(180, -180), ylim = c(60, -60)) + 
  geom_point(data = study_plot, aes(lon_mean, lat_mean,  color = continent)) + 
  labs(x = "Longitude", y = "Latitude")

global_dist + theme_harvard




ggplot(study_plot) + 
  geom_bar(aes(x = factor(year), fill = journal)) + 
  theme_classic()

#ggplot(study_plot) + 
  geom_bar(aes( = journal)) + 
  facet_wrap(~year) + 
  theme_harvard
