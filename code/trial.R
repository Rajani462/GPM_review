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
  facet_grid(~continent, space = "free", scales = "free_x") + 
  theme_very_small + 
  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(angle = 40, hjust = 0.8, vjust = 0.9))
##################################
Country_barplot <- ggplot(plot_country) + 
  geom_bar(aes(x = reorder(country, -prop),
               y = prop,
               #color = continent, 
               fill = continent),
           stat = "identity") + 
  labs(x = "Country", y = "Studies (%)") + 
  scale_fill_manual(values = mycol_continent5) + 
  
  #coord_flip() + 
  theme_small + 
  theme(legend.position = "bottom") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.9)) + 
  labs(fill = "Continents") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())



world_map <- ggplot(data = world) + 
  geom_sf(fill = "white") + 
  coord_sf(xlim = c(-180, 180), ylim = c(-90, 90)) + 
  geom_point(data = study_plot2, aes(lon_mean, lat_mean, 
                                     color = continent)) + 
  scale_color_manual(values = mycol_continent5) +  
  
  theme(axis.title.x = element_text(vjust = -3), 
        axis.title.y = element_text(vjust = 3)) + # move away for axis
  #labs(x = "Longitude", y = "Latitude")
  xlab(label = "Longitude") +
  ylab(label = "Latitude") + 
  theme_generic + 
  theme(legend.title=element_blank()) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())



Country_barplot + annotation_custom(grob = world_map, xmin = xmin, xmax = xmax, 
                                    ymin = ymin-1.5, ymax = ymin+1.5)

