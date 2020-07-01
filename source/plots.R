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

###########Spatial distribution of publication
library("sf")
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
  theme_classic()
ggsave("results/plots/paperfraction_per_continent.png", p1, dpi = 300, width = 170, height = 100, units = "mm")

#continent wise papers per year
p2 <- ggplot(study_plot) + 
  geom_bar(aes(x = factor(year), fill = continent)) + 
  scale_fill_manual(values = c("#4D648D", "#337BAE",
                               "#97B8C2",  "#739F3D",
                               "#ACBD78",  
                               "#F4CC70", "#EBB582")) + 
  labs(x = "Year", y = "Number of papers") +
  theme_classic()
ggsave("results/plots/papers_per_year_continents.png", p2, dpi = 300, width = 170, height = 100, units = "mm")

#country wise papers reoodered
p3 <- ggplot(plot_country, aes(x = reorder(country, prop),
                       y = prop)) +
  geom_bar(stat = "identity") + 
  labs(x = "Country", y = "Papers fraction") + 
  coord_flip() + 
  theme_classic()
ggsave("results/plots/paperfraction_per_country.png", p3, dpi = 300, width = 150, height = 120, units = "mm")

#################################################

#scatter plot
ggplot(na.omit(imerg_combi), aes(x=lat_mean, 
                                 y=lon_mean, 
                                 col = imerg_vers)) + 
  geom_point() + 
  theme_bw()

ggplot(na.omit(imerg_combi), aes(x=lat_mean, y=lon_mean,
                                 size = record_length,
                                 col = imerg_vers)) + 
  geom_point() + 
  theme_classic()

ggplot(na.omit(imerg_combi), aes(x=lat_mean, y=lon_mean,
                                 #size = record_length,
                                 col = imerg_type)) + 
  geom_point() + 
  theme_classic()

ggplot(na.omit(imerg_combi), aes(imerg_vers, year)) + 
  geom_line()

ggplot(na.omit(imerg_combi), aes(comparison_method, downscale)) + 
  geom_point()

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

  
ggplot(na.omit(imerg_combi), aes(x=imerg_type, 
                                 y=record_length)) +
  geom_boxplot(alpha=0.4) + 
  theme_classic()

#record length and continents line plot
plot_recordlength <- study_plot[, .('paper_count' = .N),
                           by = .(record_length, continent)][, percent := round(paper_count / sum(paper_count) * 100, 2)]

ggplot(plot_recordlength, aes(x = record_length)) +
  geom_histogram() +
  facet_wrap(~ continent)

#data_continent wise
asia <- study_plot[continent == "Asia"]
africa <- study_plot[continent == "Africa"]
europe <- study_plot[continent == "Europe"]
south_america <- study_plot[continent == "South America"]
north_america <- study_plot[continent == "North America"]
global <- study_plot[continent == "Global"]

#data for histogram or barplot
plot_asia <- asia[, .('count' = .N),
                   by = .(record_length, continent)][, percent := round(count / sum(count) * 100, 2)]


ggplot(plot_asia, aes(x= factor(record_length))) + 
  
  geom_bar() +
  
  theme_classic()



plot_europe <- europe[, .(id, 'count' = .N),
                  by = .(record_length, continent)][, percent := round(count / sum(count) * 100, 2)]




ggplot(plot_asia, aes(x = record_length, y = percent)) + 
  geom_line() + 
  #geom_bar(stat = "identity") + 
  theme_classic()


ggplot(plot_recordlength, aes(x =  record_length,
                                  y = percent, fill)) + 
  geom_bar(stat = "identity") + 
  theme_classic()

hist(plot_recordlength[plot_recordlength$continent == "Asia"])

plot_recordlength %>%
  filter(continent == "Asia") %>%
  ggplot() + 
  geom_histogram(aes(x = record_length), fill = "grey", color = "black")

ggplot(Asia, aes(x = record_length)) +
  geom_bar()


length <- Asia$record_length
hist(length)

     main="Maximum daily temperature at La Guardia Airport",
     xlab="Temperature in degrees Fahrenheit",
     xlim=c(10,48),
     col="chocolate",
     border="brown",
     breaks=c(12,20,30,40))
hist(length)
