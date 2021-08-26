source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')
##################################

hydrological <- readRDS('./data/hydrological.Rds')

# data preparation for plot ----------------------------------------------

hydro_metrics <- hydrological[, .(Ref, Location, Basin, Data_type, Model, NSE, Bias)]

#remove the IMERGFRUnCal from the database.
hydro_metrics <- subset(hydro_metrics, hydro_metrics$Data_type != "IMERGFRUnCal") 

#reorder the factors 
hydro_metrics$Data_type <- factor(hydro_metrics$Data_type, 
                                 levels = c("IMERG_E", "IMERG_L", "IMERG_F"))

# plot --------------------------------------------------------------------

##NSE trial
ggplot(na.omit(hydro_metrics), aes(x=Data_type, 
                                   y= NSE)) + 
  geom_point(aes(shape=Model, color = Location), size= 4) + 
  scale_shape_manual(values=c(15, 13, 18, 20, 8)) + 
  theme_small

#### NSE

NSE <- ggplot(na.omit(hydro_metrics), aes(x=Data_type, y= NSE)) + 
  labs(x = "IMERG_RUN", y = "NSE") + 
  geom_jitter(width = 0.2, aes(shape=Model, color = Location), size= 4) + 
  scale_shape_manual(values=c(18, 15, 16, 17, 8)) + 
  scale_color_manual(values = c("#FC4E07", "#0000FF", "#E7B800", "#00BA38", "#619CFF")) + 
  theme_generic 

#### BIAS

BIAS <- ggplot(na.omit(hydro_metrics), aes(x=Data_type, y= Bias)) + 
  labs(x = "IMERG_RUN", y = "Realative bias (%)") + 
  geom_jitter(width = 0.2, aes(shape=Model, color = Location), size= 4) + 
  scale_shape_manual(values=c(18, 15, 16, 17, 8)) + 
  scale_color_manual(values = c("#FC4E07", "#0000FF", "#E7B800", "#00BA38", "#619CFF")) + 
  #scale_color_discrete(guide = "none") + 
  #facet_wrap(~Location) + 
  theme_generic + 
  theme(legend.position = "none")


NSE_BIAS <- ggarrange(NSE, BIAS, ncol = 2, common.legend = TRUE, legend = "bottom")


ggsave("results/plots_paper/hydrological_NSE_BIAS_rev1.png", NSE_BIAS, width = 9.2,
       height = 5.3, units = "in", dpi = 600)
