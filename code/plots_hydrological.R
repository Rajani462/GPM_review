
hydro_metrics <- subset(hydro_metrics, hydro_metrics$Data_type != "IMERGFRUnCal") #remove the IMERGFRUnCal from the database.

ggplot(na.omit(hydro_metrics), aes(x=Data_type, 
                                   y= NSE)) + 
  geom_jitter(aes(shape=Model, color = Location), size= 4) + 
  scale_shape_manual(values=c(15, 13, 18, 20, 25)) + 
  theme_small + 
  #geom_vline(aes(xintercept= Data_type), color="#990000", linetype="dashed")



ggsave("results/plots/hydrological_NSE.png", width = 7.2,
       height = 5.3, units = "in", dpi = 600)




ggplot(na.omit(hydro_metrics), aes(x=Data_type, 
                                   y= Bias, 
                                   shape = Model,
                                   col = Data_type)) + 
  labs(x = "IMERG_RUN", y = "Realative bias (%)") + 
  geom_jitter() + 
  scale_color_discrete(guide = "none") + 
  facet_wrap(~Location) + 
  theme_small

ggsave("results/plots/hydrological_RBIAS.png", width = 7.2,
       height = 5.3, units = "in", dpi = 600)