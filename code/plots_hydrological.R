
hydro_metrics <- subset(hydro_metrics, hydro_metrics$Data_type != "IMERGFRUnCal") #remove the IMERGFRUnCal from the database.

NSE <- ggplot(na.omit(hydro_metrics), aes(x=Data_type, y= NSE)) + 
  labs(x = "IMERG_RUN", y = "NSE") + 
  geom_jitter(aes(shape=Model, color = Location), size= 4) + 
  #scale_shape_manual(values=c(15, 13, 18, 20, 25)) + 
  theme_small
  #geom_vline(aes(xintercept= Data_type), color="#990000", linetype="dashed")



ggsave("results/plots/hydrological_NSE.png", width = 7.2,
       height = 5.3, units = "in", dpi = 600)




BIAS <- ggplot(na.omit(hydro_metrics), aes(x=Data_type, y= Bias)) + 
  labs(x = "IMERG_RUN", y = "Realative bias (%)") + 
  geom_jitter(aes(shape=Model, color = Location), size= 4) + 
  scale_color_discrete(guide = "none") + 
  #facet_wrap(~Location) + 
  theme_small + 
  theme(legend.position = "none")


#NSE_BIAS <- grid.arrange(NSE, BIAS, ncol = 2)
NSE_BIAS <- ggarrange(NSE, BIAS, ncol = 2, common.legend = TRUE, legend = "bottom")
ggsave("results/plots/hydrological_NSE_BIAS.png", NSE_BIAS, width = 7.2,
       height = 5.3, units = "in", dpi = 600)
