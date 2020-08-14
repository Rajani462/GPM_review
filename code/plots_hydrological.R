ggplot(na.omit(hydro_metrics), aes(x=Data_type, 
                                 y= NSE, 
                                 shape = Model,
                                 col = Data_type)) + 
  labs(x = "IMERG_RUN", y = "NSE") +  
  geom_jitter() + 
  scale_color_discrete(guide = "none") + 
  facet_wrap(~Location) + 
  theme_generic
#geom_vline(aes(xintercept= Data_type), color="#990000", linetype="dashed")

ggplot(na.omit(hydro_metrics), aes(x=Data_type, 
                                   y= Bias, 
                                   shape = Model,
                                   col = Data_type)) + 
  labs(x = "IMERG_RUN", y = "Realative bias (%)") + 
  geom_jitter() + 
  scale_color_discrete(guide = "none") + 
  facet_wrap(~Location) + 
  theme_small
