ggplot(na.omit(hydro_metrics), aes(x=Data_type, 
                                   y= NSE)) + 
  geom_point(aes(shape=Model, color = Location), size= 4) + 
  scale_shape_manual(values=c(15, 13, 18, 20, 25)) + 
  theme_generic


NSE + geom_point(aes(colour = factor(Location)), size = 4) +
  geom_point(colour = "grey90", size = 1.5)

  
   geom_point() + 
  #scale_color_discrete(guide = "none") + 
  #facet_wrap(~Location) + 
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
