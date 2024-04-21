
library(ggplot2)

ggplt <- ggplot(Orange,aes(x=circumference,y=age,shape=Tree))+
  geom_point()+
  theme_classic()

ggplt

# Plotting multiple Regression Lines
ggplt+geom_smooth(method=lm,se=FALSE,fullrange=TRUE,
                  aes(color=Tree))

