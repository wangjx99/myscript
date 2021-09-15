library(gcookbook)
library(plyr)

tophit <- tophitters2001[1:25, ]
tophit[,c("name", "lg", "avg")]
nameorder <- tophit$name[order(tophit$lg, tophit$avg)] #提取name并排序
tophit$name <- factor(tophit$name, levels = nameorder) #转因子，水平同nameorder

ggplot(tophit,aes (x = avg, y = name))+
  geom_segment(aes(yend = name), xend = 0, colour = "grey50")+
  geom_point(size = 3, aes(colour = lg))+
  scale_colour_brewer(palette = "Set1", limits = c("NL", "AL"))+
  theme_bw()+
  theme(panel.grid.major.y = element_blank(), #删除网格线
        legend.position = c(1, 0.55), #将图例放置在网格区域里
        legend.justification = c(1, 0.5))+
  facet_grid(lg ~ ., scales = "free_y", space = "free_y") #分面


sunspotyear <- data.frame(
  Year = as.numeric(time(sunspot.year)),
  Sunspots = as.numeric(sunspot.year)
)

ggplot(ChickWeight, aes(x = Time, y = weight))+
  geom_boxplot(aes(group = Time))
