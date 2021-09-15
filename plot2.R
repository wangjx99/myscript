library(rgdal)
library(magrittr)
library(ggplot2)
library(metR)
library(ggthemes)
library(raster)
library(RColorBrewer)

test <- readRDS('E:/test.rds')  ## import data
test$x %<>% {. - 180}

## download .shp file
## https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural/ne_10m_admin_0_countries.zip
continent <- st_read('D:/SHP/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp')

rast(test, type = 'xyz', crs = 'wgs84') %>% 
  terra::project('ESRI:54030') %>% 
  as.data.frame(xy = T) %>% 
  ggplot(.) + 
  geom_contour_filled(aes(x = x, y = y, z = data), 
                      breaks = c(-Inf, seq(-2, 2, 0.5), Inf)) + 
  geom_sf(data = continent, fill = NA, lwd = 0.8, color = 'grey40') + 
  coord_sf(crs = 'ESRI:54030') + 
  labs(x = NULL, y = NULL, fill = 'Diff (°C)', title = 'This is an example') + 
  guides(fill = guide_coloursteps(title.position = "bottom")) + 
  scale_fill_manual(values = rev(brewer.pal(10, 'RdBu')), label = as.numeric) + 
  theme_void() + 
  theme(text = element_text(size = 15, color ='#1F2124'), 
        plot.title = element_text(hjust = 0.5, colour = '#4e4e4e', size = 25), 
        legend.key.width = unit(5, 'cm'), 
        legend.key.height = unit(0.5, 'cm'),
        legend.title = element_text(hjust = 0.5, size = 13),
        legend.text = element_text(size = 11, vjust = 0.4), 
        legend.position = 'bottom')

rasterFromXYZ(test, crs = 4326) %>% 
  projectRaster(res = 50000, crs = 'ESRI:54030') %>% 
  rasterToPoints() %>% 
  as.data.frame() %>% 
  ggplot(.) + 
  geom_contour_filled(aes(x = x, y = y, z = data), 
                      breaks = c(-Inf, seq(-2, 2, 0.5), Inf)) + 
  geom_sf(data = continent, fill = NA, lwd = 0.8, color = 'grey40') + 
  coord_sf(crs = st_crs('ESRI:54030')) + 
  labs(x = NULL, y = NULL, fill = 'Diff (°C)', title = 'This is an example') + 
  guides(fill = guide_coloursteps(title.position = "bottom")) + 
  scale_fill_manual(values = rev(brewer.pal(10, 'RdBu')), label = as.numeric) + 
  theme_void() + 
  theme(text = element_text(size = 15, color ='#1F2124'), 
        plot.title = element_text(hjust = 0.5, colour = '#4e4e4e', size = 25), 
        legend.key.width = unit(5, 'cm'), 
        legend.key.height = unit(0.5, 'cm'),
        legend.title = element_text(hjust = 0.5, size = 13),
        legend.text = element_text(size = 11, vjust = 0.4), 
        legend.position = 'bottom')

ggsave(filename = 'eg.pdf', width = 12, height = 8)