library(tidyr)
library(foreach)
library(magrittr)
library(data.table)
library(ncdf4)
library(nctools)
library(terra)
library(matrixStats)
library(glue)
library(sf)
library(ggplot2)
library(RColorBrewer)
library(Ipaper)

grp <- dir('E:/cmip6/remaped', full.names = T) %>% 
  data.table(dir = ., file = basename(.)) %>% 
  separate(col = 2, 
           into = c('variable', 'model', 'expt', 'variant', 'grid', 'date'), 
           sep = '_')

#registerDoFuture()
#plan(multisession, workers = 3)

data <- foreach(i = grp$model %>% unique(), .combine = rbind) %do% {
  sub <- grp[model == i]
  if(nrow(sub) == 1) return()  # if this model's file number = 1, skip
  
  nc1 <- nc_open(sub$dir[1])
  arr1 <- ncvar_get(nc1)
  nc2 <- nc_open(sub$dir[2])
  arr2 <- ncvar_get(nc2)
  
  coord <- raster(sub$dir[1]) %>% 
    coordinates()
  date <- nc_date(nc1)
  
  foreach(p = c('2021:2040', '2041:2060', '2081:2100'), .combine = rbind) %do% {
    p %<>% {eval(parse(text = .))}
    data1 <- which(year(date) %in% p) %>% arr1[, , .]
    data2 <- which(year(date) %in% p) %>% arr2[, , .]
    
    dim <- dim(data1)
    dim(data1) <- c(prod(dim[1:2]),dim[3])
    data1 %<>% rowMeans2()
    dim(data2) <- c(prod(dim[1:2]),dim[3])
    data2 %<>% rowMeans2()
    data <- data1 - data2
    
    plot <- data.frame(coord, data)
    plot$x %<>% {. - 180}
    rm(data, data1, data2)
    gc()
    
    rast(plot, type = 'xyz', crs = 'wgs84') %>% 
      terra::project('ESRI:54030') %>% 
      terra::as.data.frame(xy = T) %>% 
      data.table(data.table(date = glue('{min(p)}-{max(p)}'), model = i))
      
  }
}


continent <- st_read('D:/SHP/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp')

p <- ggplot(data) + 
  geom_contour_filled(aes(x = x, y = y, z = data), 
                      breaks = c(-Inf, seq(-2, 2, 0.5), Inf)) + 
  geom_sf(data = continent, fill = NA, lwd = 0.3, color = 'grey40') + 
  coord_sf(crs = st_crs('ESRI:54030')) + 
  labs(x = NULL, y = NULL, fill = 'Difference between esm-ssp585-ssp126Lu and esm-ssp585 (°C)') + 
  guides(fill = guide_coloursteps(title.position = "bottom")) + 
  scale_fill_manual(values = rev(brewer.pal(10, 'RdBu')), label = as.numeric) + 
  facet_grid(model~date, switch = 'y') + 
  theme_void() + 
  theme(text = element_text(size = 15, color ='#1F2124'), 
        plot.title = element_text(hjust = 0.5, colour = '#4e4e4e', size = 25), 
        legend.key.width = unit(5, 'cm'), 
        legend.key.height = unit(0.5, 'cm'),
        legend.title = element_text(hjust = 0.5, size = 13),
        legend.text = element_text(size = 11, vjust = 0.4), 
        legend.position = 'bottom', 
        panel.spacing.y = unit(0.8, 'cm'), 
        legend.box.margin = unit(c(0.8, 0.8, 0.8, 0.8), 'cm'), 
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), 'cm'), 
        strip.text.x = element_text(margin = margin(b = 0.4, t = 0.2, unit = 'cm')), 
        strip.text = element_text(size = 13))

#ggsave(p, 'E:/test.pdf', width = 15, height = 30)
write_fig(p, 'E:/test.pdf', width = 11, height = 10)
