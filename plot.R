library(ncdf4)
library(magrittr)
library(lubridate)
library(matrixStats)
library(raster)
library(ggplot2)
library(metR)
library(sf)
library(ggthemes)
library(Ipaper)

nc <- "tas_Amon_BCC-CSM2-MR_esm-hist_r1i1p1f1_gn_185001-201412.nc" %>% nc_open()
date <- nc_date(nc)
#date <- ncvar_get(nc, 'time') %>% as_date(origin = '1850-01-01')
arr <- ncvar_get(nc)
coord <- raster("tas_Amon_BCC-CSM2-MR_esm-hist_r1i1p1f1_gn_185001-201412.nc") %>% 
  coordinates()
data1 <- which(year(date)%in%1995:2014) %>% arr[,,.]

nc2 <- "tas_Amon_BCC-CSM2-MR_esm-ssp585-ssp126Lu_r1i1p1f1_gn_201501-210012.nc" %>% nc_open()
date <- ncvar_get(nc, 'time') %>% as_date(origin = '2015-01-01')
arr2 <- ncvar_get(nc)
data2 <- which(year(date)%in%2021:2040) %>% arr2[,,.]

dim <- dim(data1)
dim(data1) <- c(prod(dim[1:2]),dim[3])
data1 %<>% rowMeans2()
#plot1 <- data.frame(coord,data1)

dim <- dim(data2)
dim(data2) <- c(prod(dim[1:2]),dim[3])
data2 %<>% rowMeans2()
data <- data2 - data1
plot <- data.frame(coord,data)


ggplot()+geom_contour_fill(data = plot, aes(x, y, z = data))+
  labs(title = "585-hist_2021-2040")+
  theme_void()+scale_fill_viridis_c(option = 'viridis')
ggsave(file = 'E:/esm_585_hist_2021-2040.png', width = 10, height = 5)


file1 <- 'E:/cmip6/tas_Amon/tas_Amon_CanESM5_esm-ssp585-ssp126Lu_r1i1p2f1_gn_201501-210012.nc'
file2 <- 'E:/cmip6/tas_Amon/tas_Amon_CanESM5_esm-ssp585_r1i1p1f1_gn_201501-210012.nc'

draw_diff <- function(file1, period1, file2, period2, title = 'defult') {
  if(length(period1) != length(period2)) stop('not the same length')
  nc <- nc_open(file1)
  date <- nc_date(nc)
  arr <- ncvar_get(nc)
  coord <- raster(file1) %>% 
    coordinates()
  data1 <- which(year(date) %in% period1) %>% arr[,,.]
  #shp_data <- sf::st_read("./continent.shp")
  
  shp_data <- readOGR('./continent.shp') %>% 
    spTransform(CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
  
  nc2 <- nc_open(file2)
  date <- nc_date(nc2)
  arr2 <- ncvar_get(nc2)
  data2 <- which(year(date) %in% period2) %>% arr2[,,.]
  
  dim <- dim(data1)
  dim(data1) <- c(prod(dim[1:2]),dim[3])
  data1 %<>% rowMeans2()
  #plot1 <- data.frame(coord,data1)
  
  if(!any(dim == dim(data2))) stop('not the same dim')
  dim(data2) <- c(prod(dim[1:2]),dim[3])
  data2 %<>% rowMeans2()
  data <- data2 - data1
  plot <- data.frame(coord, data)
  return(plot)
  
  ggplot() + 
    #geom_tile(data = plot, aes(x, y, fill = data)) +
    geom_contour_fill(data = plot, aes(x - 180, y, z = data)) +
    geom_sf(data = shp_data, fill = NA, lwd = 0.8) +   
    labs(title = title) +
    coord_proj("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") + 
    theme_pander() + 
    scale_fill_distiller(palette = 'RdBu') + 
    labs(x = NULL, y = NULL, fill = 'diff') + 
    theme(plot.title = element_text(hjust = 0.5), 
          legend.key.height = unit(1.5, 'cm'))
}

draw_diff(file1, 1995:2014, file2, 2021:2040, 'no.2_585-hist-2021-2040') %>% 
  ggsave('E:/no.2_585-hist-2021-2040.pdf', ., width = 20, height = 10)
draw_diff(file1, 1995:2014, file2, 2041:2060, 'no.2_585-hist-2041-2060') %>% 
  ggsave('E:/no.2_585-hist-2041-2060.pdf', ., width = 20, height = 10)
draw_diff(file1, 1995:2014, file2, 2081:2100, 'no.2_585-hist-2081-2100') %>% 
  ggsave('E:/no.2_585-hist-2081-2100.pdf', ., width = 20, height = 10)
draw_diff(file1, 1995:2014, file3, 2021:2040, 'no.2_126-hist-2021-2040') %>% 
  ggsave('E:/no.2_126-hist-2021-2040.pdf', ., width = 20, height = 10)
draw_diff(file1, 1995:2014, file3, 2041:2060, 'no.2_126-hist-2041-2060') %>% 
  ggsave('E:/no.2_126-hist-2041-2060.pdf', ., width = 20, height = 10)
draw_diff(file1, 1995:2014, file3, 2081:2100, 'no.2_126-hist-2081-2100') %>% 
  ggsave('E:/no.2_126-hist-2081-2100.pdf', ., width = 20, height = 10)
draw_diff(file2, 2021:2040, file3, 2021:2040, 'no.2_sspcha-2021-2040') %>% 
  ggsave('E:/no.2_sspcha-2021-2040.pdf', ., width = 20, height = 10)
draw_diff(file2, 2041:2060, file3, 2041:2060, 'no.2_sspcha-2041-2060') %>% 
  ggsave('E:/no.2_sspcha-2041-2060.pdf', ., width = 20, height = 10)
draw_diff(file2, 2081:2100, file3, 2081:2100, 'no.2_sspcha-2081-2100') %>% 
  ggsave('E:/no.2_sspcha-2081-2100.pdf', ., width = 20, height = 10)