a <- {nc_open("E:/cmip6/tas_Amon/tas_Amon_ACCESS-ESM1-5_esm-ssp585-ssp126Lu_r1i1p1f1_gn_201501-210012.nc")%>% 
    ncvar_get() - nc_open("E:/cmip6/tas_Amon/tas_Amon_ACCESS-ESM1-5_esm-ssp585_r3i1p1f1_gn_201501-210012.nc")%>% 
        ncvar_get()} %>% 
  apply(3, mean) %>% 
  matrix(ncol = 12) %>%
  apply(1, mean) %>% 
  data.frame(year = 2015:2100, tem = .)
#a <- matrix(a, ncol = 12)
#a <- data.frame(a)
ggplot(a, aes( x = year, y = tem))+
  geom_line()+
  theme_bw()
ggsave('E:/plot_Amon.pdf', height = 10, width = 20)

grp <- dir('E:/cmip6/tas_Amon', full.names = T) %>%
  data.table(dir = ., file = basename(.)) %>%
  separate(col = 2, into = c('variable', 'kind','model', 'expt', 'variant', 'grid', 'date'),
           sep = '_')

data <- foreach(i = grp$model %>% unique(), .combine = rbind) %dopar% {
  sub <- grp[model == i]
  if(nrow(sub) == 1) return()  # if this model's file number = 1, skip
  {nc_open(sub$dir[2])%>% 
      ncvar_get() - nc_open(sub$dir[1])%>% 
      ncvar_get()} %>% 
    apply(3, mean) %>% 
    matrix(ncol = 12) %>%
    apply(1, mean) %>% 
    data.table(year = 2015:2100, tem = ., model = i)
}
#data$year <- as.numeric(data$year)
#data$tem <- as.data.frame(lapply(data, as.numeric))
ggplot(data)+
  geom_line(aes(x = year, y = tem))+
  facet_wrap(~model, scale = 'free')+
  theme_bw()
ggsave('E:/difference1_facet.pdf', height = 6, width = 13)

