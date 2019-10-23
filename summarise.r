library(dplyr)
d_cjsg %>% 
  group_by(orgao_julgador) %>% 
  summarise(n = n())