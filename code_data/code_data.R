library(qrcode)
library(tidyverse)
library(lingtypology)
library(grid)
library(gtable)
theme_set(theme_bw())
options(scipen=999)

# qrcodes -----------------------------------------------------------------
qrcode_gen('tinyurl.com/y6jjp38y')
qrcode_gen('tinyurl.com/y6jjp38y', wColor = "#0099CC", bColor = "white")

# maps --------------------------------------------------------------------
residence_langs <- read_csv("residence_langs_ND.csv")
map.feature(language = residence_langs$language,
            features = residence_langs$feature,
            latitude = residence_langs$lat,
            longitude = residence_langs$long,
            label = residence_langs$residence,
            tile = "Esri.WorldGrayCanvas",
            minimap = TRUE,
            minimap.position = "topright",
            legend.position = "topleft")
# 03_map_930_700
map.feature(language = residence_langs$language,
            features = residence_langs$status,
            latitude = residence_langs$lat,
            longitude = residence_langs$long,
            label = residence_langs$residence,
            tile = "Esri.WorldGrayCanvas",
            minimap = TRUE,
            minimap.position = "topright",
            legend.position = "topleft")

# 04_map_class_930_700

read_tsv("https://multidagestan.com/api/respondents-flat?format=tsv&limit=0&fields=expedition.name,name,code,residence.en,sex,direct,birth,death,language,level") %>% 
  mutate(level = if_else(level == 2, 1, level),
         level = if_else(level == -1, 0, level)) %>% 
  spread(language, level, fill = 0) %>% 
  mutate(decade = round(birth/10)*10) %>% 
  filter(expedition.name != "Richa, Chirag",
         expedition.name != "Arkhit, Kug, Laka, Khiv",
         !{expedition.name == "Mukar, Uri, Shangoda" &
             residence.en == "Shangoda"}) %>%  
  left_join(residence_langs, by = c("expedition.name" = "expedition", "residence.en" = "residence")) %>% 
  select(-37) %>%  # remove Russian 
  mutate(sum_langs = rowSums(.[9:46])-1) %>% 
  select(residence.en, decade, langs, sum_langs, status, expedition.name) %>% 
  filter(decade <= 1950,
         decade >= 1900,
         sum_langs >= 0) %>% 
  group_by(residence.en, status, decade) %>% 
  mutate(median = median(sum_langs)) %>% 
  count(residence.en, status, decade, median, sum_langs) %>% 
  group_by(residence.en, status, decade, median) %>% 
  mutate(overall = sum(n),
         ratio = n/overall) %>% 
  ungroup() %>% 
  mutate(status = factor(status, levels = c("big", "medium", "small")))->
  df

decade <- 1900
df %>% 
  distinct(residence.en, decade, median) %>% 
  filter(decade == decade) %>% 
  left_join(residence_langs, by = c("residence.en" = "residence")) ->
  df_m
map.feature(language = df_m$language,
              features = df_m$median,
              color = c("white", "red"),
              latitude = df_m$lat,
              longitude = df_m$long,
              label = df_m$residence.en,
              tile = "Esri.WorldGrayCanvas",
              minimap = TRUE,
              minimap.position = "topright",
              legend.position = "topleft",
              title = decade)

# 05_map_1900_930_700
# 06_map_1910_930_700
# 07_map_1920_930_700
# 08_map_1930_930_700
# 09_map_1940_930_700
# 10_map_1950_930_700