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



