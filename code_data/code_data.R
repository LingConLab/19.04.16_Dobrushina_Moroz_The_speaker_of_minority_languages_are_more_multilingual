# qrcodes -----------------------------------------------------------------
library(qrcode)
qrcode_gen('tinyurl.com/y6jjp38y')
qrcode_gen('tinyurl.com/y6jjp38y', wColor = "#0099CC", bColor = "white")


# data --------------------------------------------------------------------
library(tidyverse)
library(grid)
library(gtable)

theme_set(theme_bw())
options(scipen=999)

residence_langs <- read_csv("residence_langs_ND.csv")
residence_langs %>% 
  distinct(status)
  