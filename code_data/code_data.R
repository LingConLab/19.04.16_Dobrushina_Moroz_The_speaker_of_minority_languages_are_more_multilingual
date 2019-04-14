library(qrcode)
library(tidyverse)
library(extrafont)
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

my_decade <- 1910
df %>% 
  distinct(residence.en, decade, median) %>% 
  filter(decade == my_decade) %>% 
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
              title = my_decade)

# 05_map_1900_930_700
# 06_map_1910_930_700
# 07_map_1920_930_700
# 08_map_1930_930_700
# 09_map_1940_930_700
# 10_map_1950_930_700

# panel graph -------------------------------------------------------------
df %>% 
  ggplot(aes(decade, ratio, fill = factor(sum_langs)))+
  geom_col()+
  facet_wrap(~status+residence.en, ncol = 8)+
  theme(legend.position = "bottom",
        strip.background=element_blank(),
        strip.text.x = element_text(color = "white"),
        text=element_text(family="Brill", size = 16))+
  scale_fill_discrete(name = "number of languages") + 
  guides(fill = guide_legend(nrow = 1)) ->
  p

dummy <- p
dummy$layers <- NULL
dummy <- dummy + geom_rect(data=df, xmin=-Inf, ymin=-Inf, xmax=Inf, ymax=Inf,
                           aes(fill = status))+
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c"))

g1 <- ggplotGrob(p)
g2 <- ggplotGrob(dummy)

gtable_select <- function (x, ...) 
{
  matches <- c(...)
  x$layout <- x$layout[matches, , drop = FALSE]
  x$grobs <- x$grobs[matches]
  x
}

panels <- grepl(pattern="panel", g2$layout$name)
strips <- grepl(pattern="strip-t", g2$layout$name)
g2$grobs[strips] <- replicate(sum(strips), grid::nullGrob(), simplify = FALSE)
g2$layout$t[panels] <- g2$layout$t[panels] -1

new_strips <- gtable_select(g2, panels | strips)
grid.newpage()
grid.draw(new_strips)

gtable_stack <- function(g1, g2){
  g1$grobs <- c(g1$grobs, g2$grobs)
  g1$layout <- rbind(g1$layout, g2$layout)
  g1
}

## ideally you'd remove the old strips, for now they're just covered
new_plot <- gtable_stack(g1, new_strips)
new_plot <- gtable_stack(new_plot, g1)
grid.newpage()
grid.draw(new_plot)

# 11_panel_1100_750

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
  select(residence.en, decade, sum_langs, status) ->
  df


df %>% 
  ggplot(aes(status, sum_langs, fill = status))+
  geom_violin()+
  facet_wrap(~decade)+
  labs(x = "language category",
       y = "number of L2")+
  theme(legend.position = "bottom",
        text=element_text(family="Brill", size = 20))+
  scale_fill_manual(name = "", values = c("#1f77b4", "#ff7f0e", "#2ca02c"))

# 12_panel_1100_750

library(lme4)
library(lmerTest)
df$status <- factor(df$status, levels = c("small", "medium", "big"))
fit <- glmer(sum_langs ~ status + (1|residence.en)+ (1|decade), data = df, family = "poisson")
summary(fit)
sjstats::overdisp(fit)

# 13_poisson

plot(fit)

# 14_poisson_residuals_750_400

library(effects)
ef <- effect("status", fit)
ef <-  as.data.frame(ef)
ef %>% 
  ggplot(aes(status, fit, color = status))+
  geom_pointrange(aes(ymin = lower, ymax = upper), size = 1.2)+
  labs(x = "", y = "predicted number of L2")+
  theme(text=element_text(family="Brill", size = 20))+
  scale_color_manual(name = "", values = c("#1f77b4", "#ff7f0e", "#2ca02c"))

# 15_predicted_750_400