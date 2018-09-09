library(tidyverse)
library(lingtypology)
df <- read_tsv("https://raw.githubusercontent.com/LingConLab/2018_fedorenko_ALS_Circassian_isoglosses/master/2018_field_data%20-%20elicitation.tsv")

df %>% 
  mutate(grammaticality = if_else(stringr::str_detect(sentence, "\\*"), "*", "+"),
         en_village = case_when(
           village == "Адамий" ~ "Adamiy",
           village == "Гатлукай" ~ "Gatlukay",
           village == "Джамбечий" ~ "Dzhambichi",
           village == "Натухай" ~ "Natukhay",
           village == "Нешукай" ~ "Neshukay",
           village == "Панахес" ~ "Panakhes",
           village == "Псекупс" ~ "Psekups",
           village == "Уляп" ~ "Ulyap")
         ) %>%
  left_join(circassian[,c(1:3, 6)], by = c("en_village" = "village")) ->
  df

# look at the data
df %>% 
  filter(feature == "бенефактивный послелог",
         variable == "ой пае") %>% 
  group_by(variable, grammaticality) %>% 
  summarise(vilages = paste0(sort(en_village), collapse = ", "),
            n = n())

# visualise the data
df %>% 
  filter(feature == "бенефактивный послелог",
         variable == "ой пае") ->
  t

map.feature(languages = t$language,
              features = t$grammaticality,
              latitude = t$latitude,
              longitude = t$longitude,
              label = t$en_village,
              width = 10,
              label.fsize = 20,
              label.hide = FALSE,
              tile = "Stamen.TonerBackground",
              title = "Benefactive </br> postposition </br> /wej paje/",
              minimap = TRUE)


# clusterisation
# cluster villages
df %>%
  distinct() %>% 
  mutate(grammaticality = if_else(grammaticality == "+", 1, 0)) %>% 
  select(grammaticality, en_village, variable) %>% 
  spread(variable, grammaticality) %>% 
  as.data.frame() ->
  matrix_v

rownames(matrix_v) <- matrix_v$en_village
matrix_v <- matrix_v[,-1]

matrix_v %>% 
  dist() %>% 
  hclust() ->
  hc

plot(hc)

# cluster features
df %>%
  distinct() %>% 
  mutate(grammaticality = if_else(grammaticality == "+", 1, 0)) %>% 
  select(grammaticality, en_village, variable) %>% 
  spread(en_village, grammaticality) %>% 
  as.data.frame() ->
  matrix_f

rownames(matrix_f) <- matrix_f$variable
matrix_f <- matrix_f[,-1]

matrix_f %>% 
  dist() %>% 
  hclust() ->
  hc

# it doesn't work because of a big amount of NA, but probably it will works, if you select some features
