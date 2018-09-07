library(tidyverse)
library(lingtypology)
df <- read_tsv("https://raw.githubusercontent.com/LingConLab/2018_fedorenko_ALS_Circassian_isoglosses/master/2018_field_data%20-%20elicitation.tsv")

df %>% 
  mutate(grammaticality = if_else(str_detect(sentence, "\\*"), "*", "+"),
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

df %>% 
  filter(feature == "бенефактивный послелог",
         variable == "ой пае") -> t
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


