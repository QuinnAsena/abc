library(tidyverse) # For data wrangling and plotting
library(readxl) # to read in excel sheets


abc_2020 <- read_excel("ABC2020/abc_master_2020.xlsx")

abc_2020_long <- abc_2020 %>%
  select(c(4, 5, 12, 19:ncol(.))) %>%
  rename_with(~ c("transect_point", "transect_set", "spp", "total",
                  "lat", "long", "alt", "gps_id", "site_name", "easting", 
                  "northing", "m_name",	"latin_name",	"euro_name", "cons_status",
                  "nz_cons_status")) %>%
  filter(!grepl("Unknown", spp)) %>%
  group_by(site_name, transect_set, transect_point, easting, northing, spp, nz_cons_status) %>%
  summarise(total = sum(total)) %>%
  mutate(spp = str_replace_all(spp, " ", "_"),
         year = 2020) %>%
  ungroup()


abc_2019 <- read_excel("ABC2020/ABC- Master data 2019.xlsx")

abc_2019_long <- abc_2019 %>% 
  select(c(2, 4, 5, 12, 19)) %>%
  rename_with(~ c("site_name", "transect_point", "transect_set", "spp", "total")) %>%
  filter(!grepl("Unknown", spp)) %>%
  group_by(site_name,transect_set, transect_point, spp) %>%
  summarise(total = sum(total)) %>%
  mutate(spp = str_replace_all(spp, " ", "_"),
         site_name = case_when(site_name %in% "A" ~ "awana", 
                               site_name %in% "C" ~ "claris", # not sure here
                               site_name %in% "CC" ~ "cooper_castle",
                               site_name %in% "G" ~ "glenfern",
                               site_name %in% "H" ~ "haratoanga",
                               site_name %in% "Me" ~ "medlands",
                               site_name %in% "Mo" ~ "motairehe",
                               site_name %in% "MY" ~ "mt_young",
                               site_name %in% "N" ~ "needle_rock", # not sure here
                               site_name %in% "Op" ~ "okupu",
                               site_name %in% "Ow" ~ "Okiwi",
                               site_name %in% "R" ~ "rakitu",
                               site_name %in% "T" ~ "rangitawhiri_tryphena", # not sure here
                               site_name %in% "TP" ~ "te_paparahi",
                               site_name %in% "W" ~ "whangaparapara",
                               site_name %in% "WH" ~ "windy_hill",
                               TRUE ~ site_name),
         year = 2019) %>%
  ungroup()

head(abc_2019_long)
head(abc_2020_long)

abc_19_20_long <- bind_rows(abc_2020_long, abc_2019_long)
head(abc_19_20_long)
tail(abc_19_20_long)







