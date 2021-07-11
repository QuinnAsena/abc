library(tidyverse) # For data wrangling and plotting
library(readxl) # to read in excel sheets
library(viridis)
library(vegan)
library(ggrepel)

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
         site_name = str_replace_all(site_name, "wreck_bay", "rangiwhakaea_bay"),
         year = 2020) %>%
  ungroup()


abc_2020_wide_summary_by_site <- abc_2020_long %>%
  group_by(year, site_name, spp) %>%
  summarise(total_count_per_spp = sum(total)) %>%
  pivot_wider(names_from = spp, values_from = total_count_per_spp, names_prefix = "spp_") %>%
  replace(is.na(.), 0) %>%
  ungroup() %>%
  mutate(
    divers_by_site = diversity(select(., starts_with("spp_")), "shannon", MARGIN = 1),
    rich_by_site = rowSums(select(., starts_with("spp_")) > 0)
  ) %>%
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
                               site_name %in% "C" ~ "kaitoke", # not sure here
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

unique(abc_2019_long$spp)
abc_2019_long <- abc_2019_long %>%
  mutate(spp = str_replace_all(spp, "DunNck", "Duck_sp"))

abc_19_20_long <- bind_rows(abc_2020_long, abc_2019_long)
head(abc_19_20_long)
tail(abc_19_20_long)


abc_2019_wide_summary_by_site <- abc_2019_long %>%
  group_by(year, site_name, spp) %>%
  summarise(total_count_per_spp = sum(total)) %>%
  pivot_wider(names_from = spp, values_from = total_count_per_spp, names_prefix = "spp_") %>%
  replace(is.na(.), 0) %>%
  ungroup() %>%
  mutate(
    divers_by_site = diversity(select(., starts_with("spp_")), "shannon", MARGIN = 1),
    rich_by_site = rowSums(select(., starts_with("spp_")) > 0)
  ) %>%
  ungroup()



abc_19_20_wide <- bind_rows(abc_2020_wide_summary_by_site, abc_2019_wide_summary_by_site) %>%
  mutate(site_label = str_to_title(str_replace_all(site_name, "_", " ")))



ggplot(abc_19_20_wide, aes(x = year, y = divers_by_site, group = site_name)) +
  geom_line(aes(colour = site_name)) +
  geom_point(aes(colour = site_name)) +
  scale_color_viridis(discrete = T) +
  geom_text_repel(aes(label = site_label)) +
  theme_minimal()







