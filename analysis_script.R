library(tidyverse) # For data wrangling and plotting
library(readxl) # to read in excel sheets
library(Manu) # for pretty native bird colours on the plots
library(ggthemes) # more themes
library(vegan) # For the vegdist function
library(ggspatial)
library(sf) # For mapping
library(ggrepel) # For text on GBI map
library(cowplot) # For inset graph on GBI map
library(patchwork) # For simple patching together graphs (can replace with cow)
library(dendextend) # For coloured dendrogram branches
library(factoextra) # For determining the number of clusters
library(NbClust) # Also for determining the number of clusters

# library(ggdendro) 
# library(raster) # select function clash with dplyr
# library(spData)
# library(spDataLarge)
# library(tmap)    # for static and interactive maps
# library(leaflet) # for interactive maps
# library(ggmap)


# Data wrangling ----------------------------------------------------------

abc_2020 <- read_excel("ABC2020/abc_master_2020.xlsx")
head(abc_2020) # ST (start time) incorrect structure but dropped from DF
unique(abc_2020$Species) # Check names for errors
unique(abc_2020$cons_status) # Check names for errors
sum(!grepl("Unknown", unique(abc_2020$Species))) # How many spp identified

unique(abc_2020$Species)[grepl("Unknown", unique(abc_2020$Species))]

abc_2020$Species[grepl("Plover", abc_2020$Species)]
length(abc_2020$Species[grepl("Rail", abc_2020$Species)])

### How many native/endemic spp
abc_2020 %>% 
  select(Species, cons_status, nz_cons_status) %>% 
  filter(nz_cons_status %in% c("Native", "Endemic")) %>% 
  distinct(Species, .keep_all = T) %>% 
  mutate(native_endemic =)

### Create a long dataset of spp per transect point
## some unnecessary code left over
abc_2020_long <- abc_2020 %>%
  select(c(4, 5, 12, 19:ncol(.))) %>%
  rename_with(~ c("transect_point", "transect_set", "spp", "total",
                  "lat", "long", "alt", "gps_id", "site_name", "easting", 
                  "northing", "m_name",	"latin_name",	"euro_name", "cons_status",
                  "nz_cons_status")) %>%
  filter(!grepl("Unknown", spp)) %>%
  group_by(site_name, transect_set, transect_point, easting, northing, spp, nz_cons_status) %>%
  summarise(total = sum(total)) %>%
  mutate(spp = str_replace_all(spp, " ", "_")) %>%
  ungroup()


### Check out some simple summaries
## Checking for unevenness in numer of transect surveyed or points per transect
abc_2020_long_summary_by_site <- abc_2020_long %>%
  group_by(site_name) %>%
  summarise(
    no_transects = sum(length(unique(transect_set))), 
    no_transect_points = sum(length(unique(transect_point))),
    no_spp = sum(length(unique(spp))),
    total_count = sum(total)
  ) %>%
  ungroup()


### Create a summary table for report
## in markdown could use flextable but copying into word for report
abc_2020_long_summary_by_site_table <- abc_2020_long_summary_by_site %>% 
  select(site_name, no_spp, total_count) %>%
  mutate(site_name = str_to_title(str_replace_all(site_name, "_", " ")),
         site_name = if_else(site_name %in% "Cooper Castle", "Cooper's Castle", as.character(site_name))) %>% 
  add_row(site_name = "Total", no_spp = NA, total_count = sum(.[,3])) %>%
  rename_with(~ c("Site", "Number of species", "Total count"))

# write.csv(abc_2020_long_summary_by_site_table, file = "maps_imgs/summary_by_site_table.csv", row.names = F) 


### Create a wide dataset to calculate richness and diversity
## Diversity and richness calculated per transect point
# Only used for map later on
abc_2020_wide <- abc_2020_long %>%
  pivot_wider(names_from = spp, values_from = total, names_prefix = "spp_") %>%
  replace(is.na(.), 0) %>%
  mutate(
    divers_by_point = diversity(select(., starts_with("spp_")), "shannon", MARGIN = 1),
    rich_by_point = rowSums(select(., starts_with("spp_")) > 0)
  )

abc_2020_wide_summary_by_point <- abc_2020_wide %>%
  group_by(site_name) %>%
  summarise(
    no_transects = sum(length(unique(transect_set))), # number of transects surveyed
    no_transect_points = sum(length(unique(transect_point))),
    # ave_divers_by_point = mean(divers_by_point),
    # ave_rich_by_point = mean(rich_by_point),
    ave_easting = mean(easting),
    ave_northing = mean(northing)
  ) %>%
  ungroup()


### Diversity and richness calculated per site
## per site is used throughout
abc_2020_wide_summary_by_site <- abc_2020_long %>%
  group_by(site_name, spp) %>%
  summarise(total_count_per_spp = sum(total)) %>%
  pivot_wider(names_from = spp, values_from = total_count_per_spp, names_prefix = "spp_") %>%
  replace(is.na(.), 0) %>%
  ungroup() %>%
  mutate(
    divers_by_site = diversity(select(., starts_with("spp_")), "shannon", MARGIN = 1),
    rich_by_site = rowSums(select(., starts_with("spp_")) > 0)
  ) %>%
  ungroup()

# Smaller df with ave location per site for maps
abc_2020_wide_summary <- left_join(
  abc_2020_wide_summary_by_point,
  abc_2020_wide_summary_by_site %>% select(site_name, rich_by_site, divers_by_site),
  by = "site_name"
) %>% 
  left_join(abc_2020_long_summary_by_site %>% select(site_name, total_count),
            by = "site_name"
)


### Replacement for previous summary table by site
abc_2020_wide_summary_table <- abc_2020_wide_summary %>% 
  select(site_name, rich_by_site, divers_by_site, total_count)  %>% 
  mutate(site_name = str_to_title(str_replace_all(site_name, "_", " ")),
                     site_name = if_else(site_name %in% "Cooper Castle", "Cooper's Castle", as.character(site_name))) %>% 
  add_row(site_name = "Total", rich_by_site = NA, divers_by_site = NA, total_count = sum(.[,4])) %>%
  rename_with(~ c("Site", "Species richness", "Species diversity", "Total count"))

write.csv(abc_2020_wide_summary_table, file = "maps_imgs/summary_by_site_table.csv", row.names = F) 


### Create a summary table of target spp for report
##
abc_2020_key_spp_table <- abc_2020_wide_summary_by_site %>% 
  select(site_name, spp_Tui, spp_Kaka, spp_Kakariki, spp_Kereru) %>%
  mutate(site_name = str_to_title(str_replace_all(site_name, "_", " ")),
         site_name = if_else(site_name %in% "Cooper Castle", "Cooper's Castle", as.character(site_name))) %>% 
    rename_with(~ c("Site", "Tui", "Kaka", "Kakariki", "Kereru"))

write.csv(abc_2020_key_spp_table, file = "maps_imgs/key_spp_table.csv", row.names = F) 


### Create a table of names and status for report
##
names_table <- abc_2020 %>%
  select(c(m_name, latin_name, euro_name, ends_with("status"))) %>% 
  filter(!grepl("Unknown", latin_name),
         !latin_name %in% c("Chicken", "Rooster")) %>%
  mutate(m_name = ifelse(m_name == euro_name, NA, as.character(m_name))) %>% 
  distinct(latin_name, .keep_all = T) %>% 
  arrange(m_name, ) %>% 
  replace_na(list(m_name = "-")) %>% 
  rename_with(~ c("Maori name", "Latin name", "European name",
                  "Conservation status", "NZ conservation status"))

write.csv(names_table, file = "maps_imgs/names_table.csv", row.names = F) 



# Bar plot top 25 spp -----------------------------------------------------

#### Create a dataset grouped by species.
### Only necessary so that the bar plot plots in decending order.
## If using abc_2020_long, bar chart either ordered alphabetically, 
# or by number of rows per spp not the total column
# More rows for Tui but fewer observations than for kaka.
abc_total <- abc_2020_long %>%
  group_by(spp, nz_cons_status) %>%
  summarise(
    stdev = sd(total), # Error bars unnecessary but used early on
    total = sum(total)
  ) %>%
  arrange(desc(total)) %>%
  filter(!grepl("Unknown", spp)) %>%
  # slice(1:25) %>%
  mutate(spp = str_replace_all(spp, "_", " ")) %>% 
  ungroup()


top_25_bar <- ggplot(abc_total %>% slice(1:25), aes(x = fct_inorder(spp), y = total)) +
  geom_bar(stat = "identity", fill = get_pal("Kotare")[1]) +
  # geom_errorbar(aes(ymin = total - stdev, ymax = total + stdev),
  #               width = .2) +
  labs(x = "Species", y = "Count") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 45),
    plot.margin = margin(0, 20, 0, 0)
  )

# ggsave(top_25_bar, filename = "maps_imgs/top_25_bar.pdf", device = "pdf", width = 6, height = 6)
ggsave(top_25_bar, filename = "maps_imgs/top_25_bar.png", device = "png", dpi = 600, width = 6, height = 6)


top_25_bar_fill_status <- ggplot(abc_total %>% slice(1:25),
                     aes(x = fct_inorder(spp), y = total, fill = nz_cons_status)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = get_pal("Kotare")[1:4]) +
  # geom_errorbar(aes(ymin = total - stdev, ymax = total + stdev),
  #               width = .2) +
  labs(x = "Species", y = "Count", fill = "NZ conservation status") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 45),
    plot.margin = margin(0, 20, 0, 0),
    legend.position = "bottom"
  )


# ggsave(top_25_bar_fill_status, filename = "maps_imgs/top_25_bar_fill_status.pdf", device = "pdf", width = 6, height = 6)
ggsave(top_25_bar_fill_status, filename = "maps_imgs/top_25_bar_fill_status.png", device = "png", dpi = 600, width = 6, height = 6)


# Bar plot of all spp (unknown filtered out) ------------------------------

all_spp_bar <- ggplot(abc_total, aes(x = fct_inorder(spp), y = total)) +
  geom_bar(stat = "identity", fill = get_pal("Kotare")[1]) +
  labs(x = "Species", y = "Count") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 45),
    plot.margin = margin(0, 20, 0, 0)
  )

# ggsave(all_spp_bar, filename = "maps_imgs/all_spp_bar.pdf", device = "pdf", width = 9, height = 7)
ggsave(all_spp_bar, filename = "maps_imgs/all_spp_bar.png", device = "png", dpi = 600, width = 9, height = 7)


all_spp_bar_fill_status <- ggplot(abc_total, aes(x = fct_inorder(spp), y = total, fill = nz_cons_status)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = get_pal("Kotare")[1:4]) +
  labs(x = "Species", y = "Count", fill = "NZ conservation status") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 45),
    plot.margin = margin(0, 20, 0, 0),
    legend.position = "bottom"
  )

# ggsave(all_spp_bar_fill_status, filename = "maps_imgs/all_spp_bar_fill_status.pdf", device = "pdf", width = 9, height = 7)
ggsave(all_spp_bar_fill_status, filename = "maps_imgs/all_spp_bar_fill_status.png", device = "png", dpi = 600, width = 9, height = 7)




# GBI map -----------------------------------------------------------------
### https://r-spatial.github.io/sf/articles/sf1.html
nz_shp <- st_read("gps/coastIslandsPoly50/nz-coastlines-and-islands-polygons-topo-150k.shp")
gbi_shp <- st_crop(nz_shp, c(xmin = 1797288.958, xmax = 1843372.480,
                             ymin = 5969435.039, ymax = 6012678.714))
# gbi_shp <- st_crop(nz_shp, c(xmin = 175.2, xmax = 175.7, ymin = -36.4, ymax = -35.9))
# gbi_shp <- st_crop(nz_shp, c(xmin = 1690128.928, xmax = 1863421.282, ymin = 5789936.153, ymax = 6015588.319))

###
map_lables <- abc_2020_wide_summary %>%
  select(site_name, ave_easting, ave_northing) %>%
  ungroup() %>%
  mutate(
    site_name = str_to_title(str_replace_all(site_name, "_", " ")),
    site_name = if_else(site_name %in% "Cooper Castle", "Cooper's Castle", as.character(site_name)),
    x_nudge = c(-5, 3, -10, -5, -2, -2, 5, 0, 0, 0, -3, 0, 0, -3, -3, -10),
    y_nudge = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  )


# GBI map
# https://r-spatial.org/r/2018/10/25/ggplot2-sf-2.html
gbi_map <- ggplot(gbi_shp) +
  geom_sf(fill = get_pal("Kaka")[5], alpha = 0.5, colour = "NA") +
  geom_point(
    data = map_lables,
    aes(x = ave_easting, y = ave_northing),
    colour = get_pal("Tui")[2],
    size = 3
  ) +
  # geom_text(data = abc_2020_wide %>% group_by(site_name) %>% slice(1), aes(x = easting, y = northing, label = str_to_title(str_replace_all(site_name, "_", " ")) ), fontface = "bold")
  geom_text_repel(
    data = map_lables, aes(x = ave_easting, y = ave_northing, label = site_name),
    fontface = "bold", force = 30, min.segment.length = 0.2, size = 3
    # nudge_x = map_lables$x_nudge, nudge_y = map_lables$y_nudge,
  ) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(
    location = "bl", which_north = "true",
    pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
    style = north_arrow_fancy_orienteering
  ) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_minimal()


# Inset map
nz_shp_crop <- st_crop(nz_shp, c(xmin = 1089354, xmax = 2301624.600,
                                 ymin = 4742396.719, ymax = 6731624))

nz_shp_crop_north_island <- st_crop(nz_shp, c(xmin = 1517181.853, xmax = 2247040.483,
                                              ymin = 5349739.292, ymax = 6215674.780)) # coords used in lat long 172.0 -42.0, 180.0 -34.0

gbi_bbox <- st_as_sfc(st_bbox(c(xmin = 1797288.958, xmax = 1843372.480,
                                ymin = 5969435.039, ymax = 6012678.714),
                                crs = 2193))

nz_inset <- ggplot(nz_shp_crop_north_island) +
  geom_sf(fill = get_pal("Kaka")[5]) +
  geom_sf(data = gbi_bbox, fill = NA, color = get_pal("Kakariki")[4], size = 1.2) +
  theme_void() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
    panel.background = element_rect(fill = "white")
  )


gbi_nz <- ggdraw() +
  draw_plot(gbi_map) +
  draw_plot(nz_inset, x = 0.63, y = 0.635, width = 0.3, height = 0.3)


# ggsave(gbi_nz, filename = "maps_imgs/gbi_inset.pdf", device = "pdf")
ggsave(gbi_nz, filename = "maps_imgs/gbi_inset.png", device = "png", dpi = 600, width = 8, height = 6)



# GBI diversity and richness maps -----------------------------------------
# also tried facet plot using the pivot and facet code commented out
###  Scatter-plot visualisation
ggplot(abc_2020_wide_summary, aes(x = rich_by_site, y = divers_by_site, label = site_name)) +
  geom_point() +
  geom_text(hjust = 0, nudge_x = 0.4) +
  theme_minimal()

### Could group sites within 1 SD and colour by within/without 1 SD
gbi_divers_map <- ggplot(gbi_shp) +
  geom_sf(fill = get_pal("Kaka")[5], alpha = 0.2) +
  geom_point(
    data = abc_2020_wide_summary,
    aes(x = ave_easting, y = ave_northing, size = divers_by_site),
    colour = get_pal("Tui")[2]
  ) +
  # geom_point(data = abc_2020_wide_ave_divers_rich, aes(x = ave_east, y = ave_north, size = rich_by_site ), colour = get_pal("Tui")[5]) +
  # facet_wrap(~divers_rich) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(
    location = "bl", which_north = "true",
    pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
    style = north_arrow_fancy_orienteering
  ) +
  labs(x = "Longitude", y = "Latitude", size = "Site diversity", title = "B") +
  theme_minimal() +
  theme(legend.position = "bottom")



gbi_rich_map <- ggplot(gbi_shp) +
  geom_sf(fill = get_pal("Kaka")[5], alpha = 0.2) +
  geom_point(
    data = abc_2020_wide_summary,
    aes(x = ave_easting, y = ave_northing, size = rich_by_site),
    colour = get_pal("Tui")[5]
    # colour = "black", fill = get_pal("Tui")[5], shape = 21, alpha = 0.8
  ) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(
    location = "bl", which_north = "true",
    pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
    style = north_arrow_fancy_orienteering
  ) +
  labs(x = "Longitude", y = "Latitude", size = "Site richness", title = "A") +
  theme_minimal() +
  theme(legend.position = "bottom")

gbi_richness_diversity_map <- gbi_rich_map + gbi_divers_map

# ggsave(gbi_richness_diversity_map, filename = "maps_imgs/gbi_richness_diversity_map.pdf", device = "pdf", width = 10)
ggsave(gbi_richness_diversity_map, filename = "maps_imgs/gbi_richness_diversity_map.png", device = "png", dpi = 600, width = 10)




# Target spp and total abundance map --------------------------------------


abc_2020_long_target_spp <- abc_2020_long %>%
  group_by(site_name, spp) %>%
  summarise(
    total = sum(total),
    ave_east = mean(easting),
    ave_north = mean(northing)
  ) %>%
  filter(spp %in% c("Tui", "Kaka", "Kereru", "Kakariki"))



gbi_target_spp_map <- ggplot(gbi_shp) +
  geom_sf(fill = get_pal("Kaka")[5], alpha = 0.2) +
  # geom_jitter(data = abc_2020_long_target_spp, aes(x = ave_east, y = ave_north, colour = spp, size = total, stroke = 2), width = 700, height = 800) +
  geom_jitter(
    data = abc_2020_long_target_spp, aes(x = ave_east, y = ave_north, fill = spp, size = total),
    colour = "black", shape = 21, width = 700, height = 700, alpha = 0.8
  ) +
  # scale_colour_manual(values = get_pal("Takahe")[1:4]) +
  scale_fill_manual(values = c(
    get_pal("Kaka")[1],
    get_pal("Kakariki")[1],
    get_pal("Kereru")[2],
    get_pal("Tui")[2]
  )) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(
    location = "bl", which_north = "true",
    pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
    style = north_arrow_fancy_orienteering
  ) +
  labs(x = "Longitude", y = "Latitude", fill = "Species", size = "Count", title = "B") +
  guides(fill = guide_legend(override.aes = list(size = 5, alpha = 1))) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.box = "vertical")

# ggsave(gbi_target_spp_map, filename = "gbi_target_spp_map.pdf", device = 'pdf')


#
abc_2020_long_count <- abc_2020_long %>%
  group_by(site_name) %>%
  summarise(
    total = sum(total),
    ave_east = mean(easting),
    ave_north = mean(northing)
  )


gbi_total_count_map <- ggplot(gbi_shp) +
  geom_sf(fill = get_pal("Kaka")[5], alpha = 0.2) +
  # geom_jitter(data = abc_2020_long_target_spp, aes(x = ave_east, y = ave_north, colour = spp, size = total, stroke = 2), width = 700, height = 800) +
  geom_point(
    data = abc_2020_long_count,
    aes(x = ave_east, y = ave_north, size = total),
    colour = "black",
    fill = get_pal("Takahe")[3],
    shape = 21,
    alpha = 0.8
  ) +
  # scale_colour_manual(values = get_pal("Takahe")[1:4]) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(
    location = "bl", which_north = "true",
    pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
    style = north_arrow_fancy_orienteering
  ) +
  labs(x = "Longitude", y = "Latitude", fill = "Species", size = "Count", title = "A") +
  guides(fill = guide_legend(override.aes = list(size = 3, alpha = 1))) +
  theme_minimal() +
  theme(legend.position = "bottom")


total_target_counts_map <- gbi_total_count_map + gbi_target_spp_map

# ggsave(total_target_counts_map, filename = "maps_imgs/total_target_counts_map.pdf", device = "pdf", width = 10)
ggsave(total_target_counts_map, filename = "maps_imgs/total_target_counts_map.png", device = "png", width = 10, dpi = 600)




# NMDS and dendro ---------------------------------------------------------

### vegdist dendro
## https://uc-r.github.io/hc_clustering

abc_2020_wide_nmds <- abc_2020_wide_summary_by_site %>%
  mutate(site_name = str_to_title(str_replace_all(site_name, "_", " ")),
         site_name = if_else(site_name %in% "Cooper Castle", "Cooper's Castle", as.character(site_name))) %>%
  select(site_name, starts_with("spp")) %>%
  column_to_rownames("site_name") %>%
  as.matrix()


bray_dist <- vegdist(abc_2020_wide_nmds, method = "bray")
bray_dist_clust <- hclust(bray_dist, method =  "average")
# plot(bray_dist_clust)
# rect.hclust(bray_dist_clust, k = 3, border = 2:5)
# plot(hclust(vegdist(t(abc_2020_wide_nmds), method = "bray")))

# pdf("maps_imgs/site_dendro.pdf")
png("maps_imgs/site_dendro.png", height = 6, width = 6, units="in", res = 600)
par(mar=c(9,4.2,3,3))
plot(bray_dist_clust %>% colour_branches(3),
     ylab = "Bray-Curtis distance")
dev.off()


# https://www.r-bloggers.com/2019/01/10-tips-for-choosing-the-optimal-number-of-clusters/
# https://www.rdocumentation.org/packages/NbClust/versions/3.0/topics/NbClust
library(NbClust)
n_clust <- NbClust(data = NULL, diss = bray_dist, distance = NULL, min.nc = 3, 
                   max.nc = 4, method = "average", index = "silhouette")
n_clust$Best.nc

# https://www.rdocumentation.org/packages/factoextra/versions/1.0.7/topics/fviz_nbclust
library(factoextra)
fviz_nbclust(x = as.matrix(bray_dist), FUNcluster = hcut, diss = bray_dist, method = "wss", k.max = 15) + 
  theme_minimal() + 
  ggtitle("The Silhouette Plot")


### ggdendro not used, I wanted coloured branches...
# site_dendro <- ggdendrogram(bray_dist_clust, theme_dendro = F) +
#   labs(x = "", y = "Bray-Curtis distance") +
#   theme_classic() +
#   theme(axis.line.x = element_blank())
# 
# ggsave(site_dendro, filename = "maps_imgs/site_dendro.pdf", device = "pdf")

### Checking richness with rarefaction see:
## https://cran.r-project.org/web/packages/vegan/vignettes/diversity-vegan.pdf
# Probably unnecessary in this context
rarefy(abc_2020_wide_nmds, min(rowSums(abc_2020_wide_nmds)))


# NMDS, incomplete. Decided dendro more appropriate for audience
# https://jonlefcheck.net/2012/10/24/nmds-tutorial-in-r/
# https://websites.pmc.ucsc.edu/~mclapham/Rtips/cluster.htm
# https://peat-clark.github.io/BIO381/veganTutorial.html
# https://cougrstats.wordpress.com/2019/12/11/non-metric-multidimensional-scaling-nmds-in-r/

# NMDS not used partially coded
# # scale matrix for nmds?
# 
# nmds_results <- metaMDS(
#   comm = abc_2020_wide_nmds, # Define the community data
#   distance = "bray", # Specify a bray-curtis distance
#   try = 100,
#   autotransform = F
# ) # Number of iterations
# 
# site_scores <- as.data.frame(scores(nmds_results)) %>%
#   rownames_to_column("site_name") %>%
#   as_tibble()
# 
# species_scores <- as.data.frame(scores(nmds_results, "species")) %>%
#   rownames_to_column("species") %>%
#   as_tibble()
# 
# 
# 
# ggplot() +
#   geom_text_repel(
#     data = species_scores, aes(x = NMDS1, y = NMDS2, label = species),
#     alpha = 0.5
#   ) +
#   geom_point(data = site_scores, aes(
#     x = NMDS1, y = NMDS2,
#     color = site_name
#   ), size = 3) +
#   scale_color_viridis(discrete = T) +
#   annotate(
#     geom = "label", x = -1, y = 1.25,
#     label = paste("Stress: ", round(nmds_results$stress, digits = 3))
#   ) +
#   theme_minimal()
# 

## ok - if i were doing this i would pick a few key species (eg kereru, tui) and
## scale the point size by their abundance or presence somehow.
## i think there's way too many species for the colours to make sense.
# 
# ggplot() +
#   geom_text_repel(
#     data = site_scores, aes(x = NMDS1, y = NMDS2, label = site_name),
#     alpha = 0.5
#   ) +
#   geom_point(data = species_scores, aes(
#     x = NMDS1, y = NMDS2,
#     color = species
#   ), size = 3) +
#   scale_color_viridis(discrete = T) +
#   annotate(
#     geom = "label", x = -1, y = 1.25,
#     label = paste("Stress: ", round(nmds_results$stress, digits = 3))
#   ) +
#   theme_minimal()




# Additional interactive maps: --------------------------------------------
# Not used
# add markers no longer working since conversion to easting/northing
# Can use lag lon cols
# https://cengel.github.io/R-spatial/mapping.html#choropleth-mapping-with-ggplot2
# https://ggplot2-book.org/maps.html
# https://bookdown.org/lexcomber/brunsdoncomber2e/Ch5.html#spatial-intersection-and-clip-operations
# https://geocompr.robinlovelace.net/adv-map.html
# https://geocompr.github.io/post/2019/ggplot2-inset-maps/

m <- leaflet() %>%
  setView(lng = 175.39079332, lat = -36.20271829, zoom = 11) %>%
  addTiles() %>%
  addMarkers(~long, ~lat, popup = ~ as.character(site_name), label = ~ as.character(site_name), data = abc_2020_wide)


# tmap
tmap_mode("view")

tm_shape(nz_shp_crop_north_island) +
  tm_fill() +
  tm_borders()

tmap_mode("plot")
