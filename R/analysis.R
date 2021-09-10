##############################################################################################
##  Cluster analysis
##############################################################################################

library(tidyverse)
library(data.table)
library(factoextra)
agg <- function(x) { x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x));  return(x) }

#-------------------------------------------------------------------------
# Make intitial settings
#-------------------------------------------------------------------------
regions <- fread(file="/mnt/nfs_fineprint/tmp/fabio/v2/regions.csv")
items <- fread(file="/mnt/nfs_fineprint/tmp/fabio/v2/items.csv")
years <- c(1987, 2012)
year <- years[2]


results <- read_csv(paste0("output/results_",year,".csv"))
results <- results %>% 
  filter(group != "nonfood") %>% 
  filter(iso3c != "TWN") %>% 
  mutate(ghg = round(ghg / `Population, total` * 1000000, 0),  # convert into kg
         luh2 = round(luh2 / `Population, total` * 1000000, 0), # convert into kg
         water = round(water / `Population, total`, 0),  # keep it in m3
         cropland = round(cropland / `Population, total` * 10000, 0), # convert into m2
         grazing = round(grazing / `Population, total` * 10000, 0), # convert into m2
         consumption = round(consumption / `Population, total` * 1000, 0), # convert into kg
         gdp = round(`GDP (current US$)` / `Population, total`, 0), # keep US$
         gdp_ppp = round(`GDP, PPP (current international $)` / `Population, total`, 0), # keep US$
         urban = round(`Urban population (% of total population)`),
         `GDP (current US$)` = NULL, `GDP, PPP (current international $)` = NULL, 
         `Population, total` = NULL, `Urban population (% of total population)` = NULL)



##############################################################
# Cluster dendrogram of meat footprints
##############################################################

data <- results %>% 
  filter(group == "meat") %>% 
  select(iso3c, ghg, water, cropland) %>% 
  as.data.frame()

rownames(data) <- data$iso3c

dfs <- scale(data[, -1])

res <- dist(dfs, method = "euclidean")

res_hc <- hclust(res, method = "complete")
res_hc2 <- hclust(res, method = "average")

fviz_dend(res_hc, cex = 0.5)
fviz_dend(res_hc2, cex = 0.5)



##############################################################
# Cluster dendrogram of diets
##############################################################

data <- results %>% 
  select(iso3c, group, consumption) %>% 
  spread(group, consumption) %>% 
  as.data.frame()

rownames(data) <- data$iso3c

dfs <- scale(data[, -1])

res <- dist(dfs, method = "euclidean")

res_hc <- hclust(res, method = "complete")
res_hc2 <- hclust(res, method = "average")

fviz_dend(res_hc, cex = 0.5)
fviz_dend(res_hc2, cex = 0.5)



##############################################################
# K-MEANS clustering of diets
##############################################################

data <- results %>% 
  select(iso3c, group, consumption) %>% 
  spread(group, consumption) %>% 
  as.data.frame()

rownames(data) <- data$iso3c

dfs <- scale(data[, -1])
# dfs$continent <- regions$continent[match(rownames(dfs), regions$iso3c)]

fviz_nbclust(dfs, kmeans, method = "wss")
fviz_nbclust(dfs, kmeans, method = "silhouette")

km <- kmeans(dfs, 6, nstart = 25)

fviz_cluster(km, dfs, ellipse.type = "convex", ggtheme = theme_minimal())

aggregate(data[, -1], by = list(cluster = km$cluster), mean) %>% round() %>% t()


# plot world map

library("ggplot2")
theme_set(theme_bw())
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
world$cluster <- km$cluster[match(world$iso_a3, names(km$cluster))]

ggplot(data = world) +
  geom_sf(aes(fill = cluster)) +
  scale_fill_viridis_c(option = "viridis")



##############################################################
# K-MEANS clustering of meat footprints
##############################################################

data <- results %>% 
  filter(group == "meat") %>% 
  select(iso3c, ghg, water, cropland) %>% 
  as.data.frame()

rownames(data) <- data$iso3c

dfs <- scale(data[, -1])
# dfs$continent <- regions$continent[match(rownames(dfs), regions$iso3c)]

fviz_nbclust(dfs, kmeans, method = "wss")
fviz_nbclust(dfs, kmeans, method = "silhouette")

km <- kmeans(dfs, 5, nstart = 25)

fviz_cluster(km, dfs, ellipse.type = "convex", ggtheme = theme_minimal())

aggregate(data[, -1], by = list(cluster = km$cluster), mean) %>% round() %>% t()


# plot world map

library("ggplot2")
theme_set(theme_bw())
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
world$cluster <- km$cluster[match(world$iso_a3, names(km$cluster))]

ggplot(data = world) +
  geom_sf(aes(fill = cluster)) +
  scale_fill_viridis_c(option = "viridis")
