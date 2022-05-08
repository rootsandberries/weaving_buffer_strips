library(here)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(fuzzyjoin)

install.packages("rworldmap")
library(rworldmap)


myclusters <- read.csv(here("data", "20220507_clusters-louv_lens_api.csv"))
mynations <- read.csv(here("data", "13750_2018_126_MOESM6_ESM-outcome.csv"))
load("./data/LENS_dataframe_cleaned.RData") #loads record_df data object
record_df_data.bib <- record_df %>% select(data.lens_id, data.title)

#Note: seem to be losing something on this merge
#Merge cluster to main dataset on Lens ID
myclusters <- rename(myclusters, lensID = V1, cluster = V2)
record_df_data.bib <- rename(record_df_data.bib, lensID = data.lens_id, Title = data.title)
myclusters_bibx <- merge(myclusters, record_df_data.bib, by=c("lensID"))

#Trim whitespace and make title lowercase in both cluster and outcome datasets
myclusters_bibx$Title <- trimws(tolower(myclusters_bibx$Title))
mynations$Title <- trimws(tolower(mynations$Title))

#Remove duplicates from outcomes dataset
mynations_unique <- distinct(mynations, Title, .keep_all = TRUE)

#Note: Need to check this to make sure matching is correct
#Fuzzy join of clusters with outcome dataset on title (adjust distance until total records are found)
mydata <- stringdist_inner_join(myclusters_bibx, mynations_unique, by = "Title", max_dist = 11,
                                distance_col = "distance")
dim(mydata)

#Group by and tally nations by clusters
nation_by_cluster <- mydata %>%
  group_by(Nation, cluster) %>%
  tally() %>% filter(Nation!="Multiple")



nations_3 <- subset(nation_by_cluster, cluster==3)
nations_4 <- subset(nation_by_cluster, cluster==4)

nations_1 <- subset(nation_by_cluster, cluster==1)
mapdata_1 <- joinCountryData2Map(nations_1
                               ,joinCode = "NAME"
                               ,nameJoinColumn = "Nation")

mapDevice()
mapCountryData(mapdata_1, nameColumnToPlot='n', catMethod="fixedWidth")

nations_2 <- subset(nation_by_cluster, cluster==2)
mapdata_2 <- joinCountryData2Map(nations_2
                               ,joinCode = "NAME"
                               ,nameJoinColumn = "Nation")

mapDevice()
mapCountryData(mapdata_2, nameColumnToPlot='n', catMethod="fixedWidth")

nations_3 <- subset(nation_by_cluster, cluster==3)
mapdata_3 <- joinCountryData2Map(nations_3
                               ,joinCode = "NAME"
                               ,nameJoinColumn = "Nation")

mapDevice()
mapCountryData(mapdata_3, nameColumnToPlot='n', catMethod="fixedWidth")


nations_4 <- subset(nation_by_cluster, cluster==4)
mapdata_4 <- joinCountryData2Map(nations_4
                             ,joinCode = "NAME"
                             ,nameJoinColumn = "Nation")

mapDevice()
mapCountryData(mapdata_4, nameColumnToPlot='n', catMethod="fixedWidth", colourPalette = "heat")

#Different map option with facet_wrap

library(ggplot2)
library(dplyr)
require(maps)
require(viridis)
theme_set(
  theme_void()
)

world_map <- map_data("world")
p <- ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill="lightgray", colour = "white")

nation_by_cluster_1 <- rename(nation_by_cluster, region = Nation)

world_map <- map_data("world")
nation.map <- left_join(world_map, nation_by_cluster_1, by = "region")

ggplot(nation.map_fixednas, aes(long, lat, group = group))+
  geom_polygon(aes(fill = n), color = "black", size = 0.05) +
  #scale_fill_viridis_c(option = "B") +
  scale_fill_continuous(low="#edf8e9", high="#006d2c", 
                            guide="colorbar", na.value="white") +
  facet_wrap(~cluster, drop=TRUE)

#Need to create a dataset for all NA values and have one row assigned to each cluster for borders to appear

nas_only <- nation.map[is.na(nation.map$cluster),]
write_csv(nas_only, "nas_only.csv")

no_nas <- nation.map[!is.na(nation.map$cluster),]
write_csv(no_nas, "no_nas.csv")

nation.map_fixednas <- read.csv("all_fixed_nas.csv")




