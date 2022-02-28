#Analysis of Outcome Measure by Bibliographic Coupling Cluster

#Install packages
#install.packages("fuzzyjoin")
#install.packages("here")

#Load libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(fuzzyjoin)
library(here)
library("RColorBrewer")


#Import data
myclusters <- read.csv(here("data", "20211021_rwbs_clusters-louv_lens.csv"))
mybibrecords <- read.csv(here("data", "20211020_rwbs_bibliometrix-export.csv"))
myoutcomes <- read.csv(here("data", "13750_2018_126_MOESM6_ESM-outcome.csv"))

#Merge cluster to main dataset on bibliometrix name
myclusters <- rename(myclusters, short_title = V1, cluster = V2)
mybibrecords <- rename(mybibrecords, short_title = X, Title = TI)
myclusters_bibx <- merge(myclusters, mybibrecords,by=c("short_title"))

#Trim whitespace and make title lowercase in both cluster and outcome datasets
myclusters_bibx$Title <- trimws(tolower(myclusters_bibx$Title))
myoutcomes$Title <- trimws(tolower(myoutcomes$Title))

#Remove duplicates from outcomes dataset
myoutcomes_unique <- distinct_at(myoutcomes, vars("Title"))

#Fuzzy join of clusters with outcome dataset on title (adjust distance until total records are found)
mydata <- stringdist_inner_join(myclusters_bibx, myoutcomes_unique, by = "Title", max_dist = 5,
                                distance_col = "distance")

mydata <- merge(myclusters_bibx, myoutcomes, by=c("Title"))
mydatadf <- as.data.frame(mydata)

#Split into multiple rows by title and outcome
mydata_sep <- separate_rows(mydatadf, Outcomes, sep=";")

#Keep only unique rows to remove duplicate outcomes
mydata_unique <- distinct_all(mydata_sep)

#Group by and tally outcomes by clusters
outcome_by_cluster <- mydata_unique %>%
  group_by(Outcomes, cluster) %>%
  tally() %>% filter(Outcomes!="Other")

plot <- ggplot(outcome_by_cluster[order(outcome_by_cluster$n, decreasing = T),], aes(x=cluster, y=n, fill=factor(Outcomes, levels=c("Soil Physical", "Soil Chemistry", "Pollution", "Recreation", "Social", "Human Use", "Ecosystem Functioning", "Biodiversity")))) + 
  geom_bar(position="fill", stat="identity") +
  labs(x = "Cluster", y = "", fill = "Functional Role") +
  scale_y_continuous(labels=scales::percent) +
  theme_bw(base_size = 12) +
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(legend.title = element_text(size = 12), legend.text = element_text(size = 10)) +
  scale_fill_manual(values = c("Biodiversity" = "#5AAE61", "Ecosystem Functioning" = "#A6DBA0", "Human Use" = "#ABD9E9", "Social" = "#74ADD1", "Recreation" = "#4575B4", "Pollution" = "#9970AB", "Soil Chemistry" = "#C2A5CF", "Soil Physical" = "#E7D4E8"))

ggsave(plot, filename = "rwbs_tole_plot.png", width = 7, height = 5, units = "in" )


#Analysis of Outcome Measure by Bibliographic Coupling Cluster Using Lens API data


#Load libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(fuzzyjoin)
library(here)


#Import data
myclusters_api <- read.csv(here("data", "20220205_clusters-louv_lens_api.csv"))
myoutcomes <- read.csv(here("data", "13750_2018_126_MOESM6_ESM-outcome.csv"))

load("./data/LENS_dataframe.RData") #loads record_df data object
record_df_data.bib <- record_df %>% select(data.lens_id, data.title)
dim(record_df_data.bib)
names(record_df_data.bib)

#Merge cluster to main dataset on Lens ID
myclusters_api <- rename(myclusters_api, lensID = V1, cluster = V2)
record_df_data.bib <- rename(record_df_data.bib, lensID = data.lens_id, Title = data.title)
myclusters_bibx <- merge(myclusters_api, record_df_data.bib, by=c("lensID"))

#Trim whitespace and make title lowercase in both cluster and outcome datasets
myclusters_bibx$Title <- trimws(tolower(myclusters_bibx$Title))
myoutcomes$Title <- trimws(tolower(myoutcomes$Title))

#Remove duplicates from outcomes dataset
myoutcomes_unique <- distinct(myoutcomes, Title, .keep_all = TRUE)

#Fuzzy join of clusters with outcome dataset on title (adjust distance until total records are found)
mydata <- stringdist_inner_join(myclusters_bibx, myoutcomes_unique, by = "Title", max_dist = 8,
                                distance_col = "distance")
dim(mydata)

mydatadf <- as.data.frame(mydata)

#Split into multiple rows by title and outcome
mydata_sep <- separate_rows(mydatadf, Outcomes, sep=";")

#Keep only unique rows to remove duplicate outcomes
mydata_unique <- distinct_all(mydata_sep)

#Group by and tally outcomes by clusters
outcome_by_cluster <- mydata_unique %>%
  group_by(Outcomes, cluster) %>%
  tally() %>% filter(Outcomes!="Other")

plot <- ggplot(outcome_by_cluster[order(outcome_by_cluster$n, decreasing = T),], aes(x=cluster, y=n, fill=factor(Outcomes, levels=c("Soil Physical", "Soil Chemistry", "Pollution", "Recreation", "Social", "Human Use", "Ecosystem Functioning", "Biodiversity")))) + 
  geom_bar(position="fill", stat="identity") +
  labs(x = "Cluster", y = "", fill = "Functional Role") +
  scale_y_continuous(labels=scales::percent) +
  theme_bw(base_size = 12) +
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(legend.title = element_text(size = 12), legend.text = element_text(size = 10)) +
  #scale_fill_brewer(palette="RdYlBu")
  scale_fill_manual(values = c("Biodiversity" = "#5AAE61", "Ecosystem Functioning" = "#A6DBA0", "Human Use" = "#ABD9E9", "Social" = "#74ADD1", "Recreation" = "#4575B4", "Pollution" = "#9970AB", "Soil Chemistry" = "#C2A5CF", "Soil Physical" = "#E7D4E8"))

ggsave(plot, filename = "rwbs_role_plot_api.png", width = 7, height = 5, units = "in" )


# Variable width column chart version

clust_prop <- mydata_unique %>% 
  group_by(cluster) %>%
  summarise(clust_prop = n()/nrow(mydata_unique))

mydata_unique = left_join(mydata_unique, clust_prop)

outcome_clust_prop <- mydata_unique %>% 
  count(cluster, Outcomes, clust_prop) %>% 
  group_by(cluster) %>% 
  mutate(freq = n / sum(n)) %>% 
  ungroup

plot <- ggplot(data = outcome_clust_prop[order(outcome_clust_prop$n, decreasing = T),], aes(x = cluster,  y = freq, width = clust_prop*2.2, fill = factor(Outcomes, levels=c("Soil Physical", "Soil Chemistry", "Pollution", "Recreation", "Social", "Human Use", "Ecosystem Functioning", "Biodiversity")))) +
  geom_bar(position="fill", stat = "identity") +
  theme_bw(base_size = 12) +
  labs(x = "Cluster", y = "", fill = "Functional Role") + 
  scale_y_continuous(labels=scales::percent, expand = c(0,0)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  #scale_x_continuous(expand = c(0,0)) +
  #theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))
  scale_fill_manual(values = c("Biodiversity" = "#5AAE61", "Ecosystem Functioning" = "#A6DBA0", "Human Use" = "#ABD9E9", "Social" = "#74ADD1", "Recreation" = "#4575B4", "Pollution" = "#9970AB", "Soil Chemistry" = "#C2A5CF", "Soil Physical" = "#E7D4E8"))

ggsave(plot, filename = "rwbs_role_plot_prop_api.png", width = 7, height = 5, units = "in" )

# Same width but with n on top

clust_n <- mydata_unique %>% 
  group_by(cluster) %>%
  summarise(clust_n = n())

mydata_unique = left_join(mydata_unique, clust_n)

outcome_clust_n <- mydata_unique %>% 
  count(cluster, Outcomes, clust_n) %>% 
  group_by(cluster) 

#Need to filter out Other and add N to top

plot <- ggplot(outcome_by_cluster[order(outcome_clust_n$n, decreasing = T),], aes(x=cluster, y=n, fill=factor(Outcomes, levels=c("Soil Physical", "Soil Chemistry", "Pollution", "Recreation", "Social", "Human Use", "Ecosystem Functioning", "Biodiversity")))) + 
  geom_bar(position="fill", stat="identity") +
  labs(x = "Cluster", y = "", fill = "Functional Role") +
  scale_y_continuous(labels=scales::percent) +
  theme_bw(base_size = 12) +
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(legend.title = element_text(size = 12), legend.text = element_text(size = 10)) +
  scale_fill_manual(values = c("Biodiversity" = "#5AAE61", "Ecosystem Functioning" = "#A6DBA0", "Human Use" = "#ABD9E9", "Social" = "#74ADD1", "Recreation" = "#4575B4", "Pollution" = "#9970AB", "Soil Chemistry" = "#C2A5CF", "Soil Physical" = "#E7D4E8"))


