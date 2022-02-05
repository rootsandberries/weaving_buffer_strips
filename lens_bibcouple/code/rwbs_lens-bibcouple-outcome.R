#Analysis of Outcome Measure by Bibliographic Coupling Cluster

#Install packages
install.packages("fuzzyjoin")
install.packages("here")

#Load libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(fuzzyjoin)
library(here)


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
  tally()

plot <- ggplot(outcome_by_cluster[order(outcome_by_cluster$n, decreasing = T),], aes(x=cluster, y=n, fill=factor(Outcomes, levels=c("Other", "Soil Physical", "Soil Chemistry", "Pollution", "Recreation", "Social", "Human Use", "Ecosystem Functioning", "Biodiversity")))) + 
  geom_bar(position="fill", stat="identity") +
  labs(x = "Cluster", y = "", fill = "Outcome") +
  coord_flip() +
  scale_y_continuous(labels=scales::percent) +
  #scale_x_discrete(labels = wrap_format(13)) +
  theme_bw(base_size = 12) +
  theme(axis.text.x = element_text(angle = 65, size = 12), axis.text.y = element_text(size = 12)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(legend.title = element_text(size = 12), legend.text = element_text(size = 10)) #+
  #scale_fill_manual(values = c("Biodiversity" = "darkseagreen", "Ecosystem Functioning" = "darkolivegreen4", "Human Use" = "darkslategray3", "Social" = "deepskyblue2", "Recreation" = "dodgerblue4", "Pollution" = "darkorange", "Soil Chemistry" = "burlywood3", "Soil Physical" = "chocolate4", "Other" = "gray"))

ggsave(plot, filename = "rwbs_outcome_plot.png", width = 7, height = 5, units = "in" )







