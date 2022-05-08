# Create graph of publications over time by cluster

library(tidyverse)
library(fuzzyjoin)
library(dplyr)
library(ggplot2)
library(here)

load("./data/LENS_dataframe_cleaned.RData") #loads record_df data object
record_df_data.pubyear <- record_df %>% select(data.lens_id, data.year_published)
record_df_data.pubyear <- rename(record_df_data.pubyear, lensID = data.lens_id, pubyear = data.year_published)

myclusters <- read.csv("./data/20220507_clusters-louv_lens_api.csv")
myclusters <- rename(myclusters, lensID = V1, cluster = V2)

myclusters_pubyear <- merge(myclusters, record_df_data.pubyear, by=c("lensID"))
View(myclusters_pubyear)

#Fix missing publication years 
myclusters_pubyear[57, 4] = 2010
myclusters_pubyear[83, 4] = 2004
myclusters_pubyear[88, 4] = 2012
myclusters_pubyear[90, 4] = 2009
myclusters_pubyear[143, 4] = 2006
myclusters_pubyear[189, 4] = 1997
myclusters_pubyear[272, 4] = 2015
myclusters_pubyear[328, 4] = 2004
myclusters_pubyear[406, 4] = 2000
myclusters_pubyear[436, 4] = 2011
myclusters_pubyear[524, 4] = 2013
myclusters_pubyear[526, 4] = 2008

png("./plots/rwbs_bibcouple_pubyear.png", 800, 600)
par(mar = rep(0, 4))
ggplot(myclusters_pubyear) + 
  geom_line(aes(x=pubyear, y=..count..), stat="bin", binwidth=1) +
  facet_wrap(~ cluster, ncol = 1) +
  labs(y = "Number of Publications") +
  scale_x_continuous(name = "Publication Year", limits=c(1960, 2020)) +
  theme_bw()
dev.off()

myclusters_pubyear_group <- myclusters_pubyear %>% group_by(cluster, pubyear) %>% 
  tally()

png("./plots/rwbs_bibcouple_pubyear.png", 800, 600)
par(mar = rep(0, 4))
ggplot(myclusters_pubyear_group) + 
  geom_line(aes(x=pubyear, y=n)) +
  facet_grid(vars(cluster)) +
  labs(y = "Number of Publications") +
  scale_x_continuous(name = "Publication Year", limits=c(1965, 2017), breaks=c(1965,1970,1975,1980,1985,1990,1995,2000,2005,2010,2015)) +
  theme_bw()
dev.off()

#Option using facet_wrap
png("./plots/rwbs_bibcouple_pubyear.png", 800, 600)
par(mar = rep(0, 4))
ggplot(myclusters_pubyear) + 
  geom_line(aes(x=pubyear, y=..count..), stat="bin", binwidth=1) +
  facet_wrap(~ cluster, ncol = 1) +
  labs(y = "Number of Publications") +
  scale_x_continuous(name = "Publication Year", limits=c(1960, 2020), breaks=c(1965,1970,1975,1980,1985,1990,1995,2000,2005,2010,2015)) +
  theme_bw()
dev.off()





