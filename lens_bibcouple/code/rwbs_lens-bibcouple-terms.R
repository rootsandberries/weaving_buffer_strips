#Analysis of term usage by Bibliographic Coupling Cluster Using Lens API data

#Install packages
#install.packages("fuzzyjoin")
#install.packages("here")

#Load libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(fuzzyjoin)
library(here)
library(wordcloud)
library("RColorBrewer")


#Import data
myclusters_api <- read.csv(here("data", "20220205_clusters-louv_lens_api.csv")) #Clusters identified by bibliographic coupling
myterms <- read.csv(here("data", "13750_2018_126_MOESM6_ESM-outcome.csv")) #Subset of original map data containing vegetated strips terms
dim(myterms)
names(myterms)

load("./data/LENS_dataframe.RData") #loads record_df data object--set of records found via Lens API
record_df_data.bib <- record_df %>% select(data.lens_id, data.title)
dim(record_df_data.bib)
names(record_df_data.bib)

#Merge cluster info to main dataset on Lens ID
myclusters_api <- rename(myclusters_api, lensID = V1, cluster = V2)
record_df_data.bib <- rename(record_df_data.bib, lensID = data.lens_id, Title = data.title)
myclusters_bibx <- merge(myclusters_api, record_df_data.bib, by=c("lensID"))

#Trim whitespace and make title lowercase in both cluster and terms datasets
myclusters_bibx$Title <- trimws(tolower(myclusters_bibx$Title))
myterms$Title <- trimws(tolower(myterms$Title))

#Remove duplicates from terms dataset
myterms_unique <- distinct(myterms, Title, .keep_all = TRUE)
dim(myterms_unique)
names(myterms_unique)

#Fuzzy join of clusters with terms dataset on title (adjust distance until total records are found)
mydata <- stringdist_inner_join(myclusters_bibx, myterms_unique, by = "Title", max_dist = 8,
                                distance_col = "distance")
dim(mydata)
names(mydata)

mydatadf <- as.data.frame(mydata)

mydatadf <- rename(mydatadf, vs_terms = Vegetated.strip.description.info)

#Split into multiple rows by title and term
mydata_sep <- separate_rows(mydatadf, vs_terms, sep=";")

#Keep only unique rows to remove duplicate terms
mydata_unique <- distinct_all(mydata_sep)

#Group by and tally outcomes by clusters
terms_by_cluster <- mydata_unique %>%
  group_by(vs_terms, cluster) %>%
  tally()

#Word Cloud visualizations

terms_1 <- subset(terms_by_cluster, cluster == 1)
terms_2 <- subset(terms_by_cluster, cluster == 2)
terms_3 <- subset(terms_by_cluster, cluster == 3)
terms_4 <- subset(terms_by_cluster, cluster == 4)

set.seed(1234) # for reproducibility 
wordcloud(words = terms_1$vs_terms, freq = terms_1$n, min.freq = 1, 
          max.words=200, random.order=FALSE, rot.per=0,            
          colors=brewer.pal(8, "Dark2"), scale=c(3.5,0.25))

set.seed(1234) # for reproducibility 
wordcloud(words = terms_2$vs_terms, freq = terms_2$n, min.freq = 1, 
          max.words=200, random.order=FALSE, rot.per=0,            
          colors=brewer.pal(8, "Dark2"), scale=c(3.5,0.25))

set.seed(1234) # for reproducibility 
wordcloud(words = terms_3$vs_terms, freq = terms_3$n, min.freq = 1, 
          max.words=200, random.order=FALSE, rot.per=0,            
          colors=brewer.pal(8, "Dark2"), scale=c(3.5,0.25))

set.seed(1234) # for reproducibility 
wordcloud(words = terms_4$vs_terms, freq = terms_4$n, min.freq = 1, 
          max.words=200, random.order=FALSE, rot.per=0,            
          colors=brewer.pal(8, "Dark2"), scale=c(3.5,0.25))

#Heatmap table of terms per cluster

library(reshape2)
library(scales)

terms_by_cluster <- subset(terms_by_cluster, n>2)

ggplot(terms_by_cluster, aes(x = reorder(vs_terms, desc(vs_terms)), cluster)) + # x and y axes => Var1 and Var2
  geom_tile(aes(fill = n), color = "gray") + # background colours are mapped according to the value column
  geom_text(aes(label = terms_by_cluster$n), size=2.5) + # write the values
  scale_fill_gradient2(low = muted("darkred"), 
                       mid = "white", 
                       high = muted("midnightblue"), 
                       midpoint = 0) + # determine the colour
  theme(panel.grid.major.x=element_blank(), #no gridlines
        panel.grid.minor.x=element_blank(), 
        panel.grid.major.y=element_blank(), 
        panel.grid.minor.y=element_blank(),
        panel.background=element_rect(fill="white"), # background=white
        axis.text.y = element_text(size = 10),
        axis.ticks = element_blank()) + 
  theme(legend.position="none") +
  coord_flip() +
  scale_x_discrete(name="", expand = c(0,0)) +
  scale_y_discrete(name="", expand = c(0,0))

#In case we need this to be a matrix

#term_mtx <- acast(terms_by_cluster, vs_terms~cluster, value.var="n")


