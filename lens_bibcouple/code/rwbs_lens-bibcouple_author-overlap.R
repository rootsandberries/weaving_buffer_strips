#Analysis of author overlap between by Bibliographic Coupling Clusters Using Lens API data

#Load libraries
library(tidyverse)
library(dplyr)
library(eulerr)
library(here)
library("RColorBrewer")


#Import data
myclusters_api <- read.csv(here("data", "20220205_clusters-louv_lens_api.csv")) #Clusters identified by bibliographic coupling

load("./data/LENS_dataframe.RData") #loads record_df data object--set of records found via Lens API

#Unnest author data
record_df_data.authors <- record_df %>% 
  select(data.lens_id, data.title, data.authors) %>% 
  unnest(data.authors)
dim(record_df_data.authors)
names(record_df_data.authors)

#Add a new column with author name made of last name and initials
record_df_data.authors$Author <- paste(record_df_data.authors$last_name, record_df_data.authors$initials, sep=", ")
#Need to use cleaned author data evenutually, but for testing purposes this should be okay

#Collapse Author column by ID with semi-colon separator
library(plyr)
df <- ddply(record_df_data.authors, .(data.lens_id), summarise, Authors = paste(Author, collapse = ";"))


#Merge Author names to cluster dataset on Lens ID
myclusters_api <- dplyr::rename(myclusters_api, lensID = V1, cluster = V2)
df <- dplyr::rename(df, lensID = data.lens_id)
myclusters_authors <- merge(myclusters_api, df, by=c("lensID"))
myclusters_authors <- subset(myclusters_authors, select = -c(X, lensID))
names(myclusters_authors)

#Split rows on Author names and remove duplicate rows
cluster_authors_pairs <- myclusters_authors %>% 
  separate_rows(Authors, sep = ";") %>% 
  distinct()

#Bring together clusters with a list of all authors separated by semi-colon
cluster_authors_all <- ddply(cluster_authors_pairs, .(cluster), summarise, Authors = paste(Authors, collapse = ";"))

#Create vectors lists of author names for each cluster
cluster1_authors <- cluster_authors_all %>% 
  subset(subset = rownames(cluster_authors_all) == 1)
cls1 <- unlist(strsplit(cluster1_authors$Authors,";"))

cluster2_authors <- cluster_authors_all %>% 
  subset(subset = rownames(cluster_authors_all) == 2)
cls2 <- unlist(strsplit(cluster2_authors$Authors,";"))

cluster3_authors <- cluster_authors_all %>% 
  subset(subset = rownames(cluster_authors_all) == 3)
cls3 <- unlist(strsplit(cluster3_authors$Authors,";"))

cluster4_authors <- cluster_authors_all %>% 
  subset(subset = rownames(cluster_authors_all) == 4)
cls4 <- unlist(strsplit(cluster4_authors$Authors,";"))



#Plot Venn diagram using VennDiagram and eulerr
library(VennDiagram)
library(eulerr)  

#Transpose from long to wide with TRUE/FALSE values depending on presence of author in cluster
#This is a cleaner way to doing this than creating vectors lists and manually entering number but R keep crashing when I try to run the eulerr matrix command
#venn_df <- as.matrix(table(cluster_authors_pairs) > 0)


#Calculate overlap between clusters
overlap <- calculate.overlap(x = list(cls1, cls2, cls3, cls4))

#Help function to display plot in R
display_venn <- function(x, ...){
  library(VennDiagram)
  grid.newpage()
  venn_object <- venn.diagram(x, filename = NULL, ...)
  grid.draw(venn_object)
}

#Display Venn diagram in VennDiagram package to retrieve overlap information
display_venn(
  x = list(cls1, cls2, cls3, cls4),
  category.names = c("Cluster 1" , "Cluster 2 " , "Cluster 3", "Cluster 4"),
  fill = c("#999999", "#E69F00", "#56B4E9", "#009E73"),
  lwd = 2,
  lty = 'blank',
  # Numbers
  cex = .9,
  # Set names
  cat.cex = 1,
  cat.fontface = "bold",
  cat.default.pos = "outer",
  cat.dist = c(0.055, 0.055, 0.1, 0.1)
)


#Input overlap information from above into euler for nicer plot
fit <- euler(c(A = 811, B = 831, C = 402, D = 54, "A&B" = 36, "A&C" = 72, 
               "A&D" = 2,
               "B&C" = 11,
               "B&D" = 3,
               "C&D" = 2,
               "A&B&C" = 2,
               "A&B&D" = 0,
               "A&C&D" = 2,
               "B&C&D" = 0,
               "A&B&C&D" = 0))

plot(fit, quantities = TRUE)

betterfit <- c(A = 811, B = 831, C = 402, D = 54, "A&B" = 36, "A&C" = 72, 
               "A&D" = 2,
               "B&C" = 11,
               "B&D" = 3,
               "C&D" = 2,
               "A&B&C" = 2,
               "A&B&D" = 0,
               "A&C&D" = 2,
               "B&C&D" = 0,
               "A&B&C&D" = 0)

fit3 <- euler(betterfit, shape = "ellipse")

plot(fit3, quantities = TRUE)
#Still not quite correct as its missing overlap with C and B

