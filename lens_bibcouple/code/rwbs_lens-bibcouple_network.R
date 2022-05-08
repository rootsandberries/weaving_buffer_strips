# Creating networks from Lens export
# Using data from Lens API

# Load libraries

library(igraph)
library(tidyverse)
library(fuzzyjoin)
library(dplyr)
library(here)

load("./data/LENS_dataframe_cleaned.RData") #loads record_df data object

length(unique(record_df$data.title)) #968 unique titles
#View(record_df[duplicated(record_df$data.title) | duplicated(record_df$data.title, fromLast=TRUE), ]) #visual check - some records have more info than others

#Write out full lens dataset for manual checking against original map data
#all_found_lens <- as.data.frame(record_df) %>% select(data.lens_id, data.title)
#write.csv(all_found_lens, "all_found_lens_api.csv")

#Based on manual checking, remove records from Lens dataset that are false matches (i.e. duplicates, etc.)
record_df_clean <- record_df %>% filter(data.lens_id != "163-791-444-765-886", data.lens_id !="135-649-556-815-160", data.lens_id != "023-784-116-518-93X", data.lens_id != "083-431-696-176-966", data.lens_id != "000-329-490-788-869", data.lens_id != "161-425-757-559-60X", data.lens_id != "070-142-965-217-664", data.lens_id != "090-581-029-835-945", data.lens_id != "096-184-264-826-781", data.lens_id != "061-017-051-481-03X", data.lens_id != "066-806-618-413-681", data.lens_id != "150-615-763-454-941", data.lens_id != "006-902-184-255-332", data.lens_id != "149-960-450-431-003", data.lens_id != "091-975-063-830-994", data.lens_id != "144-639-707-343-162", data.lens_id != "031-987-261-230-852", data.lens_id != "013-843-692-321-806", data.lens_id != "101-679-988-502-372", data.lens_id != "130-012-333-732-368", data.lens_id != "093-741-681-931-769")
dim(record_df_clean) #948

#sum(sapply(record_df$data.fields_of_study, is.null)) #number of records without fields_of_study, also likely to have other missing data
#record_df$data.has_fields_of_study <- sapply(record_df$data.fields_of_study, is.null)

sum(sapply(record_df_clean$data.references, is.null)) #number of records without references for bibliographic coupling

#Remove records without references and deduplicate
record_df_clean$data.has_no_references <- sapply(record_df_clean$data.references, is.null)
record_df_clean %>% filter(data.has_no_references==FALSE) %>% distinct(data.title, .keep_all = TRUE) -> record_df_unique
dim(record_df_unique) #842

#Check removed records
#record_df_clean %>% filter(data.has_no_references==TRUE) -> record_df_removed
#record_df_removed.lensid <- record_df_removed %>% select(data.lens_id, data.title)
#write.csv(record_df_removed.lensid, "removed-due-to-noref.csv")

#Prepare merged dataset of Lens ID, Abstract and original map data
record_df_data.lensid <- record_df_unique %>% select(data.lens_id, data.title, data.abstract)
dim(record_df_data.lensid)
names(record_df_data.lensid)
sysmap_data <- read.csv("./data/13750-2018-126-MOESM6-ESM-dedup-1019-outcome.csv")

#Trim whitespace and make title lowercase in both cluster and outcome datasets
record_df_data.lensid <- rename(record_df_data.lensid, lensID = data.lens_id, Title = data.title)
sysmap_data$Title <- trimws(tolower(sysmap_data$Title))
record_df_data.lensid$Title <- trimws(tolower(record_df_data.lensid$Title))

#Fuzzy join of original map data with lens abstract and id on title (adjust distance until total records are found)
sysmap_lens <- stringdist_inner_join(sysmap_data, record_df_data.lensid, by = "Title", max_dist = 5,
                                distance_col = "distance")
dim(sysmap_lens) #839

#Find non-matching items and add manually
#These are not matching due to missing sub-titles or titles in two languages
sysmap_lens_anti <- stringdist_anti_join(record_df_data.lensid, sysmap_data, by = "Title", max_dist = 5,
                                     distance_col = "distance")
dim(sysmap_lens_anti) #3

#Write matching and non-matching to csv and manually append non-matching to appropriate rows
write_csv(sysmap_lens, "sysmap.lensid.abstract_bibcouple.csv")
write_csv(sysmap_lens_anti, "sysmap.lensid.abstract_bibcouple_nonmatch.csv")

#Write to csv to check for non-matches manually
#lensid_df <- as.data.frame(record_df_data.lensid)
#lensid_df <- apply(lensid_df, 2, as.character)
#write.csv(lensid_df, "test_lensid.csv")
#write.csv(sysmap_data, "test_sysmap_data.csv")

#unnest list of references
record_df_data.refs <- record_df_clean %>% select(data.lens_id, data.title, data.publication_type, data.year_published, data.references) %>% unnest(data.references)
dim(record_df_data.refs)
names(record_df_data.refs)

#prepare data frame for igraph
refs_df <- data.frame(pub.id = record_df_data.refs$data.lens_id, refs = record_df_data.refs$lens_id)
str(refs_df)

# Prepare data for igraph
refs_df %>%
        merge(refs_df, by = "refs") %>%
        .[.$pub.id.x < .$pub.id.y, c('pub.id.x', 'pub.id.y', 'refs')] %>%
        count(pub.id.x, pub.id.y) -> myedgelist

# Convert dataframe to graph object for igraph
igraph_lens_api <- graph_from_data_frame(myedgelist, directed = FALSE)

# Check graph object
E(igraph_lens_api)
V(igraph_lens_api)

# Remove isolated nodes with degree less than 2
igraph_lens_api_noiso <- delete.vertices(igraph_lens_api, which(degree(igraph_lens_api)<2))

# Examine removed nodes. Keep only isolated nodes and convert to dataframe.
#igraph_lens_api_iso <- delete.vertices(igraph_lens_api, which(degree(igraph_lens_api)>3))
#isolated_nodes_df <- data.frame(name = V(igraph_lens_api_iso)$name)
#write_csv(isolated_nodes_df, "rwbs_bibcouple_removed-nodes.csv")

# Plot bibliographic coupling network in igraph
bibcouple_lens_api <- plot.igraph(igraph::simplify(igraph_lens_api_noiso, remove.loops = TRUE, remove.multiple = TRUE), 
                                  vertex.size=2, edge.curved=T, edge.width = 0.5, edge.color = "light gray",
                                  vertex.label = NA, vertex.frame.color = "black",
                                  layout=layout_with_fr(igraph_lens_api_noiso))

# Run community detection algorithm in igraph
clusters_lens_api<- cluster_louvain(igraph_lens_api_noiso, weights = NULL)

# Check clusters
length(clusters_lens_api)
sizes(clusters_lens_api)

# Create membership object
cluster_grps1_api <- membership(clusters_lens_api)

# Get vertex names
cluster_grps2_api <- cbind(V(igraph_lens_api_noiso)$name, clusters_lens_api$membership)

# Plot with clusters

bibcouple_lens_api_com <- plot(clusters_lens_api, igraph_lens_api_noiso, 
                               vertex.label = NA,
                               vertex.size=2,
                               edge.width = 0.5,
                               mark.groups = NULL,
                               edge.color = "light gray")

# Save plot to file

png("rwbs_bibcouple_plot_api.png", 600, 600)
par(mar = rep(0, 4))
plot(clusters_lens_api, igraph_lens_api_noiso, 
     vertex.label = NA,
     vertex.size=2,
     edge.width = 0.5,
     mark.groups = NULL,
     edge.color = "light gray"
)
dev.off()

# Write cluster membership to .csv
write.csv(cluster_grps2_api, file ="./exports/20220506_clusters-louv_lens_api.csv")


# For visualization in gephi

#devtools::install_github("RMHogervorst/gephi")
#library(gephi)
#gephi_write_edges(igraph_lens_api, "edges.csv")

#write.graph(igraph_lens_api_noiso, file='graph.gml', format="gml")

