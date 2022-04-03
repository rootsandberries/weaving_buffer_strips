# Creating networks from Lens export

# Load libraries

library(bibliometrix)
library(igraph)
library(tidyverse)

# Import data from Lens export
bib_lens <- convert2df("./data/20211019_rwbs_lens-export.csv", 
                         dbsource = "lens", format = "csv")

# Filter out records with no references
biblens_ref <- filter(bib_lens, CR != "")

# Export file for mapping to lens ID
# write.csv(biblens_ref, "20211020_rwbs_bibliometrix-export.csv")

# Convert to dgCMatrix object 
NetMatrix_CR <- biblioNetwork(biblens_ref, 
                              analysis = "coupling", 
                              network = "references", 
                              sep = "; ") 

# Plot bibliographic coupling network with bibliometrix and applying community detection
net <- networkPlot(NetMatrix_CR, 
                   type = "fruchterman", size.cex=FALSE, 
                   remove.multiple=FALSE, edgesize = 5,
                   label = FALSE, remove.isolates = TRUE, 
                   community.repulsion = 0.3,
                   cluster = "louvain")


# Convert adjaceny matrix to graph object for igraph
igraph_lens <- graph_from_adjacency_matrix(NetMatrix_CR, mode = "undirected",
                                           diag = FALSE)

# Check graph object
E(igraph_lens)
V(igraph_lens)

# Remove isolated nodes with degree less than 2
igraph_lens_noiso <- delete.vertices(igraph_lens, which(degree(igraph_lens)<2))


# Plot bibliographic coupling network in igraph
bibcouple_lens <- plot.igraph(igraph::simplify(igraph_lens_noiso, remove.loops = TRUE, remove.multiple = TRUE), 
                              vertex.size=2, edge.curved=T, edge.width = 0.5, edge.color = "light gray",
                              vertex.label = NA, vertex.frame.color = "black",
                              layout=layout_with_fr(igraph_lens_noiso))


# Run community detection algorithm in igraph
clusters_lens <- cluster_louvain(igraph_lens_noiso, weights = NULL)

# Check clusters
length(clusters_lens)
sizes(clusters_lens)

# Create membership object
cluster_grps1 <- membership(clusters_lens)

# Get vertex names
cluster_grps2 <- cbind(V(igraph_lens_noiso)$name, clusters_lens$membership)

# Plot with clusters

bibcouple_lens_com <- plot(clusters_lens, igraph_lens_noiso, 
                           vertex.label = NA,
                           vertex.size=2,
                           edge.width = 0.5,
                           mark.groups = NULL,
                           edge.color = "light gray"
)

# Save plot to file

png("rwbs_bibcouple_plot.png", 600, 600)
plot(clusters_lens, igraph_lens_noiso, 
     vertex.label = NA,
     vertex.size=2,
     edge.width = 0.5,
     mark.groups = NULL,
     edge.color = "light gray"
)
dev.off()


# Write cluster membership to .csv
write.csv(cluster_grps2, file ="rwbs_lens-bibcouple_network_exports/20211021_clusters-louv_lens.csv")


devtools::install_github("RMHogervorst/gephi")
library(gephi)
gephi_write_edges(igraph_lens, "edges.csv")

write.graph(igraph_lens_noiso, file='graph.gml', format="gml")


# Repeating above process but with data from Lens API

# Load libraries

library(igraph)
library(tidyverse)

load("./data/LENS_dataframe.RData") #loads record_df data object

length(unique(record_df$data.title)) #974 unique titles
#View(record_df[duplicated(record_df$data.title) | duplicated(record_df$data.title, fromLast=TRUE), ]) #visual check - some records have more info than others

sum(sapply(record_df$data.fields_of_study, is.null)) #number of records without fields_of_study, also likely to have other missing data
record_df$data.has_fields_of_study <- sapply(record_df$data.fields_of_study, is.null)

record_df %>% arrange(data.has_fields_of_study, data.title) %>% distinct(data.title, .keep_all = TRUE) -> record_df_unique #place the records without fields_of_study at the end and remove duplicates dim(record_df_unique) #check dimensions
record_df <- record_df_unique #reassign
dim(record_df) #974

#unnest list of references
record_df_data.refs <- record_df %>% select(data.lens_id, data.title, data.publication_type, data.year_published, data.references) %>% unnest(data.references)
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

# Plot bibliographic coupling network in igraph
bibcouple_lens_api <- plot.igraph(igraph::simplify(igraph_lens_api_noiso, remove.loops = TRUE, remove.multiple = TRUE), 
                                  vertex.size=2, edge.curved=T, edge.width = 0.5, edge.color = "light gray",
                                  vertex.label = NA, vertex.frame.color = "black",
                                  layout=layout_with_fr(igraph_lens_api_noiso))

# Run community detection algorithm in igraph
clusters_lens_api <- cluster_louvain(igraph_lens_api_noiso, weights = NULL)

# Check clusters
length(clusters_lens_api)
sizes(clusters_lens_api)

# Create membership object
cluster_grps1_api <- membership(clusters_lens_api)

# Get vertex names
cluster_grps2_api <- cbind(V(igraph_lens_noiso)$name, clusters_lens$membership)

# Plot with clusters

bibcouple_lens_api_com <- plot(clusters_lens_api, igraph_lens_api_noiso, 
                               vertex.label = NA,
                               vertex.size=2,
                               edge.width = 0.5,
                               mark.groups = NULL,
                               edge.color = "light gray")

# Save plot to file

png("rwbs_bibcouple_plot_api.png", 600, 600)
plot(clusters_lens_api, igraph_lens_api_noiso, 
     vertex.label = NA,
     vertex.size=2,
     edge.width = 0.5,
     mark.groups = NULL,
     edge.color = "light gray"
)
dev.off()


