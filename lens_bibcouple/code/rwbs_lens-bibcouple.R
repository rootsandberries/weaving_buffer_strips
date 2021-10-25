# Creating networks from Lens export

# Set working directory
getwd()
setwd("~/Documents/Current_Projects/Research-Weaving-Buffer-Strips/RWBS_Data/rwbs_lens-bibcouple/rwbs_lens-bibcouple_network")

# Install packages
# install.packages("bibliometrix")

# Load libraries

library(bibliometrix)
library(igraph)
library(tidyverse)

# Import data from Lens export
bib_lens <- convert2df("20211019_rwbs_lens-export.csv", dbsource = "lens", format = "csv")

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

# Write cluster membership to .csv
write.csv(cluster_grps2, file ="rwbs_lens-bibcouple_network_exports/20211021_clusters-louv_lens.csv")



# This code should create an edge list from the matrix, and for some reason, there
# are many repeat pairs. It seems like there should only be at most two (if they
# are citing each other). Bibliometrix says it creates a squared matrix. Not sure
# if that is related. The second option, when imported to Gephi, indicates 
# 'parallel edges'. 

devtools::install_github("RMHogervorst/gephi")
library(gephi)
gephi_write_edges(igraph_lens, "edges.csv")

write.graph(igraph_lens_noiso, file='graph.gml', format="gml")


