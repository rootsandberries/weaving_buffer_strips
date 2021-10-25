# Creating search string of included studies for Lens.org

# Set working directory
getwd()
setwd("~/RWBS_data/rwbs_lens-bibcouple/rwbs_lens-bibcouple_search")

# Install packages

install.packages("remotes")
library(remotes)
install_github("elizagrames/litsearchr", ref="main")


# Load libraries

library(tidyverse)
library(litsearchr)

# Import data

rwbs_zotero_export <- read_csv("20211019_rwbs_zotero-export.csv", na = "NA")

# Filter to only records with missing DOIs

rwbs_zotero_no_dois <- filter(rwbs_zotero_export, DOI == "")

# Generate Boolean search with titles and save to text file

rwbs_titles <- as.vector(rwbs_zotero_no_dois['Title'])
rwbs_title_search <- litsearchr::write_search(groupdata = rwbs_titles, 
                                              languages = "English", 
                                              stemming = FALSE, 
                                              closure = "none", 
                                              exactphrase = TRUE)

search <- file("rwbs_title-search.txt")
writeLines(my_search, search)
close(search)



