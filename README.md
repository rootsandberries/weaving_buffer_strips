# Collaboration and term usage dynamics in agricultural buffer strip research
This repository contains the code associated with the paper:
Young, S., Haddaway, N. R., Nakagawa, S., Lagisz, M., & Callaghan, M. W. (2021). Collaboration and term usage dynamics in agricultural buffer strip research: a research weaving protocol. Ecological Solutions and Evidence, 2, e12084. https://doi.org/10.1002/2688-8319.12084 

---------------------
File Overview
---------------------
### Code:
1. Filename: publications_LENS_preprocessing.RMD   
Short description: R Markdown file containing code for cleaning and preprocessing Lens.org data for further analysis.        
        
2. Filename: publications_LENS_igraph.RMD       
Short description: R Markdown file containing code for constructing co-authorship network in igraph and analyzing co-authorship network attributes and sub-communities.

3. Filename: publications_LENS_bibcouple.RMD     
Short description: R Markdown file containing code for analysis of bibliographic coupling clusters related to functional roles, publication years, geographic location and buffer strip terminology usage.

4. Filename: topic-model-lens.ipynb
Short desicription: Python script for identifying topics based on studies included in the systematic map.

5. Filename: topic-model.ipynb
Short desicription: Python script for identifying topics based on studies included in the systematic map.

6. Filename: bibcouple.ipynb
Short desicription: Python script for constructing bibliographic coupling network and visualizing network clusters and topics.

7. Filename: heatmaps.ipynb
Short desicription: Python script used for generating heatmaps to examine topic models and functional roles studied in papers.


### Data/
Directory containing the following data files:
1. Filename: 20201215_EGM_Net_all-articles_clean.csv       
Short description: Contains complete list of all studies included in the original systematic map by [Haddaway et al.](https://doi.org/10.1186/s13750-016-0067-6) File used to query Lens.org.        
        
2. Filename: LENS_dataframe.RData       
Short description: RData file containing output from API query of Lens.org of studies included in systematic map. Contains 1073 rows.       
        
3. Filename: LENS_dataframe_cleaned.RData        
Short description: Cleaned version of LENS_dataframe.RData. Duplicate records were removed and author names and ids cleaned.

4. Filename: record_df_data.authors.ids_cleaned.csv
Short description: Unnested and cleaned author data from LENS_dataframe_cleaned.RData.

5. Filename: record_df_data.authors.aff_cleaned.csv
Short description: Unnested and cleaned author data including cleaned country affiliations.

6. Filename: 13750-2018-126-MOESM6-ESM-dedup-1019-outcome.csv
Short desription: Original systematic mapping data including bibliographic data and manually extracted data related to study charactistics. Includes a column "all_outcomes_group", which is a cleaned and grouped version of the original data extracted on outcomes at the study level.

7. Filename: bc_clustered.csv
Short description: List of studies included in the bibliographic coupling network and their associated assigned cluster number.

8. Filename: sysmap.lensid.abstract_bibcouple.csv
File used to generate heatmaps with functional roles studied (variable name: all_outcomes_group).

9. Filename: 20211019_rwbs_lens-export.csv
File used to generate bibliographic coupling network with references from studies (variable name: References).

### Plots/
Directory containing plots found in manuscript figures. See file names and paper for more information about each plot.

### tm-results/
Directory containing topic modelling features, parameters and heatmap plots.

### bibcouple-results/
Directory containing bibliographic coupling figure versions generated in bibcouple.ipynb. 



