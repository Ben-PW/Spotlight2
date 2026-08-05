library(ggplot2)
library(dplyr)

# here::here()

if (!dir.exists(here::here("Figures"))) {
  dir.create(
    here::here("Figures"),
    recursive = TRUE
  )
}

if (!dir.exists(here::here("Figures", "Figures_supplemental"))) {
  dir.create(
    here::here("Figures", "Figures_supplemental"),
    recursive = TRUE
  )
}

################################### Sampling completeness #############################

# Heatmap of sampling coverage over different levels of spotlight effect
# Target metrics:
# - tie recovery proportion
# Design variables
# - p_obs_spotlight
# - p_obs_nonspotlight
# - gt centralisation
# - alpha

# Required dataframe: coverage_heatmap_df
source(here::here("Scripts", "Vis_scripts", "Coverage_heatmap.R"))

############################## Network level metrics #############################

# Mean absolute relative bais in network level metrics and mean relative bias in
# network level metrics.
# Variables visualised are:
# Target metrics: 
# - clustering
# - degree centralisation
# - APL
# Design variables:
# - alpha
# - missingness level
# - spotlight strength
# - ground truth centralisation

# Required dataframe: mn_abs_rel_bias_nets
source(here::here("Scripts", "Vis_scripts", "Network_bias_plots.R"))

############################### Node level metrics ##############################

############## Aggregating over all nodes (no spitlight distinction) ############

# Correlation plots between node level values 
# Target metrics:
# - Degree centrality
# - Betweenness centrality
# - Closeness centrality
# - Eigenvector centrality
# Design variables
# - alpha
# - missingness level
# - spotlight strength
# - ground truth centralisation

# Required dataframe: node_corr_df
library(stringr)
source(here::here("Scripts", "Vis_scripts", "Corr_nodes.R"))

############################# Non aggregated node metrics ########################

# Plots of node level outcomes such as TopN, OverlapN etc
# Target metrics:
# - Degree centrality
# - Betweenness centrality
# - Closeness centrality
# - Eigenvector centrality
# Design variables
# - alpha
# - missingness level
# - spotlight strength
# - ground truth centralisation

# Required dataframe: node_rank_df
source(here::here("Scripts", "Vis_scripts", "Node_rank_bias2.R"))

# Plots of node level rank change statistics
# Target metrics:
# - Degree centrality rank change
# - Betweenness centrality rank change
# - Closeness centrality rank change
# - Eigenvector centrality rank change
# Design variables
# - alpha
# - missingness level
# - spotlight strength
# - ground truth centralisation

# Required dataframe: rank_lift_df2
source(here::here("Scripts", "Vis_scripts", "Node_rank_change.R"))

# Spotlight strength plots
# These seem quite promising, will need to check on full run though
# Target metrics:
# - Degree centrality rank change
# - Betweenness centrality rank change
# - Closeness centrality rank change
# - Eigenvector centrality rank change
# Design variables
# - Spotlight strength (p_obs_spotlit - p_obs_nonspotlit)
# - GT centralisation
# - alpha

# Required dataframe: rank_lift_df2
source(here::here("Scripts", "Vis_scripts", "Spotlight_strength_plot3.R"))

