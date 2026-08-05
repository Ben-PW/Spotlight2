################################################################################

# This script is for visualisations of AGGREGATED node level centrality correlations, both
# rank and pearson. Any spotlight specific visualisations should go in the dedicated 
# script which doesn't exist yet but fingers crossed will by the end of the day.

################################################################################

################################################################################
# Heatmaps of node-rank correlation
################################################################################

library(dplyr)
library(ggplot2)
library(stringr)

######################## Prepare heatmap dataframe ##############################

# make_node_corr_heatmap_df <- function(
#     df,
#     corr_col,
#     spotlight_pct_choice = 0.10
# ) {
#   
#   df |>
#     dplyr::filter(
#       spotlight_pct == spotlight_pct_choice
#     ) |>
#     dplyr::mutate(
#       
#       # Extract target baseline centralisation from dataset name
#       gt_cent = dplyr::case_when(
#         stringr::str_detect(dataset, "_c1$") ~ "Low",
#         stringr::str_detect(dataset, "_c3$") ~ "Medium",
#         stringr::str_detect(dataset, "_c5$") ~ "High",
#         TRUE ~ "WARNING"
#       )
#     ) |>
#     dplyr::filter(
#       gt_cent != "WARNING"
#     ) |>
#     dplyr::group_by(
#       gt_cent,
#       alpha,
#       spotlight_pct,
#       p_obs_spotlit,
#       p_obs_nonspotlit
#     ) |>
#     dplyr::summarise(
#       mean_corr = mean(
#         .data[[corr_col]],
#         na.rm = TRUE
#       ),
#       
#       median_corr = stats::median(
#         .data[[corr_col]],
#         na.rm = TRUE
#       ),
#       
#       q25 = stats::quantile(
#         .data[[corr_col]],
#         probs = 0.25,
#         na.rm = TRUE
#       ),
#       
#       q75 = stats::quantile(
#         .data[[corr_col]],
#         probs = 0.75,
#         na.rm = TRUE
#       ),
#       
#       n_graphs = sum(
#         !is.na(.data[[corr_col]])
#       ),
#       
#       .groups = "drop"
#     ) |>
#     dplyr::mutate(
#       gt_cent = factor(
#         gt_cent,
#         levels = c("High", "Medium", "Low")
#       ),
#       
#       alpha = factor(alpha),
#       
#       p_obs_spotlit = factor(
#         p_obs_spotlit,
#         levels = sort(unique(p_obs_spotlit))
#       ),
#       
#       p_obs_nonspotlit = factor(
#         p_obs_nonspotlit,
#         levels = sort(unique(p_obs_nonspotlit))
#       )
#     )
# }

################################################################################

#plot_node_corr_heatmap <- function(
#    plot_df,
#    metric_label,
#    use_median = FALSE
#) {
#  
#  fill_var <- if (use_median) {
#    "median_corr"
#  } else {
#    "mean_corr"
#  }
#  
#  ggplot(
#    plot_df,
#    aes(
#      x = p_obs_nonspotlit,
#      y = p_obs_spotlit,
#      fill = .data[[fill_var]]
#    )
#  ) +
#    geom_tile(
#      colour = "white",
#      linewidth = 0.4
#    ) +
#    
#    # Optional: print correlation inside each tile
#    geom_text(
#      aes(
#        label = sprintf("%.2f", .data[[fill_var]])
#      ),
#      size = 3
#    ) +
#    
#    facet_grid(
#      gt_cent ~ alpha,
#      labeller = label_both
#    ) +
#    
#    scale_fill_viridis_c(
#      limits = c(-1, 1),
#      oob = scales::squish,
#      name = "Rank\ncorrelation"
#    ) +
#    
#    coord_equal() +
#    
#    labs(
#      x = "Observation probability: non-spotlit ties",
#      y = "Observation probability: spotlit ties",
#      title = paste(metric_label, "correlation"),
#      subtitle = paste0(
#        "Spotlight proportion = ",
#        scales::percent(
#          unique(plot_df$spotlight_pct),
#          accuracy = 1
#        )
#      )
#    ) +
#    
#    theme_minimal() +
#    theme(
#      panel.grid = element_blank(),
#      axis.text.x = element_text(angle = 45, hjust = 1),
#      strip.text = element_text(face = "bold")
#    )
#}

################################################################################



#rank_corr_cols <- c(
#  'Degree rank' = "degree_rank_corr",
#  'Betweenness rank' = "betweenness_rank_corr",
#  'Closeness rank' = "closeness_rank_corr",
#  'Eigenvector rank' = "eigenvector_rank_corr"
#)
#
#corr_cols <- c(
#  Degree = "degree_corr",
#  Betweenness = "betweenness_corr",
#  Closeness = "closeness_corr",
#  Eigenvector = "eigenvector_corr"
#)
#
#node_rank_heatmaps <- purrr::imap(
#  rank_corr_cols,
#  function(corr_col, metric_name) {
#    
#    plot_df <- make_node_corr_heatmap_df(
#      df = node_corr_df,
#      corr_col = corr_col,
#      spotlight_pct_choice = 0.10
#    )
#    
#    plot_node_corr_heatmap(
#      plot_df = plot_df,
#      metric_label = metric_name
#    )
#  }
#)

#node_rank_heatmaps$Degree
#node_rank_heatmaps$Betweenness
#node_rank_heatmaps$Closeness
#node_rank_heatmaps$Eigenvector
#
#node_corr_heatmaps <- purrr::imap(
#  corr_cols,
#  function(corr_col, metric_name) {
#    
#    plot_df <- make_node_corr_heatmap_df(
#      df = node_corr_df,
#      corr_col = corr_col,
#      spotlight_pct_choice = 0.10
#    )
#    
#    plot_node_corr_heatmap(
#      plot_df = plot_df,
#      metric_label = metric_name
#    )
#  }
#)

#node_corr_heatmaps$Closeness

################################################################################

# Unfaceted versions

################################################################################

# Make the dataframe

make_node_corr_heatmap_df <- function(
    df,
    corr_col,
    spotlight_pct_choice = 0.10
) {
  
  df |>
    dplyr::filter(
      spotlight_pct == spotlight_pct_choice
    ) |>
    dplyr::mutate(
      
      # Extract target baseline centralisation from dataset name
      gt_cent = dplyr::case_when(
        stringr::str_detect(dataset, "_c1$") ~ "Low",
        stringr::str_detect(dataset, "_c3$") ~ "Medium",
        stringr::str_detect(dataset, "_c5$") ~ "High",
        TRUE ~ "WARNING"
      )
    ) |>
    dplyr::filter(
      gt_cent != "WARNING"
    ) |>
    dplyr::group_by(
      #gt_cent,
      #alpha,
      spotlight_pct,
      p_obs_spotlit,
      p_obs_nonspotlit
    ) |>
    dplyr::summarise(
      mean_corr = mean(
        .data[[corr_col]],
        na.rm = TRUE
      ),
      
      median_corr = stats::median(
        .data[[corr_col]],
        na.rm = TRUE
      ),
      
      q25 = stats::quantile(
        .data[[corr_col]],
        probs = 0.25,
        na.rm = TRUE
      ),
      
      q75 = stats::quantile(
        .data[[corr_col]],
        probs = 0.75,
        na.rm = TRUE
      ),
      
      n_graphs = sum(
        !is.na(.data[[corr_col]])
      ),
      
      .groups = "drop"
    ) |>
    dplyr::mutate(
#      gt_cent = factor(
#        gt_cent,
#        levels = c("High", "Medium", "Low")
#      ),
      
#      alpha = factor(alpha),
      
      p_obs_spotlit = factor(
        p_obs_spotlit,
        levels = sort(unique(p_obs_spotlit))
      ),
      
      p_obs_nonspotlit = factor(
        p_obs_nonspotlit,
        levels = sort(unique(p_obs_nonspotlit))
      )
    )
}

# # Plotting function
# 
# plot_node_corr_heatmap <- function(
#     plot_df,
#     metric_label,
#     use_median = FALSE
# ) {
#   
#   fill_var <- if (use_median) {
#     "median_corr"
#   } else {
#     "mean_corr"
#   }
#   
#   ggplot(
#     plot_df,
#     aes(
#       x = p_obs_nonspotlit,
#       y = p_obs_spotlit,
#       fill = .data[[fill_var]]
#     )
#   ) +
#     geom_tile(
#       colour = "white",
#       linewidth = 0.4
#     ) +
#     
#     # Optional: print correlation inside each tile
#     geom_text(
#       aes(
#         label = sprintf("%.2f", .data[[fill_var]])
#       ),
#       size = 3
#     ) +
#     
# #    facet_grid(
# #      gt_cent ~ alpha,
# #      labeller = label_both
# #    ) +
#     
#     scale_fill_viridis_c(
#       limits = c(-1, 1),
#       oob = scales::squish,
#       name = "Rank\ncorrelation"
#     ) +
#     
#     coord_equal() +
#     
#     labs(
#       x = "Observation probability: non-spotlit ties",
#       y = "Observation probability: spotlit ties",
#       title = paste(metric_label, "correlation"),
#       subtitle = paste0(
#         "Spotlight proportion = ",
#         scales::percent(
#           unique(plot_df$spotlight_pct),
#           accuracy = 1
#         )
#       )
#     ) +
#     
#     theme_minimal() +
#     theme(
#       panel.grid = element_blank(),
#       axis.text.x = element_text(angle = 45, hjust = 1),
#       strip.text = element_text(face = "bold")
#     )
# }
# 
# # Plot
# 
# rank_corr_cols <- c(
#   'Degree rank' = "degree_rank_corr",
#   'Betweenness rank' = "betweenness_rank_corr",
#   'Closeness rank' = "closeness_rank_corr",
#   'Eigenvector rank' = "eigenvector_rank_corr"
# )
# 
# corr_cols <- c(
#   Degree = "degree_corr",
#   Betweenness = "betweenness_corr",
#   Closeness = "closeness_corr",
#   Eigenvector = "eigenvector_corr"
# )
# 
# node_rank_heatmaps <- purrr::imap(
#   rank_corr_cols,
#   function(corr_col, metric_name) {
#     
#     plot_df <- make_node_corr_heatmap_df(
#       df = node_corr_df,
#       corr_col = corr_col,
#       spotlight_pct_choice = 0.10
#     )
#     
#     plot_node_corr_heatmap(
#       plot_df = plot_df,
#       metric_label = metric_name
#     )
#   }
# )
# 
# node_rank_heatmaps$Degree
# node_rank_heatmaps$Betweenness
# node_rank_heatmaps$Closeness
# node_rank_heatmaps$Eigenvector

######################## Plot the comp

################################################################################

# Build one combined dataframe containing all four metrics
node_rank_heatmap_df <- purrr::imap_dfr(
  rank_corr_cols,
  function(corr_col, metric_name) {
    
    make_node_corr_heatmap_df(
      df = node_corr_df,
      corr_col = corr_col,
      spotlight_pct_choice = 0.10
    ) |>
      dplyr::mutate(
        metric = metric_name
      )
  }
) |>
  dplyr::mutate(
    metric = factor(
      metric,
      levels = names(rank_corr_cols)
    )
  )

###############################

node_rank_composite <- ggplot(
  node_rank_heatmap_df,
  aes(
    x = p_obs_nonspotlit,
    y = p_obs_spotlit,
    fill = mean_corr
  )
) +
  geom_tile(
    colour = "white",
    linewidth = 0.4
  ) +
  geom_text(
    aes(label = sprintf("%.2f", mean_corr)),
    size = 3
  ) +
  facet_wrap(
    ~ metric,
    ncol = 2
  ) +
  scale_fill_viridis_c(
    limits = c(-1, 1),
    oob = scales::squish,
    name = "Rank\ncorrelation"
  ) +
  coord_equal() +
  labs(
    x = "Observation probability: non-spotlit ties",
    y = "Observation probability: spotlit ties",
    title = "Rank correlations under spotlighted observation",
    subtitle = "Spotlight proportion = 10%"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    strip.text = element_text(
      face = "bold"
    )
  )

node_rank_composite
