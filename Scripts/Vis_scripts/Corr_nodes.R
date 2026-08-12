################################################################################
# Script: Corr_nodes.R
#
# Contains functions required to generate the correlation heatmaps for node 
# ranking. Can also facilitate pearsom correlations if required
################################################################################

node_correlation_column_map <- list(
  rank = c(
    Degree = "degree_rank_corr",
    Betweenness = "betweenness_rank_corr",
    Closeness = "closeness_rank_corr",
    Eigenvector = "eigenvector_rank_corr"
  ),
  pearson = c(
    Degree = "degree_corr",
    Betweenness = "betweenness_corr",
    Closeness = "closeness_corr",
    Eigenvector = "eigenvector_corr"
  )
)


prepare_node_correlation_heatmap_df <- function(
    df,
    correlation_columns,
    spotlight_pct_choice,
    alphas_to_plot
) {
  validate_visualisation_data(
    df = df,
    data_name = "node_corr_df",
    required_columns = c(
      "alpha",
      "spotlight_pct",
      "p_obs_spotlit",
      "p_obs_nonspotlit",
      unname(correlation_columns)
    )
  )

  resolve_numeric_selection(
    requested_values = spotlight_pct_choice,
    available_values = df$spotlight_pct,
    setting_name = "config$visualisations$main_spotlight_pct"
  )

  selected_alphas <- resolve_numeric_selection(
    requested_values = alphas_to_plot,
    available_values = df$alpha,
    setting_name = "config$visualisations$alphas_to_plot"
  )

  filtered_df <- df |>
    dplyr::filter(
      dplyr::near(spotlight_pct, spotlight_pct_choice),
      numeric_selection_mask(alpha, selected_alphas)
    )

  if (nrow(filtered_df) == 0L) {
    stop("No node-correlation data remain after applying selections.")
  }

  purrr::imap_dfr(
    correlation_columns,
    function(correlation_column, metric_name) {
      filtered_df |>
        dplyr::group_by(
          spotlight_pct,
          p_obs_spotlit,
          p_obs_nonspotlit
        ) |>
        dplyr::summarise(
          mean_corr = mean(
            .data[[correlation_column]],
            na.rm = TRUE
          ),
          median_corr = stats::median(
            .data[[correlation_column]],
            na.rm = TRUE
          ),
          q25 = stats::quantile(
            .data[[correlation_column]],
            probs = 0.25,
            na.rm = TRUE
          ),
          q75 = stats::quantile(
            .data[[correlation_column]],
            probs = 0.75,
            na.rm = TRUE
          ),
          n_graphs = sum(
            !is.na(.data[[correlation_column]])
          ),
          .groups = "drop"
        ) |>
        dplyr::mutate(
          metric = metric_name
        )
    }
  ) |>
    dplyr::mutate(
      metric = factor(
        metric,
        levels = names(correlation_columns)
      ),
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


plot_node_correlation_heatmap <- function(
    plot_df,
    correlation_type,
    spotlight_pct_choice
) {
  correlation_title <- if (correlation_type == "rank") {
    "Rank correlations"
  } else {
    "Pearson correlations"
  }

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = p_obs_nonspotlit,
      y = p_obs_spotlit,
      fill = mean_corr
    )
  ) +
    ggplot2::geom_tile(
      colour = "white",
      linewidth = 0.4
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = sprintf("%.2f", mean_corr)
      ),
      size = 3
    ) +
    ggplot2::facet_wrap(
      ~ metric,
      ncol = 2
    ) +
    ggplot2::scale_fill_viridis_c(
      limits = c(-1, 1),
      oob = scales::squish,
      name = paste0(correlation_title, "\ncoefficient")
    ) +
    ggplot2::coord_equal() +
    ggplot2::labs(
      x = expression(p[plain(n)]),
      y = expression(p[plain(s)]),
      title = paste0(
        correlation_title,
        " under spotlighted observation"
      ),
      subtitle = paste0(
        "Spotlight proportion = ",
        scales::percent(
          spotlight_pct_choice,
          accuracy = 1
        )
      )
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(
        angle = 45,
        hjust = 1
      ),
      strip.text = ggplot2::element_text(
        face = "bold"
      )
    )
}


build_node_correlation_plots <- function(
    df,
    correlation_types,
    centrality_metrics,
    spotlight_pct_choice,
    alphas_to_plot = NULL
) {
  plots <- list()

  for (correlation_type in correlation_types) {
    correlation_columns <- node_correlation_column_map[[
      correlation_type
    ]][centrality_metrics]

    plot_df <- prepare_node_correlation_heatmap_df(
      df = df,
      correlation_columns = correlation_columns,
      spotlight_pct_choice = spotlight_pct_choice,
      alphas_to_plot = alphas_to_plot
    )

    plot_name <- paste0(
      "node_correlation_",
      correlation_type
    )

    plots[[plot_name]] <- plot_node_correlation_heatmap(
      plot_df = plot_df,
      correlation_type = correlation_type,
      spotlight_pct_choice = spotlight_pct_choice
    )
  }

  plots
}
