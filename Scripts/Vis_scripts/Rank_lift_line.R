################################################################################
# Script: Rank_lift_line.R
#
# This script contains the functions for visualising mean normalised rank lift by 
# spotlight strength, where spotlight strength is 
# p_obs_spotlit - p_obs_nonspotlit.
################################################################################

prepare_rank_lift_strength_df <- function(
    df,
    metrics_to_keep,
    spotlight_pct_choice,
    alphas_to_plot
) {
  validate_visualisation_data(
    df = df,
    data_name = "rank_lift_df2",
    required_columns = c(
      "metric",
      "alpha",
      "spotlight_pct",
      "p_obs_spotlit",
      "p_obs_nonspotlit",
      "mean_norm_rank_lift_spotlit"
    )
  )

  resolve_character_selection(
    requested_values = metrics_to_keep,
    available_values = df$metric,
    setting_name = "config$visualisations$centrality_metrics"
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

  output_df <- df |>
    dplyr::mutate(
      metric = as.character(metric),
      alpha = as.numeric(as.character(alpha)),
      spotlight_pct = as.numeric(as.character(spotlight_pct)),
      p_obs_spotlit = as.numeric(as.character(p_obs_spotlit)),
      p_obs_nonspotlit = as.numeric(as.character(p_obs_nonspotlit))
    ) |>
    dplyr::filter(
      metric %in% metrics_to_keep,
      dplyr::near(spotlight_pct, spotlight_pct_choice),
      numeric_selection_mask(alpha, selected_alphas)
    ) |>
    dplyr::mutate(
      spotlight_strength = round(
        p_obs_spotlit - p_obs_nonspotlit,
        2
      )
    ) |>
    dplyr::group_by(
      metric,
      alpha,
      spotlight_pct,
      spotlight_strength
    ) |>
    dplyr::summarise(
      mean_strength_lift = mean(
        mean_norm_rank_lift_spotlit,
        na.rm = TRUE
      ),
      q05 = stats::quantile(
        mean_norm_rank_lift_spotlit,
        probs = 0.05,
        na.rm = TRUE
      ),
      q95 = stats::quantile(
        mean_norm_rank_lift_spotlit,
        probs = 0.95,
        na.rm = TRUE
      ),
      n_observations = sum(
        !is.na(mean_norm_rank_lift_spotlit)
      ),
      n_probability_pairs = dplyr::n_distinct(
        interaction(
          p_obs_spotlit,
          p_obs_nonspotlit,
          drop = TRUE
        )
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      metric = factor(
        metric,
        levels = metrics_to_keep
      )
    )

  if (nrow(output_df) == 0L) {
    stop("No rank-lift line data remain after applying selections.")
  }

  output_df
}


plot_rank_lift_strength <- function(
    plot_df,
    alphas_to_plot,
    spotlight_pct_choice
) {
  plot_df <- plot_df |>
    dplyr::mutate(
      alpha = factor(
        format(alpha, trim = TRUE, scientific = FALSE),
        levels = format(
          alphas_to_plot,
          trim = TRUE,
          scientific = FALSE
        )
      )
    )

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = spotlight_strength,
      y = mean_strength_lift
    )
  ) +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "dashed",
      linewidth = 0.4
    ) +
    ggplot2::geom_vline(
      xintercept = 0,
      linetype = "dotted",
      linewidth = 0.4
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        ymin = q05,
        ymax = q95
      ),
      width = 0.03,
      linewidth = 0.4
    ) +
    ggplot2::geom_line(
      ggplot2::aes(group = 1),
      linewidth = 0.8
    ) +
    ggplot2::geom_point(
      size = 2
    ) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(metric),
      cols = ggplot2::vars(alpha),
      switch = "y",
      drop = FALSE,
      labeller = ggplot2::labeller(
        alpha = function(value) {
          paste0("\u03b1 = ", value)
        }
      )
    ) +
    ggplot2::scale_x_continuous(
      breaks = sort(
        unique(plot_df$spotlight_strength)
      )
    ) +
    ggplot2::labs(
      x = expression(
          p[plain(s)] - p[plain(n)] 
      ),
      y = "Mean normalised rank lift among spotlit nodes",
      title = "Rank lift by spotlight strength",
      subtitle = paste0(
        "Bars show 5th\u201395th percentiles across simulated results; ",
        "spotlight proportion = ",
        scales::percent(
          spotlight_pct_choice,
          accuracy = 1
        )
      )
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.spacing = grid::unit(0.8, "lines"),
      strip.placement = "outside",
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold"),
      strip.text.y.left = ggplot2::element_text(
        angle = 0,
        hjust = 1
      ),
      axis.text.x = ggplot2::element_text(
        angle = 45,
        hjust = 1
      ),
      axis.title.x = ggplot2::element_text(
        margin = ggplot2::margin(t = 10)
      ),
      axis.title.y = ggplot2::element_text(
        margin = ggplot2::margin(r = 10)
      ),
      plot.title = ggplot2::element_text(face = "bold")
    )
}


build_rank_lift_line_plot <- function(
    df,
    metrics_to_keep,
    spotlight_pct_choice,
    alphas_to_plot = NULL
) {
  selected_alphas <- resolve_numeric_selection(
    requested_values = alphas_to_plot,
    available_values = as.numeric(as.character(df$alpha)),
    setting_name = "config$visualisations$alphas_to_plot"
  )

  plot_df <- prepare_rank_lift_strength_df(
    df = df,
    metrics_to_keep = metrics_to_keep,
    spotlight_pct_choice = spotlight_pct_choice,
    alphas_to_plot = selected_alphas
  )

  plot_rank_lift_strength(
    plot_df = plot_df,
    alphas_to_plot = selected_alphas,
    spotlight_pct_choice = spotlight_pct_choice
  )
}
