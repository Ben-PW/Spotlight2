################################################################################
# Script: Rel_bias_contour.R
#
# This script contains the functions required to visualise mean relative bias
# contour plots. It also allows visualisation of mean absolute bias contour
# plots.
################################################################################

network_bias_metric_labels <- c(
  dcent = "Degree centralisation",
  clustering = "Clustering",
  APL = "Average path length"
)


prepare_network_bias_plot_df <- function(
    df,
    metric_choice,
    bias_type,
    spotlight_pct_choice,
    alphas_to_plot
) {
  bias_column <- if (bias_type == "absolute") {
    "abs_relative_bias"
  } else {
    "relative_bias"
  }

  validate_visualisation_data(
    df = df,
    data_name = "mn_abs_rel_bias_nets",
    required_columns = c(
      "metric",
      "target_centralisation",
      "alpha",
      "spotlight_pct",
      "p_obs_spotlit",
      "p_obs_nonspotlit",
      bias_column
    )
  )

  resolve_character_selection(
    requested_values = metric_choice,
    available_values = df$metric,
    setting_name = "config$visualisations$network_bias_metrics"
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
    dplyr::filter(
      metric == metric_choice,
      dplyr::near(spotlight_pct, spotlight_pct_choice),
      numeric_selection_mask(alpha, selected_alphas)
    ) |>
    dplyr::group_by(
      target_centralisation,
      alpha,
      spotlight_pct,
      p_obs_spotlit,
      p_obs_nonspotlit
    ) |>
    dplyr::summarise(
      mean_bias = mean(.data[[bias_column]], na.rm = TRUE),
      q25 = stats::quantile(
        .data[[bias_column]],
        0.25,
        na.rm = TRUE
      ),
      q75 = stats::quantile(
        .data[[bias_column]],
        0.75,
        na.rm = TRUE
      ),
      q05 = stats::quantile(
        .data[[bias_column]],
        0.05,
        na.rm = TRUE
      ),
      q95 = stats::quantile(
        .data[[bias_column]],
        0.95,
        na.rm = TRUE
      ),
      n_reps = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      target_centralisation = ordered_numeric_factor(
        target_centralisation,
        decreasing = TRUE
      ),
      alpha = factor(
        format(alpha, trim = TRUE, scientific = FALSE),
        levels = format(
          selected_alphas,
          trim = TRUE,
          scientific = FALSE
        )
      )
    )

  if (nrow(output_df) == 0L) {
    stop(
      "No network-bias data remain for metric `",
      metric_choice,
      "` and bias type `",
      bias_type,
      "`."
    )
  }

  output_df
}


interpolate_bias_surface <- function(
    df,
    grid_n
) {
  df |>
    dplyr::group_by(
      target_centralisation,
      alpha
    ) |>
    dplyr::group_modify(
      ~ {
        facet_df <- .x |>
          dplyr::arrange(
            p_obs_nonspotlit,
            p_obs_spotlit
          )

        x_values <- sort(
          unique(facet_df$p_obs_nonspotlit)
        )
        y_values <- sort(
          unique(facet_df$p_obs_spotlit)
        )

        z_matrix <- facet_df |>
          dplyr::select(
            p_obs_nonspotlit,
            p_obs_spotlit,
            mean_bias
          ) |>
          tidyr::pivot_wider(
            names_from = p_obs_spotlit,
            values_from = mean_bias,
            names_sort = TRUE
          ) |>
          dplyr::arrange(p_obs_nonspotlit) |>
          dplyr::select(-p_obs_nonspotlit) |>
          as.matrix()

        interpolated <- akima::bicubic.grid(
          x = x_values,
          y = y_values,
          z = z_matrix,
          xlim = range(x_values),
          ylim = range(y_values),
          nx = grid_n,
          ny = grid_n
        )

        tidyr::expand_grid(
          p_obs_spotlit = interpolated$y,
          p_obs_nonspotlit = interpolated$x
        ) |>
          dplyr::mutate(
            mean_bias = as.vector(interpolated$z)
          ) |>
          dplyr::select(
            p_obs_nonspotlit,
            p_obs_spotlit,
            mean_bias
          )
      }
    ) |>
    dplyr::ungroup()
}


make_bias_breaks <- function(
    values,
    bias_type
) {
  maximum_magnitude <- max(
    abs(values),
    na.rm = TRUE
  )

  limit <- ceiling(maximum_magnitude * 10) / 10

  if (!is.finite(limit) || limit == 0) {
    limit <- 0.1
  }

  if (bias_type == "absolute") {
    seq(0, limit, length.out = 11)
  } else {
    seq(-limit, limit, length.out = 21)
  }
}


plot_network_bias_contour <- function(
    plot_df,
    metric_label,
    bias_type,
    spotlight_pct_choice,
    grid_n
) {
  x_breaks <- sort(unique(plot_df$p_obs_nonspotlit))
  y_breaks <- sort(unique(plot_df$p_obs_spotlit))

  interpolated_df <- interpolate_bias_surface(
    df = plot_df,
    grid_n = grid_n
  )

  bias_breaks <- make_bias_breaks(
    values = interpolated_df$mean_bias,
    bias_type = bias_type
  )

  fill_label <- if (bias_type == "absolute") {
    "Mean absolute\nrelative bias"
  } else {
    "Mean\nrelative bias"
  }

  plot <- ggplot2::ggplot(
    interpolated_df,
    ggplot2::aes(
      x = p_obs_nonspotlit,
      y = p_obs_spotlit,
      z = mean_bias
    )
  ) +
    ggplot2::geom_contour_filled(
      ggplot2::aes(
        fill = ggplot2::after_stat(level_mid)
      ),
      breaks = bias_breaks
    )

  if (
    bias_type == "relative" &&
    length(x_breaks) >= 3L &&
    length(y_breaks) >= 3L &&
    surface_has_contour(
      df = interpolated_df,
      x_column = "p_obs_nonspotlit",
      y_column = "p_obs_spotlit",
      value_column = "mean_bias",
      group_columns = c("target_centralisation", "alpha"),
      contour_level = 0.01
    )
  ) {
    plot <- plot +
      ggplot2::geom_contour(
        breaks = 0.01,
        colour = "white",
        linetype = "solid",
        linewidth = 0.2
      )
  }

  if (
    bias_type == "relative" &&
    length(x_breaks) >= 3L &&
    length(y_breaks) >= 3L &&
    surface_has_contour(
      df = interpolated_df,
      x_column = "p_obs_nonspotlit",
      y_column = "p_obs_spotlit",
      value_column = "mean_bias",
      group_columns = c("target_centralisation", "alpha"),
      contour_level = -0.01
    )
  ) {
    plot <- plot +
      ggplot2::geom_contour(
        breaks = -0.01,
        colour = "white",
        linetype = "dashed",
        linewidth = 0.2
      )
  }

  plot +
    ggplot2::geom_point(
      data = plot_df,
      ggplot2::aes(
        x = p_obs_nonspotlit,
        y = p_obs_spotlit
      ),
      inherit.aes = FALSE,
      colour = "black",
      size = 0.6,
      alpha = 0.6
    ) +
    ggplot2::annotate(
      "segment",
      x = min(x_breaks),
      y = min(y_breaks),
      xend = max(x_breaks),
      yend = max(y_breaks),
      colour = "black",
      linewidth = 0.5,
      linetype = "dashed"
    ) +
    ggplot2::scale_fill_viridis_b(
      option = "magma",
      breaks = bias_breaks,
      labels = scales::label_number(accuracy = 0.1),
      limits = range(bias_breaks),
      name = fill_label,
      guide = ggplot2::guide_coloursteps(
        direction = "vertical",
        show.limits = TRUE,
        ticks = TRUE,
        even.steps = FALSE,
        barheight = grid::unit(8, "cm")
      )
    ) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(target_centralisation),
      cols = ggplot2::vars(alpha),
      #switch = "y",
      drop = FALSE,
      labeller = ggplot2::label_bquote( #convert factors back to numeric
        rows = C == .(
          as.numeric(as.character(target_centralisation))
        ),
        cols = alpha == .(
          as.numeric(as.character(alpha))
        )
      )
    ) +
    ggplot2::scale_x_continuous(
      breaks = x_breaks
    ) +
    ggplot2::scale_y_continuous(
      breaks = y_breaks
    ) +
    ggplot2::labs(
      x = expression(p[plain(n)]),
      y = expression(p[plain(s)]),
      title = metric_label,
      subtitle = paste0(
        "Spotlight proportion = ",
        scales::percent(
          spotlight_pct_choice,
          accuracy = 1
        )
      )
    ) +
    ggplot2::coord_fixed() +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      strip.placement = "outside",
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold"),
      strip.text.y.left = ggplot2::element_text(
        angle = 0,
        hjust = 0
      )
    )
}


build_network_bias_plots <- function(
    df,
    metrics,
    bias_types,
    spotlight_pct_choice,
    alphas_to_plot = NULL,
    grid_n = 100L
) {
  plots <- list()

  for (metric in metrics) {
    for (bias_type in bias_types) {
      plot_df <- prepare_network_bias_plot_df(
        df = df,
        metric_choice = metric,
        bias_type = bias_type,
        spotlight_pct_choice = spotlight_pct_choice,
        alphas_to_plot = alphas_to_plot
      )

      plot_name <- paste(
        "network_bias",
        metric,
        bias_type,
        sep = "_"
      )

      plots[[plot_name]] <- plot_network_bias_contour(
        plot_df = plot_df,
        metric_label = network_bias_metric_labels[[metric]],
        bias_type = bias_type,
        spotlight_pct_choice = spotlight_pct_choice,
        grid_n = grid_n
      )
    }
  }

  plots
}
