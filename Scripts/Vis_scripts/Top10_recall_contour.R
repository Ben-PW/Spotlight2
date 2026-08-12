################################################################################
# Script: Top10_recall_contour.R
#
# This script contains the functions required to visualise top10 recall using
# contour plots, as well as interpolating the simulated values
################################################################################

prepare_recall_surface_df <- function(
    df,
    spotlight_pct_choice,
    metrics_to_keep,
    alphas_to_plot
) {
  validate_visualisation_data(
    df = df,
    data_name = "node_rank_df",
    required_columns = c(
      "metric",
      "alpha",
      "spotlight_pct",
      "p_obs_spotlit",
      "p_obs_nonspotlit",
      "recall",
      "n_spotlit_gt_top",
      "top_n_cutoff"
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
    dplyr::filter(
      dplyr::near(spotlight_pct, spotlight_pct_choice),
      metric %in% metrics_to_keep,
      numeric_selection_mask(alpha, selected_alphas)
    ) |>
    dplyr::group_by(
      metric,
      alpha,
      p_obs_nonspotlit,
      p_obs_spotlit
    ) |>
    dplyr::summarise(
      mean_recall = mean(
        recall,
        na.rm = TRUE
      ),
      mean_gt_spotlight_alignment = mean(
        n_spotlit_gt_top / top_n_cutoff,
        na.rm = TRUE
      ),
      n_graphs = sum(!is.na(recall)),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      metric = factor(
        metric,
        levels = metrics_to_keep
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
    stop("No Top-N recall data remain after applying selections.")
  }

  output_df
}


interpolate_recall_surface <- function(
    df,
    grid_n
) {
  df |>
    dplyr::group_by(
      metric,
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
            mean_recall
          ) |>
          tidyr::pivot_wider(
            names_from = p_obs_spotlit,
            values_from = mean_recall,
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
            mean_recall = as.vector(interpolated$z),
            mean_recall = pmin(
              pmax(mean_recall, 0),
              1
            )
          ) |>
          dplyr::select(
            p_obs_nonspotlit,
            p_obs_spotlit,
            mean_recall
          )
      }
    ) |>
    dplyr::ungroup()
}


plot_recall_contour <- function(
    plot_df,
    spotlight_pct_choice,
    top_n_proportion,
    grid_n,
    show_alignment
) {
  x_breaks <- sort(unique(plot_df$p_obs_nonspotlit))
  y_breaks <- sort(unique(plot_df$p_obs_spotlit))
  top_n_label <- make_top_n_label(top_n_proportion)

  recall_breaks <- seq(
    0,
    1,
    by = 0.10
  )

  interpolated_df <- interpolate_recall_surface(
    df = plot_df,
    grid_n = grid_n
  )

  x_range <- diff(range(x_breaks))
  y_range <- diff(range(y_breaks))

  alignment_df <- plot_df |>
    dplyr::group_by(
      metric,
      alpha
    ) |>
    dplyr::summarise(
      mean_gt_spotlight_alignment = mean(
        mean_gt_spotlight_alignment,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      alignment_label = paste0(
        "GT ",
        top_n_label,
        " spotlit: ",
        scales::percent(
          mean_gt_spotlight_alignment,
          accuracy = 1
        )
      ),
      x_position = min(x_breaks) + 0.05 * x_range,
      y_position = max(y_breaks) - 0.05 * y_range
    )

  plot <- ggplot2::ggplot(
    interpolated_df,
    ggplot2::aes(
      x = p_obs_nonspotlit,
      y = p_obs_spotlit,
      z = mean_recall
    )
  ) +
    ggplot2::geom_contour_filled(
      ggplot2::aes(
        fill = ggplot2::after_stat(level_mid)
      ),
      breaks = recall_breaks
    )

  for (contour_break in c(0.50, 0.75)) {
    if (
      length(x_breaks) >= 3L &&
      length(y_breaks) >= 3L &&
      surface_has_contour(
        df = interpolated_df,
        x_column = "p_obs_nonspotlit",
        y_column = "p_obs_spotlit",
        value_column = "mean_recall",
        group_columns = c("metric", "alpha"),
        contour_level = contour_break
      )
    ) {
      plot <- plot +
        ggplot2::geom_contour(
          breaks = contour_break,
          colour = "white",
          linewidth = 0.25
        )
    }
  }

  plot <- plot +
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
      breaks = recall_breaks,
      labels = scales::label_number(accuracy = 0.1),
      limits = c(0, 1),
      name = paste0("Mean ", top_n_label, "\nrecall"),
      guide = ggplot2::guide_coloursteps(
        direction = "vertical",
        show.limits = TRUE,
        ticks = TRUE,
        even.steps = TRUE,
        barheight = grid::unit(8, "cm")
      )
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
      breaks = x_breaks
    ) +
    ggplot2::scale_y_continuous(
      breaks = y_breaks
    ) +
    ggplot2::labs(
      x = expression(p[plain(n)]),
      y = expression(p[plain(s)]),
      title = paste0(
        top_n_label,
        " node recall under spotlighted observation"
      ),
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
      axis.text.x = ggplot2::element_text(
        angle = 45,
        hjust = 1
      ),
      strip.placement = "outside",
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold"),
      strip.text.y.left = ggplot2::element_text(
        angle = 0,
        hjust = 1
      )
    )

  if (show_alignment) {
    plot <- plot +
      ggplot2::geom_label(
        data = alignment_df,
        ggplot2::aes(
          x = x_position,
          y = y_position,
          label = alignment_label
        ),
        inherit.aes = FALSE,
        hjust = 0,
        vjust = 1,
        size = 2.1,
        linewidth = 0,
        fill = "white",
        colour = "black",
        alpha = 0.8
      )
  }

  plot
}


build_top_n_recall_plot <- function(
    df,
    spotlight_pct_choice,
    metrics_to_keep,
    alphas_to_plot = NULL,
    top_n_proportion,
    grid_n = 100L,
    show_alignment = TRUE
) {
  plot_df <- prepare_recall_surface_df(
    df = df,
    spotlight_pct_choice = spotlight_pct_choice,
    metrics_to_keep = metrics_to_keep,
    alphas_to_plot = alphas_to_plot
  )

  plot_recall_contour(
    plot_df = plot_df,
    spotlight_pct_choice = spotlight_pct_choice,
    top_n_proportion = top_n_proportion,
    grid_n = grid_n,
    show_alignment = show_alignment
  )
}
