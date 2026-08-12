################################################################################
# Script: Rank_lift_contour.R
#
# Contains functions required to generate the mean normalised rank lift 
# contour plots
################################################################################

# Primary plotting function
build_rank_lift_contour_plot <- function(
    df,
    metrics_to_keep,
    alphas_to_plot,
    spotlight_pct_choice,
    grid_n
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

  surface_df <- df |>
    dplyr::mutate(
      metric = as.character(metric),
      alpha = as.numeric(as.character(alpha)),
      spotlight_pct = as.numeric(as.character(spotlight_pct)),
      p_obs_spotlit = as.numeric(as.character(p_obs_spotlit)),
      p_obs_nonspotlit = as.numeric(as.character(p_obs_nonspotlit))
    ) |>
    dplyr::filter(
      metric %in% metrics_to_keep,
      numeric_selection_mask(alpha, selected_alphas),
      dplyr::near(
        spotlight_pct,
        spotlight_pct_choice
      )
    ) |>
    dplyr::group_by(
      metric,
      alpha,
      p_obs_nonspotlit,
      p_obs_spotlit
    ) |>
    dplyr::summarise(
      mean_rank_lift = mean(
        mean_norm_rank_lift_spotlit,
        na.rm = TRUE
      ),
      n_graphs = sum(
        !is.na(mean_norm_rank_lift_spotlit)
      ),
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

  if (nrow(surface_df) == 0L) {
    stop("No rank-lift contour data remain after applying selections.")
  }

  x_breaks <- sort(
    unique(surface_df$p_obs_nonspotlit)
  )
  y_breaks <- sort(
    unique(surface_df$p_obs_spotlit)
  )

  interpolated_df <- surface_df |>
    dplyr::group_by(
      metric,
      alpha
    ) |>
    dplyr::group_modify(
      ~ {
        x_values <- sort(
          unique(.x$p_obs_nonspotlit)
        )
        y_values <- sort(
          unique(.x$p_obs_spotlit)
        )

        z_matrix <- .x |>
          dplyr::select(
            p_obs_nonspotlit,
            p_obs_spotlit,
            mean_rank_lift
          ) |>
          tidyr::pivot_wider(
            names_from = p_obs_spotlit,
            values_from = mean_rank_lift,
            names_sort = TRUE
          ) |>
          dplyr::arrange(p_obs_nonspotlit) |>
          dplyr::select(-p_obs_nonspotlit) |>
          as.matrix()

        if (anyNA(z_matrix)) {
          stop(
            "Missing probability combinations for metric ",
            .y$metric,
            ", alpha ",
            .y$alpha,
            "."
          )
        }

        interpolated <- akima::bicubic.grid(
          x = x_values,
          y = y_values,
          z = z_matrix,
          xlim = range(x_values),
          ylim = range(y_values),
          nx = grid_n,
          ny = grid_n
        )

        observed_minimum <- min(
          .x$mean_rank_lift,
          na.rm = TRUE
        )
        observed_maximum <- max(
          .x$mean_rank_lift,
          na.rm = TRUE
        )

        tidyr::expand_grid(
          p_obs_spotlit = interpolated$y,
          p_obs_nonspotlit = interpolated$x
        ) |>
          dplyr::mutate(
            mean_rank_lift = as.vector(interpolated$z),
            mean_rank_lift = pmin(
              pmax(
                mean_rank_lift,
                observed_minimum
              ),
              observed_maximum
            )
          )
      }
    ) |>
    dplyr::ungroup()

  fill_limit <- max(
    abs(interpolated_df$mean_rank_lift),
    na.rm = TRUE
  )

  fill_limit <- ceiling(fill_limit * 20) / 20

  if (!is.finite(fill_limit) || fill_limit == 0) {
    fill_limit <- 0.05
  }

  contour_breaks <- seq(
    -fill_limit,
    fill_limit,
    length.out = 11
  )

  plot <- ggplot2::ggplot(
    interpolated_df,
    ggplot2::aes(
      x = p_obs_nonspotlit,
      y = p_obs_spotlit,
      z = mean_rank_lift
    )
  ) +
    ggplot2::geom_contour_filled(
      ggplot2::aes(
        fill = ggplot2::after_stat(level_mid)
      ),
      breaks = contour_breaks
    )

  if (
    length(x_breaks) >= 3L &&
    length(y_breaks) >= 3L &&
    surface_has_contour(
      df = interpolated_df,
      x_column = "p_obs_nonspotlit",
      y_column = "p_obs_spotlit",
      value_column = "mean_rank_lift",
      group_columns = c("metric", "alpha"),
      contour_level = 0.01
    )
  ) {
    plot <- plot +
      ggplot2::geom_contour(
        breaks = 0.01,
        colour = "white",
        linewidth = 0.45,
        linetype = "solid"
      )
  }

  if (
    length(x_breaks) >= 3L &&
    length(y_breaks) >= 3L &&
    surface_has_contour(
      df = interpolated_df,
      x_column = "p_obs_nonspotlit",
      y_column = "p_obs_spotlit",
      value_column = "mean_rank_lift",
      group_columns = c("metric", "alpha"),
      contour_level = -0.01
    )
  ) {
    plot <- plot +
      ggplot2::geom_contour(
        breaks = -0.01,
        colour = "white",
        linewidth = 0.45,
        linetype = "dashed"
      )
  }

  plot +
    ggplot2::geom_point(
      data = surface_df,
      ggplot2::aes(
        x = p_obs_nonspotlit,
        y = p_obs_spotlit
      ),
      inherit.aes = FALSE,
      size = 0.55,
      alpha = 0.65
    ) +
    ggplot2::geom_abline(
      intercept = 0,
      slope = 1,
      linetype = "dashed"
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
    ggplot2::scale_fill_viridis_b(
      option = "magma",
      name = "Mean normalised\nrank lift",
      breaks = contour_breaks,
      limits = c(-fill_limit, fill_limit),
      guide = ggplot2::guide_coloursteps(
        direction = "vertical",
        show.limits = TRUE,
        ticks = TRUE,
        even.steps = FALSE,
        barheight = grid::unit(8, "cm")
      )
    ) +
    ggplot2::scale_x_continuous(
      breaks = x_breaks
    ) +
    ggplot2::scale_y_continuous(
      breaks = y_breaks
    ) +
    ggplot2::coord_fixed() +
    ggplot2::labs(
      x = expression(p[plain(n)]),
      y = expression(p[plain(s)]),
      title = "Rank lift among spotlit nodes",
      subtitle = paste0(
        "Mean normalised rank lift across simulated networks; ",
        "spotlight proportion = ",
        scales::percent(
          spotlight_pct_choice,
          accuracy = 1
        )
      )
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      panel.spacing = grid::unit(0.7, "lines"),
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
      plot.title = ggplot2::element_text(face = "bold")
    )
}

# # Wrapper to apply to list of spotlight percentages
# 
# build_rank_lift_contour_plots <- function(
#     df,
#     main_spotlight_pct,
#     metrics_to_keep,
#     alphas_to_plot = NULL,
#     grid_n = 100L,
#     include_supplementary = TRUE
# ) {
#   available_spotlight_pcts <- sort(
#     unique(as.numeric(as.character(df$spotlight_pct)))
#   )
#   
#   # Ensure the requested main value exists in the data.
#   resolve_numeric_selection(
#     requested_values = main_spotlight_pct,
#     available_values = available_spotlight_pcts,
#     setting_name = "config$visualisations$main_spotlight_pct"
#   )
#   
#   spotlight_pcts_to_plot <- if (include_supplementary) {
#     available_spotlight_pcts
#   } else {
#     main_spotlight_pct
#   }
#   
#   plots <- purrr::map(
#     spotlight_pcts_to_plot,
#     function(spotlight_pct) {
#       build_rank_lift_contour_plot(
#         df = df,
#         metrics_to_keep = metrics_to_keep,
#         alphas_to_plot = alphas_to_plot,
#         spotlight_pct_choice = spotlight_pct,
#         grid_n = grid_n
#       )
#     }
#   )
#   
#   names(plots) <- paste0(
#     "rank_lift_contour_",
#     vapply(
#       spotlight_pcts_to_plot,
#       make_spotlight_pct_slug,
#       character(1)
#     )
#   )
#   
#   attr(plots, "spotlight_pcts") <- spotlight_pcts_to_plot
#   
#   plots
# }
