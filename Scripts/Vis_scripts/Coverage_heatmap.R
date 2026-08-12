################################################################################
# Script: Coverage_heatmap.R
#
# Function-only module for sampling-coverage heatmaps.
################################################################################

prepare_coverage_heatmap_df <- function(
    df,
    alphas_to_plot
) {
  validate_visualisation_data(
    df = df,
    data_name = "coverage_heatmap_df",
    required_columns = c(
      "target_centralisation",
      "alpha",
      "spotlight_pct",
      "p_obs_spotlit",
      "p_obs_nonspotlit",
      "mean_missingness"
    )
  )

  selected_alphas <- resolve_numeric_selection(
    requested_values = alphas_to_plot,
    available_values = df$alpha,
    setting_name = "config$visualisations$alphas_to_plot"
  )

  output_df <- df |>
    dplyr::filter(
      numeric_selection_mask(alpha, selected_alphas)
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

  if (nrow(output_df) == 0L) {
    stop("No coverage data remain after applying visualisation selections.")
  }

  output_df
}


plot_coverage_heatmap <- function(
    df,
    spotlight_pct_choice,
    show_values = TRUE
) {
  plot_df <- df |>
    dplyr::filter(
      dplyr::near(spotlight_pct, spotlight_pct_choice)
    ) |>
    dplyr::mutate(
      text_colour = dplyr::if_else(
        mean_missingness < 0.45,
        "black",
        "white"
      )
    )

  if (nrow(plot_df) == 0L) {
    stop(
      "No coverage data found for spotlight_pct = ",
      spotlight_pct_choice,
      "."
    )
  }

  plot <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = p_obs_nonspotlit,
      y = p_obs_spotlit,
      fill = mean_missingness
    )
  ) +
    ggplot2::geom_tile(
      colour = "white",
      linewidth = 0.4
    ) +
    ggplot2::facet_grid(
      target_centralisation ~ alpha,
      labeller = ggplot2::labeller(
        target_centralisation = function(value) {
          paste(value, "centralisation")
        },
        alpha = function(value) {
          paste0("alpha = ", value)
        }
      )
    ) +
    ggplot2::scale_fill_viridis_c(
      option = "magma",
      direction = -1,
      limits = c(0, 1),
      oob = scales::squish,
      labels = scales::label_percent(accuracy = 1),
      name = "Mean tie\nmissingness"
    ) +
    ggplot2::coord_equal() +
    ggplot2::labs(
      x = expression(p[plain(n)]),
      y = expression(p[plain(s)]),
      title = paste0(
        "Tie missingness under spotlighted observation: ",
        scales::percent(spotlight_pct_choice, accuracy = 1),
        " of nodes spotlit"
      ),
      subtitle = paste(
        "Values averaged across ground-truth networks,",
        "network sizes and average-degree conditions"
      )
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold"),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "right"
    )

  if (show_values) {
    plot <- plot +
      ggplot2::geom_text(
        ggplot2::aes(
          label = scales::percent(mean_missingness, accuracy = 1),
          colour = text_colour
        ),
        size = 2.7,
        show.legend = FALSE
      ) +
      ggplot2::scale_colour_identity()
  }

  plot
}

# Slightly oddly coded, used to have functionality to plot supplemental plots
# within the function but that has been moved to main
build_coverage_heatmaps <- function(
    df,
    main_spotlight_pct,
    alphas_to_plot = NULL,
    show_values = TRUE
) {
  available_spotlight_pcts <- sort(
    unique(as.numeric(as.character(df$spotlight_pct)))
  )
  
  resolve_numeric_selection(
    requested_values = main_spotlight_pct,
    available_values = available_spotlight_pcts,
    setting_name = "config$visualisations$main_spotlight_pct"
  )
  
  prepared_df <- prepare_coverage_heatmap_df(
    df = df,
    alphas_to_plot = alphas_to_plot
  )
  
  plots <- stats::setNames(
    lapply(
      main_spotlight_pct,
      function(spotlight_pct) {
        plot_coverage_heatmap(
          df = prepared_df,
          spotlight_pct_choice = spotlight_pct,
          show_values = show_values
        )
      }
    ),
    paste0(
      "coverage_",
      vapply(
        main_spotlight_pct,
        make_spotlight_pct_slug,
        character(1)
      )
    )
  )
  
  attr(plots, "spotlight_pcts") <- main_spotlight_pct
  
  plots
}