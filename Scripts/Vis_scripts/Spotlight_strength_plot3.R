# make_rank_lift_strength_composite_df <- function(
#     df,
#     metrics_to_keep = c(
#       "Degree",
#       "Betweenness",
#       "Eigenvector",
#       "Closeness"
#     ),
#     spotlight_pct_choice = 0.10
# ) {
#   
#   output_df <- df |>
#     dplyr::filter(
#       metric %in% metrics_to_keep,
#       dplyr::near(
#         spotlight_pct,
#         spotlight_pct_choice
#       )
#     ) |>
#     dplyr::mutate(
#       p_obs_spotlit = as.numeric(
#         as.character(p_obs_spotlit)
#       ),
#       p_obs_nonspotlit = as.numeric(
#         as.character(p_obs_nonspotlit)
#       ),
#       spotlight_strength = round(
#         p_obs_spotlit - p_obs_nonspotlit,
#         2
#       )
#     ) |>
#     dplyr::group_by(
#       metric,
#       alpha,
#       spotlight_pct,
#       spotlight_strength
#     ) |>
#     dplyr::summarise(
#       mean_strength_lift = mean(
#         mean_norm_rank_lift_spotlit,
#         na.rm = TRUE
#       ),
#       q05 = stats::quantile(
#         mean_norm_rank_lift_spotlit,
#         probs = 0.05,
#         na.rm = TRUE
#       ),
#       q95 = stats::quantile(
#         mean_norm_rank_lift_spotlit,
#         probs = 0.95,
#         na.rm = TRUE
#       ),
#       n_observations = sum(
#         !is.na(mean_norm_rank_lift_spotlit)
#       ),
#       n_probability_pairs = dplyr::n_distinct(
#         interaction(
#           p_obs_spotlit,
#           p_obs_nonspotlit,
#           drop = TRUE
#         )
#       ),
#       .groups = "drop"
#     ) |>
#     dplyr::mutate(
#       metric = factor(
#         metric,
#         levels = metrics_to_keep
#       )
#     )
#   
#   if (nrow(output_df) == 0) {
#     stop(
#       paste0(
#         "No rows found for the selected metrics at spotlight_pct = ",
#         spotlight_pct_choice,
#         "."
#       )
#     )
#   }
#   
#   output_df
# }
# 
# rank_lift_strength_composite_df <-
#   make_rank_lift_strength_composite_df(
#     df = rank_lift_df2,
#     metrics_to_keep = c(
#       "Degree",
#       "Betweenness",
#       "Eigenvector",
#       "Closeness"
#     ),
#     spotlight_pct_choice = 0.10
#   )
# 
# plot_rank_lift_strength_composite <- function(
#     strength_df,
#     alphas = sort(unique(strength_df$alpha)),
#     spotlight_pct_choice = 0.10
# ) {
#   
#   plot_df <- strength_df |>
#     dplyr::filter(
#       alpha %in% alphas,
#       dplyr::near(
#         spotlight_pct,
#         spotlight_pct_choice
#       )
#     ) |>
#     dplyr::mutate(
#       alpha = factor(
#         alpha,
#         levels = alphas
#       )
#     )
#   
#   if (nrow(plot_df) == 0) {
#     stop(
#       paste0(
#         "No rows remain after filtering to the selected alpha values ",
#         "and spotlight_pct = ",
#         spotlight_pct_choice,
#         "."
#       )
#     )
#   }
#   
#   ggplot2::ggplot(
#     plot_df,
#     ggplot2::aes(
#       x = spotlight_strength,
#       y = mean_strength_lift
#     )
#   ) +
#     ggplot2::geom_hline(
#       yintercept = 0,
#       linetype = "dashed",
#       linewidth = 0.4
#     ) +
#     ggplot2::geom_vline(
#       xintercept = 0,
#       linetype = "dotted",
#       linewidth = 0.4
#     ) +
#     ggplot2::geom_errorbar(
#       ggplot2::aes(
#         ymin = q05,
#         ymax = q95
#       ),
#       width = 0.03,
#       linewidth = 0.4
#     ) +
#     ggplot2::geom_line(
#       ggplot2::aes(group = 1),
#       linewidth = 0.8
#     ) +
#     ggplot2::geom_point(
#       size = 2
#     ) +
#     ggplot2::facet_grid(
#       rows = ggplot2::vars(metric),
#       cols = ggplot2::vars(alpha),
#       switch = "y",
#       drop = FALSE,
#       labeller = ggplot2::labeller(
#         alpha = function(x) {
#           paste0("\u03b1 = ", x)
#         }
#       )
#     ) +
#     ggplot2::scale_x_continuous(
#       breaks = sort(
#         unique(plot_df$spotlight_strength)
#       )
#     ) +
#     ggplot2::labs(
#       x = expression(
#         "Spotlight strength (" *
#           p[obs~spotlit] - p[obs~nonspotlit] *
#           ")"
#       ),
#       y = "Mean normalised rank lift among spotlit nodes",
#       title = "Rank lift by spotlight strength",
#       subtitle = paste0(
#         "Bars show 5th–95th percentiles across probability pairs; ",
#         "spotlight proportion = ",
#         scales::percent(
#           spotlight_pct_choice,
#           accuracy = 1
#         )
#       )
#     ) +
#     ggplot2::theme_minimal() +
#     ggplot2::theme(
#       panel.grid.minor = ggplot2::element_blank(),
#       
#       panel.spacing = grid::unit(
#         0.8,
#         "lines"
#       ),
#       
#       strip.placement = "outside",
#       strip.background = ggplot2::element_blank(),
#       
#       strip.text = ggplot2::element_text(
#         face = "bold"
#       ),
#       
#       strip.text.y.left = ggplot2::element_text(
#         angle = 0,
#         hjust = 1
#       ),
#       
#       axis.text.x = ggplot2::element_text(
#         angle = 45,
#         hjust = 1
#       ),
#       
#       axis.title.x = ggplot2::element_text(
#         margin = ggplot2::margin(
#           t = 10
#         )
#       ),
#       
#       axis.title.y = ggplot2::element_text(
#         margin = ggplot2::margin(
#           r = 10
#         )
#       ),
#       
#       plot.title = ggplot2::element_text(
#         face = "bold"
#       )
#     )
# }
# 
# rank_lift_strength_composite <-
#   plot_rank_lift_strength_composite(
#     strength_df = rank_lift_strength_composite_df,
#     alphas = c(0, 2, 8),
#     spotlight_pct_choice = 0.10
#   )
# 
# rank_lift_strength_composite

################################################################################

# Trying contour plots

################################################################################

# This basically does the same as the whole network metric contours, but the 
# Data wrangling is done within the function as it was getting quite messy 
# otherwise

plot_rank_lift_contours <- function(
    df,
    metrics_to_keep = c(
      "Degree",
      "Betweenness",
      "Eigenvector",
      "Closeness"
    ),
    alphas = c(0, 2, 8),
    spotlight_pct_choice = 0.10,
    grid_n = 100
) {
  
  # Calculate mean outcome at each simulated probability combination
  
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
      alpha %in% alphas,
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
        alpha,
        levels = alphas
      )
    )
  
  if (nrow(surface_df) == 0) {
    stop(
      "No observations remain after filtering."
    )
  }
  
  x_breaks <- sort(
    unique(surface_df$p_obs_nonspotlit)
  )
  
  y_breaks <- sort(
    unique(surface_df$p_obs_spotlit)
  )
  
  # Interpolate each metric × alpha surface separately

  
  interp_df <- surface_df |>
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
            values_from = mean_rank_lift
          ) |>
          dplyr::arrange(
            p_obs_nonspotlit
          ) |>
          dplyr::select(
            -p_obs_nonspotlit
          ) |>
          as.matrix()
        
        if (anyNA(z_matrix)) {
          stop(
            paste0(
              "Missing probability combinations for metric ",
              .y$metric,
              ", alpha ",
              .y$alpha,
              "."
            )
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
        
        # Prevent bicubic interpolation from creating values outside
        # the observed range for that particular surface
        observed_min <- min(
          .x$mean_rank_lift,
          na.rm = TRUE
        )
        
        observed_max <- max(
          .x$mean_rank_lift,
          na.rm = TRUE
        )
        
        tidyr::expand_grid(
          p_obs_spotlit = interpolated$y,
          p_obs_nonspotlit = interpolated$x
        ) |>
          dplyr::mutate(
            mean_rank_lift = as.vector(
              interpolated$z
            ),
            mean_rank_lift = pmin(
              pmax(
                mean_rank_lift,
                observed_min
              ),
              observed_max
            )
          )
      }
    ) |>
    dplyr::ungroup()
  

  # Use a common fill scale across all panels
  
  fill_limit <- max(
    abs(interp_df$mean_rank_lift),
    na.rm = TRUE
  )
  
  # Round upwards to a reasonably clean limit
  fill_limit <- ceiling(
    fill_limit * 20
  ) / 20
  
  if (fill_limit == 0) {
    fill_limit <- 0.05
  }
  
  contour_breaks <- seq(
    -fill_limit,
    fill_limit,
    length.out = 11
  )
  

  ################################ Plot

  
  ggplot2::ggplot(
    interp_df,
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
    ) +
    
    # Zero-rank-lift boundary
#    ggplot2::geom_contour(
#      breaks = 0,
#      colour = "black",
#      linewidth = 0.45
#    ) +
    
    ggplot2::geom_contour(
      breaks = 0.01,
      colour = "white",
      linewidth = 0.45,
      linetype = "solid"
    ) +
    
    ggplot2::geom_contour(
      breaks = -0.01,
      colour = "white",
      linewidth = 0.45,
      linetype = "dashed"
    ) +
    
    # Actual simulated probability combinations
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
    
    # Equal-observation reference line
    ggplot2::geom_abline(
      intercept = 0,
      slope = 1,
      linetype = "dashed"#,
#      linewidth = 0.45
    ) +
    
    ggplot2::facet_grid(
      rows = ggplot2::vars(metric),
      cols = ggplot2::vars(alpha),
      switch = "y",
      drop = FALSE,
      labeller = ggplot2::labeller(
        alpha = function(x) {
          paste0("\u03b1 = ", x)
        }
      )
    ) +
    
#    ggplot2::scale_fill_steps2(
#      midpoint = 0,
#      limits = c(
#        -fill_limit,
#        fill_limit
#      ),
#      breaks = contour_breaks,
#      oob = scales::squish,
#      name = "Mean normalised\nrank lift"
#    ) +
    ggplot2::scale_fill_viridis_b(
      option = "magma",
      name = "Mean normalised\nrank lift",
      breaks = contour_breaks,
      limits = c(
        -fill_limit, 
        fill_limit
      ),
      guide = ggplot2::guide_coloursteps(
        direction = "vertical",
        show.limits = TRUE,
        ticks = TRUE,
        even.steps = FALSE,
        barheight = unit(8, "cm")
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
      x = "Observation probability: non-spotlit ties",
      y = "Observation probability: spotlit ties",
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
      
      panel.spacing = grid::unit(
        0.7,
        "lines"
      ),
      
      strip.placement = "outside",
      strip.background = ggplot2::element_blank(),
      
      strip.text = ggplot2::element_text(
        face = "bold"
      ),
      
      strip.text.y.left = ggplot2::element_text(
        angle = 0,
        hjust = 1
      ),
      
      axis.text.x = ggplot2::element_text(
        angle = 45,
        hjust = 1
      ),
      
      plot.title = ggplot2::element_text(
        face = "bold"
      )
    )
}

rank_lift_contour_composite <- plot_rank_lift_contours(
  df = rank_lift_df2,
  metrics_to_keep = c(
    "Degree",
    "Betweenness",
    "Eigenvector",
    "Closeness"
  ),
  alphas = c(0, 2, 8),
  spotlight_pct_choice = 0.10,
  grid_n = 100
)

rank_lift_contour_composite
