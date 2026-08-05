################################################################################

# This is the script for plots of mean absolute relative bias and mean relative
# bias. As the two are pretty similar I'm chucking it all in here

################################################################################

################################################################################

# Function to create the dataframe according to desired specifications. Currently 
# plan to keep spotlight at 10%

################################################################################

make_network_bias_plot_df <- function(
    df,
    metric_choice,
    bias_type = c("absolute", "relative"),
    spotlight_pct_choice = 0.10
) {
  
  bias_type <- match.arg(bias_type)
  
  bias_col <- if (bias_type == "absolute") {
    "abs_relative_bias"
  } else {
    "relative_bias"
  }
  
  df |>
    dplyr::filter(
      metric == metric_choice,
      spotlight_pct == spotlight_pct_choice
    ) |>
    dplyr::group_by(
      baseline_centralisation,
      alpha,
      spotlight_pct,
      p_obs_spotlit,
      p_obs_nonspotlit
    ) |>
    dplyr::summarise(
      mean_bias = mean(.data[[bias_col]], na.rm = TRUE),
      q25 = quantile(.data[[bias_col]], 0.25, na.rm = TRUE),
      q75 = quantile(.data[[bias_col]], 0.75, na.rm = TRUE),
      q05 = quantile(.data[[bias_col]], 0.05, na.rm = TRUE),
      q95 = quantile(.data[[bias_col]], 0.95, na.rm = TRUE),
      n_reps = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      baseline_centralisation = factor(
        baseline_centralisation,
        levels = c("High", "Med", "Low")
      ),
      alpha = factor(alpha)
    )
}

################################################################################

# Function to create contour plots

################################################################################

################### Old function (less problematic)

bias_breaks <- seq(-1, 1, by = 0.1)

label_breaks <- seq(-1, 1, by = 0.2)

plot_network_bias_contour <- function(plot_df, 
                                      metric_label, 
                                      fill_label, 
                                      alpha_vals = c(0, 1, 2, 4, 8)) {
  
  plot_df <- plot_df |>
    filter(
      alpha %in% alpha_vals
    ) 
  ggplot(
    plot_df,
    aes(
      x = p_obs_nonspotlit,
      y = p_obs_spotlit,
      z = mean_bias
    )
  ) +
    #geom_contour_filled(bins = bins) +
#    geom_point(
#      colour = "black",
#      size = 1.2,
#      alpha = 0.7
#    ) +
    ggplot2::geom_contour_filled(
      ggplot2::aes(z = mean_bias,
                   fill = ggplot2::after_stat(level_mid)),
      breaks = bias_breaks
    ) +
    ggplot2::geom_contour(
      ggplot2::aes(z = mean_bias),
      breaks = 0,
      colour = "white",
      linewidth = 0.3
    ) +
    geom_contour(
      aes(z = mean_bias),
      breaks = 0.01,
      colour = "white",
      linetype = "dashed",
      linewidth = 0.2
    ) +
    annotate(
      "segment",
      x = 0.2,
      y = 0.2,
      xend = 1.0,
      yend = 1.0,
      colour = "black",
      linewidth = 0.5,
      linetype = "dashed"
    ) +
    ggplot2::scale_fill_viridis_b(
      option = "magma",
      breaks = label_breaks,
      limits = range(bias_breaks),
      guide = ggplot2::guide_coloursteps(
        direction = "vertical",
        show.limits = TRUE,
        ticks = TRUE,
        even.steps = FALSE,
        barheight = unit(8, "cm")
      )
    ) +
    facet_grid(
      baseline_centralisation ~ alpha,
      labeller = label_both
    ) +
    scale_x_continuous(
      breaks = sort(unique(plot_df$p_obs_nonspotlit))
    ) +
    scale_y_continuous(
      breaks = sort(unique(plot_df$p_obs_spotlit))
    ) +
    labs(
      x = "Observation probability: non-spotlit ties",
      y = "Observation probability: spotlit ties",
      fill = fill_label,
      title = metric_label
    ) +
    coord_fixed() +
    theme_minimal()
}

############################################################################

# New function with better interpolation

interpolate_bias_surface <- function(
    df,
    grid_n = 100
) {
  
  df |>
    dplyr::group_by(
      baseline_centralisation,
      alpha
    ) |>
    dplyr::group_modify(
      ~ {
        
        facet_df <- .x |>
          dplyr::arrange(
            p_obs_nonspotlit,
            p_obs_spotlit
          )
        
        x_vals <- sort(unique(facet_df$p_obs_nonspotlit))
        y_vals <- sort(unique(facet_df$p_obs_spotlit))
        
        # Create z matrix where:
        # rows correspond to x values
        # columns correspond to y values
        z_matrix <- facet_df |>
          dplyr::select(
            p_obs_nonspotlit,
            p_obs_spotlit,
            mean_bias
          ) |>
          tidyr::pivot_wider(
            names_from = p_obs_spotlit,
            values_from = mean_bias
          ) |>
          dplyr::arrange(p_obs_nonspotlit) |>
          dplyr::select(-p_obs_nonspotlit) |>
          as.matrix()
        
        interp_out <- akima::bicubic.grid(
          x = x_vals,
          y = y_vals,
          z = z_matrix,
          xlim = range(x_vals),
          ylim = range(y_vals),
          nx = grid_n,
          ny = grid_n
        )
        
        tidyr::expand_grid(
          p_obs_spotlit = interp_out$y,
          p_obs_nonspotlit = interp_out$x
        ) |>
          dplyr::mutate(
            mean_bias = as.vector(interp_out$z)
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

plot_network_bias_contour <- function(
    plot_df,
    metric_label,
    fill_label,
    alpha_vals = c(0, 1, 2, 4, 8),
    grid_n = 100
) {
  
  plot_df <- plot_df |>
    dplyr::filter(
      alpha %in% alpha_vals
    )
  
  # Save the original axis values before interpolation
  x_breaks <- sort(unique(plot_df$p_obs_nonspotlit))
  y_breaks <- sort(unique(plot_df$p_obs_spotlit))
  
  # Interpolate each centralisation × alpha surface separately
  interp_df <- interpolate_bias_surface(
    df = plot_df,
    grid_n = grid_n
  )
  
  ggplot2::ggplot(
    interp_df,
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
    ) +
#    ggplot2::geom_contour(
#      breaks = 0,
#      colour = "white",
#      linewidth = 0.3
#    ) +
    ggplot2::geom_contour(
      breaks = 0.01,
      colour = "white",
      linetype = "solid",
      linewidth = 0.2
    ) +
    ggplot2::geom_contour(
      breaks = -0.01,
      colour = "white",
      linetype = "dashed",
      linewidth = 0.2
    ) +
    
    # Show the actual simulated conditions
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
      labels = bias_labels,
      limits = range(bias_breaks),
      guide = ggplot2::guide_coloursteps(
        direction = "vertical",
        show.limits = TRUE,
        ticks = TRUE,
        even.steps = FALSE,
        barheight = grid::unit(8, "cm")
      )
    ) +
    ggplot2::facet_grid(
      baseline_centralisation ~ alpha,
      labeller = ggplot2::label_both
    ) +
    ggplot2::scale_x_continuous(
      breaks = x_breaks
    ) +
    ggplot2::scale_y_continuous(
      breaks = y_breaks
    ) +
    ggplot2::labs(
      x = "Observation probability: non-spotlit ties",
      y = "Observation probability: spotlit ties",
      fill = fill_label,
      title = metric_label
    ) +
    ggplot2::coord_fixed() +
    ggplot2::theme_minimal()
}

################################################################################

#mn_abs_rel_bias_nets |>
#  dplyr::filter(metric == "clustering") |>
#  dplyr::arrange(desc(abs_relative_bias)) |>
#  dplyr::select(
#    dataset,
#    replicate_id,
#    alpha,
#    spotlight_pct,
#    p_obs_spotlit,
#    p_obs_nonspotlit,
#    baseline_centralisation,
#    observed_value,
#    gt_value,
#    relative_bias,
#    abs_relative_bias
#    ) |>
#  head(20)

# Above shows that clustering has some crazy high abs rel bias values, as high 
# as 6, typically low centralisation low density networks

################################### Degree centralisation

dcent_abs_df <- make_network_bias_plot_df(
  df = mn_abs_rel_bias_nets,
  metric_choice = "dcent",
  bias_type = "absolute",
  spotlight_pct_choice = 0.10
)

dcent_rel_df <- make_network_bias_plot_df(
  df = mn_abs_rel_bias_nets,
  metric_choice = "dcent",
  bias_type = "relative",
  spotlight_pct_choice = 0.01
)


# Slice plots seem quite hard to read, contour plots are likely better

bias_labels <- ifelse(
  round(bias_breaks * 10) %% 2 == 0,
  sprintf("%.1f", bias_breaks),
  ""
)

bias_breaks <- seq(-1, 1, by = 0.1)

plot_network_bias_contour(
  plot_df = dcent_abs_df,
  metric_label = "Degree centralisation",
  fill_label = "Mean absolute\nrelative bias"
)

plot_network_bias_contour(
  plot_df = dcent_rel_df,
  metric_label = "Degree centralisation",
  fill_label = "Mean\nrelative bias"
  #bins = 15
)

################################ Clustering

clust_abs_df <- make_network_bias_plot_df(
  df = mn_abs_rel_bias_nets,
  metric_choice = "clustering",
  bias_type = "absolute",
  spotlight_pct_choice = 0.10
)

clust_rel_df <- make_network_bias_plot_df(
  df = mn_abs_rel_bias_nets,
  metric_choice = "clustering",
  bias_type = "relative", 
  spotlight_pct_choice = 0.10
)

plot_network_bias_contour(
  plot_df = clust_rel_df,
  metric_label = "Clustering",
  fill_label = "Mean\n relative bias"
)

############################### APL

# IMPORTANT: APL is now excluded from analysis due to interpretation issues

APL_abs_df <- make_network_bias_plot_df(
  df = mn_abs_rel_bias_nets,
  metric_choice = "APL",
  bias_type = "absolute",
  spotlight_pct_choice = 0.10
)

APL_rel_df <- make_network_bias_plot_df(
  df = mn_abs_rel_bias_nets,
  metric_choice = "APL",
  bias_type = "relative",
  spotlight_pct_choice = 0.10
)

plot_network_bias_contour(
  plot_df = APL_rel_df,
  metric_label = "APL",
  fill_label = "Mean relative\n bias"
)


