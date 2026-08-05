################################################################################

# Updated version of Node_rank_bias to use the new spotlight strength 
# metric


# Two stage aggregation required to stop conditions with more graph rows 
# having greater weight than others (e.g. spotlight strength = 0.2 is
# very common)

library(stringr)
library(ggplot2)

# Make the dataframe

make_topN_strength_df <- function(
    df,
    metric_choice,
    outcome_col,
    spotlight_pct_choice = 0.10
) {
  
  df %>%
    filter(
      metric == metric_choice,
      spotlight_pct == spotlight_pct_choice
    ) %>%
    mutate(
      spotlight_strength = log(round(
        p_obs_spotlit / p_obs_nonspotlit,
        6
        )
      ),
      
      gt_cent = case_when(
        str_detect(dataset, "_c1$") ~ "Low",
        str_detect(dataset, "_c3$") ~ "Med",
        str_detect(dataset, "_c5$") ~ "High",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(gt_cent)) %>%
    
    # First calculate the mean for each distinct structural and
    # observation-probability condition
    group_by(
      gt_cent,
      alpha,
      spotlight_pct,
      p_obs_spotlit,
      p_obs_nonspotlit,
      spotlight_strength
    ) %>%
    summarise(
      condition_mean = mean(
        .data[[outcome_col]],
        na.rm = TRUE
      ),
      n_graphs = sum(
        !is.na(.data[[outcome_col]])
      ),
      .groups = "drop"
    ) %>%
    
    # Then pool condition-level means across alpha, centralisation,
    # and equivalent probability pairs
    group_by(
      spotlight_pct,
      spotlight_strength
    ) %>%
    summarise(
      mean_value = mean(
        condition_mean,
        na.rm = TRUE
      ),
      
      q05 = quantile(
        condition_mean,
        0.05,
        na.rm = TRUE
      ),
      
      q95 = quantile(
        condition_mean,
        0.95,
        na.rm = TRUE
      ),
      
      minimum = min(
        condition_mean,
        na.rm = TRUE
      ),
      
      maximum = max(
        condition_mean,
        na.rm = TRUE
      ),
      
      n_conditions = sum(
        !is.na(condition_mean)
      ),
      
      n_graphs = sum(
        n_graphs,
        na.rm = TRUE
      ),
      
      .groups = "drop"
    ) %>%
    arrange(spotlight_strength)
}

################################################################################

# Plotting function

plot_topN_strength_outcome <- function(
    plot_df,
    metric_choice,
    outcome_label,
    y_limits = NULL,
    reference_line = NULL
) {
  
  p <- ggplot(
    plot_df,
    aes(
      x = spotlight_strength,
      y = mean_value
    )
  )
  
  if (!is.null(reference_line)) {
    p <- p +
      geom_hline(
        yintercept = reference_line,
        linetype = "dashed",
        linewidth = 0.4
      )
  }
  
  p <- p +
    geom_vline(
      xintercept = 0,
      linetype = "dotted",
      linewidth = 0.4
    ) +
    geom_ribbon(
      aes(
        ymin = q05,
        ymax = q95
      ),
      alpha = 0.2
    ) +
    geom_line(
      linewidth = 1
    ) +
    geom_point(
      size = 2
    ) +
    scale_x_continuous(
      breaks = log(c(0.25, 0.5, 1, 2, 4)),
      labels = c(
        "0.25×",
        "0.5×",
        "1×\n(equal)",
        "2×",
        "4×"
      )
    ) +
    labs(
      x = "Spotlight strength",
      y = outcome_label,
      title = paste(
        metric_choice,
        "-",
        outcome_label
      ),
      subtitle = paste0(
        "Pooled mean, ribbon shows the 5th–95th percentile "
      )
    ) +
    theme_minimal()
  
  if (!is.null(y_limits)) {
    p <- p +
      coord_cartesian(
        ylim = y_limits
      )
  }
  
  p
}

#################################################################################

topN_outcomes <- c(
  "precision",
  "recall",
  "jaccard_overlap",
  "spotlight_lift_obs_top",
  "spotlight_lift_gt_top",
  "excess_spotlight_lift"
)

topN_labels <- c(
  precision = "Top-10 precision",
  recall = "Top-10 recall",
  jaccard_overlap = "Top-10 Jaccard overlap",
  spotlight_lift_obs_top = "Observed Top-10 spotlight lift",
  spotlight_lift_gt_top = "GT Top-10 spotlight lift",
  excess_spotlight_lift = "Excess observed spotlight lift"
)

topN_metrics <- c(
  "Degree",
  "Betweenness",
  "Closeness",
  "Eigenvector"
)

##################################################################################

topN_strength_plots <- list()

for (m in topN_metrics) {
  
  for (outcome in topN_outcomes) {
    
    plot_df <- make_topN_strength_df(
      df = node_rank_df,
      metric_choice = m,
      outcome_col = outcome,
      spotlight_pct_choice = 0.05
    )
    
    y_limits <- NULL
    reference_line <- NULL
    
    if (
      outcome %in% c(
        "precision",
        "recall",
        "jaccard_overlap"
      )
    ) {
      y_limits <- c(0, 1)
    }
    
    if (
      outcome %in% c(
        "spotlight_lift_obs_top",
        "spotlight_lift_gt_top"
      )
    ) {
      reference_line <- 1
    }
    
    if (
      outcome == "excess_spotlight_lift"
    ) {
      reference_line <- 0
    }
    
    plot_name <- paste(
      m,
      outcome,
      sep = "_"
    )
    
    topN_strength_plots[[plot_name]] <-
      plot_topN_strength_outcome(
        plot_df = plot_df,
        metric_choice = m,
        outcome_label = topN_labels[[outcome]],
        y_limits = y_limits,
        reference_line = reference_line
      )
  }
}

###############################################################################

topN_strength_plots$Degree_excess_spotlight_lift
topN_strength_plots$Betweenness_excess_spotlight_lift
topN_strength_plots$Closeness_excess_spotlight_lift
topN_strength_plots$Eigenvector_excess_spotlight_lift

topN_strength_plots$Degree_recall

topN_strength_plots$Degree_jaccard_overlap
topN_strength_plots$Betweenness_jaccard_overlap
topN_strength_plots$Closeness_jaccard_overlap
topN_strength_plots$Eigenvector_jaccard_overlap

###############################################################################


################################################################################

# Contour plots for recall

################################################################################

################################################################################
# Prepare mean recall surface
################################################################################

make_recall_surface_df <- function(
    df,
    metric_choice,
    spotlight_pct_choice = 0.10
) {
  
  df |>
    dplyr::filter(
      metric == metric_choice,
      spotlight_pct == spotlight_pct_choice
    ) |>
    dplyr::mutate(
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
      gt_cent,
      alpha,
      p_obs_nonspotlit,
      p_obs_spotlit
    ) |>
    dplyr::summarise(
      mean_recall = mean(
        recall,
        na.rm = TRUE
      ),
      
      # Proportion of GT Top-N nodes that were spotlighted
      mean_gt_spotlight_alignment = mean(
        n_spotlit_gt_top / top_n_cutoff,
        na.rm = TRUE
      ),
      
      n_graphs = sum(
        !is.na(recall)
      ),
      
      .groups = "drop"
    ) |>
    dplyr::mutate(
      gt_cent = factor(
        gt_cent,
        levels = c(
          "High",
          "Medium",
          "Low"
        )
      )
    )
}

################################################################################
# Bicubic interpolation of recall
################################################################################

interpolate_recall_surface <- function(
    df,
    grid_n = 100
) {
  
  df |>
    dplyr::group_by(
      gt_cent,
      alpha
    ) |>
    dplyr::group_modify(
      ~ {
        
        facet_df <- .x |>
          dplyr::arrange(
            p_obs_nonspotlit,
            p_obs_spotlit
          )
        
        x_vals <- sort(
          unique(facet_df$p_obs_nonspotlit)
        )
        
        y_vals <- sort(
          unique(facet_df$p_obs_spotlit)
        )
        
        expected_cells <- length(x_vals) * length(y_vals)
        
        if (nrow(facet_df) != expected_cells) {
          stop(
            "Incomplete probability grid for gt_cent = ",
            unique(facet_df$gt_cent),
            ", alpha = ",
            unique(facet_df$alpha)
          )
        }
        
        z_matrix <- facet_df |>
          dplyr::select(
            p_obs_nonspotlit,
            p_obs_spotlit,
            mean_recall
          ) |>
          tidyr::pivot_wider(
            names_from = p_obs_spotlit,
            values_from = mean_recall
          ) |>
          dplyr::arrange(
            p_obs_nonspotlit
          ) |>
          dplyr::select(
            -p_obs_nonspotlit
          ) |>
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
            mean_recall = as.vector(
              interp_out$z
            ),
            
            # Bicubic interpolation can overshoot slightly
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

################################################################################
# Plot interpolated Top-N recall
################################################################################

plot_recall_contour <- function(
    plot_df,
    metric_label,
    alpha_vals = c(0, 1, 2, 4, 8),
    grid_n = 100,
    show_alignment = TRUE
) {
  
  plot_df <- plot_df |>
    dplyr::filter(
      alpha %in% alpha_vals
    )
  
  x_breaks <- sort(
    unique(plot_df$p_obs_nonspotlit)
  )
  
  y_breaks <- sort(
    unique(plot_df$p_obs_spotlit)
  )
  
  recall_breaks <- seq(
    0,
    1,
    by = 0.10
  )
  
  interp_df <- interpolate_recall_surface(
    df = plot_df,
    grid_n = grid_n
  )
  
  # Alignment is fixed across the probability grid within each facet.
  alignment_df <- plot_df |>
    dplyr::group_by(
      gt_cent,
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
        "GT Top-N spotlighted: ",
        scales::percent(
          mean_gt_spotlight_alignment,
          accuracy = 1
        )
      ),
      x_position = min(x_breaks) + 0.1,
      y_position = max(y_breaks) - 0.78
    )
  
  p <- ggplot2::ggplot(
    interp_df,
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
    ) +
    
    # Optional reference contours
    ggplot2::geom_contour(
      breaks = c(0.50, 0.75),
      colour = "white",
      linewidth = 0.25
    ) +
    
    # Show the actual simulated probability combinations
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
    
    # Equal-observation reference line
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
      labels = scales::label_number(
        accuracy = 0.1
      ),
      limits = c(0, 1),
      guide = ggplot2::guide_coloursteps(
        direction = "vertical",
        show.limits = TRUE,
        ticks = TRUE,
        even.steps = TRUE,
        barheight = grid::unit(8, "cm")
      )
    ) +
    
    ggplot2::facet_grid(
      gt_cent ~ alpha,
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
      fill = "Mean Top-10%\nrecall",
      title = metric_label
    ) +
    
    ggplot2::coord_fixed() +
    ggplot2::theme_minimal()
  
  if (show_alignment) {
    p <- p +
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
        size = 2.4,
        linewidth = 0,
        fill = "white",
        colour = "black",
        alpha = 0.8
      )
  }
  
  p
}

####################### Plots

# Degree

degree_recall_df <- make_recall_surface_df(
  df = node_rank_df,
  metric_choice = "Degree",
  spotlight_pct_choice = 0.10
)

degree_recall_plot <- plot_recall_contour(
  plot_df = degree_recall_df,
  metric_label = "Recovery of ground-truth Top-10% degree nodes"
)

degree_recall_plot

# Betweenness

betweenness_recall_df <- make_recall_surface_df(
  df = node_rank_df,
  metric_choice = "Betweenness",
  spotlight_pct_choice = 0.10
)

betweenness_recall_plot <- plot_recall_contour(
  plot_df = betweenness_recall_df,
  metric_label = "Recovery of ground-truth Top-10% degree nodes"
)

betweenness_recall_plot

# Eigenvector

eigenvector_recall_df <- make_recall_surface_df(
  df = node_rank_df,
  metric_choice = "Eigenvector",
  spotlight_pct_choice = 0.10
)

eigenvector_recall_plot <- plot_recall_contour(
  plot_df = eigenvector_recall_df,
  metric_label = "Recovery of ground-truth Top-10% degree nodes"
)

eigenvector_recall_plot

# Closeness

closeness_recall_df <- make_recall_surface_df(
  df = node_rank_df,
  metric_choice = "Closeness",
  spotlight_pct_choice = 0.10
)

closeness_recall_plot <- plot_recall_contour(
  plot_df = closeness_recall_df,
  metric_label = "Recovery of ground-truth Top-10% degree nodes"
)

closeness_recall_plot

################################################################################

# Composite plot

################################################################################

make_recall_surface_df_composite <- function(
    df,
    spotlight_pct_choice = 0.10,
    metrics_to_keep = c("Degree", "Betweenness", "Eigenvector", "Closeness")
) {
  
  df |>
    dplyr::filter(
      spotlight_pct == spotlight_pct_choice,
      metric %in% metrics_to_keep
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
        levels = c("Degree", "Betweenness", "Eigenvector")
      )
    )
}

################################################################################
# Interpolate
################################################################################

interpolate_recall_surface <- function(
    df,
    grid_n = 100
) {
  
  df |>
    dplyr::group_by(
     # gt_cent,
      alpha
    ) |>
    dplyr::group_modify(
      ~ {
        
        facet_df <- .x |>
          dplyr::arrange(
            p_obs_nonspotlit,
            p_obs_spotlit
          )
        
        x_vals <- sort(
          unique(facet_df$p_obs_nonspotlit)
        )
        
        y_vals <- sort(
          unique(facet_df$p_obs_spotlit)
        )
        
        expected_cells <- length(x_vals) * length(y_vals)
        
        z_matrix <- facet_df |>
          dplyr::select(
            p_obs_nonspotlit,
            p_obs_spotlit,
            mean_recall
          ) |>
          tidyr::pivot_wider(
            names_from = p_obs_spotlit,
            values_from = mean_recall
          ) |>
          dplyr::arrange(
            p_obs_nonspotlit
          ) |>
          dplyr::select(
            -p_obs_nonspotlit
          ) |>
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
            mean_recall = as.vector(
              interp_out$z
            ),
            
            # Bicubic interpolation can overshoot slightly
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

################################################################################
# Plot
################################################################################

plot_recall_contour <- function(
    plot_df,
    metric_label,
    alpha_vals = c(0, 1, 2, 4, 8),
    grid_n = 100,
    show_alignment = TRUE
) {
  
  plot_df <- plot_df |>
    dplyr::filter(
      alpha %in% alpha_vals
    )
  
  x_breaks <- sort(
    unique(plot_df$p_obs_nonspotlit)
  )
  
  y_breaks <- sort(
    unique(plot_df$p_obs_spotlit)
  )
  
  recall_breaks <- seq(
    0,
    1,
    by = 0.10
  )
  
  interp_df <- interpolate_recall_surface(
    df = plot_df,
    grid_n = grid_n
  )
  
  # Alignment is fixed across the probability grid within each facet.
  alignment_df <- plot_df |>
    dplyr::group_by(
      #gt_cent,
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
        "GT Top-N spotlighted: ",
        scales::percent(
          mean_gt_spotlight_alignment,
          accuracy = 1
        )
      ),
      x_position = min(x_breaks) + 0.1,
      y_position = max(y_breaks) - 0.78
    )
  
  p <- ggplot2::ggplot(
    interp_df,
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
    ) +
    
    # Optional reference contours
    ggplot2::geom_contour(
      breaks = c(0.50, 0.75),
      colour = "white",
      linewidth = 0.25
    ) +
    
    # Show the actual simulated probability combinations
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
    
    # Equal-observation reference line
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
      labels = scales::label_number(
        accuracy = 0.1
      ),
      limits = c(0, 1),
      guide = ggplot2::guide_coloursteps(
        direction = "vertical",
        show.limits = TRUE,
        ticks = TRUE,
        even.steps = TRUE,
        barheight = grid::unit(8, "cm")
      )
    ) +
    
    ggplot2::facet_grid(
       ~ alpha,
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
      fill = "Mean Top-10%\nrecall",
      title = metric_label
    ) +
    
    ggplot2::coord_fixed() +
    ggplot2::theme_minimal()
  
  if (show_alignment) {
    p <- p +
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
        size = 2.4,
        linewidth = 0,
        fill = "white",
        colour = "black",
        alpha = 0.8
      )
  }
  
  p
}

################################################################################

# Composite plot

################################################################################


############ Make dataframe

make_recall_surface_df_composite <- function(
    df,
    spotlight_pct_choice = 0.10,
    metrics_to_keep = c("Degree", "Betweenness", "Eigenvector", "Closeness")
) {
  
  df |>
    dplyr::filter(
      spotlight_pct == spotlight_pct_choice,
      metric %in% metrics_to_keep
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
        levels = metrics_to_keep#c("Degree", "Betweenness", "Eigenvector", "Closeness")
      )
    )
}

#################### Interpolate over alpha and metric

interpolate_recall_surface_composite <- function(
    df,
    grid_n = 100
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
        
        x_vals <- sort(unique(facet_df$p_obs_nonspotlit))
        y_vals <- sort(unique(facet_df$p_obs_spotlit))
        
        z_matrix <- facet_df |>
          dplyr::select(
            p_obs_nonspotlit,
            p_obs_spotlit,
            mean_recall
          ) |>
          tidyr::pivot_wider(
            names_from = p_obs_spotlit,
            values_from = mean_recall
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
            mean_recall = as.vector(interp_out$z),
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


################### Plot composite

plot_recall_contour_composite <- function(
    plot_df,
    alpha_vals = c(0, 1, 2, 4, 8),
    grid_n = 100,
    show_alignment = TRUE
) {
  
  plot_df <- plot_df |>
    dplyr::filter(
      alpha %in% alpha_vals
    )
  
  x_breaks <- sort(unique(plot_df$p_obs_nonspotlit))
  y_breaks <- sort(unique(plot_df$p_obs_spotlit))
  
  recall_breaks <- seq(
    0,
    1,
    by = 0.10
  )
  
  interp_df <- interpolate_recall_surface_composite(
    df = plot_df,
    grid_n = grid_n
  )
  
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
        "Obs / GT topN: ",
        scales::percent(
          mean_gt_spotlight_alignment,
          accuracy = 1
        )
      ),
      x_position = min(x_breaks) + 0.20,
      y_position = max(y_breaks) - 0.70
    )
  
  p <- ggplot2::ggplot(
    interp_df,
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
    ) +
    ggplot2::geom_contour(
      breaks = c(0.50, 0.75),
      colour = "white",
      linewidth = 0.25
    ) +
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
      labels = scales::label_number(
        accuracy = 0.1
      ),
      limits = c(0, 1),
      guide = ggplot2::guide_coloursteps(
        direction = "vertical",
        show.limits = TRUE,
        ticks = TRUE,
        even.steps = TRUE,
        barheight = grid::unit(8, "cm")
      )
    ) +
    ggplot2::facet_grid(
      metric ~ alpha,
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
      fill = "Mean Top-10%\nrecall",
      title = "Top-10% node recall under spotlighted observation"#,
      #subtitle = "Spotlight proportion = 10%"
    ) +
    ggplot2::coord_fixed() +
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
  
  if (show_alignment) {
    p <- p +
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
  
  p
}

node_rank_recall_df <- make_recall_surface_df_composite(
  df = node_rank_df,
  spotlight_pct_choice = 0.10
)

node_rank_recall_composite <- plot_recall_contour_composite(
  plot_df = node_rank_recall_df,
  alpha_vals = c(0, 1, 2, 4, 8),
  grid_n = 100,
  show_alignment = TRUE
)

node_rank_recall_composite
