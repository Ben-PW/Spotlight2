################################################################################

# Script for spotlight strength plot, where strength is defined as just p_obs_spotlit
# subtract p_obs_nonspotlit. Obviously a crude method without any sort of control
# for missingness etc but a useful exploratory plot

################################################################################
library(dplyr)
library(stringr)

make_spotlit_rank_lift_df <- function(
    df,
    metric_choice,
    spotlight_pct_choice = 0.10
) {
  
  df |>
    filter(
      metric == metric_choice,
      spotlight_pct == spotlight_pct_choice
    ) |>
    mutate(
      gt_cent = case_when(
        str_detect(dataset, "_c1$") ~ "Low",
        str_detect(dataset, "_c3$") ~ "Medium",
        str_detect(dataset, "_c5$") ~ "High",
        TRUE ~ "WARNING"
      )
    ) |>
    filter(gt_cent != "WARNING") |>
    group_by(
      gt_cent,
      alpha,
      spotlight_pct,
      p_obs_spotlit,
      p_obs_nonspotlit
    ) |>
    summarise(
      mean_rank_lift = mean(
        mean_norm_rank_lift_spotlit,
        na.rm = TRUE
      ),
      n_graphs = sum(!is.na(mean_norm_rank_lift_spotlit)),
      .groups = "drop"
    ) |>
    mutate(
      gt_cent = factor(
        gt_cent,
        levels = c("High", "Medium", "Low")
      ),
      alpha = factor(alpha),
      p_obs_spotlit = factor(p_obs_spotlit),
      p_obs_nonspotlit = factor(p_obs_nonspotlit)
    )
}

degree_rank_lift_df <- make_spotlit_rank_lift_df(
  rank_lift_df2,
  metric_choice = "Degree",
  spotlight_pct_choice = 0.10
)

betweenness_rank_lift_df <- make_spotlit_rank_lift_df(
  rank_lift_df2,
  metric_choice = "Betweenness",
  spotlight_pct_choice = 0.10
)

closeness_rank_lift_df <- make_spotlit_rank_lift_df(
  rank_lift_df2,
  metric_choice = "Closeness",
  spotlight_pct_choice = 0.10
)

eigenvector_rank_lift_df <- make_spotlit_rank_lift_df(
  rank_lift_df2,
  metric_choice = "Eigenvector",
  spotlight_pct_choice = 0.10
)

################################################################################

#degree_strength_df <- betweenness_rank_lift_df |>
#  dplyr::mutate(
#    spotlight_strength = #log(
#      round(
#      as.numeric(as.character(p_obs_spotlit)) - ####### CHANGE HERE ( - <- /)
#        as.numeric(as.character(p_obs_nonspotlit)),
#      2
#    )
#    #)
#    
#  ) |>
#  dplyr::group_by(
#    gt_cent,
#    alpha,
#    spotlight_pct,
#    spotlight_strength
#  ) |>
#  dplyr::summarise(
#    mean_strength_lift = mean(mean_rank_lift, na.rm = TRUE),
#    q25 = quantile(mean_rank_lift, 0.25, na.rm = TRUE),
#    q75 = quantile(mean_rank_lift, 0.75, na.rm = TRUE),
#    n_probability_pairs = dplyr::n(),
#    .groups = "drop"
#  )

################################################################################

#plot_rank_lift_by_strength <- function(
#    strength_df,
#    metric_label
#) {
#  
#  ggplot(
#    strength_df,
#    aes(
#      x = spotlight_strength,
#      y = mean_strength_lift
#    )
#  ) +
#    geom_hline(
#      yintercept = 0,
#      linetype = "dashed",
#      linewidth = 0.4
#    ) +
#    geom_vline(
#      xintercept = 1, ####### CHANGE HERE (0 <- 1, altere for division plots)
#      linetype = "dotted",
#      linewidth = 0.4
#    ) +
#    geom_errorbar(
#      aes(
#        ymin = q25,
#        ymax = q75
#      ),
#      width = 0.03
#    ) +
#    geom_line(
#      aes(group = 1),
#      linewidth = 0.8
#    ) +
#    geom_point(size = 2) +
#    facet_grid(
#      gt_cent ~ alpha,
#      labeller = label_both
#    ) +
#    scale_x_continuous(
#      breaks = log(c(0.25, 0.5, 1, 2, 4)),
#      labels = c(
#        "0.25×",
#        "0.5×",
#        "1×\n(equal)",
#        "2×",
#        "4×"
#      )
#    ) +
#    labs(
#      x = expression(
#        "Spotlight strength (" *
#          p[obs~spotlit] / p[obs~nonspotlit] *
#          ")"
#      ),
#      y = "Mean normalised rank lift among spotlit nodes",
#      title = paste(
#        metric_label,
#        "rank lift by spotlight strength"
#      ),
#      subtitle = paste0(
#        "Bars show 5th - 95th percentiles across probability pairs; ",
#        "spotlight proportion = ",
#        scales::percent(
#          unique(strength_df$spotlight_pct)
#        )
#      )
#    ) +
#    theme_minimal()
#}

################################################################################

#plot_rank_lift_by_strength(
#  degree_strength_df,
#  "Betweenness centrality"
#)

################################################################################

# Aggregated plots look feasible

################################################################################

################################################################################

# More minimal aggregation

################################################################################

#plot_rank_lift_by_strength2 <- function(
#    strength_df,
#    metric_label
#) {
#  
#  ggplot(
#    strength_df,
#    aes(
#      x = spotlight_strength,
#      y = mean_strength_lift
#    )
#  ) +
#    geom_hline(
#      yintercept = 0,
#      linetype = "dashed",
#      linewidth = 0.4
#    ) +
#    geom_vline(
#      xintercept = log(1), ####### CHANGE HERE (0 <- 1, altere for division plots)
#      linetype = "dotted",
#      linewidth = 0.4
#    ) +
#    geom_errorbar(
#      aes(
#        ymin = q25,
#        ymax = q75
#      ),
#      width = 0.03
#    ) +
#    geom_line(
#      aes(group = 1),
#      linewidth = 0.8
#    ) +
#    geom_point(size = 2) +
#    facet_wrap(
#      ~ alpha
#    ) +
#    scale_x_continuous(
#      breaks = log(c(0.25, 0.5, 1, 2, 4)),
#      labels = c(
#        "0.25×",
#        "0.5×",
#        "1×\n(equal)",
#        "2×",
#        "4×"
#      )
#    ) +
#    labs(
#      x = expression(
#        "Spotlight strength (" *
#          p[obs~spotlit] - p[obs~nonspotlit] *
#          ")"
#      ),
#      y = "Mean normalised rank lift among spotlit nodes",
#      title = paste(
#        metric_label,
#        "rank lift by spotlight strength"
#      ),
#      subtitle = paste0(
#        "Bars show 5th - 95th percentiles across probability pairs; ",
#        "spotlight proportion = ",
#        scales::percent(
#          unique(strength_df$spotlight_pct)
#        )
#      )
#    ) +
#    theme_minimal()
#}

####################### updated version of above with alpha filtering

plot_rank_lift_by_strength2 <- function(
    strength_df,
    metric_label,
    alphas = sort(unique(strength_df$alpha))
) {
  
  plot_df <- strength_df %>%
    dplyr::filter(
      alpha %in% alphas
    ) %>%
    dplyr::mutate(
      alpha = factor(
        alpha,
        levels = alphas
      )
    )
  
  if (nrow(plot_df) == 0) {
    stop("No rows remain after filtering to the selected alpha values.")
  }
  
  ggplot(
    plot_df,
    aes(
      x = spotlight_strength,
      y = mean_strength_lift
    )
  ) +
    geom_hline(
      yintercept = 0,
      linetype = "dashed",
      linewidth = 0.4
    ) +
    geom_vline(
      xintercept = log(1),
      linetype = "dotted",
      linewidth = 0.4
    ) +
    geom_errorbar(
      aes(
        ymin = q25,
        ymax = q75
      ),
      width = 0.03
    ) +
    geom_line(
      aes(group = 1),
      linewidth = 0.8
    ) +
    geom_point(size = 2) +
    facet_wrap(
      ~ alpha
    ) +
    labs(
      x = expression(
        "Spotlight strength (" *
          p[obs~spotlit] - p[obs~nonspotlit] *
          ")"
      ),
      y = "Mean normalised rank lift among spotlit nodes",
      title = paste(
        metric_label,
        "rank lift by spotlight strength"
      ),
      subtitle = paste0(
        "Bars show 5th–95th percentiles across probability pairs; ",
        "spotlight proportion = ",
        scales::percent(
          unique(plot_df$spotlight_pct)
        )
      )
    ) +
    theme_minimal()
}

degree_strength_df2 <- betweenness_rank_lift_df |>
  dplyr::mutate(
    spotlight_strength = #log(
      round(
      as.numeric(as.character(p_obs_spotlit)) - ####### CHANGE HERE ( - <- /)
        as.numeric(as.character(p_obs_nonspotlit)),
      2 # was 6 for accurate log calculation
    )
    #)
  ) |>
  dplyr::group_by(
    alpha,
    spotlight_pct,
    spotlight_strength
  ) |>
  dplyr::summarise(
    mean_strength_lift = mean(mean_rank_lift, na.rm = TRUE),
    q25 = quantile(mean_rank_lift, 0.05, na.rm = TRUE),
    q75 = quantile(mean_rank_lift, 0.95, na.rm = TRUE),
    n_probability_pairs = dplyr::n(),
    .groups = "drop"
  )

plot_rank_lift_by_strength2(
  degree_strength_df2,
  "Betweenness centrality",
  alphas = c(0, 2, 8)
)

################################################################################

# Maximum aggregation

################################################################################

# IMPORTANT

# THIS PLOT USES 05-95 IQR

make_pooled_rank_df <- function(plot_df) {
  
  plot_df |>
    dplyr::mutate(
      p_obs_spotlit = as.numeric(as.character(p_obs_spotlit)),
      p_obs_nonspotlit = as.numeric(as.character(p_obs_nonspotlit)),
      spotlight_strength = log(round(
        p_obs_spotlit / p_obs_nonspotlit, ######## CHANGE HERE (- <- /)
        5
      ))
    ) |>
    dplyr::group_by(
      spotlight_pct,
      spotlight_strength
    ) |>
    dplyr::summarise(
      mean_strength_lift = mean(mean_rank_lift, na.rm = TRUE),
      q05 = quantile(mean_rank_lift, 0.05, na.rm = TRUE),
      q95 = quantile(mean_rank_lift, 0.95, na.rm = TRUE),
      n_observations = sum(!is.na(mean_rank_lift)),
      .groups = "drop"
    )
}

plot_pooled_rank_lift <- function(
    strength_df,
    metric_label
) {
  
  ggplot(
    strength_df,
    aes(
      x = spotlight_strength,
      y = mean_strength_lift
    )
  ) +
    geom_hline(
      yintercept = 0,
      linetype = "dashed",
      linewidth = 0.4
    ) +
    geom_vline(
      xintercept = log(1),
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
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
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
      x = expression(
        "log(" *
          p[spotlit] / p[nonspotlit] *
          ")"
      ),
      y = "Mean normalised rank lift among spotlit nodes",
      title = paste(metric_label, "rank lift by spotlight strength"),
      subtitle = paste0(
        "Spotlight proportion = ",
        scales::percent(unique(strength_df$spotlight_pct))
      )
    ) +
    theme_minimal()
}

bet_strength_pooled <- make_pooled_rank_df(
  betweenness_rank_lift_df
)

bet_pooled <- plot_pooled_rank_lift(
  bet_strength_pooled,
  "Betweenness centrality"
)

deg_strength_pooled <- make_pooled_rank_df(
  degree_rank_lift_df
)

deg_pooled <- plot_pooled_rank_lift(
  deg_strength_pooled,
  "Degree centrality"
)

clo_strength_pooled <- make_pooled_rank_df(
  closeness_rank_lift_df
)

clo_pooled <- plot_pooled_rank_lift(
  clo_strength_pooled,
  "Closeness centrality"
)

eig_strength_pooled <- make_pooled_rank_df(
  eigenvector_rank_lift_df
)

eig_pooled <- plot_pooled_rank_lift(
  eig_strength_pooled,
  "Eigenvector centrality"
)

pooled_rank_plot <- patchwork::wrap_plots(
  bet_pooled,
  deg_pooled,
  clo_pooled,
  eig_pooled,
  ncol = 2,
  guides = "collect"
) +
  patchwork::plot_annotation(
    tag_levels = "A"
  ) &
  theme(
    legend.position = "bottom"
  )

pooled_rank_plot
