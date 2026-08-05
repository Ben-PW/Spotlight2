#################################################################################

# This is the script for the sampling coverage heatmaps. 

#################################################################################

# Transform the queried data as necessary
coverage_heatmap_df <- coverage_heatmap_df |>
  dplyr::mutate(
    centralisation_band = factor(
      centralisation_band,
      levels = c("High", "Medium", "Low")
    ),
    
    alpha = factor(
      alpha,
      levels = sort(unique(alpha))
    ),
    
    spotlight_pct = factor(
      spotlight_pct,
      levels = sort(unique(spotlight_pct)),
      labels = scales::percent(
        sort(unique(spotlight_pct)),
        accuracy = 1
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

# Create plotting function so it can be applied to all spotlight percentages (not
# actually sure how much difference they will make so will have to check)

plot_coverage_heatmap <- function(
    df,
    spotlight_pct_choice,
    show_values = TRUE
) {
  
  plot_df <- df |>
    dplyr::filter(
      spotlight_pct == spotlight_pct_choice
    ) |>
    dplyr::mutate(
      text_colour = dplyr::if_else(
        mean_missingness < 0.45,
        "black",
        "white"
      )
    )
  
  p <- ggplot2::ggplot(
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
      centralisation_band ~ alpha,
      labeller = ggplot2::labeller(
        centralisation_band = function(x) {
          paste(x, "centralisation")
        },
        alpha = function(x) {
          paste0("alpha = ", x)
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
      x = "Observation probability: non-spotlit ties",
      y = "Observation probability: spotlit ties",
      title = paste(
        "Tie missingness under spotlighted observation:",
        spotlight_pct_choice,
        "of nodes spotlit"
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
    p <- p +
      ggplot2::geom_text(
        ggplot2::aes(
          label = scales::percent(
            mean_missingness,
            accuracy = 1
          ),
          colour = text_colour
        ),
        size = 2.7,
        show.legend = FALSE
      ) +
      ggplot2::scale_colour_identity()
  }
  
  p
}

# Generate one plot per spotlight percentage

coverage_heatmaps <- lapply(
  levels(coverage_heatmap_df$spotlight_pct),
  function(sp) {
    plot_coverage_heatmap(
      df = coverage_heatmap_df,
      spotlight_pct_choice = sp,
      show_values = TRUE
    )
  }
)

names(coverage_heatmaps) <-
  levels(coverage_heatmap_df$spotlight_pct)

coverage_heatmaps$`5%`
coverage_heatmaps$`1%`
coverage_heatmaps$`10%`
