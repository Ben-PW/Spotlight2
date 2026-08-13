################################################################################
# Illustrating the spotlight assignment and observation processes
################################################################################

library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(patchwork)

# Ensure the current spotlight assignment function is available
source(here::here("Scripts", "Spotlight_helpers.R"))


################################################################################
# 1. Select one representative ground-truth network
################################################################################

datasets <- readRDS(here::here("Data","Datasets_final"))
# Change this object if another network gives a cleaner visualisation
chosen_network <- datasets$n60_ad3_c3$networks[[1]]

# Convert to igraph once
g_true <- if (inherits(chosen_network, "igraph")) {
  chosen_network
} else {
  intergraph::asIgraph(chosen_network)
}

# Add a stable node identifier if one is not already present
if (is.null(igraph::vertex_attr(g_true, "NodeID"))) {
  g_true <- igraph::set_vertex_attr(
    g_true,
    name = "NodeID",
    value = seq_len(igraph::vcount(g_true))
  )
}


################################################################################
# 2. Generate one fixed network layout
################################################################################

set.seed(123)

fixed_layout <- igraph::layout_with_fr(
  g_true,
  niter = 2000
)

# Rescale to a common plotting range
fixed_layout[, 1] <- scales::rescale(
  fixed_layout[, 1],
  to = c(-1, 1)
)

fixed_layout[, 2] <- scales::rescale(
  fixed_layout[, 2],
  to = c(-1, 1)
)


################################################################################
# 3. Construct reusable node and edge data frames
################################################################################

node_base <- tibble::tibble(
  vertex_id = seq_len(igraph::vcount(g_true)),
  NodeID = as.integer(
    igraph::vertex_attr(g_true, "NodeID")
  ),
  degree = igraph::degree(g_true),
  x = fixed_layout[, 1],
  y = fixed_layout[, 2]
)

edge_ends <- igraph::ends(
  g_true,
  igraph::E(g_true),
  names = FALSE
)

edge_base <- tibble::tibble(
  edge_id = seq_len(igraph::ecount(g_true)),
  from = edge_ends[, 1],
  to = edge_ends[, 2],
  x = fixed_layout[edge_ends[, 1], 1],
  y = fixed_layout[edge_ends[, 1], 2],
  xend = fixed_layout[edge_ends[, 2], 1],
  yend = fixed_layout[edge_ends[, 2], 2]
)


################################################################################
# 4. Estimate each node's probability of spotlight selection
################################################################################

estimate_spotlight_probabilities <- function(
    graph,
    spotlight_pct,
    alpha,
    n_runs = 1000,
    seed = NULL
) {
  
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  n_nodes <- igraph::vcount(graph)
  
  # Each column is one spotlight assignment
  spotlight_draws <- vapply(
    seq_len(n_runs),
    function(run_id) {
      
      assigned_graph <- assignSpotlight(
        graph_list = list(graph),
        spotlight_pct = spotlight_pct,
        alpha = alpha
      )[[1]]
      
      as.integer(
        igraph::vertex_attr(
          assigned_graph,
          "Spotlight"
        )
      )
    },
    FUN.VALUE = integer(n_nodes)
  )
  
  tibble::tibble(
    vertex_id = seq_len(n_nodes),
    p_spotlit = rowMeans(spotlight_draws)
  )
}


################################################################################
# FIGURE 1
# Estimated probability of spotlight selection
################################################################################

figure1_conditions <- tidyr::crossing(
  spotlight_pct = c(0.01, 0.10),
  alpha = c(0, 2, 8)
) |>
  dplyr::mutate(
    seed = 1000L + dplyr::row_number()
  )

figure1_nodes <- purrr::pmap_dfr(
  figure1_conditions,
  function(spotlight_pct, alpha, seed) {
    
    estimated_probabilities <- estimate_spotlight_probabilities(
      graph = g_true,
      spotlight_pct = spotlight_pct,
      alpha = alpha,
      n_runs = 1000,
      seed = seed
    )
    
    node_base |>
      dplyr::left_join(
        estimated_probabilities,
        by = "vertex_id"
      ) |>
      dplyr::mutate(
        spotlight_pct = spotlight_pct,
        alpha = alpha
      )
  }
) |>
  dplyr::mutate(
    alpha_label = factor(
      paste0("\u03B1 = ", alpha),
      levels = c(
        "\u03B1 = 0",
        "\u03B1 = 2",
        "\u03B1 = 8"
      )
    ),
    
    spotlight_label = factor(
      paste0(
        "Spotlight = ",
        spotlight_pct * 100,
        "%"
      ),
      levels = c(
        "Spotlight = 1%",
        "Spotlight = 10%"
      )
    )
  )


# Use exactly the same fill scale in both figures
spotlight_fill_scale <- function() {
  
  ggplot2::scale_fill_viridis_c(
    option = "magma",
    direction = -1,
    limits = c(0, 1),
    breaks = c(0, 0.25, 0.50, 0.75, 1),
    labels = scales::label_percent(accuracy = 1),
    name = "Estimated\nP(spotlit)"
  )
}


figure1 <- ggplot2::ggplot() +
  
  # The same true edges are repeated in every facet
  ggplot2::geom_segment(
    data = edge_base,
    mapping = ggplot2::aes(
      x = x,
      y = y,
      xend = xend,
      yend = yend
    ),
    colour = "grey75",
    linewidth = 0.35,
    alpha = 0.65
  ) +
  
  ggplot2::geom_point(
    data = figure1_nodes,
    mapping = ggplot2::aes(
      x = x,
      y = y,
      fill = p_spotlit
    ),
    shape = 21,
    size = 3.5,
    stroke = 0.3,
    colour = "grey15"
  ) +
  
  ggplot2::facet_grid(
    rows = ggplot2::vars(spotlight_label),
    cols = ggplot2::vars(alpha_label)
  ) +
  
  spotlight_fill_scale() +
  
  ggplot2::coord_equal(
    clip = "off"
  ) +
  
  ggplot2::labs(
    title = "Probability of selection into the investigative spotlight",
    subtitle = paste0(
      "Node colour shows estimated selection probability across ",
      "1,000 spotlight assignments"
    )
  ) +
  
  ggplot2::theme_void() +
  
  ggplot2::theme(
    strip.text = ggplot2::element_text(
      face = "bold",
      size = 11
    ),
    strip.background = ggplot2::element_rect(
      fill = "grey95",
      colour = "grey75"
    ),
    plot.title = ggplot2::element_text(
      face = "bold",
      size = 14
    ),
    plot.subtitle = ggplot2::element_text(
      size = 10
    ),
    legend.position = "right",
    panel.spacing = grid::unit(0.8, "lines"),
    plot.margin = ggplot2::margin(
      t = 10,
      r = 10,
      b = 10,
      l = 10
    )
  )

figure1

################################################################################

################################ Figure 2 ######################################

################################################################################

################################################################################
# FIGURE 2
# One realised spotlight assignment under different observation conditions
################################################################################

# Assign the spotlight once
set.seed(2468)

g_realised_spotlight <- assignSpotlight(
  graph_list = list(g_true),
  spotlight_pct = 0.10,
  alpha = 2
)[[1]]


################################################################################
# Extract the realised node and edge spotlight classifications
################################################################################

actual_node_spotlight <- as.integer(
  igraph::vertex_attr(
    g_realised_spotlight,
    "Spotlight"
  )
)

actual_edge_spotlight <- as.integer(
  igraph::edge_attr(
    g_realised_spotlight,
    "Spotlight"
  )
)


################################################################################
# Retrieve the estimated P(spotlit) for alpha = 8 and spotlight = 10%
################################################################################

figure2_node_probabilities <- figure1_nodes |>
  dplyr::filter(
    alpha == 8,
    spotlight_pct == 0.10
  ) |>
  dplyr::select(
    vertex_id,
    p_spotlit
  )

figure2_nodes <- node_base |>
  dplyr::left_join(
    figure2_node_probabilities,
    by = "vertex_id"
  ) |>
  dplyr::mutate(
    actually_spotlit = actual_node_spotlight
  )


################################################################################
# Define the four observation conditions
################################################################################

observation_conditions <- tibble::tribble(
  ~panel_order, ~condition_name,        ~p_obs_spotlit, ~p_obs_nonspotlit,
  1L,           "Full network",          1.00,           1.00,
  2L,           "Uniform observation",   0.60,           0.60,
  3L,           "Moderate spotlight",    0.80,           0.40,
  4L,           "Strong spotlight",      0.80,           0.20
)


################################################################################
# Apply one common observation realisation across all conditions
################################################################################

# One fixed random number is generated for every true edge.
# Each panel applies a different probability threshold to the same values.
set.seed(9753)

edge_uniform_draw <- stats::runif(
  igraph::ecount(g_realised_spotlight)
)


figure2_edges <- purrr::pmap_dfr(
  observation_conditions,
  function(
    panel_order,
    condition_name,
    p_obs_spotlit,
    p_obs_nonspotlit
  ) {
    
    edge_probability <- ifelse(
      actual_edge_spotlight == 1L,
      p_obs_spotlit,
      p_obs_nonspotlit
    )
    
    observed <- edge_uniform_draw <= edge_probability
    
    realised_missingness <- mean(!observed)
    
    if (panel_order == 1L) {
      
      panel_label <- paste0(
        condition_name,
        "\nGround-truth network"
      )
      
    } else {
      
      panel_label <- paste0(
        condition_name,
        "\n",
        "p(s) = ",
        sprintf("%.1f", p_obs_spotlit),
        ", p(ns) = ",
        sprintf("%.1f", p_obs_nonspotlit),
        "\nRealised missingness = ",
        scales::percent(
          realised_missingness,
          accuracy = 0.1
        )
      )
    }
    
    edge_base |>
      dplyr::mutate(
        panel_order = panel_order,
        condition_name = condition_name,
        panel_label = panel_label,
        p_obs_spotlit = p_obs_spotlit,
        p_obs_nonspotlit = p_obs_nonspotlit,
        edge_spotlight = actual_edge_spotlight,
        observed = observed,
        edge_status = factor(
          ifelse(
            observed,
            "Observed",
            "Unobserved"
          ),
          levels = c(
            "Observed",
            "Unobserved"
          )
        ),
        realised_missingness = realised_missingness
      )
  }
)


################################################################################
# Preserve the intended left-to-right panel order
################################################################################

figure2_panel_levels <- figure2_edges |>
  dplyr::distinct(
    panel_order,
    panel_label
  ) |>
  dplyr::arrange(panel_order) |>
  dplyr::pull(panel_label)

figure2_edges <- figure2_edges |>
  dplyr::mutate(
    panel_label = factor(
      panel_label,
      levels = figure2_panel_levels
    )
  )


################################################################################
# Draw Figure 2
################################################################################

figure2 <- ggplot2::ggplot() +
  
  ggplot2::geom_segment(
    data = figure2_edges |>
      dplyr::filter(observed),
    mapping = ggplot2::aes(
      x = x,
      y = y,
      xend = xend,
      yend = yend
    ),
    colour = "grey72",
    linewidth = 0.35,
    alpha = 0.55,
    lineend = "round"
  ) +
  
  # All nodes: fill shows long-run P(spotlit)
  ggplot2::geom_point(
    data = figure2_nodes,
    mapping = ggplot2::aes(
      x = x,
      y = y,
      fill = p_spotlit
    ),
    shape = 21,
    size = 3.5,
    stroke = 0.3,
    colour = "grey20"
  ) +
  
  # Thick ring around nodes selected in this realised spotlight assignment
  ggplot2::geom_point(
    data = figure2_nodes |>
      dplyr::filter(actually_spotlit == 1L),
    mapping = ggplot2::aes(
      x = x,
      y = y
    ),
    shape = 21,
    size = 4.8,
    stroke = 1.2,
    fill = NA,
    colour = "black"
  ) +
  
  ggplot2::facet_wrap(
    ggplot2::vars(panel_label),
    nrow = 2
  ) +
  
  spotlight_fill_scale() +
  
  ggplot2::guides(
    fill = ggplot2::guide_colourbar(
      order = 1
    )
  ) +
  
  ggplot2::coord_equal(
    clip = "off"
  ) +
  
  ggplot2::labs(
    title = "Illustrative realisations of spotlight-driven observation",
    subtitle = paste0(
      "The ground-truth network, node positions and realised spotlight ",
      "assignment are held constant across panels"
    ),
    caption = paste0(
      "Node fill shows estimated P(spotlit). ",
      "Black rings identify nodes selected in the realised spotlight assignment. ",
      "Unobserved edges are omitted."
    )
  ) +
  
  ggplot2::theme_void() +
  
  ggplot2::theme(
    strip.text = ggplot2::element_text(
      face = "bold",
      size = 10
    ),
    strip.background = ggplot2::element_rect(
      fill = "grey95",
      colour = "grey75"
    ),
    plot.title = ggplot2::element_text(
      face = "bold",
      size = 14
    ),
    plot.subtitle = ggplot2::element_text(
      size = 10
    ),
    plot.caption = ggplot2::element_text(
      size = 9,
      hjust = 0
    ),
    legend.position = "right",
    panel.spacing = grid::unit(0.8, "lines"),
    plot.margin = ggplot2::margin(
      t = 10,
      r = 10,
      b = 10,
      l = 10
    )
  )

figure2

################################################################################

# Reducing size of fig 2

################################################################################

make_observation_plot <- function(
    panel_orders,
    plot_title,
    plot_subtitle = NULL
) {
  
  edge_data <- figure2_edges |>
    dplyr::filter(
      panel_order %in% panel_orders,
      observed
    ) |>
    dplyr::mutate(
      panel_label = droplevels(panel_label)
    )
  
  ggplot2::ggplot() +
    
    ggplot2::geom_segment(
      data = edge_data,
      mapping = ggplot2::aes(
        x = x,
        y = y,
        xend = xend,
        yend = yend
      ),
      colour = "grey35",
      linewidth = 0.70,
      alpha = 0.85,
      lineend = "round"
    ) +
    
    ggplot2::geom_point(
      data = figure2_nodes,
      mapping = ggplot2::aes(
        x = x,
        y = y,
        fill = p_spotlit
      ),
      shape = 21,
      size = 3.5,
      stroke = 0.3,
      colour = "grey20"
    ) +
    
    ggplot2::geom_point(
      data = figure2_nodes |>
        dplyr::filter(actually_spotlit == 1L),
      mapping = ggplot2::aes(
        x = x,
        y = y
      ),
      shape = 21,
      size = 4.8,
      stroke = 1.2,
      fill = NA,
      colour = "black"
    ) +
    
    ggplot2::facet_wrap(
      ggplot2::vars(panel_label),
      ncol = 2
    ) +
    
    spotlight_fill_scale() +
    
    ggplot2::guides(
      fill = ggplot2::guide_colourbar(order = 1)
    ) +
    
    ggplot2::coord_equal(
      clip = "off"
    ) +
    
    ggplot2::labs(
      title = plot_title,
      subtitle = plot_subtitle,
      caption = paste0(
        "Node fill shows estimated P(spotlit). ",
        "Black rings identify nodes selected in the realised spotlight assignment. ",
        "Unobserved edges are omitted."
      )
    ) +
    
    ggplot2::theme_void() +
    
    ggplot2::theme(
      strip.text = ggplot2::element_text(
        face = "bold",
        size = 11
      ),
      strip.background = ggplot2::element_rect(
        fill = "grey95",
        colour = "grey75"
      ),
      plot.title = ggplot2::element_text(
        face = "bold",
        size = 14
      ),
      plot.subtitle = ggplot2::element_text(
        size = 10
      ),
      plot.caption = ggplot2::element_text(
        size = 9,
        hjust = 0
      ),
      legend.position = "right",
      panel.spacing = grid::unit(1, "lines"),
      plot.margin = ggplot2::margin(
        t = 10,
        r = 10,
        b = 10,
        l = 10
      )
    )
}

figure2a <- make_observation_plot(
  panel_orders = c(1, 2),
  plot_title = "Ground-truth and uniform observation",
  plot_subtitle = paste0(
    "The same network and spotlight assignment are shown before ",
    "and after non-differential tie observation"
  )
)

figure2b <- make_observation_plot(
  panel_orders = c(3, 4),
  plot_title = "Moderate and strong spotlight effects",
  plot_subtitle = paste0(
    "Observation is increasingly concentrated around the ",
    "realised spotlighted nodes"
  )
)

figure2a
figure2b
