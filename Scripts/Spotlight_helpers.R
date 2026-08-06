#################################################################################################
# Script: Spotlight_helpers.R
#
# This script contains the functions required for the actual spotlight process
# assignSpotlight: Determines which nodes are spotlit 
# observeSpotlight: Samples ties accordingly

####################################################################################

# library(netUtils) - probs don't need anymore tbf

# Function to assign spotlight conditional on spotlight degree
# It's a bit hacky but for attribute related spotlight, I'm just going to manually
# assign 'Degree' as a proxy for attribute

assignSpotlight <- function(graph_list, spotlight_pct, alpha = 0) {
  lapply(graph_list, function(g) {
    
    n <- igraph::vcount(g)
    k <- max(1L, round(n * spotlight_pct))
    
    Spotlight <- integer(n)
    
    deg <- igraph::degree(g)
    w <- (deg + 1) ^ alpha
    
    idx <- sample.int(n, size = k, replace = FALSE, prob = w)
    Spotlight[idx] <- 1L
    
    # Assign to nodes
    igraph::V(g)$Spotlight <- Spotlight
    
    # Assign to edges
      ends_mat <- igraph::ends(g, igraph::E(g), names = FALSE)
      igraph::E(g)$Spotlight <-
        as.integer(Spotlight[ends_mat[,1]] | Spotlight[ends_mat[,2]]) # assign spotlight if either end is incident on a spotlit node
  
    
    g
  })
}


# Function to sample nodes conditionally on spotlight (b = 1 means equal)
# b > 1 biases towards non-spotlit
# might want to change the weighting system later but not difficult

######################## Observe ties probabilistically #########################

observeSpotlight <- function(
    graph_list,
    p_obs_spotlit,
    p_obs_nonspotlit
) {
  
  # Validate probabilities before entering the graph loop
  probability_args <- c(
    p_obs_spotlit = p_obs_spotlit,
    p_obs_nonspotlit = p_obs_nonspotlit
  )
  
  if (
    anyNA(probability_args) ||
    any(!is.finite(probability_args)) ||
    any(probability_args < 0 | probability_args > 1)
  ) {
    stop("Observation probabilities must be finite values between 0 and 1.")
  }

  
  lapply(graph_list, function(g) {
    
    m_true <- igraph::ecount(g)
    
    if (m_true == 0L) {
      warning("A ground-truth graph contains no edges.")
      
      igraph::graph_attr(g, "n_edges_true") <- 0L
      igraph::graph_attr(g, "n_edges_observed") <- 0L
      
      igraph::graph_attr(g, "n_spotlit_edges_true") <- 0L
      igraph::graph_attr(g, "n_nonspotlit_edges_true") <- 0L
      
      igraph::graph_attr(g, "n_spotlit_edges_observed") <- 0L
      igraph::graph_attr(g, "n_nonspotlit_edges_observed") <- 0L
      
      igraph::graph_attr(g, "realised_obs_rate_overall") <- NA_real_
      igraph::graph_attr(g, "realised_obs_rate_spotlit") <- NA_real_
      igraph::graph_attr(g, "realised_obs_rate_nonspotlit") <- NA_real_
      igraph::graph_attr(g, "realised_missingness") <- NA_real_
      igraph::graph_attr(g, "spotlight_edge_coverage") <- NA_real_
      
      return(g)
    }
    
    edge_spotlight <- igraph::E(g)$Spotlight
    
    edge_spotlight <- as.integer(edge_spotlight)
    
    # Assign the observation probability for each true edge
    p_edge <- ifelse(
      edge_spotlight == 1L,
      p_obs_spotlit,
      p_obs_nonspotlit
    )
    
    # Independent observation process
    observed <- stats::rbinom(
      n = m_true,
      size = 1L,
      prob = p_edge
    ) == 1L
    
    n_spotlit_true <- sum(edge_spotlight == 1L)
    n_nonspotlit_true <- sum(edge_spotlight == 0L)
    
    n_spotlit_observed <- sum(
      observed & edge_spotlight == 1L
    )
    
    n_nonspotlit_observed <- sum(
      observed & edge_spotlight == 0L
    )
    
    n_observed <- sum(observed)
    
    # Store observation-process diagnostics before deleting edges
    igraph::graph_attr(g, "n_edges_true") <- m_true
    igraph::graph_attr(g, "n_edges_observed") <- n_observed
    
    igraph::graph_attr(
      g,
      "n_spotlit_edges_true"
    ) <- n_spotlit_true
    
    igraph::graph_attr(
      g,
      "n_nonspotlit_edges_true"
    ) <- n_nonspotlit_true
    
    igraph::graph_attr(
      g,
      "n_spotlit_edges_observed"
    ) <- n_spotlit_observed
    
    igraph::graph_attr(
      g,
      "n_nonspotlit_edges_observed"
    ) <- n_nonspotlit_observed
    
    igraph::graph_attr(
      g,
      "realised_obs_rate_overall"
    ) <- n_observed / m_true
    
    igraph::graph_attr(
      g,
      "realised_obs_rate_spotlit"
    ) <- if (n_spotlit_true > 0L) {
      n_spotlit_observed / n_spotlit_true
    } else {
      NA_real_
    }
    
    igraph::graph_attr(
      g,
      "realised_obs_rate_nonspotlit"
    ) <- if (n_nonspotlit_true > 0L) {
      n_nonspotlit_observed / n_nonspotlit_true
    } else {
      NA_real_
    }
    
    igraph::graph_attr(
      g,
      "realised_missingness"
    ) <- 1 - (n_observed / m_true)
    
    # Proportion of true edges falling inside the spotlight
    igraph::graph_attr(
      g,
      "spotlight_edge_coverage"
    ) <- n_spotlit_true / m_true
    
    # Delete edges that were not observed
    g <- igraph::delete_edges(
      g,
      igraph::E(g)[!observed]
    )
    
    g
  })
}
