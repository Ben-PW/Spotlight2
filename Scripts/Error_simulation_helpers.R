#####################################################################################################
#
# Helper functions for spotlight simulation
#
#####################################################################################################

############################ Basic graph preparation ############################

undirect <- function(graph_list) {
  lapply(graph_list, function(g) {
    if (igraph::is_directed(g)) {
      g <- igraph::as_undirected(g, mode = "collapse")
    }
    
    g
  })
}


IDNodes <- function(graph_list) {
  lapply(graph_list, function(g) {
    igraph::V(g)$NodeID <- seq_len(igraph::vcount(g))
    g
  })
}


######################## Safely retrieve graph attributes #######################

graphAttrOrNA <- function(g, attribute, default = NA_real_) {
  
  value <- igraph::graph_attr(g, attribute)
  
  if (is.null(value) || length(value) == 0L) {
    return(default)
  }
  
  value
}


############################ Graph identification ###############################

makeGraphID <- function(
    dataset,
    replicate_id,
    source,
    alpha = NA_real_,
    spotlight_pct = NA_real_,
    p_obs_spotlit = NA_real_,
    p_obs_nonspotlit = NA_real_
) {
  
  paste(
    paste0("ds_", dataset),
    paste0("rep_", replicate_id),
    paste0("src_", source),
    paste0("a_", alpha),
    paste0("sp_", spotlight_pct),
    paste0("pos_", p_obs_spotlit),
    paste0("pon_", p_obs_nonspotlit),
    sep = "__"
  )
}


############################ Store graph metadata ###############################

tagGraphs <- function(
    graph_list,
    dataset,
    source,
    alpha = NA_real_,
    spotlight_pct = NA_real_,
    p_obs_spotlit = NA_real_,
    p_obs_nonspotlit = NA_real_
) {
  
  purrr::imap(graph_list, function(g, replicate_id) {
    
    replicate_id <- as.integer(replicate_id)
    
    igraph::graph_attr(g, "dataset") <- dataset
    igraph::graph_attr(g, "replicate_id") <- replicate_id
    igraph::graph_attr(g, "source") <- source
    
    igraph::graph_attr(g, "alpha") <- alpha
    igraph::graph_attr(g, "spotlight_pct") <- spotlight_pct
    
    igraph::graph_attr(
      g,
      "p_obs_spotlit"
    ) <- p_obs_spotlit
    
    igraph::graph_attr(
      g,
      "p_obs_nonspotlit"
    ) <- p_obs_nonspotlit
    
    igraph::graph_attr(g, "graph_id") <- makeGraphID(
      dataset = dataset,
      replicate_id = replicate_id,
      source = source,
      alpha = alpha,
      spotlight_pct = spotlight_pct,
      p_obs_spotlit = p_obs_spotlit,
      p_obs_nonspotlit = p_obs_nonspotlit
    )
    
    g
  })
}

########################## Isolate removal handling ############################

# Added to accomodate reviewer 1 request for node deletion conditions. Deletes
# isolates and records data on how many were removed, adding this to the graph
# metadata

applyNodeBoundary <- function(graph_list, remove_isolates = FALSE) {
  lapply(graph_list, function(g) {
    n_before <- igraph::vcount(g)
    isolates <- which(igraph::degree(g, mode = "all") == 0)
    
    n_removed <- if (remove_isolates) {
      length(isolates)
    } else {
      0L
    }
    
    if (remove_isolates && length(isolates) > 0L) {
      g <- igraph::delete_vertices(g, isolates)
    }
    
    igraph::graph_attr(g, "n_nodes_before_removal") <- n_before
    igraph::graph_attr(g, "n_nodes_removed") <- n_removed
    igraph::graph_attr(g, "remove_isolates") <- remove_isolates
    
    g
  })
}

######################## Compute network-level metrics ##########################

computeMetrics <- function(graph_list) {
  
  purrr::map_dfr(graph_list, function(g) {
    
    # Extract structure of input network
    comp <- igraph::components(g)
    component_sizes <- comp$csize
    n_nodes <- igraph::vcount(g)
    node_degrees <- igraph::degree(g, mode = "all")
    
    # Handle components in disconnected graphs
    largest_component_n <- if (length(component_sizes) > 0L) {
      max(component_sizes)
    } else {
      0L
    }
    
    # Attach simulation conditions
    tibble::tibble(
      dataset = graphAttrOrNA(g, "dataset", NA_character_),
      replicate_id = graphAttrOrNA(g, "replicate_id", NA_integer_),
      source = graphAttrOrNA(g, "source", NA_character_),
      
      alpha = graphAttrOrNA(g, "alpha"),
      spotlight_pct = graphAttrOrNA(g, "spotlight_pct"),
      
      p_obs_spotlit = graphAttrOrNA(g, "p_obs_spotlit"),
      p_obs_nonspotlit = graphAttrOrNA(
        g,
        "p_obs_nonspotlit"
      ),
      
      graph_id = graphAttrOrNA(g, "graph_id", NA_character_),
      
      # Observation-process diagnostics
      n_edges_true = graphAttrOrNA(
        g,
        "n_edges_true",
        NA_integer_
      ),
      
      n_edges_observed = graphAttrOrNA(
        g,
        "n_edges_observed",
        NA_integer_
      ),
      
      n_spotlit_edges_true = graphAttrOrNA(
        g,
        "n_spotlit_edges_true",
        NA_integer_
      ),
      
      n_nonspotlit_edges_true = graphAttrOrNA(
        g,
        "n_nonspotlit_edges_true",
        NA_integer_
      ),
      
      n_spotlit_edges_observed = graphAttrOrNA(
        g,
        "n_spotlit_edges_observed",
        NA_integer_
      ),
      
      n_nonspotlit_edges_observed = graphAttrOrNA(
        g,
        "n_nonspotlit_edges_observed",
        NA_integer_
      ),
      
      realised_obs_rate_overall = graphAttrOrNA(
        g,
        "realised_obs_rate_overall"
      ),
      
      realised_obs_rate_spotlit = graphAttrOrNA(
        g,
        "realised_obs_rate_spotlit"
      ),
      
      realised_obs_rate_nonspotlit = graphAttrOrNA(
        g,
        "realised_obs_rate_nonspotlit"
      ),
      
      realised_missingness = graphAttrOrNA(
        g,
        "realised_missingness"
      ),
      
      spotlight_edge_coverage = graphAttrOrNA(
        g,
        "spotlight_edge_coverage"
      ),
      
      # Optional node removal diagnostics
      n_nodes_before_removal = graphAttrOrNA(
        g,
        "n_nodes_before_removal",
        n_nodes
      ),
      
      n_nodes_removed = graphAttrOrNA(
        g,
        "n_nodes_removed",
        0L
      ),
      
      remove_isolates = graphAttrOrNA(
        g,
        "remove_isolates",
        FALSE
      ),
      
      # Observed network statistics
      # Updated to safely handle node deletion.
      density = if (n_nodes > 1L) {
        igraph::edge_density(g, loops = FALSE)
      } else {
        NA_real_
      },
      
      dcent = if (n_nodes > 2L) {
        igraph::centr_degree(
          g,
          mode = "all",
          normalized = TRUE
        )$centralization
      } else {
        NA_real_
      },
      
      clustering = if (n_nodes > 2L) {
        igraph::transitivity(g, type = "global")
      } else {
        NA_real_
      },
      
      size = n_nodes,
      
      APL = if (igraph::ecount(g) > 0L) {
        igraph::mean_distance(
          g,
          directed = FALSE,
          unconnected = TRUE
        )
      } else {
        NA_real_
      },
      
      components = comp$no,
      
      largest_component_n = largest_component_n,
      
      largest_component_prop = if (n_nodes > 0L) {
        largest_component_n / n_nodes
      } else {
        NA_real_
      },
      
      n_isolates = sum(node_degrees == 0),
      
      isolate_prop = if (n_nodes > 0L) {
        sum(node_degrees == 0) / n_nodes
      } else {
        NA_real_
      }
    )
  })
}


######################### Compute node-level metrics ############################

computeCentralityDf <- function(graph_list) {
  
  purrr::map_dfr(graph_list, function(g) {
    
    # Added to safely handle possible edge cases if all nodes are deleted. Graph
    # will contribute no actor rows to the databse but will retain its network
    # level row
    
    if (igraph::vcount(g) == 0L) {
      return(tibble::tibble(
        dataset = character(),
        replicate_id = integer(),
        source = character(),
        alpha = double(),
        spotlight_pct = double(),
        p_obs_spotlit = double(),
        p_obs_nonspotlit = double(),
        graph_id = character(),
        NodeID = integer(),
        Spotlight = integer(),
        Degree_norm = double(),
        Degree_raw = double(),
        Betweenness_norm = double(),
        Betweenness_raw = double(),
        Harmonic_norm = double(),
        Harmonic_raw = double(),
        Eigenvector = double()
      ))
    }
    
    tibble::tibble(
      dataset = graphAttrOrNA(g, "dataset", NA_character_),
      replicate_id = graphAttrOrNA(g, "replicate_id", NA_integer_),
      source = graphAttrOrNA(g, "source", NA_character_),
      
      alpha = graphAttrOrNA(g, "alpha"),
      spotlight_pct = graphAttrOrNA(g, "spotlight_pct"),
      
      p_obs_spotlit = graphAttrOrNA(g, "p_obs_spotlit"),
      p_obs_nonspotlit = graphAttrOrNA(
        g,
        "p_obs_nonspotlit"
      ),
      
      graph_id = graphAttrOrNA(g, "graph_id", NA_character_),
      
      NodeID = as.integer(igraph::V(g)$NodeID),
      
      Spotlight = {
        value <- igraph::V(g)$Spotlight
        
        if (is.null(value)) {
          rep(NA_integer_, igraph::vcount(g))
        } else {
          as.integer(value)
        }
      },
      
      Degree_norm = igraph::degree(
        g,
        mode = "all",
        normalized = TRUE
      ),
      
      Degree_raw = igraph::degree(
        g,
        mode = "all",
        normalized = FALSE
      ),
      
      Betweenness_norm = igraph::betweenness(
        g,
        directed = igraph::is_directed(g),
        normalized = TRUE
      ),
      
      Betweenness_raw = igraph::betweenness(
        g,
        directed = igraph::is_directed(g),
        normalized = FALSE
      ),
      
      Harmonic_norm = igraph::harmonic_centrality(
        g,
        mode = "all",
        normalized = TRUE
      ),
      
      Harmonic_raw = igraph::harmonic_centrality(
        g,
        mode = "all",
        normalized = FALSE
      ),
      
      Eigenvector = igraph::eigen_centrality(
        g,
        directed = igraph::is_directed(g)
      )$vector
    )
  })
}