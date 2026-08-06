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


######################## Compute network-level metrics ##########################

computeMetrics <- function(graph_list) {
  
  purrr::map_dfr(graph_list, function(g) {
    
    # Extract structure of input network
    comp <- igraph::components(g)
    component_sizes <- comp$csize
    n_nodes <- igraph::vcount(g)
    node_degrees <- igraph::degree(g, mode = "all")
    
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
      
      # Observed network statistics
      density = igraph::edge_density(g, loops = FALSE),
      
      dcent = igraph::centr_degree(
        g,
        mode = "all",
        normalized = TRUE
      )$centralization,
      
      clustering = igraph::transitivity(
        g,
        type = "global"
      ),
      
      size = n_nodes,
      
      APL = igraph::mean_distance(
        g,
        directed = FALSE,
        unconnected = TRUE
      ),
      
      components = comp$no,
      
      largest_component_n = max(component_sizes),
      
      largest_component_prop = max(component_sizes) / n_nodes,
      
      n_isolates = sum(node_degrees == 0),
      
      isolate_prop = sum(node_degrees == 0) / n_nodes
    )
  })
}


######################### Compute node-level metrics ############################

computeCentralityDf <- function(graph_list) {
  
  purrr::map_dfr(graph_list, function(g) {
    
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
      
      Closeness_norm = igraph::closeness(
        g,
        mode = "all",
        normalized = TRUE
      ),
      
      Closeness_raw = igraph::closeness(
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