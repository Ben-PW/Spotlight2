
####### Function to check basic descriptives of simulated network lists ########

summariseNetworks <- function(net_list) {
  
  data.frame(
    mean_density = mean(sapply(net_list, network::network.density)),
    mean_triangles = mean(sapply(net_list, function(g)
      sna::triad.census(g, mode = "graph")[4])),
    mean_degree = mean(sapply(net_list, function(g)
      mean(sna::degree(g, gmode = "graph")))),
    mean_max_degree = mean(sapply(net_list, function(g)
      max(sna::degree(g, gmode = "graph")))),
    mean_components = mean(sapply(net_list, function(g)
      length(sna::component.dist(g)$csize))),
    mean_component_coverage = mean(sapply(net_list, function(g)
      (max(sna::component.dist(g)$csize)/network::network.size(g))*100)),
    mean_centralisation = mean(sapply(net_list, function(g)
      igraph::centr_degree(intergraph::asIgraph(g), normalized = TRUE)$centralization)
    ))
}

####### Function to turn generated degree sequences into network objects ########

netFromDegSeq <- function(degree_sequences) {
  
  g_list <- lapply(degree_sequences, function(x) {
    igraph::realize_degseq(
      x,
      allowed.edge.types = "simple",
      method = "smallest"
    )
  })
  
  net_list <- lapply(g_list, intergraph::asNetwork)
  
  return(net_list)
}

################ Function to simply plot a list of simulated networks #################

plotSimNetworks <- function(net_list) {
  
  obj_name <- deparse(substitute(net_list))
  
  # flatten one level if needed
  if (is.list(net_list[[1]]) && !inherits(net_list[[1]], "network")) {
    net_list <- unlist(net_list, recursive = FALSE)
  }
  
  for (i in seq_along(net_list)) {
    plot(
      net_list[[i]],
      main = paste0(obj_name, " ", i)
    )
  }
}

################### Small helper for network naming conventions ##################

format_c <- function(x) {
  paste0("c", stringr::str_remove(formatC(x * 10, format = "f", digits = 0), "\\."))
}

################ Function to calculate avdeg tolerance via density ###############

calc_ad_tol <- function(size, density_tol = 0.01) {
  round(density_tol * (size - 1), 2)
}

##################### Function to name bases from parameters #####################

make_basis_name <- function(size, average_degree, freeman_centralisation) {
  paste0(
    "n", size,
    "_ad", average_degree,
    "_", format_c(freeman_centralisation)
  )
}

################ Sample from simulated networks retaining max bases ###############
# This function expects network objects, so if I change the simulation
# function to output igraph objects, this will have to change

sampleDatasets <- function(networks, target_n = 100) {
  
  basis_ids <- vapply(
    networks,
    function(g) network::get.network.attribute(g, "basis_id"),
    numeric(1)
  )
  
  n <- length(networks)
  
  if (n < target_n) {
    warning("Only ", n, " networks available; keeping all.")
    return(networks)
  }
  
  one_per_basis <- split(seq_along(networks), basis_ids) |>
    lapply(function(idx) sample(idx, size = 1)) |>
    unlist(use.names = FALSE)
  
  if (length(one_per_basis) >= target_n) {
    keep_ids <- sample(one_per_basis, size = target_n, replace = FALSE)
    return(networks[keep_ids])
  }
  
  remaining_pool <- setdiff(seq_along(networks), one_per_basis)
  remaining_needed <- target_n - length(one_per_basis)
  
  extra_ids <- sample(remaining_pool, size = remaining_needed, replace = FALSE)
  
  keep_ids <- c(one_per_basis, extra_ids)
  
  networks[keep_ids]
}
