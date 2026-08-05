################################################################################

# This is the script for the ERGM simulator used to convert the degree sequences
# into more realistic network formats

# Keep all function calls specified as package::function as igraph and network
# packages do not play nicely with each other

################################################################################

# Check components to determine if networks are connected or not
is_connected_network <- function(net) {
  comp <- sna::component.dist(net)
  length(comp$csize) == 1L
}

################# Main simulation function ##################

simulateNetworks <- function(net_list, 
                             target_connected = 1,
                             max_attempts = 100,
                             require_connected = TRUE,
                             nfAtt = 0,
                             nmAtt = 0,
                             gwdeg = 0.5,
                             gwesp = 0.5,
                             gwdsp = -0.025,
                             att_prob = c(3, 1),
                             verbose = TRUE) {
  
  all_sims <- list()
  diagnostics <- list()
  counter <- 1L
  
  # ERGM will simulate over each base degree distribution passed in
  
  for (i in seq_along(net_list)) {
    
    net <- net_list[[i]]
    n <- network::network.size(net)
    
    deg <- sna::degree(net, gmode = "graph")
    
    accepted <- 0L # Diagnostics to determine which degree distributions may be problematic
    attempted <- 0L
    rejected <- 0L
    
    # Adding in nodal attributes, assists in maintaining cohesiveness and 
    # gives option for attribute specific investigation
    
    network::set.vertex.attribute(
      net,
      attrname = "att",
      value = sample(c("A", "B"), n, replace = TRUE, prob = att_prob)
    )
    
    # Specify the ERGM parameters used to generate networks
    form <- net ~
      nodefactor("att") +
      nodematch("att") +
      gwdegree(0.3, fixed = TRUE) +
      gwesp(0.3, fixed = TRUE) +
      gwdsp(0.3, fixed = TRUE)
    
    # Specify values associated with structural params
    coefs <- c(
      nodefactor.att.B = nfAtt,
      nodematch.att = nmAtt,
      gwdeg.fixed = gwdeg,
      gwesp.fixed = gwesp,
      gwdsp.fixed = gwdsp
    )
    
    while (accepted < target_connected && attempted < max_attempts) {
      
      attempted <- attempted + 1L
      
      # Suppress messages as ERGM will warn when it can't use triadic hints
      sim <- suppressMessages(
        ergm::simulate_formula(
          form,
          constraints = ~degreedist, # constrain degree distribution to basis
          coef = coefs,
          nsim = 1, # this needs to be fixed at 1 for diagnostics to be correct,
                    # number of generated networks is determined by target_connected
          output = "network")
      )
      
      # Below prevents issues with first output to list crashing function
      if (inherits(sim, "network")) {
        sim_net <- sim
      } else {
        sim_net <- sim[[1]]
      }
      
      connected <- is_connected_network(sim_net)
      
      if (!require_connected || connected) {
        
        accepted <- accepted + 1L
        
        # Collect info on simulated networks
        network::set.network.attribute(sim_net, "basis_id", i)
        network::set.network.attribute(sim_net, "attempt_id", attempted)
        network::set.network.attribute(sim_net, "accepted_id", accepted)
        network::set.network.attribute(sim_net, "connected", connected)
        
        network::set.network.attribute(sim_net, "basis_dmax", max(deg))
        network::set.network.attribute(sim_net, "basis_average_degree", mean(deg))
        network::set.network.attribute(sim_net, "basis_total_degree", sum(deg))
        network::set.network.attribute(sim_net, "basis_sd_degree", stats::sd(deg))
        network::set.network.attribute(sim_net, "basis_degree_iqr", stats::IQR(deg))
        network::set.network.attribute(sim_net, "basis_n_degree_values", length(unique(deg)))
        
        all_sims[[counter]] <- sim_net
        counter <- counter + 1L
        
      } else {
        rejected <- rejected + 1L
      }
    }
    
    diagnostics[[i]] <- data.frame(
      basis_id = i,
      attempted = attempted,
      accepted = accepted,
      rejected = attempted - accepted,
      success_rate = accepted / attempted,
      target_connected = target_connected,
      target_met = accepted >= target_connected,
      dmax = max(deg),
      average_degree = mean(deg),
      total_degree = sum(deg),
      sd_degree = stats::sd(deg),
      degree_iqr = stats::IQR(deg),
      n_degree_values = length(unique(deg))
    )
    
    # Recommend keep true for now
    if (verbose) {
      message(
        "Basis ", i, "/", length(net_list),
        " | accepted ", accepted, "/", target_connected,
        " | attempts = ", attempted,
        " | success rate = ", round(accepted / attempted, 3)
      )
    }
  }
  
  list(
    networks = all_sims,
    diagnostics = dplyr::bind_rows(diagnostics)
  )
}

############# Wrapper to apply simulateNetworks to basis list ##########

simulateFromBasis <- function(basis, target_total = 200, verbose = FALSE) {
  tgt <- ceiling(target_total / length(basis$selected_degree_sequences))
  
  simulateNetworks(
    basis$networks,
    nmAtt = 1,
    gwdeg = 1,
    gwesp = 0.4,
    gwdsp = -0.025,
    target_connected = tgt,
    max_attempts = 500,
    verbose = verbose
  )
}
