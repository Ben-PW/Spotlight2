


############# Function to calculate Freeman centrality from deg seq

freeman_from_degree <- function(deg) {
  n <- length(deg)
  dmax <- max(deg)
  sum(dmax - deg) / ((n - 1) * (n - 2))
}


############ Gets correct igraph function depending on version

is_graphical_safe <- function(deg) {
  if ("is_graphical" %in% getNamespaceExports("igraph")) {
    return(igraph::is_graphical(deg, allowed.edge.types = "simple"))
  }
  
  if ("is.graphical.degree.sequence" %in% getNamespaceExports("igraph")) {
    return(igraph::is.graphical.degree.sequence(deg))
  }
  
  stop("No graphical degree-sequence checker found in igraph.")
}

############### Calculates total degree of network from size and avdeg

get_total_degree <- function(size, average_degree) {
  total_degree <- round(size * average_degree)
  
  if ((total_degree %% 2) != 0) {
    total_degree <- total_degree + 1L
  }
  
  total_degree
}

#################### Constructs starting deg seq

construct_initial_degseq <- function(size,
                                     total_degree,
                                     min_degree = 1L,
                                     max_tries = 10000) {
  
  n <- as.integer(size)
  
  min_sum <- n * min_degree
  max_sum <- n * (n - 1L)
  
  if (total_degree < min_sum || total_degree > max_sum) {
    stop("total_degree incompatible with size and min_degree.")
  }
  
  for (attempt in seq_len(max_tries)) {
    deg <- rep.int(min_degree, n)
    rem <- total_degree - sum(deg)
    
    while (rem > 0) {
      eligible <- which(deg < (n - 1L))
      if (length(eligible) == 0) break
      
      i <- sample(eligible, 1)
      deg[i] <- deg[i] + 1L
      rem <- rem - 1L
    }
    
    if (rem != 0) next
    
    deg <- sample(deg)
    deg <- sort(deg, decreasing = TRUE)
    
    if (sum(deg) != total_degree) next
    if (min(deg) < min_degree) next
    if (!is_graphical_safe(deg)) next
    
    return(deg)
  }
  
  NULL
}


############################ Proposal moves for random walk
# These are mixed as increments of 1 did not lead to much variation

# This move just swaps an increment of 1 degree between two slots on the sequence
propose_move_1 <- function(deg, min_degree = 1L) {
  n <- length(deg)
  
  donors <- which(deg > min_degree)
  recipients <- which(deg < (n - 1L))
  
  if (length(donors) == 0 || length(recipients) == 0) return(NULL)
  
  i <- sample(donors, 1)
  j <- sample(recipients, 1)
  
  if (i == j) return(NULL)
  
  prop <- deg
  prop[i] <- prop[i] - 1L
  prop[j] <- prop[j] + 1L
  
  sort(prop, decreasing = TRUE)
}

# Takes 2 degree increments from a donor and redistributes to 2 recipient slots
propose_move_2split <- function(deg, min_degree = 1L) {
  n <- length(deg)
  
  donors <- which(deg >= (min_degree + 2L))
  recipients <- which(deg < (n - 1L))
  
  if (length(donors) == 0 || length(recipients) < 2) return(NULL)
  
  i <- sample(donors, 1)
  recips <- setdiff(recipients, i)
  
  if (length(recips) < 2) return(NULL)
  
  js <- sample(recips, 2, replace = FALSE)
  
  prop <- deg
  prop[i] <- prop[i] - 2L
  prop[js[1]] <- prop[js[1]] + 1L
  prop[js[2]] <- prop[js[2]] + 1L
  
  sort(prop, decreasing = TRUE)
}

# Takes a degree increment from two donors and redistributes to 1 recipient
propose_move_2merge <- function(deg, min_degree = 1L) {
  n <- length(deg)
  
  donors <- which(deg > min_degree)
  recipients <- which(deg <= (n - 3L))
  
  if (length(donors) < 2 || length(recipients) == 0) return(NULL)
  
  j <- sample(recipients, 1)
  dons <- setdiff(donors, j)
  
  if (length(dons) < 2) return(NULL)
  
  is <- sample(dons, 2, replace = FALSE)
  
  prop <- deg
  prop[is[1]] <- prop[is[1]] - 1L
  prop[is[2]] <- prop[is[2]] - 1L
  prop[j] <- prop[j] + 2L
  
  sort(prop, decreasing = TRUE)
}

# Adds two degree points to one node
# Does not preserve total degree so degree tolerance must be in place
propose_add_2 <- function(deg) {
  n <- length(deg)
  
  recipients <- which(deg <= (n - 3L))
  if (length(recipients) == 0) return(NULL)
  
  j <- sample(recipients, 1)
  
  prop <- deg
  prop[j] <- prop[j] + 2L
  
  if (prop[j] > (n - 1L)) return(NULL)
  
  sort(prop, decreasing = TRUE)
}

# As above, removes two degree points from a random node
# Degree tolerance required
propose_drop_2 <- function(deg, min_degree = 1L) {
  donors <- which(deg >= (min_degree + 2L))
  if (length(donors) == 0) return(NULL)
  
  i <- sample(donors, 1)
  
  prop <- deg
  prop[i] <- prop[i] - 2L
  
  if (min(prop) < min_degree) return(NULL)
  
  sort(prop, decreasing = TRUE)
}

# This function just determines which move will be implemented at a given step,
# based on the probabilities defined in the arguments
propose_degseq_move_mixed <- function(deg,
                                      min_degree = 1L,
                                      move_probs = c(
                                        move1 = 0.40,
                                        split2 = 0.20,
                                        merge2 = 0.20,
                                        add2 = 0.10,
                                        drop2 = 0.10
                                      )) {
  
  move_type <- sample(names(move_probs), size = 1, prob = move_probs)
  
  if (move_type == "move1") {
    prop <- propose_move_1(deg, min_degree = min_degree)
  } else if (move_type == "split2") {
    prop <- propose_move_2split(deg, min_degree = min_degree)
  } else if (move_type == "merge2") {
    prop <- propose_move_2merge(deg, min_degree = min_degree)
  } else if (move_type == "add2") {
    prop <- propose_add_2(deg)
  } else if (move_type == "drop2") {
    prop <- propose_drop_2(deg, min_degree = min_degree)
  } else {
    stop("Unknown move type.")
  }
  
  list(prop = prop, move_type = move_type)
}

################################################################################

# Main sampler

################################################################################

sampleDegSeq <- function(
    nsim,
    size,
    average_degree,
    average_degree_tolerance = 0.3,
    freeman_centralisation,
    tolerance = 0.05,
    min_degree = 1L,
    max_steps = 500000,
    max_start_tries = 10000,
    move_probs = c(
      move1 = 0.40,
      split2 = 0.20,
      merge2 = 0.20,
      add2 = 0.10,
      drop2 = 0.10
    ),
    profile_digits_c = 3,
    profile_digits_iqr = 2,
    seed = NULL,
    verbose = TRUE
) {
  
  if (!is.null(seed)) set.seed(seed)
  
  n <- as.integer(size)
  start_total <- get_total_degree(n, average_degree)
  
  is_valid <- function(deg) {
    c_val <- freeman_from_degree(deg)
    avg_val <- mean(deg)
    
    abs(c_val - freeman_centralisation) <= tolerance &&
      abs(avg_val - average_degree) <= average_degree_tolerance &&
      min(deg) >= min_degree &&
      is_graphical_safe(deg)
  }
  
  # reserve general statistics of network to identify meaningfully unique sequences
  make_profile <- function(deg) {
    paste(
      max(deg),
      round(freeman_from_degree(deg), profile_digits_c),
      round(IQR(deg), profile_digits_iqr),
      round(mean(deg), 3),
      #round(sd(deg), 2), far too granular
      #length(unique(deg)), too granular
      sep = "_"
    )
  }
  
  # save a sufficiently unique sequence
  save_sequence <- function(deg, step, profile) {
    list(
      degree_sequence = deg,
      dmax = max(deg),
      realised_centralisation = freeman_from_degree(deg),
      realised_average_degree = mean(deg),
      realised_total_degree = sum(deg),
      sd_degree = sd(deg),
      degree_iqr = IQR(deg),
      n_degree_values = length(unique(deg)),
      profile = profile,
      step = step
    )
  }
  
  # Initialise starting sequence 
  
  current <- construct_initial_degseq(
    size = n,
    total_degree = start_total,
    min_degree = min_degree,
    max_tries = max_start_tries
  )
  
  if (is.null(current)) {
    stop("Could not construct initial graphical degree sequence.")
  }
  
  current_c <- freeman_from_degree(current)
  
  # If initial sequence is outside centralisation band, walk until valid
  init_steps <- 0L
  
  while (!is_valid(current)) {
    init_steps <- init_steps + 1L
    
    if (init_steps > max_steps) {
      stop("Could not find initial sequence inside target bands.")
    }
    
    move <- propose_degseq_move_mixed(
      current,
      min_degree = min_degree,
      move_probs = move_probs
    )
    
    prop <- move$prop
    if (is.null(prop)) next
    
    prop_c <- freeman_from_degree(prop)
    
    current_dist <-
      abs(current_c - freeman_centralisation) +
      abs(mean(current) - average_degree)
    
    prop_dist <-
      abs(prop_c - freeman_centralisation) +
      abs(mean(prop) - average_degree)
    
    if (prop_dist < current_dist && is_graphical_safe(prop)) {
      current <- prop
      current_c <- prop_c
    }
  }
  
  if (verbose) {
    message(
      "Initial valid state found",
      " | C = ", round(freeman_from_degree(current), 4),
      " | avg degree = ", round(mean(current), 4),
      " | dmax = ", max(current),
      " | total degree = ", sum(current),
      " | init_steps = ", init_steps
    )
  }
  
  # profile collection
  
  out <- vector("list", nsim)
  seen_profiles <- character(0)
  seen_sequences <- character(0)
  
  saved <- 0L
  total_steps <- 0L
  attempted_moves <- 0L
  accepted_moves <- 0L
  
  ########################################### CHANGE HERE
  
  while (
    # saved < nsim &&  <- removed the limit, sampler just explores until done
    total_steps < max_steps) {
    
    total_steps <- total_steps + 1L
    
    move <- propose_degseq_move_mixed(
      current,
      min_degree = min_degree,
      move_probs = move_probs
    )
    
    prop <- move$prop
    if (is.null(prop)) next
    
    attempted_moves <- attempted_moves + 1L
    
    if (!is_valid(prop)) next
    
    current <- prop
    accepted_moves <- accepted_moves + 1L
    
    profile <- make_profile(current)
    key <- paste(current, collapse = "-")
    
    if (profile %in% seen_profiles) next
    if (key %in% seen_sequences) next
    
    saved <- saved + 1L
    
    out[[saved]] <- save_sequence(
      deg = current,
      step = total_steps,
      profile = profile
    )
    
    seen_profiles <- c(seen_profiles, profile)
    seen_sequences <- c(seen_sequences, key)
    
    if (verbose) {
      message(
        "Saved ", saved,
        " | step = ", total_steps,
        " | dmax = ", max(current),
        " | C = ", round(freeman_from_degree(current), 4),
        " | avg degree = ", round(mean(current), 4),
        " | total degree = ", sum(current),
        " | sd = ", round(sd(current), 3),
        " | IQR = ", round(IQR(current), 3),
        " | nvals = ", length(unique(current))
      )
    }
  }
  
  out <- out[seq_len(saved)]
  
  list(
    degree_sequences = lapply(out, `[[`, "degree_sequence"),
    dmax = sapply(out, `[[`, "dmax"),
    realised_centralisation = sapply(out, `[[`, "realised_centralisation"),
    realised_average_degree = sapply(out, `[[`, "realised_average_degree"),
    realised_total_degree = sapply(out, `[[`, "realised_total_degree"),
    sd_degree = sapply(out, `[[`, "sd_degree"),
    degree_iqr = sapply(out, `[[`, "degree_iqr"),
    n_degree_values = sapply(out, `[[`, "n_degree_values"),
    profiles = sapply(out, `[[`, "profile"),
    steps_saved = sapply(out, `[[`, "step"),
    n_found = saved,
    nsim_requested = nsim,
    size = n,
    average_degree_target = average_degree,
    average_degree_tolerance = average_degree_tolerance,
    freeman_target = freeman_centralisation,
    tolerance = tolerance,
    total_steps = total_steps,
    attempted_moves = attempted_moves,
    accepted_moves = accepted_moves,
    acceptance_rate = if (attempted_moves > 0) accepted_moves / attempted_moves else NA_real_,
    init_steps = init_steps
  )
}

################################################################################

# Testing (moved to Data_simulation.R for final version)

################################################################################

############### Helper function to check output

summariseDegSeq <- function(out) {
  data.frame(
    seq_id = seq_along(out$degree_sequences),
    dmax = out$dmax,
    centralisation = round(out$realised_centralisation, 3),
    average_degree = round(out$realised_average_degree, 3),
    total_degree = sapply(out$degree_sequences, sum),
    sd_degree = round(sapply(out$degree_sequences, sd), 3),
    degree_iqr = sapply(out$degree_sequences, IQR),
    n_degree_values = sapply(out$degree_sequences, function(x) length(unique(x)))
  )
}

############ Wrapper for sampling and testing stages 

makeNetworkBasis <- function(nsim,
                           size,
                           average_degree,
                           average_degree_tolerance,
                           freeman_centralisation,
                           tolerance,
                           min_degree = 1,
                           seed = NULL,
                           verbose = FALSE,
                           cut_breaks = 4,
                           slice_n = 1) {
  
  # Generate degree sequences
  degseq_out <- sampleDegSeq(
    nsim = nsim,
    size = size,
    average_degree = average_degree,
    average_degree_tolerance = average_degree_tolerance,
    freeman_centralisation = freeman_centralisation,
    tolerance = tolerance,
    min_degree = min_degree,
    seed = seed,
    verbose = verbose
  )
  
  # Summarise sampled degree sequences
  degseq_tab <- summariseDegSeq(degseq_out)
  

  degseq_summary <- degseq_tab |>
    dplyr::summarise(
      n = dplyr::n(),
      min_ad = min(average_degree),
      max_ad = max(average_degree),
      mean_ad = mean(average_degree),
      min_c = min(centralisation),
      mean_c = mean(centralisation),
      max_c = max(centralisation),
      min_dmax = min(dmax),
      max_dmax = max(dmax)
    )
  
  # Select varying degree sequences by binned stats
  selected_ids <- degseq_tab |>
    dplyr::mutate(
      ad_bin = cut(average_degree, breaks = cut_breaks),
      c_bin = cut(centralisation, breaks = cut_breaks),
      iqr_bin = cut(degree_iqr, breaks = cut_breaks),
      dmax_bin = cut(dmax, breaks = cut_breaks)
    ) |>
    dplyr::group_by(ad_bin, c_bin, iqr_bin, dmax_bin) |>
    dplyr::slice_sample(n = slice_n) |>
    dplyr::ungroup() |>
    dplyr::pull(seq_id)
  
  selected_degseqs <- degseq_out$degree_sequences[selected_ids]
  
  # Convert to networks
  selected_networks <- netFromDegSeq(selected_degseqs)
  
  # Final list-level summary
  net_summary <- summariseNetworks(selected_networks)
  
  list(
    sampler_output = degseq_out,
    sampler_table = degseq_tab,
    sampler_summary = degseq_summary,
    selected_ids = selected_ids,
    selected_degree_sequences = selected_degseqs,
    networks = selected_networks,
    network_summary = net_summary
  )
}




