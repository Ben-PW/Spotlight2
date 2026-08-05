################################################################################
# Script: config.R
#
# Purpose:
#   Store the user-adjustable settings for the ground-truth network generation
#   and spotlight observation simulation. The default values reproduce the
#   settings currently used in the analysis scripts.
#
# Usage:
#   Source this file near the beginning of main.R:
#
#     source(here::here("config.R"))
#
#   The settings can then be accessed using, for example:
#
#     config$data_simulation$sizes
#     config$spotlight_simulation$alphas
################################################################################

config <- list(
  
  ##############################################################################
  # Workflow controls
  ##############################################################################
  
  workflow = list(
    
    # TRUE: regenerate the ground-truth networks using Data_simulation.R.
    # FALSE: load the supplied pre-generated datasets file.
    full_rerun = FALSE,
    
    # Select the downstream stages to run from main.R.
    run_spotlight_simulation = TRUE,
    run_queries = TRUE,
    run_visualisations = TRUE
  ),
  
  
  ##############################################################################
  # File paths and output handling
  ##############################################################################
  
  paths = list(
    
    # Supplied pre-generated ground-truth networks.
    datasets = here::here(
      "Data",
      "datasets_final"
    ),
    
    # DuckDB results database created by Spotlight_main.R.
    database = here::here(
      "Results",
      "spotlight_probability_results.duckdb"
    ),
    
    # Error log created if an observation condition fails.
    error_log = here::here(
      "Results",
      "probability_simulation_error_log.txt"
    ),
    
    # Directory used by the visualisation scripts.
    figures = here::here("Figures")
  ),
  
  output = list(
    
    # FALSE prevents accidental deletion of an existing results database.
    # Set to TRUE only when intentionally replacing an earlier simulation run.
    overwrite_database = FALSE,
    
    # Save newly generated ground-truth networks after a full rerun.
    save_regenerated_datasets = FALSE,
    
    # Used only when save_regenerated_datasets is TRUE.
    regenerated_datasets_path = here::here(
      "Data",
      "datasets_regenerated"
    )
  ),
  
  
  ##############################################################################
  # Ground-truth data simulation
  ##############################################################################
  
  data_simulation = list(
    
    # Structural conditions used to create the network parameter grid.
    sizes = c(30L, 60L, 120L),
    average_degrees = c(3, 6),
    freeman_centralisations = c(0.1, 0.3, 0.5),
    
    # Permitted density deviation. This is converted to an average-degree
    # tolerance by calc_ad_tol().
    density_tolerance = 0.01,
    
    # Starting seed used to create one deterministic seed per parameter-grid row.
    # Data_simulation.R can use:
    #   seed = config$data_simulation$basis_seed + dplyr::row_number()
    basis_seed = 123L,
    
    # Number of candidate degree sequences requested for each structural
    # condition.
    nsim = 500L,
    
    # Arguments passed to makeNetworkBasis().
    centralisation_tolerance = 0.05,
    min_degree = 1L,
    cut_breaks = 4L,
    slice_n = 3L,
    degree_sampling_verbose = FALSE,
    
    # Controls messages produced while simulating networks from each basis.
    network_simulation_verbose = TRUE,
    
    # Seed used when selecting the final valid networks.
    network_sampling_seed = 123L,
    
    # Number of validated ground-truth networks retained per structural
    # condition.
    target_networks_per_condition = 100L
  ),
  
  
  ##############################################################################
  # Spotlight observation simulation
  ##############################################################################
  
  spotlight_simulation = list(
    
    # Seed controlling spotlight assignment and probabilistic tie observation.
    seed = 123L,
    
    # Proportion of nodes assigned to the spotlight.
    spotlight_pcts = c(0.01, 0.05, 0.10),
    
    # Strength of degree-biased spotlight assignment.
    alphas = c(0, 1, 2, 4, 8),
    
    # Observation-probability grid for ties incident on spotlit nodes.
    p_obs_spotlit_values = c(
      0.2,
      0.4,
      0.6,
      0.8,
      1.0
    ),
    
    # Observation-probability grid for ties not incident on spotlit nodes.
    p_obs_nonspotlit_values = c(
      0.2,
      0.4,
      0.6,
      0.8,
      1.0
    ),
    
    # Number of graph-condition results accumulated before writing a batch to
    # DuckDB.
    flush = 50L,
    
    # Proportion of networks retained from each dataset condition.
    # Keep at 1 for the full simulation. Smaller values are useful for testing.
    network_sample_fraction = 1
  )
)


################################################################################
# Basic configuration checks
################################################################################

stopifnot(
  is.logical(config$workflow$full_rerun),
  length(config$workflow$full_rerun) == 1L,
  
  all(config$data_simulation$sizes > 0),
  all(config$data_simulation$average_degrees > 0),
  all(config$data_simulation$freeman_centralisations >= 0),
  all(config$data_simulation$freeman_centralisations <= 1),
  config$data_simulation$nsim > 0,
  config$data_simulation$target_networks_per_condition > 0,
  
  all(config$spotlight_simulation$spotlight_pcts > 0),
  all(config$spotlight_simulation$spotlight_pcts <= 1),
  all(config$spotlight_simulation$alphas >= 0),
  all(config$spotlight_simulation$p_obs_spotlit_values >= 0),
  all(config$spotlight_simulation$p_obs_spotlit_values <= 1),
  all(config$spotlight_simulation$p_obs_nonspotlit_values >= 0),
  all(config$spotlight_simulation$p_obs_nonspotlit_values <= 1),
  config$spotlight_simulation$flush > 0,
  config$spotlight_simulation$network_sample_fraction > 0,
  config$spotlight_simulation$network_sample_fraction <= 1
)