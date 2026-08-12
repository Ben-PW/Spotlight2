################################################################################
# Script: Config.R
#
# Stores the user-adjustable settings for the ground-truth network generation
# and spotlight observation simulation. The default values reproduce the
# settings currently used in the analysis scripts.
#
# Workflow controls:
#   determine which simulation stages are run
#   (data simulation | spotlight simulation | database formatting |
#    database queries | visualisations)
#
# File paths and output handling:
#   controls paths for dataset loading, writing results to db, 
#   querying results from db
#
# Basis network simulation parameters:
#   controls the parameters of the simulated artificial networks
#
# Spotlight simulation:
#   controls the parameters of the simulated spotlight process
#
################################################################################

config <- list(
  
  ############################## Workflow controls ##############################
  
  # Alter values here to control which broad stages of the simulation are
  # carried out
  
  ##############################################################################
  
  workflow = list(
    
    # TRUE: regenerate the ground-truth networks using Data_simulation.R.
    #       (this will take several hours)
    # FALSE: load the supplied pre-generated datasets file.
    full_rerun = FALSE,
    
    # Select the downstream stages to run from Spotlight_main.R.
    # This can be FALSE if simulation has already run and written results to db
    run_spotlight_simulation = FALSE,

    # Format results database to facilitate querying
    # This can be FALSE when querying an existing, already formatted database.
    run_database_formatting = FALSE,

    # Query the formatted database and create analysis data frames in R.
    # This can be FALSE if queried datasets are already in the environment
    run_queries = FALSE,
    
    # Visualise queried data
    run_visualisations = TRUE
  ),
  
  
  ####################### File paths and output handling ########################
  
  # File paths for reading data and writing output
  # Alter values here to modify file paths if required
  
  # Other output handling features are not yet implemented 05/08/26, 
  # but if time permits will allow the saving of post-spotlight networks
  
  ##############################################################################
  
  paths = list(
    
    # Supplied pre-generated ground-truth networks.
    datasets = here::here(
      "Data",
      "datasets_final"
    ),

    # Metadata describing the conditions used to generate the supplied data.
    dataset_conditions = here::here(
      "Data",
      "datasets_final_conditions.csv"
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
    # Simulation will not run if it detects an existing db
    overwrite_database = FALSE
    
    # Save newly generated ground-truth networks after a full rerun.
    # (not yet implemented)
    # save_regenerated_datasets = FALSE,
    
    # Used only when save_regenerated_datasets is TRUE 
    # (not yet implemented)
    # regenerated_datasets_path = here::here(
    #  "Data",
    #  "datasets_regenerated"
    #)
  ),
  
  
  ###################### Basis network simulation parameters ######################

  # Alter values here to alter the characteristics of the simulated networks
  
  # NOTE:I have not implemented a robust validation step for some of these 
  # parameters If impossible parameters are specified, look out for error 
  # messages like:
  
  # "total_degree incompatible with size and min_degree."
  # "Could not construct initial graphical degree sequence."
  # "Could not find initial sequence inside target bands."

  ##############################################################################
  
  data_simulation = list(
    
    ################## Degree sequence generation parameters #####################
    
    # Structural conditions used to create the network parameter grid.
    sizes = c(30L, 60L, 120L),
    average_degrees = c(3, 6),
    freeman_centralisations = c(0.1, 0.3, 0.5),
    
    # Permitted realised density deviation. This is converted to an average-degree
    # tolerance by calc_ad_tol().
    density_tolerance = 0.01,
    
    # Starting seed used to create one deterministic seed per parameter-grid row.
    basis_seed = 123L,
    
    # Number of candidate degree sequences requested for each structural
    # condition.
    nsim = 500L,
    
    # Number of steps the sampler should take wehn generating degree sequences
    sampler_steps = 500000L,
    
    ############## Arguments passed to makeNetworkBasis(). ##################
    
    # Centralisation tolerance
    centralisation_tolerance = 0.05,
    
    # Minimum node degree
    min_degree = 1L, 
    
    # Number of bins used to group degree sequences by average degree,
    # centralisation, degree IQR, and maximum degree.
    # Higher values create more bins, attempting to ensure more variation
    cut_breaks = 4L, 
    
    # Number of degree sequences sampled from each occupied bin combination.
    # Higher values retain more bases from each bin for ERGM simulation.
    slice_n = 3L,
    
    # Set to true for degree generation logs
    degree_sampling_verbose = FALSE,
    
    ####################### ERGM simulation parameters #########################
    
    # Coefficients for ERGM parameters
    nfAtt = 0, # nodefactor of attribute
    nmAtt = 1, # nodematch by attribute
    gwdeg = 1, # geometrically weighted degree (degree dispersion)
    gwesp = 0.4, # geometrically weighted edgewise shared partners
    gwdsp = -0.025, # geometrically weighted dyadwise shared partners
    
    # Controls messages produced while simulating networks from each basis.
    network_simulation_verbose = TRUE,
    network_simulation_seed = 456L,
    
    # Seed used when selecting the final valid networks.
    network_sampling_seed = 123L,
    
    # Determine whether networks must be 1 component or not
    require_connected = TRUE,
    
    # Approximate number of ERGM candidate networks generated for each
    # size × average-degree × centralisation condition, before validation.
    # Incrcease if few valid networks are retrieved
    ergm_candidate_networks_per_condition = 200L,
    
    # Maximum ERGM attempts to produce a 1 component network allowed for each 
    # selected degree sequence.
    # Increase is success rate is low
    # If require_connected = FALSE, this argument is superfluous 
    ergm_max_attempts_per_degree_sequence = 500L,
    
    # Number of validated networks retained for each structural condition
    # (currently 100 networks per density * centralisation * size case)
    target_networks_per_condition = 100L
  ),
  
  
  ######################## Spotlight effect simulation #########################
  
  # Spotlight observation simulation parameters
  
  # Change values here to alter the parameters of the actual spotlight 
  # process
  
  ##############################################################################
  
  spotlight_simulation = list(
    
    # Seed controlling spotlight assignment and probabilistic tie observation.
    seed = 123L,
    
    # Proportion of nodes assigned to the spotlight.
    spotlight_pcts = c(#0.01, 
                       #0.05, 
                       0.10),
    
    # Strength of degree-biased spotlight assignment.
    alphas = c(0, 
              # 1, 
               2, 
             #  4, 
               8),
    
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
    flush = 50L
    
    # Proportion of networks retained from each dataset condition
    # (not yet implemented, but will allow controlling the retention of 
    # observed netwroks)
    # network_sample_fraction = 1
  ),
  
  ########################### Analysis parameters ###############################
  
  analysis = list(
    top_n_proportion = 0.10
  ),

  ######################## Visualisation parameters ###########################

  visualisations = list(

    # Plot families to create. Available values are:
    # coverage | network_bias | node_correlation | top_n_recall |
    # rank_lift_line | rank_lift_contour
    plots_to_run = c(
      "coverage",
      "network_bias",
      "node_correlation",
      "top_n_recall",
      "rank_lift_line",
      "rank_lift_contour"
    ),

    # Spotlight proportion used by every main figure. Coverage heatmaps for
    # other proportions can additionally be saved as supplementary figures.
    main_spotlight_pct = 0.10,

    # Alpha values shown in the figures. NULL uses every alpha available in
    # the queried results database.
    alphas_to_plot = NULL,

    # Node-centrality metrics included in correlation, Top-N and rank-lift
    # figures.
    centrality_metrics = c(
      "Degree",
      "Betweenness",
      "Closeness",
      "Eigenvector"
    ),

    # Correlation figures to create: "rank", "pearson", or both.
    correlation_types = "rank",

    # Network metrics and bias definitions used for bias contours.
    network_bias_metrics = c(
      "dcent",
      "clustering"
    ),
    
    # Can select "absolute" for absolute relative bias plots
    network_bias_types = "relative",

    # Resolution of interpolated contour surfaces.
    interpolation_grid_n = 100L,

    # Optional plot annotations.
    show_coverage_values = TRUE,
    show_top_n_alignment = TRUE,

    # Plot objects are always retained in `visualisation_plots`. These options
    # control PDF output.
    save_plots = TRUE,
    save_supplementary_coverage = TRUE
  )
)


################################################################################
# Basic configuration checks
# Not exhaustive
################################################################################

stopifnot(
  
  # Workflow
  is.logical(config$workflow$full_rerun),
  length(config$workflow$full_rerun) == 1L,
  is.logical(config$workflow$run_spotlight_simulation),
  length(config$workflow$run_spotlight_simulation) == 1L,
  is.logical(config$workflow$run_database_formatting),
  length(config$workflow$run_database_formatting) == 1L,
  is.logical(config$workflow$run_queries),
  length(config$workflow$run_queries) == 1L,
  is.logical(config$workflow$run_visualisations),
  length(config$workflow$run_visualisations) == 1L,
  
  # Degree generation
  all(config$data_simulation$sizes > 0),
  all(config$data_simulation$average_degrees > 0),
  all(config$data_simulation$freeman_centralisations >= 0),
  all(config$data_simulation$freeman_centralisations <= 1),
  
  # ERGM simulation
  config$data_simulation$nsim > 0,
  config$data_simulation$target_networks_per_condition > 0,
  config$data_simulation$cut_breaks > 0,
  config$data_simulation$slice_n > 0,
  config$data_simulation$sampler_steps > 0,
  config$data_simulation$ergm_candidate_networks_per_condition > 0,
  config$data_simulation$ergm_max_attempts_per_degree_sequence > 0,
  is.numeric(config$data_simulation$network_simulation_seed),
  length(config$data_simulation$network_simulation_seed) == 1L,
  
  # Spotlight simulation
  all(config$spotlight_simulation$spotlight_pcts > 0),
  all(config$spotlight_simulation$spotlight_pcts <= 1),
  all(config$spotlight_simulation$alphas >= 0),
  all(config$spotlight_simulation$p_obs_spotlit_values >= 0),
  all(config$spotlight_simulation$p_obs_spotlit_values <= 1),
  all(config$spotlight_simulation$p_obs_nonspotlit_values >= 0),
  all(config$spotlight_simulation$p_obs_nonspotlit_values <= 1),
  config$spotlight_simulation$flush > 0,
  
  # Visualisations
  is.character(config$visualisations$plots_to_run),
  length(config$visualisations$plots_to_run) > 0,
  !anyDuplicated(config$visualisations$plots_to_run),
  is.numeric(config$visualisations$main_spotlight_pct),
  length(config$visualisations$main_spotlight_pct) == 1L,
  is.finite(config$visualisations$main_spotlight_pct),
  config$visualisations$main_spotlight_pct > 0,
  config$visualisations$main_spotlight_pct <= 1,
  is.null(config$visualisations$alphas_to_plot) ||
    (
      is.numeric(config$visualisations$alphas_to_plot) &&
      all(is.finite(config$visualisations$alphas_to_plot)) &&
      all(config$visualisations$alphas_to_plot >= 0)
    ),
  is.character(config$visualisations$centrality_metrics),
  length(config$visualisations$centrality_metrics) > 0,
  is.character(config$visualisations$correlation_types),
  length(config$visualisations$correlation_types) > 0,
  is.character(config$visualisations$network_bias_metrics),
  length(config$visualisations$network_bias_metrics) > 0,
  is.character(config$visualisations$network_bias_types),
  length(config$visualisations$network_bias_types) > 0,
  is.numeric(config$visualisations$interpolation_grid_n),
  length(config$visualisations$interpolation_grid_n) == 1L,
  is.finite(config$visualisations$interpolation_grid_n),
  config$visualisations$interpolation_grid_n >= 2,
  config$visualisations$interpolation_grid_n %% 1 == 0,
  is.logical(config$visualisations$show_coverage_values),
  length(config$visualisations$show_coverage_values) == 1L,
  is.logical(config$visualisations$show_top_n_alignment),
  length(config$visualisations$show_top_n_alignment) == 1L,
  is.logical(config$visualisations$save_plots),
  length(config$visualisations$save_plots) == 1L,
  is.logical(config$visualisations$save_supplementary_coverage),
  length(config$visualisations$save_supplementary_coverage) == 1L
  # config$spotlight_simulation$network_sample_fraction > 0,
  # config$spotlight_simulation$network_sample_fraction <= 1
)
