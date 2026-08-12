################################################################################
# Script: main.R
#
# Purpose:
#   Coordinate the complete spotlight simulation and analysis workflow.
#
#   The script can either:
#   1. Regenerate the ground-truth networks from scratch; or
#   2. Load the pre-generated networks supplied with the repository.
#
#   It then runs the spotlight simulation, database queries, and visualisations
#   according to the options specified below.
#   If a database already exists, you can alter the config file to run just
#   The queries and visualisations
################################################################################


############################ Source user defined vars ##########################

source(here::here("Config", "config.R"))

######################## Obtain ground-truth networks ############################

needs_ground_truth_networks <-
  config$workflow$full_rerun ||
  config$workflow$run_spotlight_simulation

if (needs_ground_truth_networks) {

# Remove existing inputs in case the workflow was partially run in this session.
rm(
  list = intersect(
    c("datasets", "simulation_conditions"),
    ls()
  )
)

# Regenerate the ground truth networks if specified
if (config$workflow$full_rerun) {
  
  message("Regenerating ground-truth networks...")
  
  source(
    here::here(
      "Scripts",
      "Data_simulation.R" # Data_simulation sources the following files:
    )                     # ERGM_simulator.R
  )                       # Degree_sampler.R
                          # Data_simulation_helpers.R

  # Data_simulation.R returns rich condition objects. Extract their metadata
  # and reduce the downstream dataset to a consistent named list of graph lists.
  generated_datasets <- datasets

  simulation_conditions <- purrr::imap_dfr(
    generated_datasets,
    function(basis_out, dataset_name) {
      tibble::tibble(
        dataset = dataset_name,
        target_size = basis_out$sampler_output$size,
        target_average_degree =
          basis_out$sampler_output$average_degree_target,
        target_centralisation =
          basis_out$sampler_output$freeman_target,
        density_tolerance =
          config$data_simulation$density_tolerance,
        average_degree_tolerance =
          basis_out$sampler_output$average_degree_tolerance,
        centralisation_tolerance =
          basis_out$sampler_output$tolerance,
        n_networks = length(basis_out$networks)
      )
    }
  )

  datasets <- purrr::map(
    generated_datasets,
    "networks"
  )

  rm(generated_datasets)
  
  # Or load the supplied data instead of regenerating
} else {
  
  message("Loading supplied ground-truth networks...")
  
  if (!file.exists(config$paths$datasets)) {
    stop(
      "Ground-truth datasets file not found: ",
      config$paths$datasets
    )
  }

  if (!file.exists(config$paths$dataset_conditions)) {
    stop(
      "Ground-truth dataset metadata file not found: ",
      config$paths$dataset_conditions
    )
  }
  
  datasets <- readRDS(
    config$paths$datasets
  )

  simulation_conditions <- utils::read.csv(
    config$paths$dataset_conditions,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}


# Check that either route created the canonical downstream objects.

if (!exists("datasets", inherits = FALSE)) {
  stop(
    "The ground-truth network object `datasets` was not created."
  )
}

if (!exists("simulation_conditions", inherits = FALSE)) {
  stop(
    "The dataset metadata object `simulation_conditions` was not created."
  )
}

if (
  length(datasets) == 0L ||
  is.null(names(datasets)) ||
  anyNA(names(datasets)) ||
  any(names(datasets) == "") ||
  anyDuplicated(names(datasets))
) {
  stop("`datasets` must be a non-empty, uniquely named list.")
}

required_condition_columns <- c(
  "dataset",
  "target_size",
  "target_average_degree",
  "target_centralisation",
  "density_tolerance",
  "average_degree_tolerance",
  "centralisation_tolerance",
  "n_networks"
)

missing_condition_columns <- setdiff(
  required_condition_columns,
  names(simulation_conditions)
)

if (length(missing_condition_columns) > 0L) {
  stop(
    "Dataset metadata is missing required columns: ",
    paste(missing_condition_columns, collapse = ", ")
  )
}

if (anyDuplicated(simulation_conditions$dataset)) {
  stop("Dataset metadata contains duplicate dataset names.")
}

if (!setequal(
  names(datasets),
  simulation_conditions$dataset
)) {
  stop(
    "Ground-truth dataset names do not match the dataset metadata."
  )
}

# Match the metadata row order to the named dataset list.
simulation_conditions <- simulation_conditions[
  match(names(datasets), simulation_conditions$dataset),
  required_condition_columns,
  drop = FALSE
]

numeric_condition_columns <- setdiff(
  required_condition_columns,
  "dataset"
)

invalid_condition_columns <- vapply(
  simulation_conditions[numeric_condition_columns],
  function(x) {
    !is.numeric(x) || anyNA(x) || any(!is.finite(x))
  },
  logical(1)
)

if (any(invalid_condition_columns)) {
  stop(
    "Dataset metadata contains invalid numeric values in: ",
    paste(
      names(invalid_condition_columns)[invalid_condition_columns],
      collapse = ", "
    )
  )
}

actual_network_counts <- lengths(datasets)

if (!all(
  actual_network_counts == simulation_conditions$n_networks
)) {
  stop(
    "Ground-truth network counts do not match the dataset metadata."
  )
}

get_network_size <- function(g) {
  if (inherits(g, "igraph")) {
    return(igraph::vcount(g))
  }

  if (inherits(g, "network")) {
    return(network::network.size(g))
  }

  stop("Ground-truth data contains an unsupported graph class.")
}

actual_network_sizes <- vapply(
  datasets,
  function(graph_list) {
    if (length(graph_list) == 0L) {
      stop("A ground-truth dataset contains no networks.")
    }

    sizes <- unique(
      vapply(
        graph_list,
        get_network_size,
        numeric(1)
      )
    )

    if (length(sizes) != 1L) {
      stop(
        "A ground-truth dataset contains networks of different sizes."
      )
    }

    sizes
  },
  numeric(1)
)

if (!all(
  actual_network_sizes == simulation_conditions$target_size
)) {
  stop(
    "Ground-truth network sizes do not match the dataset metadata."
  )
}

message("Ground-truth networks ready.")

} else {
  message(
    "Skipping ground-truth network loading; ",
    "the requested stages use the existing results database."
  )
}


############################ Run simulation #####################################

if (config$workflow$run_spotlight_simulation) {
  
  message("Starting spotlight simulation...")
  
  source(
    here::here(
      "Scripts",
      "Spotlight_simulation.R" # Sources:
    )                          # Error_simulation_helpers.R
  )                            # Spotlight_helpers.R
  
  message("Spotlight simulation completed.")
}


########################### Format results database #############################

if (config$workflow$run_database_formatting) {

  if (!file.exists(config$paths$database)) {
    stop(
      "Results database not found: ",
      config$paths$database,
      "\nRun the simulation first or supply an existing database."
    )
  }

  message("Formatting results database...")

  source(
    here::here(
      "Scripts",
      "Database_formatting.R"
    )
  )

  message("Results database formatted.")
}


############################ Run database queries ###############################

if (config$workflow$run_queries) {
  
  if (!file.exists(config$paths$database)) {
    stop(
      "Results database not found: ",
      config$paths$database,
      "\nRun the simulation first or supply an existing database."
    )
  }
  
  message("Preparing analysis data...")
  
  source(
    here::here(
      "Scripts",
      "Database_queries.R"
    )
  )
  
  message("Analysis data prepared.")
}


############################ Create visualisations ##############################

if (config$workflow$run_visualisations) {
  
  # if (!config$workflow$run_queries) {
  #   stop(
  #     "Visualisations currently require Queries.R to be run in the same session."
  #   )
  # }
  
  message("Creating visualisations...")
  
  source(
    here::here(
      "Scripts",
      "Visualisations.R"
    )
  )
  
  message("Visualisations completed.")
}


################################ Completion #####################################

message("Requested workflow stages completed.")

