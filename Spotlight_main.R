
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
################################################################################


################################ Set up file management ##########################

here::here()


################################ User options ###################################

# TRUE  = regenerate the ground-truth networks from scratch (several hours)
# FALSE = load the supplied pre-generated datasets file

full_rerun <- FALSE


# Select which stages of the workflow to run

run_simulation <- TRUE
run_queries <- TRUE
run_visualisations <- TRUE


################################ File paths #####################################

# File path to pre-simulated networks if not doing a full re-run

datasets_path <- here::here(
  "Data",
  "datasets_final"
)

# File path to where results db will be stored

database_path <- here::here(
  "Results",
  "spotlight_probability_results.duckdb"
)

# If FALSE, simulation will not run if it detects pre-exisitng db

overwrite_database <- FALSE


######################## Obtain ground-truth networks ############################

if (full_rerun) {
  
  message("Regenerating ground-truth networks...")
  
  source(
    here::here(
      "Scripts",
      "Data_simulation.R" # Data_simulation sources the following files:
    )                     # ERGM_simulator.R
  )                       # Degree_sampler.R
                          # Data_simulation_helpers.R
} else {
  
  message("Loading supplied ground-truth networks...")
  
  if (!file.exists(datasets_path)) {
    stop(
      "Ground-truth datasets file not found: ",
      datasets_path
    )
  }
  
  datasets <- readRDS(
    datasets_path
  )
}


# Check that either route successfully created the expected object

if (!exists("datasets")) {
  stop(
    "The ground-truth network object `datasets` was not created."
  )
}

message("Ground-truth networks ready.")


############################ Run simulation #####################################

if (run_simulation) {
  
  message("Starting spotlight simulation...")
  
  source(
    here::here(
      "Scripts",
      "Spotlight_simulation.R" # Sources:
    )                          # Error_simulation_helpers.R
  )                            # Spotlight_helpers.R
  
  message("Spotlight simulation completed.")
}


############################ Run database queries ###############################

if (run_queries) {
  
  if (!file.exists(database_path)) {
    stop(
      "Results database not found: ",
      database_path,
      "\nRun the simulation first or supply an existing database."
    )
  }
  
  message("Preparing analysis data...")
  
  source(
    here::here(
      "Scripts",
      "Queries2.R"
    )
  )
  
  message("Analysis data prepared.")
}


############################ Create visualisations ##############################

if (run_visualisations) {
  
  if (!run_queries) {
    stop(
      "Visualisations currently require Queries.R to be run in the same session."
    )
  }
  
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

