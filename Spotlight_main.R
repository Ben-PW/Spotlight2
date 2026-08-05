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

source(here::here("Config", "Config.R"))

######################## Obtain ground-truth networks ############################

# Remove any existing dataset var in case sim has been partially run before
if (exists("datasets", inherits = FALSE)) {
  rm(datasets)
}

if (config$workflow$full_rerun) {
  
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
  
  if (!file.exists(config$paths$datasets)) {
    stop(
      "Ground-truth datasets file not found: ",
      config$paths$datasets
    )
  }
  
  datasets <- readRDS(
    config$paths$datasets
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
      "Queries2.R"
    )
  )
  
  message("Analysis data prepared.")
}


############################ Create visualisations ##############################

if (config$workflow$run_visualisations) {
  
  if (!config$workflow$run_queries) {
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

