##################################################################################################
# Script: Spotlight_simulation.R
# 
# This script performs the main spotlight simulation, perturbing networks and 
# storing the results in the local database
##################################################################################################


################################### Database setup ###########################################

here::here()

if (!dir.exists(here::here("Results"))) {
  dir.create(
    here::here("Results"),
    recursive = TRUE
  )
}

# Specify db path using variables defined in Config.R

db_path <- config$paths$database

if (file.exists(db_path)) {
  
  if (config$output$overwrite_database) {
    
    file.remove(db_path)
    
    wal_path <- paste0(db_path, ".wal")
    
    if (file.exists(wal_path)) {
      file.remove(wal_path)
    }
    
  } else {
    
    stop(
      "Database already exists: ",
      db_path,
      "\nSet overwrite_database <- TRUE in main.R to replace it."
    )
  }
}

# Connect to db

con <- DBI::dbConnect(
  duckdb::duckdb(),
  dbdir = db_path
)

# Exit condition for db connection

on.exit(
  {
    if (exists("con") && DBI::dbIsValid(con)) {
      DBI::dbDisconnect(con, shutdown = TRUE)
    }
  },
  add = TRUE
)


################################ Generate baseline networks #################################

#source(here::here("Scripts", "Data_simulation.R"))
source(here::here("Scripts", "Error_simulation_helpers.R"))

# datasets <- readRDS(
#  here::here("Data", "datasets_final")
# )

# Filter to remove nonconforming cases
# IMPORTANT: code to do this is in Data_simulation.R, as that is where it 
# would be run normally


# Subsample for testing

#datasets <- purrr::map(
#  datasets,
#  function(basis_out) {
#    
#    n_current <- length(basis_out$networks)
#    n_keep <- max(1L, floor(n_current * 0.10))
#    
#    keep_ids <- sample.int(
#      n = n_current,
#      size = n_keep,
#      replace = FALSE
#    )
#    
#    basis_out$networks <- basis_out$networks[keep_ids]
#    basis_out
#  }
#)

############################ Convert networks to igraph #####################################

datasets <- datasets |>
  purrr::map(function(basis_out) {
    purrr::map(basis_out$networks, function(g) {
      if (inherits(g, "igraph")) { # input data may be network objects in future
        g
      } else {
        intergraph::asIgraph(g)
      }
    })
  })


############################ Ensure networks are undirected #################################

datasets <- purrr::map(
  datasets,
  undirect
)


############################ Tag ground-truth networks ######################################

datasets <- purrr::imap(
  datasets,
  function(graph_list, ds) {
    tagGraphs(
      graph_list = graph_list,
      dataset = ds,
      source = "true"
    )
  }
)


############################ Assign persistent node IDs #####################################

datasets <- lapply(
  datasets,
  IDNodes
)


############################ Compute ground-truth metrics ###################################

graphGT <- purrr::map_dfr(datasets, computeMetrics) |>
  dplyr::select(
    -realised_missingness,
    -realised_obs_rate_overall,
    -realised_obs_rate_spotlit,
    -realised_obs_rate_nonspotlit,
    -spotlight_edge_coverage,
    -p_obs_spotlit,
    -p_obs_nonspotlit,
    -alpha,
    -spotlight_pct
  )

nodeGT <- purrr::map_dfr(
  datasets,
  computeCentralityDf
)


############################ Write ground truth to database #################################

DBI::dbWriteTable(
  con,
  "network_results_gt",
  graphGT,
  overwrite = TRUE
)

DBI::dbWriteTable(
  con,
  "node_results_gt",
  nodeGT,
  overwrite = TRUE
)

rm(list = c("graphGT", "nodeGT"))
gc(FALSE)


#################################### Begin observation simulation ############################

source(here::here("Scripts", "Spotlight_helpers.R"))


#################################### Simulation parameters ###################################

# Spotlight percentages
spotlight_pcts <- config$spotlight_simulation$spotlight_pcts

# Degree bias
alphas <- config$spotlight_simulation$alphas

# Spotlit observation probability
p_obs_spotlit_values <- config$spotlight_simulation$p_obs_spotlit_values

# Non-spotlit observation probability
p_obs_nonspotlit_values <- config$spotlight_simulation$p_obs_nonspotlit_values

############################ Display simulation size ########################################

n_probability_conditions <-
  length(p_obs_spotlit_values) *
  length(p_obs_nonspotlit_values)

n_conditions_per_dataset <-
  length(alphas) *
  length(spotlight_pcts) *
  n_probability_conditions

message(
  "Probability combinations: ",
  n_probability_conditions
)

message(
  "Observation conditions per dataset: ",
  n_conditions_per_dataset
)

message(
  "Total graph-condition applications: ",
  n_conditions_per_dataset *
    sum(lengths(datasets))
)


#################################### Database batching setup #################################

global_rows <- list()
node_rows <- list()

kg <- 1L 
kn <- 1L

flush <- config$spotlight_simulation$flush

network_batch_id <- 1L
node_batch_id <- 1L


#################################### Progress setup ###########################################

start_time <- Sys.time()

ds_names <- names(datasets)
ds_total <- length(ds_names)
ds_counter <- 0L

set.seed(config$spotlight_simulation$seed)


#################################### Main simulation loop #####################################

tryCatch(
  
  expr = {
    
    for (ds in names(datasets)) {
      
      ds_counter <- ds_counter + 1L
      base_list <- datasets[[ds]]
      
      for (a in alphas) {
        
        for (sp in spotlight_pcts) {
          
          # Spotlight assignment remains fixed across every probability
          # combination within this dataset × alpha × spotlight_pct condition.
          sp_list <- assignSpotlight(
            graph_list = base_list,
            spotlight_pct = sp,
            alpha = a
          )
          
          for (p_spot in p_obs_spotlit_values) {
            
            for (p_nonspot in p_obs_nonspotlit_values) {
              
              tryCatch(
                
                expr = {
                  
                  ################ Apply probabilistic observation ################
                  
                  obs_list <- observeSpotlight(
                    graph_list = sp_list,
                    p_obs_spotlit = p_spot,
                    p_obs_nonspotlit = p_nonspot
                  )
                  
                  
                  ################ Store simulation metadata #######################
                  
                  obs_list <- tagGraphs(
                    graph_list = obs_list,
                    dataset = ds,
                    source = "observed",
                    alpha = a,
                    spotlight_pct = sp,
                    p_obs_spotlit = p_spot,
                    p_obs_nonspotlit = p_nonspot
                  )
                  
                  ################ Calculate network-level metrics #################
                  
                  global_rows[[kg]] <- computeMetrics(obs_list)
                  kg <- kg + 1L
                  
                  
                  ################ Flush network metrics to DuckDB ##################
                  
                  if ((kg - 1L) >= flush) {
                    
                    network_batch <- dplyr::bind_rows(
                      global_rows
                    )
                    
                    DBI::dbWriteTable(
                      con,
                      "network_results",
                      network_batch,
                      append = DBI::dbExistsTable(
                        con,
                        "network_results"
                      )
                    )
                    
                    global_rows <- list()
                    kg <- 1L
                    network_batch_id <- network_batch_id + 1L
                    
                    rm(network_batch)
                    gc(FALSE)
                  }
                  
                  ################ Calculate node-level metrics ####################
                  
                  node_rows[[kn]] <- computeCentralityDf(obs_list)
                  kn <- kn + 1L
                  
                  ################ Flush node metrics to DuckDB #####################
                  
                  if ((kn - 1L) >= flush) {
                    
                    node_batch <- dplyr::bind_rows(
                      node_rows
                    )
                    
                    DBI::dbWriteTable(
                      con,
                      "node_results",
                      node_batch,
                      append = DBI::dbExistsTable(
                        con,
                        "node_results"
                      )
                    )
                    
                    node_rows <- list()
                    kn <- 1L
                    node_batch_id <- node_batch_id + 1L
                    
                    rm(node_batch)
                    gc(FALSE)
                  }
                  
                  rm(obs_list)
                  
                },
                
                error = function(e) {
                  
                  msg <- paste0(
                    "ERROR",
                    " | dataset=", ds,
                    " | alpha=", a,
                    " | spotlight_pct=", sp,
                    " | p_obs_spotlit=", p_spot,
                    " | p_obs_nonspotlit=", p_nonspot,
                    " | message=", e$message
                  )
                  
                  message(msg)
                  
                  write(
                    paste(
                      Sys.time(),
                      msg,
                      sep = " | "
                    ),
                    file = here::here(
                      "Results",
                      "probability_simulation_error_log.txt"
                    ),
                    append = TRUE
                  )
                  
                  if (exists("obs_list", inherits = FALSE)) {
                    rm(obs_list)
                  }
                  
                  gc(FALSE)
                }
              )
            }
          }
          
          rm(sp_list)
          gc(FALSE)
        }
      }
      
      
      ################ Dataset-level progress message #############################
      
      elapsed_minutes <- as.numeric(
        difftime(
          Sys.time(),
          start_time,
          units = "mins"
        )
      )
      
      message(
        "Dataset ",
        ds_counter,
        "/",
        ds_total,
        " completed: ",
        ds,
        " | elapsed: ",
        round(elapsed_minutes, 2),
        " mins"
      )
    }
  },
  
  
  #################################### Final database flush ##################################
  
  finally = {
    
    if (length(global_rows) > 0L) {
      
      network_batch <- dplyr::bind_rows(
        global_rows
      )
      
      DBI::dbWriteTable(
        con,
        "network_results",
        network_batch,
        append = DBI::dbExistsTable(
          con,
          "network_results"
        )
      )
      
      rm(network_batch)
    }
    
    if (length(node_rows) > 0L) {
      
      node_batch <- dplyr::bind_rows(
        node_rows
      )
      
      DBI::dbWriteTable(
        con,
        "node_results",
        node_batch,
        append = DBI::dbExistsTable(
          con,
          "node_results"
        )
      )
      
      rm(node_batch)
    }
    
    if (exists("con") && DBI::dbIsValid(con)) {
      DBI::dbDisconnect(
        con,
        shutdown = TRUE
      )
    }
  }
)


#################################### Cleanup ####################################

rm(
  list = intersect(
    c(
      "a",
      "alphas",
      "ans",
      "ds",
      "flush",
      "kg",
      "kn",
      "node_batch_id",
      "network_batch_id",
      "p_nonspot",
      "p_obs_nonspotlit_values",
      "p_obs_spotlit_values",
      "p_spot",
      "sp",
      "spotlight_pcts"
    ),
    ls()
  )
)

print("Probability simulation completed and environment cleaned")


################################################################################

