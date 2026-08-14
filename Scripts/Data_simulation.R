#####################################################################################################
# Script: Data_simulation.R
#
# This script runs the degree sequence sampler and passes the result into the 
# ERGM simulator. It then filters the resultant networks to ensure they conform
# to the prespecified parameter ranges (potential rounding error in the sampler
# was leading to invalud output)
#
#####################################################################################################

# requires
source(here::here("Scripts", "Data_simulation_helpers.R"))
source(here::here("Scripts", "Degree_generator.R"))
source(here::here("Scripts", "ERGM_simulator.R"))

############################## Generate degree sequences #############################

############# Create parameter grid ############

basis_grid <- tidyr::expand_grid(
  size = config$data_simulation$sizes,
  average_degree = config$data_simulation$average_degrees,
  freeman_centralisation = config$data_simulation$freeman_centralisations
) |>
  dplyr::mutate(
    average_degree_tolerance = calc_ad_tol(size = size, 
                                           density_tol = config$data_simulation$density_tolerance),
    name = purrr::pmap_chr(
      list(size, average_degree, freeman_centralisation),
      make_basis_name
    ),
    seed = config$data_simulation$basis_seed
    + dplyr::row_number()
  )

############# Create a lookup table to check simulated networks ############

target_lookup <- basis_grid |>
  dplyr::transmute(
    dataset = name,
    target_average_degree = average_degree,
    target_centralisation = freeman_centralisation
  )

############### Sample degree sequences ##########

# If below is taking ages, change the max steps argument in the Degree_sampler.R
# file, currently set to 500,000 (added to the config file)

basis_list <- basis_grid |>
  dplyr::mutate(
    result = purrr::pmap(
      list(
        name,
        size,
        average_degree,
        average_degree_tolerance,
        freeman_centralisation,
        seed
      ),
      function(name,
               size, 
               average_degree, 
               average_degree_tolerance, 
               freeman_centralisation,
               seed) {
        
        message("Sampling degseq for ", name, "...")
        
        out <- makeNetworkBasis(
          nsim = config$data_simulation$nsim,
          size = size,
          average_degree = average_degree,
          average_degree_tolerance = average_degree_tolerance,
          freeman_centralisation = freeman_centralisation,
          tolerance = config$data_simulation$centralisation_tolerance,
          min_degree = config$data_simulation$min_degree,
          seed = seed,
          verbose = config$data_simulation$degree_sampling_verbose,
          cut_breaks = config$data_simulation$cut_breaks,
          slice_n = config$data_simulation$slice_n
        )
        
        message("Finished ", name)
        
        out
      }
    )
  ) |>
  dplyr::select(name, result) |>
  tibble::deframe()


################## Simulate from degree sequences ##########

if (!is.null(
  config$data_simulation$network_simulation_seed
)) {
  set.seed(
    config$data_simulation$network_simulation_seed
  )
}

datasets <- purrr::imap(
  basis_list,
  function(basis, name) {
    message("Simulating networks from basis ", name, "...")
    
    out <- simulateFromBasis(basis, 
                             target_total = config$data_simulation$ergm_candidate_networks_per_condition,
                             verbose = config$data_simulation$network_simulation_verbose)
    
    message("Completed simulation for basis ", name)
    
    out
  }
)

#saveRDS(
#  datasets,
#  file = here::here("Data", "Run_14_05_26", "datasets")
#)

# Datasets saved from earlier
# datasets <- readRDS(here::here("Data", "datasets_final"))


################# Sample down to 100 ##############
# Retaining as many unique basis ids as possible
# Also sample to ensure networks meet requirements. There were some which didn't
# fit the criteria, this was likely due to rounding in the degree sequence generator
# average degree is fine, sometimes centralisation slipped

set.seed(
  config$data_simulation$network_sampling_seed
)

dataset_check <- purrr::imap_dfr(
  datasets,
  function(basis_out, dataset_name) {
    
    purrr::imap_dfr(
      basis_out$networks,
      function(g, replicate_id) {
        
        ig <- if (inherits(g, "igraph")) {
          g
        } else {
          intergraph::asIgraph(g)
        }
        
        deg <- igraph::degree(
          ig,
          mode = "all"
        )
        
        tibble::tibble(
          dataset = dataset_name,
          
          replicate_id =
            as.integer(replicate_id),
          
          network_size =
            igraph::vcount(ig),
          
          realised_average_degree =
            mean(deg),
          
          realised_centralisation =
            igraph::centr_degree(
              ig,
              mode = "all",
              normalized = TRUE
            )$centralization
        )
      }
    )
  }
) |>
  dplyr::left_join(
    target_lookup,
    by = "dataset"
  ) |>
  dplyr::mutate(
    centralisation_tolerance =
      config$data_simulation$centralisation_tolerance,
    
    average_degree_tolerance = calc_ad_tol(
      size = network_size,
      density_tol =
        config$data_simulation$density_tolerance
    ),
    
    inside_centralisation_band =
      !is.na(target_centralisation) &
      abs(
        realised_centralisation -
          target_centralisation
      ) <= centralisation_tolerance,
    
    inside_average_degree_band =
      !is.na(target_average_degree) &
      abs(
        realised_average_degree -
          target_average_degree
      ) <= average_degree_tolerance,
    
    inside_target_band =
      inside_centralisation_band &
      inside_average_degree_band
  )

# Filter to pull only the valid networks, trying to retain as many different
# degree sequence ids as possible

datasets <- purrr::imap(
  datasets,
  function(basis_out, dataset_name) {
    
    valid_ids <- dataset_check |>
      dplyr::filter(
        dataset == dataset_name,
        inside_target_band
      ) |>
      dplyr::pull(replicate_id)
    
    valid_networks <- basis_out$networks[valid_ids]
    
    basis_out$networks <- sampleDatasets(
      networks = valid_networks,
      target_n = config$data_simulation$target_networks_per_condition
    )
    
    basis_out
  }
)

#plot(datasets$n60_ad3_c3$diagnostics)
#par(mfrow = c(6,6), mar = c(1,1,1,1))
#plotSimNetworks(datasets$n60_ad3_c3$networks)
