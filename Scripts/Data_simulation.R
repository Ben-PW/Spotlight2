#####################################################################################################

#Data preprocessing script. Candidate networks for error simulation created here

#####################################################################################################

# requires
source(here::here("Scripts", "Data_simulation_helpers.R"))
source(here::here("Scripts", "Degree_sampler.R"))
source(here::here("Scripts", "ERGM_simulator.R"))

############################## Generate degree sequences #############################

############# Create parameter grid ############

basis_grid <- tidyr::expand_grid(
  size = c(30, 60, 120),
  average_degree = c(3, 6),
  freeman_centralisation = c(0.1, 0.3, 0.5)
) |>
  dplyr::mutate(
    average_degree_tolerance = calc_ad_tol(size), # allows 0.01 density variation
    name = purrr::pmap_chr(
      list(size, average_degree, freeman_centralisation),
      make_basis_name
    ),
    seed = 123 + dplyr::row_number()
  )


############### Sample degree sequences ##########

# If below is taking ages, change the max steps argument in the degree sampler
# file, currently set to 500,000

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
          nsim = 500,
          size = size,
          average_degree = average_degree,
          average_degree_tolerance = average_degree_tolerance,
          freeman_centralisation = freeman_centralisation,
          tolerance = 0.05,
          min_degree = 1,
          seed = seed,
          verbose = FALSE,
          cut_breaks = 4,
          slice_n = 3
        )
        
        message("Finished ", name)
        
        out
      }
    )
  ) |>
  dplyr::select(name, result) |>
  tibble::deframe()


################## Simulate from degree sequences ##########

datasets <- purrr::imap(
  basis_list,
  function(basis, name) {
    message("Simulating networks from basis ", name, "...")
    
    out <- simulateFromBasis(basis, verbose = TRUE)
    
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

set.seed(123)

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
        
        deg <- igraph::degree(ig, mode = "all")
        
        tibble::tibble(
          dataset = dataset_name,
          replicate_id = as.integer(replicate_id),
          
          network_size = igraph::vcount(ig),
          realised_average_degree = mean(deg),
          
          realised_centralisation = igraph::centr_degree(
            ig,
            mode = "all",
            normalized = TRUE
          )$centralization
        )
      }
    )
  }
) |>
  dplyr::mutate(
    
    target_centralisation = dplyr::case_when(
      stringr::str_detect(dataset, "_c1$") ~ 0.1,
      stringr::str_detect(dataset, "_c3$") ~ 0.3,
      stringr::str_detect(dataset, "_c5$") ~ 0.5,
      TRUE ~ NA_real_
    ),
    
    target_average_degree = as.numeric(
      stringr::str_match(dataset, "_ad([0-9]+)_")[, 2]
    ),
    
    centralisation_tolerance = 0.05,
    
    average_degree_tolerance = calc_ad_tol(
      size = network_size,
      density_tol = 0.01
    ),
    
    inside_centralisation_band =
      !is.na(target_centralisation) &
      abs(realised_centralisation - target_centralisation) <= centralisation_tolerance,
    
    inside_average_degree_band =
      !is.na(target_average_degree) &
      abs(realised_average_degree - target_average_degree) <= average_degree_tolerance,
    
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
      target_n = 100
    )
    
    basis_out
  }
)

#plot(datasets$n60_ad3_c3$diagnostics)
#par(mfrow = c(6,6), mar = c(1,1,1,1))
#plotSimNetworks(datasets$n60_ad3_c3$networks)
