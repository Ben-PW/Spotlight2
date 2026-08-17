################################################################################
# Script: Queries3_database_queries.R
#
# Queries an already formatted spotlight-results database and creates the R
# analysis data frames consumed by the visualisation scripts. This script does
# not recreate either persistent ranked-node table (done in fomratting)
#
# Requires config$analysis$top_n_proportion to define top-N membership.
################################################################################

library(DBI)
library(duckdb)
library(here)
library(dplyr)
library(dbplyr)

if (!exists("config", inherits = TRUE)) {
  stop("config must be loaded before running database queries.")
}

top_n_proportion <- config$analysis$top_n_proportion

# Validate Top N selection
if (
  is.null(top_n_proportion) ||
  length(top_n_proportion) != 1L ||
  !is.numeric(top_n_proportion) ||
  !is.finite(top_n_proportion) ||
  top_n_proportion <= 0 ||
  top_n_proportion > 1
) {
  stop(
    "`config$analysis$top_n_proportion` must be one finite numeric ",
    "value greater than 0 and no greater than 1."
  )
}

if (!file.exists(config$paths$database)) {
  stop(
    "Results database not found: ",
    config$paths$database,
    "\nRun the spotlight simulation first or supply an existing database."
  )
}

con <- DBI::dbConnect(
  duckdb::duckdb(),
  dbdir = config$paths$database
)

################################## Queries begin here ###########################
tryCatch(
  {

required_tables <- c(
  "network_results",
  "network_results_gt",
  "node_results",
  "node_results_gt",
  "simulation_conditions",
  "node_results_GT_ranked",
  "node_results_ranked"
)

missing_tables <- setdiff(
  required_tables,
  DBI::dbListTables(con)
)

if (length(missing_tables) > 0L) {
  stop(
    "The results database is missing required tables: ",
    paste(missing_tables, collapse = ", "),
    "\nRun database formatting first by setting ",
    "`config$workflow$run_database_formatting = TRUE`."
  )
}

simulation_conditions_df <- DBI::dbReadTable(
  con,
  "simulation_conditions"
)

required_condition_columns <- c(
  "dataset",
  "target_size",
  "target_average_degree",
  "target_centralisation",
  "average_degree_tolerance",
  "centralisation_tolerance"
)

missing_condition_columns <- setdiff(
  required_condition_columns,
  names(simulation_conditions_df)
)

if (length(missing_condition_columns) > 0L) {
  stop(
    "`simulation_conditions` is missing required columns: ",
    paste(missing_condition_columns, collapse = ", ")
  )
}

if (anyDuplicated(simulation_conditions_df$dataset)) {
  stop("`simulation_conditions` contains duplicate dataset rows.")
}

condition_value_columns <- c(
  "target_size",
  "target_average_degree",
  "target_centralisation",
  "average_degree_tolerance",
  "centralisation_tolerance"
)

# Check provided data simulation parameters
invalid_condition_values <- vapply(
  simulation_conditions_df[condition_value_columns],
  function(x) {
    !is.numeric(x) || anyNA(x) || any(!is.finite(x))
  },
  logical(1)
)

if (any(invalid_condition_values)) {
  stop(
    "`simulation_conditions` contains invalid values in: ",
    paste(
      names(invalid_condition_values)[invalid_condition_values],
      collapse = ", "
    )
  )
}

# Check that all produced datasets match the simulation params
result_datasets <- DBI::dbGetQuery(
  con,
  "SELECT DISTINCT dataset FROM network_results_gt"
)$dataset

unmatched_datasets <- setdiff(
  result_datasets,
  simulation_conditions_df$dataset
)

if (length(unmatched_datasets) > 0L) {
  stop(
    "No simulation-condition metadata found for datasets: ",
    paste(unmatched_datasets, collapse = ", ")
  )
}

# Attach data generation conditions to specified df
attach_condition_metadata <- function(df) {
  dplyr::left_join(
    df,
    simulation_conditions_df,
    by = "dataset",
    relationship = "many-to-one" # many analysis rows to one metadata row
  )
}

# Allows queries to adapt to selected Top N proportion
DBI::dbExecute(
  con,
  paste0(
    "CREATE OR REPLACE TEMP TABLE analysis_parameters AS ",
    "SELECT CAST(? AS DOUBLE) AS top_n_proportion"
  ),
  params = list(top_n_proportion)
)

############# Query top of databases to give a visual on structure ##############

# Below are primarily useful for getting table structure for writing your own
# queries, they are commented out because they are very computationally expensive

# node_results_gt <- DBI::dbGetQuery(con, "
#                                 SELECT *
#                                 FROM node_results_gt
#                                 ORDER BY dataset, replicate_id, alpha, spotlight_pct, NodeID
#                                 LIMIT 50;
#                                 ")
# 
# node_results <- DBI::dbGetQuery(con, "
#                                 SELECT *
#                                 FROM node_results
#                                 ORDER BY dataset, replicate_id, alpha, spotlight_pct, NodeID
#                                 LIMIT 50;
#                                 ")
# 
# node_results_ranked <- DBI::dbGetQuery(con, "
#                                       SELECT *
#                                       FROM node_results_ranked
#                                       ORDER BY dataset, replicate_id, alpha, spotlight_pct, NodeID
#                                       LIMIT 50;
#                                        ")
# 
# network_results_gt <- DBI::dbGetQuery(con, "
#                                       SELECT *
#                                       FROM network_results_gt
#                                       LIMIT 50;
#                                       ")
# 
# network_results <- DBI::dbGetQuery(con, "
#                                    SELECT *
#                                    FROM network_results
#                                    LIMIT 50;
#                                    ")

##################################### SAMPLING COVERAGE #####################################

# This is the query for heatmps to show sampling coverage over different conditions of
# spotlight effect

coverage_heatmap_df <- DBI::dbGetQuery(
  con,
  "
  WITH gt_aug AS (
    SELECT
      gt.dataset,
      gt.replicate_id,

      conditions.target_size,
      conditions.target_average_degree,
      conditions.target_centralisation,

      gt.size AS realised_size,
      gt.density * (gt.size - 1) AS realised_average_degree,
      gt.dcent AS realised_centralisation

    FROM network_results_gt AS gt

    INNER JOIN simulation_conditions AS conditions
      ON gt.dataset = conditions.dataset

    WHERE gt.source = 'true'
  )

  SELECT
    obs.alpha,
    obs.spotlight_pct,
    obs.p_obs_spotlit,
    obs.p_obs_nonspotlit,

    gt.target_centralisation,

    AVG(obs.realised_missingness) AS mean_missingness,
    MEDIAN(obs.realised_missingness) AS median_missingness,

    quantile_cont(
      obs.realised_missingness,
      0.25
    ) AS missingness_q25,

    quantile_cont(
      obs.realised_missingness,
      0.75
    ) AS missingness_q75,

    COUNT(*) AS n_graphs

  FROM network_results AS obs

  INNER JOIN gt_aug AS gt
    ON obs.dataset = gt.dataset
   AND obs.replicate_id = gt.replicate_id

  WHERE obs.source = 'observed'
    AND obs.realised_missingness IS NOT NULL

  GROUP BY
    obs.alpha,
    obs.spotlight_pct,
    obs.p_obs_spotlit,
    obs.p_obs_nonspotlit,
    gt.target_centralisation

  ORDER BY
    obs.spotlight_pct,
    gt.target_centralisation,
    obs.alpha,
    obs.p_obs_spotlit,
    obs.p_obs_nonspotlit
  "
)

#################################### NETWORK BIAS ###################################

######################## Query to get network level differences ######################

# Because the tables for network level results are comparatively small, they can
# just be queried straight from the database. For larger runs would need a proper
# query

network_bias_df <- tbl(con, "network_results") %>%
  inner_join(
    tbl(con, "network_results_gt"),
    by = c("dataset", "replicate_id"),
    suffix = c("_obs", "_gt")
  ) %>%
  mutate(
    density_ARB = (density_obs - density_gt)/density_gt,
    dcent_ARB = (dcent_obs - dcent_gt)/dcent_gt,
    clustering_ARB = (clustering_obs - clustering_gt)/clustering_gt,
    APL_ARB = (APL_obs - APL_gt)/APL_gt
  ) %>%
  collect()

network_bias_df <- attach_condition_metadata(network_bias_df)


################################## Node bias plots ##############################

################################# Correlation plots #############################

# This dataframe is to calculate the pearson and spearman correlations between the
# node level centrality values of the data. After testing it turns out relative 
# bias metrics aren't suitable

# Bit of a gross query but I find creating the temporary table first easier to wrap
# my head around

######################### Correlation query ####################################

# Rank correlations use the average-rank columns created in the ranked tables.
# Non-finite closeness values therefore contribute as a common bottom tie.
# Raw closeness value correlations remain restricted to finite value pairs.

node_corr_df <- DBI::dbGetQuery(con, "

-- Creating a temporary table temp of all the necessary variables

    WITH temp AS (
      SELECT
      
      -- listing obs_rank vars out explicitly for clarity instead of using SELECT *
      
        obs_rank.dataset,
        obs_rank.replicate_id,
        -- obs_rank.graph_id, removed as probably useless
        obs_rank.alpha,
        obs_rank.spotlight_pct,
        obs_rank.p_obs_spotlit,
        obs_rank.p_obs_nonspotlit,
        
        -- observed rank centralisation
        
        obs_rank.Degree_raw_avg_rank AS obs_degree_rank,
        obs_rank.Betweenness_raw_avg_rank AS obs_betweenness_rank,
        obs_rank.Closeness_raw_avg_rank AS obs_closeness_rank,
        obs_rank.Eigenvector_avg_rank AS obs_eigenvector_rank,
      
        -- gt rank centralisation
        
        gt_rank.Degree_raw_avg_rank AS gt_degree_rank,
        gt_rank.Betweenness_raw_avg_rank AS gt_betweenness_rank,
        gt_rank.Closeness_raw_avg_rank AS gt_closeness_rank,
        gt_rank.Eigenvector_avg_rank AS gt_eigenvector_rank,
        
        -- obsereved centralisation
        
        obs_rank.Degree_raw AS obs_degree,
        obs_rank.Closeness_raw AS obs_closeness,
        obs_rank.Betweenness_raw AS obs_betweenness,
        obs_rank.Eigenvector AS obs_eigenvector,
        
        -- gt centralisation
        
        gt_rank.Degree_raw AS gt_degree,
        gt_rank.Closeness_raw AS gt_closeness,
        gt_rank.Betweenness_raw AS gt_betweenness,
        gt_rank.Eigenvector AS gt_eigenvector,
      
        -- get info on data simulation params for testing later
        
        net.density * (net.size - 1) AS net_av_degree,
        net.dcent AS net_centralisation,
        net.size AS net_size
      
      -- join the required tables to supply selected variables
      
      FROM node_results_ranked AS obs_rank
      
      JOIN node_results_GT_ranked AS gt_rank
        ON obs_rank.dataset = gt_rank.dataset
        AND obs_rank.replicate_id = gt_rank.replicate_id
        AND obs_rank.NodeID = gt_rank.NodeID
        
      JOIN network_results_gt AS net
        ON obs_rank.dataset = net.dataset
        AND obs_rank.replicate_id = net.replicate_id
      
    )
    
    -- Pull all the required variables from temp
    
    SELECT
    
      dataset,
      replicate_id,
      -- graph_id, removed, as above
      alpha,
      spotlight_pct,
      p_obs_spotlit,
      p_obs_nonspotlit,
      
      net_size,
      net_av_degree,
      net_centralisation,
      
      corr(obs_degree_rank, gt_degree_rank) AS degree_rank_corr,
      corr(obs_closeness_rank, gt_closeness_rank) AS closeness_rank_corr,
      corr(obs_betweenness_rank, gt_betweenness_rank) AS betweenness_rank_corr,
      corr(obs_eigenvector_rank, gt_eigenvector_rank) AS eigenvector_rank_corr,
      
      corr(obs_degree, gt_degree) AS degree_corr,
      
      /*
      ** Raw closeness correlations cannot include non-finite values. Isolates are
      ** therefore excluded from the Pearson value correlation, but they remain in
      ** the rank correlation through the common bottom-tie rank created above.
      */
      
      corr(
      CASE 
        WHEN isfinite(obs_closeness)
          AND isfinite(gt_closeness)
        THEN obs_closeness
      END,
      CASE
        WHEN isfinite(obs_closeness)
          AND isfinite(gt_closeness)
        THEN gt_closeness
      END
      ) AS closeness_corr,
      
      corr(obs_betweenness, gt_betweenness) AS betweenness_corr,
      corr(obs_eigenvector, gt_eigenvector) AS eigenvector_corr,
      
      COUNT(*) AS n_nodes,

      COUNT(*) FILTER (
        WHERE NOT COALESCE(isfinite(obs_closeness), FALSE)
      ) AS n_obs_closeness_nonfinite
      
    FROM temp
    
    -- Need to aggregate by every other variable
      
    GROUP BY
      
      dataset,
      replicate_id,
      -- graph_id,
      alpha,
      spotlight_pct,
      p_obs_spotlit,
      p_obs_nonspotlit,
      net_size,
      net_av_degree,
      net_centralisation
      
    ORDER BY
      dataset,
      replicate_id,
      alpha,
      spotlight_pct,
      p_obs_spotlit,
      p_obs_nonspotlit;
")

node_corr_df <- attach_condition_metadata(node_corr_df)

# Diagnostics

# node_corr_df %>%
#   count(net_size, n_nodes)
# 
# node_corr_df %>%
#   summarise(
#     degree_rank_na = sum(is.na(degree_rank_corr)),
#     closeness_rank_na = sum(is.na(closeness_rank_corr)),
#     betweenness_rank_na = sum(is.na(betweenness_rank_corr)),
#     eigenvector_rank_na = sum(is.na(eigenvector_rank_corr)),
#     
#     degree_na = sum(is.na(degree_corr)),
#     closeness_na = sum(is.na(closeness_corr)),
#     betweenness_na = sum(is.na(betweenness_corr)),
#     eigenvector_na = sum(is.na(eigenvector_corr))
#   )

################################# Node rank plots ##################################

# This dataframe is for node level bias calculations, including TopN, OverlapN, etc
# this can potentially be used to investigate node level rank change, will depend
# on precisely how I structure it

node_rank_df <- DBI::dbGetQuery(con, "

/* 
** BLOCK 1: 
** This is the first block of the query, it retrieves all relevant columns
** from the tables in the db and combines them into a temporary table, which the 
** subsequent blocks will query from
*/

    WITH temp AS(
      SELECT
        obs.dataset,
        obs.replicate_id,
        obs.alpha,
        obs.p_obs_spotlit,
        obs.p_obs_nonspotlit,
        obs.NodeID,
        obs.Spotlight,
        obs.spotlight_pct,
        
        obs.Degree_raw_top_position AS obs_degree_rank,
        obs.Betweenness_raw_top_position AS obs_betweenness_rank,
        obs.Closeness_raw_top_position AS obs_closeness_rank,
        obs.Eigenvector_top_position AS obs_eigenvector_rank,
        
        gt.Degree_raw_top_position AS gt_degree_rank,
        gt.Betweenness_raw_top_position AS gt_betweenness_rank,
        gt.Closeness_raw_top_position AS gt_closeness_rank,
        gt.Eigenvector_top_position AS gt_eigenvector_rank,
        
        net.size AS net_size,
        net.density * (net.size - 1) AS net_av_deg,
        net.dcent AS net_centralisation
        
      FROM node_results_ranked AS obs
      
      JOIN node_results_GT_ranked AS gt
        ON obs.dataset = gt.dataset
        AND obs.replicate_id = gt.replicate_id
        AND obs.NodeID = gt.NodeID
        
      JOIN network_results_gt AS net
        ON obs.dataset = net.dataset
        AND obs.replicate_id = net.replicate_id
        
    ),
    
/* 
** BLOCK 2:
** Create a long table with binary indicators for configurable top-N membership.
** obs_top10 and gt_top10 are legacy but retained to stop things breaking.
*/
    
    top10_long AS(
    
    -- Top-N degree centrality
    
    SELECT
      dataset,
      replicate_id,
      alpha,
      spotlight_pct,
      p_obs_spotlit,
      p_obs_nonspotlit,
      NodeID,
      Spotlight,
      net_size,
      net_av_deg,
      net_centralisation,
        
      -- Specify string to go in new 'metric' column in long dataset
        
      'Degree' AS metric,
      obs_degree_rank AS obs_rank,
      gt_degree_rank AS gt_rank,
      CEIL((SELECT top_n_proportion FROM analysis_parameters) * net_size) AS top_n_cutoff,
        
      CASE
        WHEN obs_degree_rank <= CEIL((SELECT top_n_proportion FROM analysis_parameters) * net_size) THEN 1
        ELSE 0
      END AS obs_top10,
        
      CASE 
        WHEN gt_degree_rank <= CEIL((SELECT top_n_proportion FROM analysis_parameters) * net_size) THEN 1
        ELSE 0
      END AS gt_top10
        
    FROM temp
      
    UNION ALL
      
    -- Betweenness centrality
    
    SELECT
      dataset,
      replicate_id,
      alpha,
      spotlight_pct,
      p_obs_spotlit,
      p_obs_nonspotlit,
      NodeID,
      Spotlight,
      net_size,
      net_av_deg,
      net_centralisation,
      
      'Betweenness' AS metric,
      obs_betweenness_rank AS obs_rank,
      gt_betweenness_rank AS gt_rank,
      CEIL((SELECT top_n_proportion FROM analysis_parameters) * net_size) AS top_n_cutoff,
      
      CASE
        WHEN obs_betweenness_rank <= CEIL((SELECT top_n_proportion FROM analysis_parameters) * net_size) THEN 1
        ELSE 0
      END AS obs_top10,
      
      CASE
        WHEN gt_betweenness_rank <= CEIL((SELECT top_n_proportion FROM analysis_parameters) * net_size) THEN 1
        ELSE 0
      END AS gt_top10
      
    FROM temp
    
    UNION ALL
    
    -- Closeness centrality
    
    SELECT
      dataset,
      replicate_id,
      alpha,
      spotlight_pct,
      p_obs_spotlit,
      p_obs_nonspotlit,
      NodeID,
      Spotlight,
      net_size,
      net_av_deg,
      net_centralisation,
      
      'Closeness' AS metric,
      obs_closeness_rank AS obs_rank,
      gt_closeness_rank AS gt_rank,
      CEIL((SELECT top_n_proportion FROM analysis_parameters) * net_size) AS top_n_cutoff,
      
      CASE
        WHEN obs_closeness_rank <= CEIL((SELECT top_n_proportion FROM analysis_parameters) * net_size) THEN 1
        ELSE 0
      END AS obs_top10,
      
      CASE
        WHEN gt_closeness_rank <= CEIL((SELECT top_n_proportion FROM analysis_parameters) * net_size) THEN 1
        ELSE 0
      END AS gt_top10
      
    FROM temp
    
    UNION ALL
    
    -- Eigenvector centrality
    
    SELECT
      dataset,
      replicate_id,
      alpha,
      spotlight_pct,
      p_obs_spotlit,
      p_obs_nonspotlit,
      NodeID,
      Spotlight,
      net_size,
      net_av_deg,
      net_centralisation,
      
      'Eigenvector' AS metric,
      obs_eigenvector_rank AS obs_rank,
      gt_eigenvector_rank AS gt_rank,
      CEIL((SELECT top_n_proportion FROM analysis_parameters) * net_size) AS top_n_cutoff,
      
      CASE
        WHEN obs_eigenvector_rank <= CEIL((SELECT top_n_proportion FROM analysis_parameters) * net_size) THEN 1
        ELSE 0
      END AS obs_top10,
      
      CASE
        WHEN gt_eigenvector_rank <= CEIL((SELECT top_n_proportion FROM analysis_parameters) * net_size) THEN 1
        ELSE 0
      END AS gt_top10
      
    FROM temp
    
    ),
    
/*
** BLOCK 3:
** This block takes the top-N labels stored in the temporary table and counts
** the number of spotlit and non spotlit nodes in each condition, which can then be
** used for outcome metrics
*/

    top_counts AS(
    
    SELECT
      dataset,
      replicate_id,
      alpha,
      spotlight_pct,
      p_obs_spotlit,
      p_obs_nonspotlit,
      metric,

      net_size,
      net_av_deg,
      net_centralisation,
      top_n_cutoff,
      
      COUNT(*) AS n_nodes,
      
      SUM(obs_top10) AS obs_topN,
      SUM(gt_top10) AS gt_topN,
      
      SUM(
        CASE
          WHEN obs_top10 = 1 AND gt_top10 = 1 THEN 1
          ELSE 0
        END
      ) AS intersection_n,
      
      SUM(
        CASE
          WHEN obs_top10 = 1 OR gt_top10 = 1 THEN 1
          ELSE 0
        END
      ) AS union_n,
      
      SUM(Spotlight) AS n_spotlit,
      
      SUM(
        CASE
          WHEN obs_top10 = 1 AND Spotlight = 1 THEN 1
          ELSE 0
        END
      ) AS n_spotlit_obs_top,
      
      SUM(
        CASE
          WHEN gt_top10 = 1 AND Spotlight = 1 THEN 1
          ELSE 0
        END
      ) AS n_spotlit_gt_top
      
    FROM top10_long

    GROUP BY
      dataset,
      replicate_id,
      alpha,
      spotlight_pct,
      p_obs_spotlit,
      p_obs_nonspotlit,
      metric,
      net_size,
      net_av_deg,
      net_centralisation,
      top_n_cutoff
    )

/*
** BLOCK 4:
** This block takes the union, intersection, etc of spotlight topN membership calculated
** in block 3 and converts them into the outcome metrics for this section.
** As of the rank breaking change, some of these are now a bit superfluous
*/

    SELECT
      dataset,
      replicate_id,
      alpha,
      spotlight_pct,
      p_obs_spotlit,
      p_obs_nonspotlit,
      metric,
      
      net_size,
      net_av_deg,
      net_centralisation,
      top_n_cutoff,
      
      n_nodes,
      obs_topN,
      gt_topN,
      intersection_n,
      union_n,
      n_spotlit,
      n_spotlit_obs_top,
      n_spotlit_gt_top,
      
      -- Top-N recovery metrics
      intersection_n / NULLIF(obs_topN, 0) AS precision,
      intersection_n / NULLIF(gt_topN, 0) AS recall,
      intersection_n / NULLIF(union_n, 0) AS jaccard_overlap,

      -- Spotlight rates
      n_spotlit / NULLIF(n_nodes, 0) AS spotlight_rate_overall,
      n_spotlit_obs_top / NULLIF(obs_topN, 0) AS spotlight_rate_obs_top,
      n_spotlit_gt_top / NULLIF(gt_topN, 0) AS spotlight_rate_gt_top,

      -- Spotlight lift
      (n_spotlit_obs_top / NULLIF(obs_topN, 0))
        /
      NULLIF((n_spotlit / NULLIF(n_nodes, 0)), 0)
        AS spotlight_lift_obs_top,

      (n_spotlit_gt_top / NULLIF(gt_topN, 0))
        /
      NULLIF((n_spotlit / NULLIF(n_nodes, 0)), 0)
        AS spotlight_lift_gt_top,

      -- Excess observed spotlight enrichment beyond GT enrichment
      (
        (n_spotlit_obs_top / NULLIF(obs_topN, 0))
          /
        NULLIF((n_spotlit / NULLIF(n_nodes, 0)), 0)
      )
      -
      (
        (n_spotlit_gt_top / NULLIF(gt_topN, 0))
          /
        NULLIF((n_spotlit / NULLIF(n_nodes, 0)), 0)
      )
        AS excess_spotlight_lift

    FROM top_counts

    ORDER BY
      dataset,
      replicate_id,
      alpha,
      spotlight_pct,
      p_obs_spotlit,
      p_obs_nonspotlit,
      metric;
")

node_rank_df <- attach_condition_metadata(node_rank_df)

# node_rank_df %>%
#   filter(
#     p_obs_spotlit == 1,
#     p_obs_nonspotlit == 1
#   ) %>%
#   group_by(metric) %>%
#   summarise(
#     min_jaccard = min(jaccard_overlap, na.rm = TRUE),
#     max_jaccard = max(jaccard_overlap, na.rm = TRUE),
#     n_below_one = sum(
#       abs(jaccard_overlap - 1) > 1e-10,
#       na.rm = TRUE
#     ),
#     .groups = "drop"
#   )

# This dataframe is to get rank change metrics for nodes under different conditions
# of spotlight effect. Not using stuff like TopN, this is more how much the
# nodes' actual ranks changed

rank_lift_df2 <- DBI::dbGetQuery(con, "

/*
** BLOCK 1:
** Join observed ranked node results to GT ranked node results,
** and attach GT network-level structural information.
*/

WITH temp AS (
  SELECT
    obs.dataset,
    obs.replicate_id,
    obs.alpha,
    obs.spotlight_pct,
    obs.p_obs_spotlit,
    obs.p_obs_nonspotlit,
    obs.NodeID,
    obs.Spotlight,

    obs.Degree_raw_avg_rank AS obs_degree_rank,
    obs.Betweenness_raw_avg_rank AS obs_betweenness_rank,
    obs.Closeness_raw_avg_rank AS obs_closeness_rank,
    obs.Eigenvector_avg_rank AS obs_eigenvector_rank,

    gt.Degree_raw_avg_rank AS gt_degree_rank,
    gt.Betweenness_raw_avg_rank AS gt_betweenness_rank,
    gt.Closeness_raw_avg_rank AS gt_closeness_rank,
    gt.Eigenvector_avg_rank AS gt_eigenvector_rank,

    net.size AS net_size,
    net.density * (net.size - 1) AS net_av_deg,
    net.dcent AS net_centralisation

  FROM node_results_ranked AS obs

  JOIN node_results_GT_ranked AS gt
    ON obs.dataset = gt.dataset
   AND obs.replicate_id = gt.replicate_id
   AND obs.NodeID = gt.NodeID

  JOIN network_results_gt AS net
    ON obs.dataset = net.dataset
   AND obs.replicate_id = net.replicate_id
),

/*
** BLOCK 2:
** Convert the wide centrality-rank columns into a long metric format.
** This creates one row per node x graph-condition x centrality metric.
*/

rank_long AS (

  -- Degree centrality
  SELECT
    dataset,
    replicate_id,
    alpha,
    spotlight_pct,
    p_obs_spotlit,
    p_obs_nonspotlit,
    NodeID,
    Spotlight,
    net_size,
    net_av_deg,
    net_centralisation,

    'Degree' AS metric,
    obs_degree_rank AS obs_rank,
    gt_degree_rank AS gt_rank,

    (gt_degree_rank - obs_degree_rank) / NULLIF(net_size - 1, 0) AS norm_rank_lift,
    ABS(gt_degree_rank - obs_degree_rank) / NULLIF(net_size - 1, 0) AS abs_norm_rank_change

  FROM temp

  UNION ALL

  -- Betweenness centrality
  SELECT
    dataset,
    replicate_id,
    alpha,
    spotlight_pct,
    p_obs_spotlit,
    p_obs_nonspotlit,
    NodeID,
    Spotlight,
    net_size,
    net_av_deg,
    net_centralisation,

    'Betweenness' AS metric,
    obs_betweenness_rank AS obs_rank,
    gt_betweenness_rank AS gt_rank,

    (gt_betweenness_rank - obs_betweenness_rank) / NULLIF(net_size - 1, 0) AS norm_rank_lift,
    ABS(gt_betweenness_rank - obs_betweenness_rank) / NULLIF(net_size - 1, 0) AS abs_norm_rank_change

  FROM temp

  UNION ALL

  -- Closeness centrality
  SELECT
    dataset,
    replicate_id,
    alpha,
    spotlight_pct,
    p_obs_spotlit,
    p_obs_nonspotlit,
    NodeID,
    Spotlight,
    net_size,
    net_av_deg,
    net_centralisation,

    'Closeness' AS metric,
    obs_closeness_rank AS obs_rank,
    gt_closeness_rank AS gt_rank,

    (gt_closeness_rank - obs_closeness_rank) / NULLIF(net_size - 1, 0) AS norm_rank_lift,
    ABS(gt_closeness_rank - obs_closeness_rank) / NULLIF(net_size - 1, 0) AS abs_norm_rank_change

  FROM temp

  UNION ALL

  -- Eigenvector centrality
  SELECT
    dataset,
    replicate_id,
    alpha,
    spotlight_pct,
    p_obs_spotlit,
    p_obs_nonspotlit,
    NodeID,
    Spotlight,
    net_size,
    net_av_deg,
    net_centralisation,

    'Eigenvector' AS metric,
    obs_eigenvector_rank AS obs_rank,
    gt_eigenvector_rank AS gt_rank,

    (gt_eigenvector_rank - obs_eigenvector_rank) / NULLIF(net_size - 1, 0) AS norm_rank_lift,
    ABS(gt_eigenvector_rank - obs_eigenvector_rank) / NULLIF(net_size - 1, 0) AS abs_norm_rank_change

  FROM temp
),

/*
** BLOCK 3:
** Aggregate node-level rank movement to the graph-condition-metric level.
*/

rank_lift_counts AS (
  SELECT
    dataset,
    replicate_id,
    alpha,
    spotlight_pct,
    p_obs_spotlit,
    p_obs_nonspotlit,
    metric,

    net_size,
    net_av_deg,
    net_centralisation,

    COUNT(*) AS n_nodes,
    SUM(Spotlight) AS n_spotlit,

    AVG(norm_rank_lift) AS mean_norm_rank_lift,
    AVG(abs_norm_rank_change) AS mean_abs_norm_rank_change,

    AVG(
      CASE
        WHEN Spotlight = 1 THEN norm_rank_lift
      END
    ) AS mean_norm_rank_lift_spotlit,

    AVG(
      CASE
        WHEN Spotlight = 0 THEN norm_rank_lift
      END
    ) AS mean_norm_rank_lift_nonspotlit,

    AVG(
      CASE
        WHEN Spotlight = 1 THEN abs_norm_rank_change
      END
    ) AS mean_abs_norm_rank_change_spotlit,

    AVG(
      CASE
        WHEN Spotlight = 0 THEN abs_norm_rank_change
      END
    ) AS mean_abs_norm_rank_change_nonspotlit

  FROM rank_long

  GROUP BY
    dataset,
    replicate_id,
    alpha,
    spotlight_pct,
    p_obs_spotlit,
    p_obs_nonspotlit,
    metric,
    net_size,
    net_av_deg,
    net_centralisation
)

/*
** BLOCK 4:
** Attach realised missingness and calculate spotlight-vs-nonspotlight gaps.
*/

SELECT
  r.dataset,
  r.replicate_id,
  r.alpha,
  r.spotlight_pct,
  r.p_obs_spotlit,
  r.p_obs_nonspotlit,
  r.metric,

  r.net_size,
  r.net_av_deg,
  r.net_centralisation,

  -- Realised proportion of true ties not observed
  net.realised_missingness,

  r.n_nodes,
  r.n_spotlit,

  r.mean_norm_rank_lift,
  r.mean_abs_norm_rank_change,

  r.mean_norm_rank_lift_spotlit,
  r.mean_norm_rank_lift_nonspotlit,

  r.mean_abs_norm_rank_change_spotlit,
  r.mean_abs_norm_rank_change_nonspotlit,

  r.mean_norm_rank_lift_spotlit
    -
  r.mean_norm_rank_lift_nonspotlit
    AS spotlight_rank_lift_gap,

  r.mean_abs_norm_rank_change_spotlit
    -
  r.mean_abs_norm_rank_change_nonspotlit
    AS spotlight_abs_rank_change_gap

FROM rank_lift_counts AS r

INNER JOIN network_results AS net
  ON r.dataset = net.dataset
 AND r.replicate_id = net.replicate_id
 AND r.alpha = net.alpha
 AND r.spotlight_pct = net.spotlight_pct
 AND r.p_obs_spotlit = net.p_obs_spotlit
 AND r.p_obs_nonspotlit = net.p_obs_nonspotlit
 AND net.source = 'observed'

ORDER BY
  r.dataset,
  r.replicate_id,
  r.alpha,
  r.spotlight_pct,
  r.p_obs_spotlit,
  r.p_obs_nonspotlit,
  r.metric;
")

rank_lift_df2 <- attach_condition_metadata(rank_lift_df2)


############ Query for data for network level mean bias plots #########

# These should be presented alongside average relative bias plots, to show the 
# difference between the magnitude of bias and the direction of bias

mn_abs_rel_bias_nets <- DBI::dbGetQuery(con, "
WITH gt_aug AS (
  SELECT
    *
  FROM network_results_gt
  WHERE source = 'true'
),

bias_long AS (

  SELECT
    obs.dataset,
    obs.replicate_id,
    obs.alpha,
    obs.spotlight_pct,
    obs.p_obs_spotlit,
    obs.p_obs_nonspotlit,


    'density' AS metric,
    obs.density AS observed_value,
    gt.density AS gt_value,
    (obs.density - gt.density) / NULLIF(gt.density, 0) AS relative_bias,
    ABS((obs.density - gt.density) / NULLIF(gt.density, 0)) AS abs_relative_bias

  FROM network_results obs
  INNER JOIN gt_aug gt
    ON obs.dataset = gt.dataset
   AND obs.replicate_id = gt.replicate_id

  UNION ALL

  SELECT
    obs.dataset,
    obs.replicate_id,
    obs.alpha,
    obs.spotlight_pct,
    obs.p_obs_spotlit,
    obs.p_obs_nonspotlit,


    'dcent' AS metric,
    obs.dcent AS observed_value,
    gt.dcent AS gt_value,
    (obs.dcent - gt.dcent) / NULLIF(gt.dcent, 0) AS relative_bias,
    ABS((obs.dcent - gt.dcent) / NULLIF(gt.dcent, 0)) AS abs_relative_bias

  FROM network_results obs
  INNER JOIN gt_aug gt
    ON obs.dataset = gt.dataset
   AND obs.replicate_id = gt.replicate_id

  UNION ALL

  SELECT
    obs.dataset,
    obs.replicate_id,
    obs.alpha,
    obs.spotlight_pct,
    obs.p_obs_spotlit,
    obs.p_obs_nonspotlit,

    'clustering' AS metric,
    obs.clustering AS observed_value,
    gt.clustering AS gt_value,
    (obs.clustering - gt.clustering) / NULLIF(gt.clustering, 0) AS relative_bias,
    ABS((obs.clustering - gt.clustering) / NULLIF(gt.clustering, 0)) AS abs_relative_bias

  FROM network_results obs
  INNER JOIN gt_aug gt
    ON obs.dataset = gt.dataset
   AND obs.replicate_id = gt.replicate_id

  UNION ALL

  SELECT
    obs.dataset,
    obs.replicate_id,
    obs.alpha,
    obs.spotlight_pct,
    obs.p_obs_spotlit,
    obs.p_obs_nonspotlit,


    'APL' AS metric,
    obs.APL AS observed_value,
    gt.APL AS gt_value,
    (obs.APL - gt.APL) / NULLIF(gt.APL, 0) AS relative_bias,
    ABS((obs.APL - gt.APL) / NULLIF(gt.APL, 0)) AS abs_relative_bias

  FROM network_results obs
  INNER JOIN gt_aug gt
    ON obs.dataset = gt.dataset
   AND obs.replicate_id = gt.replicate_id
)

SELECT *
FROM bias_long
WHERE abs_relative_bias IS NOT NULL
")

mn_abs_rel_bias_nets <- attach_condition_metadata(
  mn_abs_rel_bias_nets
)



################################# Trialing a model ###################################

network_model_df <- DBI::dbGetQuery(con, "
WITH gt_aug AS (
  SELECT
    dataset,
    replicate_id,
    density AS gt_density,
    dcent AS gt_dcent,
    clustering AS gt_clustering,
    APL AS gt_APL,
    size AS gt_size,
    components AS gt_components

  FROM network_results_gt
  WHERE source = 'true'
)

SELECT
  obs.dataset,
  obs.replicate_id,
  obs.alpha,
  obs.spotlight_pct,
  obs.p_obs_nonspotlit,
  obs.p_obs_spotlit,

  obs.density AS density_obs,
  gt.gt_density,

  obs.dcent AS dcent_obs,
  gt.gt_dcent,

  obs.clustering AS clustering_obs,
  gt.gt_clustering,

  obs.APL AS APL_obs,
  gt.gt_APL,

  obs.size AS obs_size,
  gt.gt_size,

  obs.components AS components_obs,
  gt.gt_components,

  CONCAT(obs.dataset, '_', obs.replicate_id) AS base_graph_id

FROM network_results AS obs

INNER JOIN gt_aug AS gt
  ON obs.dataset = gt.dataset
 AND obs.replicate_id = gt.replicate_id

WHERE obs.source = 'observed'

ORDER BY
  obs.dataset,
  obs.replicate_id,
  obs.alpha,
  obs.spotlight_pct,
  obs.p_obs_spotlit,
  obs.p_obs_nonspotlit;
")

network_model_df <- attach_condition_metadata(
  network_model_df
)

  },
  finally = {
    if (DBI::dbIsValid(con)) {
      DBI::dbDisconnect(con, shutdown = TRUE)
    }
  }
)

