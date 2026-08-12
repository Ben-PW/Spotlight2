################################################################################
# Script: Queries3_database_formatting.R
#
# Creates the persistent ranked-node tables required by the Queries3 analysis.
# Run this after creating or replacing the raw spotlight-results database.
# The stage can be skipped when these formatted tables already exist.
################################################################################

library(DBI)
library(duckdb)

if (!exists("config", inherits = TRUE)) {
  stop("config must be loaded before database formatting.")
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

tryCatch(
  {

required_tables <- c(
  "network_results",
  "network_results_gt",
  "node_results",
  "node_results_gt",
  "simulation_conditions"
)

missing_tables <- setdiff(
  required_tables,
  DBI::dbListTables(con)
)

if (length(missing_tables) > 0L) {
  stop(
    "The results database is missing required tables: ",
    paste(missing_tables, collapse = ", ")
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

################################ Add rank order columns ############################

# The original simulation did not calculate rank order of nodes by centrality scores,
# This will be useful for outcome metrics and is efficient to do using SQL

# Create table for rank order in ground truth networks

# Use persistent NodeID as a deterministic tie breaker for exact positions

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TABLE node_results_GT_ranked AS

  SELECT
    *,

    /*##########################################################################
    # Degree: raw
    ##########################################################################*/

    RANK() OVER (
      PARTITION BY
        dataset,
        replicate_id
      ORDER BY Degree_raw DESC
    ) AS Degree_raw_rank,

    RANK() OVER (
      PARTITION BY
        dataset,
        replicate_id
      ORDER BY Degree_raw DESC
    )
    +
    (
      COUNT(*) OVER (
        PARTITION BY
          dataset,
          replicate_id,
          Degree_raw
      ) - 1
    ) / 2.0 AS Degree_raw_avg_rank,

    ROW_NUMBER() OVER (
      PARTITION BY
        dataset,
        replicate_id
      ORDER BY
        Degree_raw DESC,
        NodeID
    ) AS Degree_raw_top_position,


    /*##########################################################################
    # Degree: normalised
    ##########################################################################*/

    RANK() OVER (
      PARTITION BY
        dataset,
        replicate_id
      ORDER BY Degree_norm DESC
    ) AS Degree_norm_rank,

    RANK() OVER (
      PARTITION BY
        dataset,
        replicate_id
      ORDER BY Degree_norm DESC
    )
    +
    (
      COUNT(*) OVER (
        PARTITION BY
          dataset,
          replicate_id,
          Degree_norm
      ) - 1
    ) / 2.0 AS Degree_norm_avg_rank,

    ROW_NUMBER() OVER (
      PARTITION BY
        dataset,
        replicate_id
      ORDER BY
        Degree_norm DESC,
        NodeID
    ) AS Degree_norm_top_position,


    /*##########################################################################
    # Betweenness: raw
    ##########################################################################*/

    RANK() OVER (
      PARTITION BY
        dataset,
        replicate_id
      ORDER BY Betweenness_raw DESC
    ) AS Betweenness_raw_rank,

    RANK() OVER (
      PARTITION BY
        dataset,
        replicate_id
      ORDER BY Betweenness_raw DESC
    )
    +
    (
      COUNT(*) OVER (
        PARTITION BY
          dataset,
          replicate_id,
          Betweenness_raw
      ) - 1
    ) / 2.0 AS Betweenness_raw_avg_rank,

    ROW_NUMBER() OVER (
      PARTITION BY
        dataset,
        replicate_id
      ORDER BY
        Betweenness_raw DESC,
        NodeID
    ) AS Betweenness_raw_top_position,


    /*##########################################################################
    # Betweenness: normalised
    ##########################################################################*/

    RANK() OVER (
      PARTITION BY
        dataset,
        replicate_id
      ORDER BY Betweenness_norm DESC
    ) AS Betweenness_norm_rank,

    RANK() OVER (
      PARTITION BY
        dataset,
        replicate_id
      ORDER BY Betweenness_norm DESC
    )
    +
    (
      COUNT(*) OVER (
        PARTITION BY
          dataset,
          replicate_id,
          Betweenness_norm
      ) - 1
    ) / 2.0 AS Betweenness_norm_avg_rank,

    ROW_NUMBER() OVER (
      PARTITION BY
        dataset,
        replicate_id
      ORDER BY
        Betweenness_norm DESC,
        NodeID
    ) AS Betweenness_norm_top_position,


    /*##########################################################################
    # Closeness: raw
    #
    # Finite values are ranked normally. Non-finite values, which identify
    # isolates in these networks, form one tied block at the bottom.
    ##########################################################################*/

    RANK() OVER (
      PARTITION BY
        dataset,
        replicate_id
      ORDER BY
        CASE
          WHEN isfinite(Closeness_raw) THEN 0
          ELSE 1
        END,
        CASE
          WHEN isfinite(Closeness_raw) THEN Closeness_raw
        END DESC NULLS LAST
    ) AS Closeness_raw_rank,

    RANK() OVER (
      PARTITION BY
        dataset,
        replicate_id
      ORDER BY
        CASE
          WHEN isfinite(Closeness_raw) THEN 0
          ELSE 1
        END,
        CASE
          WHEN isfinite(Closeness_raw) THEN Closeness_raw
        END DESC NULLS LAST
    )
    +
    (
      COUNT(*) OVER (
        PARTITION BY
          dataset,
          replicate_id,
          CASE
            WHEN isfinite(Closeness_raw) THEN 0
            ELSE 1
          END,
          CASE
            WHEN isfinite(Closeness_raw) THEN Closeness_raw
          END
      ) - 1
    ) / 2.0 AS Closeness_raw_avg_rank,

    ROW_NUMBER() OVER (
      PARTITION BY
        dataset,
        replicate_id
      ORDER BY
        CASE
          WHEN isfinite(Closeness_raw) THEN 0
          ELSE 1
        END,
        CASE
          WHEN isfinite(Closeness_raw) THEN Closeness_raw
        END DESC NULLS LAST,
        NodeID
    ) AS Closeness_raw_top_position,


    /*##########################################################################
    # Closeness: normalised
    #
    # Apply the same bottom-tie treatment to non-finite normalised values.
    ##########################################################################*/

    RANK() OVER (
      PARTITION BY
        dataset,
        replicate_id
      ORDER BY
        CASE
          WHEN isfinite(Closeness_norm) THEN 0
          ELSE 1
        END,
        CASE
          WHEN isfinite(Closeness_norm) THEN Closeness_norm
        END DESC NULLS LAST
    ) AS Closeness_norm_rank,

    RANK() OVER (
      PARTITION BY
        dataset,
        replicate_id
      ORDER BY
        CASE
          WHEN isfinite(Closeness_norm) THEN 0
          ELSE 1
        END,
        CASE
          WHEN isfinite(Closeness_norm) THEN Closeness_norm
        END DESC NULLS LAST
    )
    +
    (
      COUNT(*) OVER (
        PARTITION BY
          dataset,
          replicate_id,
          CASE
            WHEN isfinite(Closeness_norm) THEN 0
            ELSE 1
          END,
          CASE
            WHEN isfinite(Closeness_norm) THEN Closeness_norm
          END
      ) - 1
    ) / 2.0 AS Closeness_norm_avg_rank,

    ROW_NUMBER() OVER (
      PARTITION BY
        dataset,
        replicate_id
      ORDER BY
        CASE
          WHEN isfinite(Closeness_norm) THEN 0
          ELSE 1
        END,
        CASE
          WHEN isfinite(Closeness_norm) THEN Closeness_norm
        END DESC NULLS LAST,
        NodeID
    ) AS Closeness_norm_top_position,


    /*##########################################################################
    # Eigenvector
    ##########################################################################*/

    RANK() OVER (
      PARTITION BY
        dataset,
        replicate_id
      ORDER BY Eigenvector DESC
    ) AS Eigenvector_rank,

    RANK() OVER (
      PARTITION BY
        dataset,
        replicate_id
      ORDER BY Eigenvector DESC
    )
    +
    (
      COUNT(*) OVER (
        PARTITION BY
          dataset,
          replicate_id,
          Eigenvector
      ) - 1
    ) / 2.0 AS Eigenvector_avg_rank,

    ROW_NUMBER() OVER (
      PARTITION BY
        dataset,
        replicate_id
      ORDER BY
        Eigenvector DESC,
        NodeID
    ) AS Eigenvector_top_position

  FROM node_results_GT;
  "
)


##################### Create table for rank order in obsereved  #################

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TABLE node_results_ranked AS

  SELECT
    *,

    /*##########################################################################
    # Degree: raw
    ##########################################################################*/

    RANK() OVER (
      PARTITION BY
        dataset,
        replicate_id,
        alpha,
        spotlight_pct,
        p_obs_spotlit,
        p_obs_nonspotlit
      ORDER BY Degree_raw DESC
    ) AS Degree_raw_rank,

    RANK() OVER (
      PARTITION BY
        dataset,
        replicate_id,
        alpha,
        spotlight_pct,
        p_obs_spotlit,
        p_obs_nonspotlit
      ORDER BY Degree_raw DESC
    )
    +
    (
      COUNT(*) OVER (
        PARTITION BY
          dataset,
          replicate_id,
          alpha,
          spotlight_pct,
          p_obs_spotlit,
          p_obs_nonspotlit,
          Degree_raw
      ) - 1
    ) / 2.0 AS Degree_raw_avg_rank,

    ROW_NUMBER() OVER (
      PARTITION BY
        dataset,
        replicate_id,
        alpha,
        spotlight_pct,
        p_obs_spotlit,
        p_obs_nonspotlit
      ORDER BY
        Degree_raw DESC,
        NodeID
    ) AS Degree_raw_top_position,


    /*##########################################################################
    # Degree: normalised
    ##########################################################################*/

    RANK() OVER (
      PARTITION BY
        dataset,
        replicate_id,
        alpha,
        spotlight_pct,
        p_obs_spotlit,
        p_obs_nonspotlit
      ORDER BY Degree_norm DESC
    ) AS Degree_norm_rank,

    RANK() OVER (
      PARTITION BY
        dataset,
        replicate_id,
        alpha,
        spotlight_pct,
        p_obs_spotlit,
        p_obs_nonspotlit
      ORDER BY Degree_norm DESC
    )
    +
    (
      COUNT(*) OVER (
        PARTITION BY
          dataset,
          replicate_id,
          alpha,
          spotlight_pct,
          p_obs_spotlit,
          p_obs_nonspotlit,
          Degree_norm
      ) - 1
    ) / 2.0 AS Degree_norm_avg_rank,

    ROW_NUMBER() OVER (
      PARTITION BY
        dataset,
        replicate_id,
        alpha,
        spotlight_pct,
        p_obs_spotlit,
        p_obs_nonspotlit
      ORDER BY
        Degree_norm DESC,
        NodeID
    ) AS Degree_norm_top_position,


    /*##########################################################################
    # Betweenness: raw
    ##########################################################################*/

    RANK() OVER (
      PARTITION BY
        dataset,
        replicate_id,
        alpha,
        spotlight_pct,
        p_obs_spotlit,
        p_obs_nonspotlit
      ORDER BY Betweenness_raw DESC
    ) AS Betweenness_raw_rank,

    RANK() OVER (
      PARTITION BY
        dataset,
        replicate_id,
        alpha,
        spotlight_pct,
        p_obs_spotlit,
        p_obs_nonspotlit
      ORDER BY Betweenness_raw DESC
    )
    +
    (
      COUNT(*) OVER (
        PARTITION BY
          dataset,
          replicate_id,
          alpha,
          spotlight_pct,
          p_obs_spotlit,
          p_obs_nonspotlit,
          Betweenness_raw
      ) - 1
    ) / 2.0 AS Betweenness_raw_avg_rank,

    ROW_NUMBER() OVER (
      PARTITION BY
        dataset,
        replicate_id,
        alpha,
        spotlight_pct,
        p_obs_spotlit,
        p_obs_nonspotlit
      ORDER BY
        Betweenness_raw DESC,
        NodeID
    ) AS Betweenness_raw_top_position,


    /*##########################################################################
    # Betweenness: normalised
    ##########################################################################*/

    RANK() OVER (
      PARTITION BY
        dataset,
        replicate_id,
        alpha,
        spotlight_pct,
        p_obs_spotlit,
        p_obs_nonspotlit
      ORDER BY Betweenness_norm DESC
    ) AS Betweenness_norm_rank,

    RANK() OVER (
      PARTITION BY
        dataset,
        replicate_id,
        alpha,
        spotlight_pct,
        p_obs_spotlit,
        p_obs_nonspotlit
      ORDER BY Betweenness_norm DESC
    )
    +
    (
      COUNT(*) OVER (
        PARTITION BY
          dataset,
          replicate_id,
          alpha,
          spotlight_pct,
          p_obs_spotlit,
          p_obs_nonspotlit,
          Betweenness_norm
      ) - 1
    ) / 2.0 AS Betweenness_norm_avg_rank,

    ROW_NUMBER() OVER (
      PARTITION BY
        dataset,
        replicate_id,
        alpha,
        spotlight_pct,
        p_obs_spotlit,
        p_obs_nonspotlit
      ORDER BY
        Betweenness_norm DESC,
        NodeID
    ) AS Betweenness_norm_top_position,


    /*##########################################################################
    # Closeness: raw
    #
    # Finite values are ranked normally. Non-finite values, which identify
    # isolates in these networks, form one tied block at the bottom.
    ##########################################################################*/

    RANK() OVER (
      PARTITION BY
        dataset,
        replicate_id,
        alpha,
        spotlight_pct,
        p_obs_spotlit,
        p_obs_nonspotlit
      ORDER BY
        CASE
          WHEN isfinite(Closeness_raw) THEN 0
          ELSE 1
        END,
        CASE
          WHEN isfinite(Closeness_raw) THEN Closeness_raw
        END DESC NULLS LAST
    ) AS Closeness_raw_rank,

    RANK() OVER (
      PARTITION BY
        dataset,
        replicate_id,
        alpha,
        spotlight_pct,
        p_obs_spotlit,
        p_obs_nonspotlit
      ORDER BY
        CASE
          WHEN isfinite(Closeness_raw) THEN 0
          ELSE 1
        END,
        CASE
          WHEN isfinite(Closeness_raw) THEN Closeness_raw
        END DESC NULLS LAST
    )
    +
    (
      COUNT(*) OVER (
        PARTITION BY
          dataset,
          replicate_id,
          alpha,
          spotlight_pct,
          p_obs_spotlit,
          p_obs_nonspotlit,
          CASE
            WHEN isfinite(Closeness_raw) THEN 0
            ELSE 1
          END,
          CASE
            WHEN isfinite(Closeness_raw) THEN Closeness_raw
          END
      ) - 1
    ) / 2.0 AS Closeness_raw_avg_rank,

    ROW_NUMBER() OVER (
      PARTITION BY
        dataset,
        replicate_id,
        alpha,
        spotlight_pct,
        p_obs_spotlit,
        p_obs_nonspotlit
      ORDER BY
        CASE
          WHEN isfinite(Closeness_raw) THEN 0
          ELSE 1
        END,
        CASE
          WHEN isfinite(Closeness_raw) THEN Closeness_raw
        END DESC NULLS LAST,
        NodeID
    ) AS Closeness_raw_top_position,


    /*##########################################################################
    # Closeness: normalised
    #
    # Apply the same bottom-tie treatment to non-finite normalised values.
    ##########################################################################*/

    RANK() OVER (
      PARTITION BY
        dataset,
        replicate_id,
        alpha,
        spotlight_pct,
        p_obs_spotlit,
        p_obs_nonspotlit
      ORDER BY
        CASE
          WHEN isfinite(Closeness_norm) THEN 0
          ELSE 1
        END,
        CASE
          WHEN isfinite(Closeness_norm) THEN Closeness_norm
        END DESC NULLS LAST
    ) AS Closeness_norm_rank,

    RANK() OVER (
      PARTITION BY
        dataset,
        replicate_id,
        alpha,
        spotlight_pct,
        p_obs_spotlit,
        p_obs_nonspotlit
      ORDER BY
        CASE
          WHEN isfinite(Closeness_norm) THEN 0
          ELSE 1
        END,
        CASE
          WHEN isfinite(Closeness_norm) THEN Closeness_norm
        END DESC NULLS LAST
    )
    +
    (
      COUNT(*) OVER (
        PARTITION BY
          dataset,
          replicate_id,
          alpha,
          spotlight_pct,
          p_obs_spotlit,
          p_obs_nonspotlit,
          CASE
            WHEN isfinite(Closeness_norm) THEN 0
            ELSE 1
          END,
          CASE
            WHEN isfinite(Closeness_norm) THEN Closeness_norm
          END
      ) - 1
    ) / 2.0 AS Closeness_norm_avg_rank,

    ROW_NUMBER() OVER (
      PARTITION BY
        dataset,
        replicate_id,
        alpha,
        spotlight_pct,
        p_obs_spotlit,
        p_obs_nonspotlit
      ORDER BY
        CASE
          WHEN isfinite(Closeness_norm) THEN 0
          ELSE 1
        END,
        CASE
          WHEN isfinite(Closeness_norm) THEN Closeness_norm
        END DESC NULLS LAST,
        NodeID
    ) AS Closeness_norm_top_position,


    /*##########################################################################
    # Eigenvector
    ##########################################################################*/

    RANK() OVER (
      PARTITION BY
        dataset,
        replicate_id,
        alpha,
        spotlight_pct,
        p_obs_spotlit,
        p_obs_nonspotlit
      ORDER BY Eigenvector DESC
    ) AS Eigenvector_rank,

    RANK() OVER (
      PARTITION BY
        dataset,
        replicate_id,
        alpha,
        spotlight_pct,
        p_obs_spotlit,
        p_obs_nonspotlit
      ORDER BY Eigenvector DESC
    )
    +
    (
      COUNT(*) OVER (
        PARTITION BY
          dataset,
          replicate_id,
          alpha,
          spotlight_pct,
          p_obs_spotlit,
          p_obs_nonspotlit,
          Eigenvector
      ) - 1
    ) / 2.0 AS Eigenvector_avg_rank,

    ROW_NUMBER() OVER (
      PARTITION BY
        dataset,
        replicate_id,
        alpha,
        spotlight_pct,
        p_obs_spotlit,
        p_obs_nonspotlit
      ORDER BY
        Eigenvector DESC,
        NodeID
    ) AS Eigenvector_top_position

  FROM node_results;
  "
)

formatted_table_pairs <- c(
  node_results_GT_ranked = "node_results_gt",
  node_results_ranked = "node_results"
)

formatted_row_counts <- vapply(
  names(formatted_table_pairs),
  function(table_name) {
    DBI::dbGetQuery(
      con,
      paste0("SELECT COUNT(*) AS n FROM ", table_name)
    )[[1L]]
  },
  numeric(1)
)

source_row_counts <- vapply(
  unname(formatted_table_pairs),
  function(table_name) {
    DBI::dbGetQuery(
      con,
      paste0("SELECT COUNT(*) AS n FROM ", table_name)
    )[[1L]]
  },
  numeric(1)
)

if (!identical(
  unname(formatted_row_counts),
  unname(source_row_counts)
)) {
  stop("Formatted ranked tables do not match their source row counts.")
}

  },
  finally = {
    if (DBI::dbIsValid(con)) {
      DBI::dbDisconnect(con, shutdown = TRUE)
    }
  }
)

