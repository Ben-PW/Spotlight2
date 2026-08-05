library(DBI)
library(duckdb)
library(here)
library(dplyr)
library(dbplyr)

con <- DBI::dbConnect(
  duckdb::duckdb(),
  dbdir = here::here("Results", "spotlight_probability_results.duckdb")
)

on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

dbListTables(con)

############# Query top of databases to give a visual on structure ##############

node_results_gt <- DBI::dbGetQuery(con, "
                                SELECT *
                                FROM node_results_gt
                                ORDER BY dataset, replicate_id, alpha, spotlight_pct, NodeID
                                LIMIT 50;
                                ")

node_results <- DBI::dbGetQuery(con, "
                                SELECT *
                                FROM node_results
                                ORDER BY dataset, replicate_id, alpha, spotlight_pct, NodeID
                                LIMIT 50;
                                ")

#node_results_ranked <- DBI::dbGetQuery(con, "
#                                       SELECT *
#                                       FROM node_results_ranked
#                                       ORDER BY dataset, replicate_id, alpha, spotlight_pct, NodeID
#                                       LIMIT 50;
#                                       ")

network_results_gt <- DBI::dbGetQuery(con, "
                                      SELECT *
                                      FROM network_results_gt
                                      LIMIT 50;
                                      ")

network_results <- DBI::dbGetQuery(con, "
                                   SELECT *
                                   FROM network_results
                                   LIMIT 50;
                                   ")

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


##################################### SAMPLING COVERAGE #####################################

# This is the query for heatmps to show sampling coverage over different conditions of
# spotlight effect

coverage_heatmap_df <- DBI::dbGetQuery(
  con,
  "
  WITH gt_aug AS (
    SELECT
      dataset,
      replicate_id,
      dcent AS gt_dcent,
      size AS gt_size,
      density AS gt_density,

      CASE
        WHEN dcent BETWEEN 0.05 AND 0.15 THEN 'Low'
        WHEN dcent BETWEEN 0.25 AND 0.35 THEN 'Medium'
        WHEN dcent BETWEEN 0.45 AND 0.55 THEN 'High'
        ELSE 'Outside target band'
      END AS centralisation_band

    FROM network_results_gt

    WHERE source = 'true'
  )

  SELECT
    obs.alpha,
    obs.spotlight_pct,
    obs.p_obs_spotlit,
    obs.p_obs_nonspotlit,

    gt.centralisation_band,

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
    AND gt.centralisation_band != 'Outside target band'
    AND obs.realised_missingness IS NOT NULL

  GROUP BY
    obs.alpha,
    obs.spotlight_pct,
    obs.p_obs_spotlit,
    obs.p_obs_nonspotlit,
    gt.centralisation_band

  ORDER BY
    obs.spotlight_pct,
    gt.centralisation_band,
    obs.alpha,
    obs.p_obs_spotlit,
    obs.p_obs_nonspotlit
  "
)

#################################### NETWORK BIAS ###################################

######################## Query to get network level differences ######################

# Because the tables for network level results are comparatively small, they can
# just be queried straight from the database. For larger runs, this would not be 
# feasible

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
** Create a long table with binary indicators for top 10% membership
** this can be re-used for all outcome measures, but will have to be 
** recalculated for any other values of N. Not complicated just time consuming
*/
    
    top10_long AS(
    
    -- Top 10 for degree centrality
    
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
      CEIL(0.10 * net_size) AS top_n_cutoff,
        
      CASE
        WHEN obs_degree_rank <= CEIL(0.10 * net_size) THEN 1
        ELSE 0
      END AS obs_top10,
        
      CASE 
        WHEN gt_degree_rank <= CEIL(0.10 * net_size) THEN 1
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
      CEIL(0.10 * net_size) AS top_n_cutoff,
      
      CASE
        WHEN obs_betweenness_rank <= CEIL(0.10 * net_size) THEN 1
        ELSE 0
      END AS obs_top10,
      
      CASE
        WHEN gt_betweenness_rank <= CEIL(0.10 * net_size) THEN 1
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
      CEIL(0.10 * net_size) AS top_n_cutoff,
      
      CASE
        WHEN obs_closeness_rank <= CEIL(0.10 * net_size) THEN 1
        ELSE 0
      END AS obs_top10,
      
      CASE
        WHEN gt_closeness_rank <= CEIL(0.10 * net_size) THEN 1
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
      CEIL(0.10 * net_size) AS top_n_cutoff,
      
      CASE
        WHEN obs_eigenvector_rank <= CEIL(0.10 * net_size) THEN 1
        ELSE 0
      END AS obs_top10,
      
      CASE
        WHEN gt_eigenvector_rank <= CEIL(0.10 * net_size) THEN 1
        ELSE 0
      END AS gt_top10
      
    FROM temp
    
    ),
    
/*
** BLOCK 3:
** This block takes the top 10 labels stored in the temporary table top10 and counts 
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
** in block 3 and converts them into the outcome metrics for this section
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


############ Query for data for network level mean bias plots #########

# These should be presented alongside average relative bias plots, to show the 
# difference between the magnitude of bias and the direction of bias

mn_abs_rel_bias_nets <- DBI::dbGetQuery(con, "
WITH gt_aug AS (
  SELECT
    *,
    CASE 
      WHEN dcent BETWEEN 0.05 AND 0.15 THEN 'Low'
      WHEN dcent BETWEEN 0.25 AND 0.35 THEN 'Med'
      WHEN dcent BETWEEN 0.45 AND 0.55 THEN 'High'
      ELSE 'WARNING'
    END AS 'baseline_centralisation'
  FROM network_results_gt
),

bias_long AS (

  SELECT
    obs.dataset,
    obs.replicate_id,
    obs.alpha,
    obs.spotlight_pct,
    obs.p_obs_spotlit,
    obs.p_obs_nonspotlit,
    gt.baseline_centralisation,


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
    gt.baseline_centralisation,


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
    gt.baseline_centralisation,

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
    gt.baseline_centralisation,


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
    components AS gt_components,

    CASE 
      WHEN dcent BETWEEN 0.05 AND 0.15 THEN 'Low'
      WHEN dcent BETWEEN 0.25 AND 0.35 THEN 'Medium'
      WHEN dcent BETWEEN 0.45 AND 0.55 THEN 'High'
      ELSE 'Outside target band'
    END AS baseline_centralisation

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

  gt.baseline_centralisation,

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
