# Spotlight simulation

This repository contains the code, simulated network data, and analysis
pipeline used for:

> [Simulating the Impacts of Spotlight Effects on Covert Networks]  
> [Benjamin Palfreeman-Watt, David Buil-Gil, Tomas Diviak, Nicholas Trajtenberg-Pareja]  

The primary purpose of the repository is to support reproduction of the
simulation results and figures reported in the paper. However, the configuration file
also allows the manual specification of a variety of simulation parameters for users who wish to explore
alternative conditions.

## Overview

The simulation investigates how a spotlighted network-observation process
affects estimates of network structure and node importance.

For each ground-truth network:

1. A specified proportion of nodes is assigned to the spotlight.
2. Spotlight selection can be biased towards nodes with higher degree.
3. Edges incident on spotlighted nodes and other edges are observed with
   separately specified probabilities.
4. Network-level statistics and node-level centralities are calculated from
   the resulting observed network.
5. These results are compared with the corresponding ground-truth network.

The general workflow is:

```text
Ground-truth network generation
            |
            v
Spotlight assignment and edge observation
            |
            v
DuckDB results database
            |
            v
Database formatting and analysis queries
            |
            v
Figures
```
## Repository Structure
Note that the results database 'spotlight_probability_results.db' is not present in this repository, as the file is approximately 16GB (if the simulation is run with default parameters on the provided data). However, this file can be fully reconstructed with the provided code.
```text
Spotlight2/
├── Spotlight_main.R
├── Config/
│   └── Config.R
├── Data/
│   ├── datasets_final
│   └── datasets_final_conditions.csv
├── Results/
│   └── spotlight_probability_results.duckdb
├── Scripts/
│   ├── Data_simulation.R
│   ├── Data_simulation_helpers.R
│   ├── Degree_generator.R
│   ├── ERGM_simulator.R
│   ├── Spotlight_simulation.R
│   ├── Spotlight_helpers.R
│   ├── Error_simulation_helpers.R
│   ├── Database_formatting.R
│   ├── Database_queries.R
│   ├── Visualisations.R
│   ├── Visualisation_helpers.R
│   └── Vis_scripts/
│       ├── Corr_nodes.R
│       ├── Coverage_heatmap.R
│       ├── Rank_lift_contour.R
│       ├── Rank_lift_line.R
│       ├── Rel_bias_contour.R
│       └── Top10_recall_contour.R
└── Figures/
```
The key files are:

```Spotlight_main.R```: Script co-ordinating the main workflow.  
```Config/config.R```: Script containing user definable variables.  
```Data/Datasets_final```: Supplied datasets used for the spotlight study.  
```Data/Datasets_final_conditions.csv```: Simulation condition metadata required for database queries. Generated automatically by data simulation stage.  
```Scripts/Database_queries.R```: Queries the database to pull required data into RAM.  
```Scripts/Visualisations.R```: Coordinates the visualisation scripts and saves output to ```Figures/```.  

## Software requirements
This analysis was performed using:  
R[4.5.2]  
RStudio[2025.9.2.418]  
Microsoft Windows 11 Enterprise [10.0.26100 Build 26100]  

This pipeline requires the following packages:  
```R
simulation_packages <- c(
  "akima",
  "DBI",
  "dbplyr",
  "dplyr",
  "duckdb",
  "ergm",
  "ggplot2",
  "here",
  "igraph",
  "intergraph",
  "network",
  "purrr",
  "scales",
  "sna",
  "stringr",
  "tibble",
  "tidyr"
)

install.packages(simulation_packages)
```
To restore the exact package environment used for this simulation, use the following:
```R
install.packages("renv")
renv::restore()
```

## Getting started
1. Download or clone the repository
2. Open ```Spotlight2.Rproj``` in RStudio
3. Review the settings in ```Config/config.R``` (ignore if running default parameters)
4. Start a fresh R session
5. Run
```R
source(Spotlight_main.R)
```
It's important to run ```Spotlight_main.R``` from the project root so the paths created by the ```here``` package resolve properly (i.e. don't move it!)

## Replicating results
This is the simplest route to replicate the results of the study. Default parameters are set to perform this workflow unless manually configured otherwise.
In ```Config/config.R``` check that the workflow parameters read as follows
```R
workflow = list(
  full_rerun = FALSE,
  run_spotlight_simulation = TRUE,
  run_database_formatting = TRUE,
  run_queries = TRUE,
  run_visualisations = TRUE
)
```
Then run
```R
source("Spotlight_main.R")
```
Please allow 4-6 hours and up to 2GB memory useage.  

## Supplementary plots
The visualisation script generates a single plot of each kind for the specified ```main_spotlight_pct``` in ```Config/config.R```.  
To generate visualisations for every spotlight percentage used in the simulation, check
```R
supplementary_plots = TRUE
```
Separate folders in ```Figures/``` will be created for each spotlight percentage. For example:
```
Figures/Figures_supplemental/spotlight_1pct/
Figures/Figures_supplemental/spotlight_5pct/
```
## Re running visualisations
Visualisations can be easily re-run without having to repeat database formatting or queries, as long as the queried dataframes are present in the same R session.  
To do this, simply alter the desired visualisation settings in ```Config/config.R```. Then ensure the following worflow options are specified:
```R
workflow = list(
  full_rerun = FALSE,
  run_spotlight_simulation = FALSE,
  run_database_formatting = FALSE,
  run_queries = FALSE,
  run_visualisations = TRUE
)
```
Then call
```R
source("Spotlight_main.R")
```
NOTE: This will overwrite the previously stored visualisations, so make sure to store them elsewhere if they are still required.

## Database safety
By default, ```config.R``` stores
```R
overwrite_database = FALSE
```
Meaning the simulation will not run if it detects an existing results database. This can be set to ```TRUE``` if the previous database is no longer required and you want to overwrite it with a new one.  
Alternatively, if you want to retain the old database but create and analyse a new one, keep
```R
overwrite_database = FALSE
```
And instead change the ```database``` path in ```config.R```, for example:
```R
# DuckDB results database created by Spotlight_main.R.
    database = here::here(
      "Results",
      "spotlight_probability_results.duckdb"
    ),
```
Becomes
```R
# DuckDB results database created by Spotlight_main.R.
    database = here::here(
      "Results",
      "NEW_DB.duckdb"
    ),
```
Then check these settings in ```config.R```
```R
workflow = list(
  full_rerun = FALSE,
  run_spotlight_simulation = TRUE,
  run_database_formatting = TRUE,
  run_queries = TRUE,
  run_visualisations = TRUE
)
```
save the ```config.R``` file and run
```R
source("Spotlight_main.R")
```
This will re-run the spotlight simulation (on the provided data), store the results in a new database called ```NEW_DB.duckdb```, which all the downstream scripts will then operate on, leaving the old database untouched.  


NOTE: This will overwrite the existing queried dataframes and visualisations from the previous simulation, if it is run in the same session. To store the generated figures in a new folder, simply change the following in ```config.R```:
```R
    # Directory used by the visualisation scripts.
    figures = here::here("Figures")
  ),
```
To something like
```R
    # Directory used by the visualisation scripts.
    figures = here::here("Figures", "NEW_FIGURES")
  ),
```
Save ```config.R``` and source the main script as before. This will create subfolder within ```Figures/``` within which the new visualisations are stored

## Regenerating ground truth networks
To regenerate ground truth networks and all downstream results, set the following arguments in ```config.R```:
```R
workflow = list(
  full_rerun = TRUE,
  run_spotlight_simulation = TRUE,
  run_database_formatting = TRUE,
  run_queries = TRUE,
  run_visualisations = TRUE
)
```
This will run the degree sequence generator and the ERGM simulation steps, then perform the rest of the pipeline on the generated datasets.
This stage is quite computationally intensive (and not particularly well optimised!) and will take several hours (~6).    
The pipeline will automatically create a new ```simulation_conditions``` table in the specified database from the specified simulation parameters and metadata, meaning you don't need to change the file ```Data/datasets_final_conditions.csv```

