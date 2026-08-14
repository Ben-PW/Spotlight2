# Spotlight simulation

This repository contains the code, simulated network data, and analysis
pipeline used for:

> [Link to paper to go here]  
> [Benjamin Palfreeman-Watt, David Buil-Gil, Tomas Diviak, Nicholas Trajtenberg-Pareja]

(Just went with alphabetical order for now)  

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


[!WARNING]
> The workflow settings in `Config/Config.R` determine which ground-truth
> networks, simulation-condition metadata, and results database are used.
> These inputs must describe the same simulation run.
>
> In particular:
>
> - When `full_rerun = TRUE`, `simulation_conditions` is generated directly
>   from the newly simulated ground-truth networks.
> - When `full_rerun = FALSE` and the spotlight simulation stage is run,
>   `simulation_conditions` is loaded from
>   `Data/datasets_final_conditions.csv`,
>   As the pipeline assumes the provided dataset is being used.
> - When only database formatting, queries, or visualisations are requested,
>   the pipeline expects the `simulation_conditions` table to already exist
>   inside the DuckDB database specified by `config$paths$database`.
>
> Changing workflow settings or file paths independently of each other
> can therefore combine results and metadata from different simulation runs.
> This may produce incorrect analyses or cause the pipeline’s validation checks
> to fail. When experimenting, use a new database filename and ensure that the
> selected datasets, condition metadata, and database all belong to the same
> run.

## Configuration
All commonly changed parameters are located in `Config/config.R`

### Ground truth network parameters
These include
- Network size
- Target average degree
- Target Freeman centralisation
- Density and centralisation tolerances
- Number of candidate degree sequences
- ERGM coefficients
- Network connectedness requirements
- Target number of retained networks per size \* average degree \* centralisation condition


### Spotlight observation parameters
These include
- `spotlight_pcts` the proportion of nodes to be spotlit
- `alphas` the various weights assigned to degree when sampling nodes for spotlight
- `p_obs_spotlit_values` the observation probabilities of spotlit ties
- `p_obs_nonspotlit_values` the observation probabilities of non-spotlit ties


Users are welcome to experiment with these parameters, however there are a few things to bear in mind:
- It is a good idea to specify a new database path if you have existing results of interest. e.g.
```R
config$paths$database <- here::here(
  "Results",
  "experimental_results.duckdb"
)
```
- Multiple databases can start to use a significant amount of disk space. If performing many exploratory simulations, you can keep the paths the same and just set
```R
overwrite_database <- TRUE
```
- Increasing the number of different parameter values causes the run time and database sizes to grow rapidly. The database formatting stage also significantly increases the size of the database
- Database formatting and querying can be RAM intensive (compared to the rest of the pipeline). The packages used typically write overflow to a temporary folder, however very large databases could cause issues on machines with RAM under 6GB

### Analysis and visualisation parameters
These include
- The definition of Top N
- Plot types to generate
- Centrality measures
- Correlation types
- Network statistics
- Alpha values displayed in visualisations
- Main spotlight proportion to visualise
- Supplementary figures


## Outputs
The results database contains tables for:
- ground-truth network statistics
- ground-truth node centralities
- observed network statistics
- observed node centralities
- simulation-condition metadata
- ranked node results used by the analysis  


The analysis includes:
- edge coverage and realised missingness
- relative and absolute network-statistic bias
- node-centrality correlation
- Top-N node recall
- spotlighted-node rank lift

#### Analysis dataframes
When `config$workflow$run_queries = TRUE`, the pipeline will output several dataframes into the current R session.  
These contain the data used for the visualisations, but they can also be saved and used for further analysis. 

| Object | Description |
| ------- | ----------- |
| `coverage_heatmap_df` | Aggregated edge-missingness results across spotlight assignment and observation conditions. Includes mean, median, and interquartile-range estimates of realised missingness, and network target centralisation |
| `network_bias_df` | Network-level observed and ground-truth statistics joined by dataset and replicate_id (unique identifier of network within `dataset` group). Includes relative bias measures for density, degree centralisation, clustering. |
| `mn_abs_rel_bias_nets` | Long-format network-bias data used by the contour plots. Contains observed and ground-truth values, signed relative bias, and absolute relative bias for each network metric. |
| `node_corr_df` | Pearson and rank correlations between observed and ground-truth node-centrality values for each network and observation condition. |
| `node_rank_df` | Top-N ranking results, including overlap, recall, Jaccard similarity, spotlight alignment, and related ranking measures. NB: Due to the rank breaking employed in this simulation, Precision = Recall = Intersection/N | 
| `rank_lift_df2` | Changes in node rank under the observation process, separated by spotlight status and centrality measure. This is used for the rank-lift line and contour plots. Also includes realised tie missingness for each condition |
| `simulation_conditions_df` | Metadata describing each ground-truth dataset condition, including target network size, average degree, and centralisation. |  


The condition metadata is attached to the analysis data frames, making it possible to group or model results according to the simulation parameters of the ground-truth networks. For example:
```R
library(dplyr)

network_bias_df |>
  group_by(
    target_size,
    target_average_degree,
    target_centralisation,
    spotlight_pct,
    alpha
  ) |>
  summarise(
    mean_centralisation_bias = mean(
      dcent_ARB, # degree centralisation average relative bias
      na.rm = TRUE
    ),
    .groups = "drop"
  )
```

## Randomness and reproducibility 
Random seeds are specified separately for:  
- degree-sequence generation
- ERGM network simulation
- selection of retained ground-truth networks
- spotlight assignment and probabilistic edge observation


The supplied ground-truth networks and results database should be treated as the 'official' inputs for reproducing the reported figures.  
Exact regeneration from seeds may also depend on the R version and package versions, particularly for the network-generation stages. For exact replication, it is advised to use 
```R
install.packages("renv")
renv::restore()
```
before running the simulation

## Troubleshooting
Some common pitfalls could include the following


#### The database already exists
The simulation will not run if a database is detected on `config$paths$database` and `overwrite_database = FALSE`.  
You can either change the `database` path or enable overwriting and run the simulation


#### Required query objects are missing
If visualisations are requested without rerunning queries, the required query data frames must already exist in the current R session.  
If the database already exists and formatting stages have run, just set `run_queries <- TRUE` and re-run main.


#### Required database tables are missing
Unformatted databases will not contain the required `simulation_conditions` table or the node ranking tables which are expected by the queries. Check:
```R
run_database_formatting = TRUE
```
in `config.R`


#### Simulation condition failure


The pipeline logs errors in `Results/error_log.txt`. This file is only created if the pipeline has encountered errors during the simulation, so it is useful to check during and after the simulation runs.  


Error messages take the form:  
`ERROR | dataset = a | alpha = b | spotlight_pct = c | p_obs_spotlit = d | p_obs_nonspotlit = e | message = f`

## Citation
If you use the datasets or code provided in this repository, please cite
[link to paper to go here hopefully!]  
Repository  
[Repository archive to go here]  
License  
[License type to ogo here]

