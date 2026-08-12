# Spotlight simulation

This repository contains the code, simulated network data, and analysis
pipeline used for:

> [Simulating the Impacts of Spotlight Effects on Covert Networks]  
> [Benjamin Palfreeman-Watt, David Buil-Gil, Tomas Diviak, Nicholas Trajtenberg-Pareja]  

The primary purpose of the repository is to support reproduction of the
simulation results and figures reported in the paper. The configuration file
also exposes the main simulation parameters for users who wish to explore
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
│   ├── Vis_scripts/
└── Figures/
```
