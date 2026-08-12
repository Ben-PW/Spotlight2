################################################################################
# Script: Visualisations.R
#
# Build the requested visualisations from query data frames, retain every plot
# in `visualisation_plots`, and optionally save the plots as PDFs.
################################################################################

if (!exists("config", inherits = TRUE)) {
  stop("`config` must be loaded before running Visualisations.R.")
}

source(
  here::here(
    "Scripts",
    "Visualisation_helpers.R"
  )
)

validate_visualisation_packages()
validate_visualisation_config(config$visualisations)

visualisation_module_files <- c(
  "Coverage_heatmap.R",
  "Rel_bias_contour.R",
  "Corr_nodes.R",
  "Top10_recall_contour.R",
  "Rank_lift_line.R",
  "Rank_lift_contour.R"
)

invisible(
  lapply(
    visualisation_module_files,
    function(module_file) {
      source(
        here::here(
          "Scripts",
          "Vis_scripts",
          module_file
        )
      )
    }
  )
)

visualisation_data_objects <- c(
  coverage = "coverage_heatmap_df",
  network_bias = "mn_abs_rel_bias_nets",
  node_correlation = "node_corr_df",
  top_n_recall = "node_rank_df",
  rank_lift_line = "rank_lift_df2",
  rank_lift_contour = "rank_lift_df2"
)

required_data_objects <- unique(
  visualisation_data_objects[
    config$visualisations$plots_to_run
  ]
)

missing_data_objects <- required_data_objects[
  !vapply(
    required_data_objects,
    exists,
    inherits = TRUE,
    FUN.VALUE = logical(1)
  )
]

if (length(missing_data_objects) > 0L) {
  stop(
    "The requested visualisations require missing query objects: ",
    paste(missing_data_objects, collapse = ", "),
    ". Run the database-query stage first."
  )
}

visualisation_config <- config$visualisations
visualisation_plots <- list()

append_visualisation_plots <- function(
    existing_plots,
    new_plots
) {
  duplicate_names <- intersect(
    names(existing_plots),
    names(new_plots)
  )

  if (length(duplicate_names) > 0L) {
    stop(
      "Duplicate visualisation names were generated: ",
      paste(duplicate_names, collapse = ", ")
    )
  }

  c(existing_plots, new_plots)
}

# Run coverage heatmap if selected
if ("coverage" %in% visualisation_config$plots_to_run) {
  coverage_plots <- build_coverage_heatmaps(
    df = get("coverage_heatmap_df", inherits = TRUE),
    main_spotlight_pct = visualisation_config$main_spotlight_pct,
    alphas_to_plot = visualisation_config$alphas_to_plot,
    show_values = visualisation_config$show_coverage_values,
    include_supplementary = TRUE
  )

  visualisation_plots <- append_visualisation_plots(
    visualisation_plots,
    coverage_plots
  )
}

# Run relative / absolute bias contour plots if selected
if ("network_bias" %in% visualisation_config$plots_to_run) {
  network_bias_plots <- build_network_bias_plots(
    df = get("mn_abs_rel_bias_nets", inherits = TRUE),
    metrics = visualisation_config$network_bias_metrics,
    bias_types = visualisation_config$network_bias_types,
    spotlight_pct_choice = visualisation_config$main_spotlight_pct,
    alphas_to_plot = visualisation_config$alphas_to_plot,
    grid_n = visualisation_config$interpolation_grid_n
  )

  visualisation_plots <- append_visualisation_plots(
    visualisation_plots,
    network_bias_plots
  )
}

# Run node correlation heatmaps if selected
if ("node_correlation" %in% visualisation_config$plots_to_run) {
  node_correlation_plots <- build_node_correlation_plots(
    df = get("node_corr_df", inherits = TRUE),
    correlation_types = visualisation_config$correlation_types,
    centrality_metrics = visualisation_config$centrality_metrics,
    spotlight_pct_choice = visualisation_config$main_spotlight_pct,
    alphas_to_plot = visualisation_config$alphas_to_plot
  )

  visualisation_plots <- append_visualisation_plots(
    visualisation_plots,
    node_correlation_plots
  )
}

# Run TopN recall contour plots if selected
if ("top_n_recall" %in% visualisation_config$plots_to_run) {
  top_n_proportion <- config$analysis$top_n_proportion

  if (
    is.null(top_n_proportion) ||
    !is.numeric(top_n_proportion) ||
    length(top_n_proportion) != 1L ||
    !is.finite(top_n_proportion) ||
    top_n_proportion <= 0 ||
    top_n_proportion > 1
  ) {
    stop(
      "`config$analysis$top_n_proportion` must be one finite numeric ",
      "value greater than 0 and no greater than 1."
    )
  }

  top_n_recall_plot <- build_top_n_recall_plot(
    df = get("node_rank_df", inherits = TRUE),
    spotlight_pct_choice = visualisation_config$main_spotlight_pct,
    metrics_to_keep = visualisation_config$centrality_metrics,
    alphas_to_plot = visualisation_config$alphas_to_plot,
    top_n_proportion = top_n_proportion,
    grid_n = visualisation_config$interpolation_grid_n,
    show_alignment = visualisation_config$show_top_n_alignment
  )

  visualisation_plots <- append_visualisation_plots(
    visualisation_plots,
    list(top_n_recall = top_n_recall_plot)
  )
}

# Run rank lift line plots if selected
if ("rank_lift_line" %in% visualisation_config$plots_to_run) {
  rank_lift_line_plot <- build_rank_lift_line_plot(
    df = get("rank_lift_df2", inherits = TRUE),
    metrics_to_keep = visualisation_config$centrality_metrics,
    spotlight_pct_choice = visualisation_config$main_spotlight_pct,
    alphas_to_plot = visualisation_config$alphas_to_plot
  )

  visualisation_plots <- append_visualisation_plots(
    visualisation_plots,
    list(rank_lift_line = rank_lift_line_plot)
  )
}

# Run rank lift contour plots if selected
if ("rank_lift_contour" %in% visualisation_config$plots_to_run) {
  rank_lift_contour_plot <- build_rank_lift_contour_plot(
    df = get("rank_lift_df2", inherits = TRUE),
    metrics_to_keep = visualisation_config$centrality_metrics,
    alphas_to_plot = visualisation_config$alphas_to_plot,
    spotlight_pct_choice = visualisation_config$main_spotlight_pct,
    grid_n = visualisation_config$interpolation_grid_n
  )

  visualisation_plots <- append_visualisation_plots(
    visualisation_plots,
    list(rank_lift_contour = rank_lift_contour_plot)
  )
}

# Plot saving configuration
visualisation_plot_files <- character()

if (visualisation_config$save_plots) {
  figures_directory <- config$paths$figures
  supplementary_directory <- file.path(
    figures_directory,
    "Figures_supplemental"
  )

  main_coverage_name <- paste0(
    "coverage_",
    make_spotlight_pct_slug(
      visualisation_config$main_spotlight_pct
    )
  )

  get_plot_dimensions <- function(plot_name) {
    if (startsWith(plot_name, "coverage_")) {
      return(c(width = 180, height = 135))
    }

    if (startsWith(plot_name, "node_correlation_")) {
      return(c(width = 180, height = 150))
    }

    c(width = 240, height = 180)
  }

  for (plot_name in names(visualisation_plots)) {
    is_supplementary_coverage <-
      startsWith(plot_name, "coverage_") &&
      plot_name != main_coverage_name

    if (
      is_supplementary_coverage &&
      !visualisation_config$save_supplementary_coverage
    ) {
      next
    }

    output_directory <- if (is_supplementary_coverage) {
      supplementary_directory
    } else {
      figures_directory
    }

    dimensions <- get_plot_dimensions(plot_name)
    output_filename <- file.path(
      output_directory,
      paste0(plot_name, ".pdf")
    )

    visualisation_plot_files[[plot_name]] <-
      save_visualisation_pdf(
        plot = visualisation_plots[[plot_name]],
        filename = output_filename,
        width_mm = dimensions[["width"]],
        height_mm = dimensions[["height"]]
      )
  }
}

message(
  "Created ",
  length(visualisation_plots),
  " visualisation plot object",
  if (length(visualisation_plots) == 1L) "." else "s."
)
