################################################################################
# Script: Visualisation_helpers.R
#
# Shared validation, selection, labelling and PDF-output helpers used by the
# visualisation modules. This script defines functions only.
################################################################################

# Check the currently required packagaes are available
validate_visualisation_packages <- function() {
  required_packages <- c(
    "akima",
    "dplyr",
    "ggplot2",
    "purrr",
    "scales",
    "tidyr"
  )

  missing_packages <- required_packages[
    !vapply(
      required_packages,
      requireNamespace,
      quietly = TRUE,
      FUN.VALUE = logical(1)
    )
  ]

  if (length(missing_packages) > 0L) {
    stop(
      "The visualisation pipeline requires missing packages: ",
      paste(missing_packages, collapse = ", ")
    )
  }
}

# Validation check for config parameters
validate_character_choices <- function(
    values,
    allowed,
    setting_name
) {
  if (
    !is.character(values) ||
    length(values) == 0L ||
    anyNA(values) ||
    any(values == "") ||
    anyDuplicated(values)
  ) {
    stop(
      "`",
      setting_name,
      "` must be a non-empty character vector without missing or duplicate ",
      "values."
    )
  }

  unsupported <- setdiff(values, allowed)

  if (length(unsupported) > 0L) {
    stop(
      "`",
      setting_name,
      "` contains unsupported values: ",
      paste(unsupported, collapse = ", "),
      ". Available values are: ",
      paste(allowed, collapse = ", "),
      "."
    )
  }

  invisible(values)
}

# Validation check that required config parameters are specified
validate_visualisation_config <- function(vis_config) {
  if (!is.list(vis_config)) {
    stop("`config$visualisations` must be a list.")
  }
  
  # Must be specified for code to function
  required_settings <- c(
    "plots_to_run",
    "main_spotlight_pct",
    "alphas_to_plot",
    "centrality_metrics",
    "correlation_types",
    "network_bias_metrics",
    "network_bias_types",
    "interpolation_grid_n",
    "show_coverage_values",
    "show_top_n_alignment",
    "save_plots",
    "save_supplementary_coverage"
  )

  missing_settings <- setdiff(
    required_settings,
    names(vis_config)
  )

  if (length(missing_settings) > 0L) {
    stop(
      "`config$visualisations` is missing settings: ",
      paste(missing_settings, collapse = ", ")
    )
  }

  validate_character_choices(
    values = vis_config$plots_to_run,
    allowed = c(
      "coverage",
      "network_bias",
      "node_correlation",
      "top_n_recall",
      "rank_lift_line",
      "rank_lift_contour"
    ),
    setting_name = "config$visualisations$plots_to_run"
  )

  validate_character_choices(
    values = vis_config$centrality_metrics,
    allowed = c(
      "Degree",
      "Betweenness",
      "Closeness",
      "Eigenvector"
    ),
    setting_name = "config$visualisations$centrality_metrics"
  )

  validate_character_choices(
    values = vis_config$correlation_types,
    allowed = c("rank", "pearson"),
    setting_name = "config$visualisations$correlation_types"
  )

  validate_character_choices(
    values = vis_config$network_bias_metrics,
    allowed = c("dcent", "clustering", "APL"),
    setting_name = "config$visualisations$network_bias_metrics"
  )

  validate_character_choices(
    values = vis_config$network_bias_types,
    allowed = c("relative", "absolute"),
    setting_name = "config$visualisations$network_bias_types"
  )
  # Validate spotlight_pct values
  if (
    !is.numeric(vis_config$main_spotlight_pct) ||
    length(vis_config$main_spotlight_pct) != 1L ||
    !is.finite(vis_config$main_spotlight_pct) ||
    vis_config$main_spotlight_pct <= 0 ||
    vis_config$main_spotlight_pct > 1
  ) {
    stop(
      "`config$visualisations$main_spotlight_pct` must be one finite numeric ",
      "value greater than 0 and no greater than 1."
    )
  }
  # Validate alpha values
  if (!is.null(vis_config$alphas_to_plot)) {
    if (
      !is.numeric(vis_config$alphas_to_plot) ||
      length(vis_config$alphas_to_plot) == 0L ||
      anyNA(vis_config$alphas_to_plot) ||
      any(!is.finite(vis_config$alphas_to_plot)) ||
      any(vis_config$alphas_to_plot < 0) ||
      anyDuplicated(vis_config$alphas_to_plot)
    ) {
      stop(
        "`config$visualisations$alphas_to_plot` must be NULL or a non-empty ",
        "numeric vector of unique, finite, non-negative values."
      )
    }
  }
  # validate interpolation settings
  if (
    !is.numeric(vis_config$interpolation_grid_n) ||
    length(vis_config$interpolation_grid_n) != 1L ||
    !is.finite(vis_config$interpolation_grid_n) ||
    vis_config$interpolation_grid_n < 2 ||
    vis_config$interpolation_grid_n %% 1 != 0
  ) {
    stop(
      "`config$visualisations$interpolation_grid_n` must be one integer ",
      "greater than or equal to 2."
    )
  }

  logical_settings <- c(
    "show_coverage_values",
    "show_top_n_alignment",
    "save_plots",
    "save_supplementary_coverage"
  )

  invalid_logical_settings <- logical_settings[
    !vapply(
      vis_config[logical_settings],
      function(x) {
        is.logical(x) && length(x) == 1L && !is.na(x)
      },
      logical(1)
    )
  ]

  if (length(invalid_logical_settings) > 0L) {
    stop(
      "These visualisation settings must each be TRUE or FALSE: ",
      paste(invalid_logical_settings, collapse = ", ")
    )
  }

  invisible(vis_config)
}


validate_visualisation_data <- function(
    df,
    data_name,
    required_columns
) {
  if (!is.data.frame(df)) {
    stop("`", data_name, "` must be a data frame.")
  }

  if (nrow(df) == 0L) {
    stop("`", data_name, "` contains no rows.")
  }

  missing_columns <- setdiff(
    required_columns,
    names(df)
  )

  if (length(missing_columns) > 0L) {
    stop(
      "`",
      data_name,
      "` is missing required columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }

  invisible(df)
}

# Important - due to the nature of the simulation some deviation around 
# expected values is inevitable. Filter values which are near, not exact
numeric_selection_mask <- function(values, selected_values) {
  vapply(
    values,
    function(value) {
      any(dplyr::near(value, selected_values))
    },
    logical(1)
  )
}


resolve_numeric_selection <- function(
    requested_values,
    available_values,
    setting_name
) {
  available_values <- sort(unique(available_values))

  if (
    !is.numeric(available_values) ||
    length(available_values) == 0L ||
    anyNA(available_values) ||
    any(!is.finite(available_values))
  ) {
    stop(
      "Cannot resolve `",
      setting_name,
      "` because the available database values are invalid."
    )
  }

  if (is.null(requested_values)) {
    return(available_values)
  }

  unavailable_values <- requested_values[
    !vapply(
      requested_values,
      function(value) {
        any(dplyr::near(value, available_values))
      },
      logical(1)
    )
  ]

  if (length(unavailable_values) > 0L) {
    stop(
      "`",
      setting_name,
      "` requests values not present in the queried database: ",
      paste(unavailable_values, collapse = ", "),
      ". Available values are: ",
      paste(available_values, collapse = ", "),
      "."
    )
  }

  requested_values
}


resolve_character_selection <- function(
    requested_values,
    available_values,
    setting_name
) {
  unavailable_values <- setdiff(
    requested_values,
    unique(available_values)
  )

  if (length(unavailable_values) > 0L) {
    stop(
      "`",
      setting_name,
      "` requests values not present in the queried database: ",
      paste(unavailable_values, collapse = ", "),
      ". Available values are: ",
      paste(sort(unique(available_values)), collapse = ", "),
      "."
    )
  }

  requested_values
}


ordered_numeric_factor <- function(values, decreasing = TRUE) {
  numeric_values <- as.numeric(as.character(values))

  if (anyNA(numeric_values) || any(!is.finite(numeric_values))) {
    stop("Cannot create ordered numeric labels from non-numeric values.")
  }

  formatted_values <- format(
    numeric_values,
    trim = TRUE,
    scientific = FALSE
  )

  formatted_levels <- format(
    sort(unique(numeric_values), decreasing = decreasing),
    trim = TRUE,
    scientific = FALSE
  )

  factor(
    formatted_values,
    levels = formatted_levels
  )
}


make_spotlight_pct_slug <- function(value) {
  formatted_value <- format(
    100 * value,
    trim = TRUE,
    scientific = FALSE
  )

  paste0(
    gsub(".", "p", formatted_value, fixed = TRUE),
    "pct"
  )
}


make_top_n_label <- function(top_n_proportion) {
  paste0(
    "Top-",
    scales::percent(
      top_n_proportion,
      accuracy = 1
    )
  )
}


surface_has_contour <- function(
    df,
    x_column,
    y_column,
    value_column,
    group_columns,
    contour_level
) {
  group_key <- interaction(
    df[group_columns],
    drop = TRUE,
    lex.order = TRUE
  )

  surfaces <- split(df, group_key)

  any(
    vapply(
      surfaces,
      function(surface_df) {
        x_values <- sort(unique(surface_df[[x_column]]))
        y_values <- sort(unique(surface_df[[y_column]]))

        if (length(x_values) < 2L || length(y_values) < 2L) {
          return(FALSE)
        }

        z_matrix <- matrix(
          NA_real_,
          nrow = length(x_values),
          ncol = length(y_values)
        )

        z_matrix[
          cbind(
            match(surface_df[[x_column]], x_values),
            match(surface_df[[y_column]], y_values)
          )
        ] <- surface_df[[value_column]]

        if (anyNA(z_matrix) || any(!is.finite(z_matrix))) {
          return(FALSE)
        }

        length(
          suppressWarnings(
            grDevices::contourLines(
              x = x_values,
              y = y_values,
              z = z_matrix,
              levels = contour_level
            )
          )
        ) > 0L
      },
      logical(1)
    )
  )
}


save_visualisation_pdf <- function(
    plot,
    filename,
    width_mm,
    height_mm
) {
  output_directory <- dirname(filename)

  dir.create(
    output_directory,
    recursive = TRUE,
    showWarnings = FALSE
  )

  ggplot2::ggsave(
    filename = filename,
    plot = plot,
    device = grDevices::cairo_pdf,
    width = width_mm,
    height = height_mm,
    units = "mm",
    bg = "white"
  )

  normalizePath(
    filename,
    winslash = "/",
    mustWork = TRUE
  )
}
