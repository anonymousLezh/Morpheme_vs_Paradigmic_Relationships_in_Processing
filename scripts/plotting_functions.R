#######
# Anonymous
# Generate AIC comparison Plot

# Input:  aic_values: c() of aic values to be compared
#         model_names: c() of strings for labels for graph, must correspond with 
#                   aic_values order
#         input_title: Title for the graph, will be appended to ": Pairwise AIC Comparison"
#         maxInt: (optional) maximum integer for the plot. 

# Output: Graph with pairwise comparisons that treats any AIC value <10 as blank. 


# Define the function
generate_AIC_comparison_plot <- function(aic_values, model_names, input_title, maxInt = Inf, output_file = NULL) {
  # Create dataframe with AIC values
  comparisons <- data.frame(
    model_name = model_names,
    AIC_value = aic_values
  )
  
  # Calculate differences in AIC values
  AIC_diff <- outer(comparisons$AIC_value, comparisons$AIC_value, "-")
  
  # Create a matrix with model names as row and column names
  rownames(AIC_diff) <- comparisons$model_name
  colnames(AIC_diff) <- comparisons$model_name
  
  # Convert the matrix to a dataframe
  AIC_diff_df <- as.data.frame(as.table(AIC_diff))
  names(AIC_diff_df) <- c("Model1", "Model2", "Difference")
  
  print(AIC_diff_df)
  # Neutralize non-significant differences
  AIC_diff_df <- AIC_diff_df %>%
    mutate(Difference = ifelse(Difference > -10 & Difference < 10, 0, Difference))
  
  # Ensure factor levels are consistent
  AIC_diff_df$Model1 <- factor(AIC_diff_df$Model1, levels = model_names)
  AIC_diff_df$Model2 <- factor(AIC_diff_df$Model2, levels = model_names)
  
  # Make title:
  input_title = paste(input_title, "")
  
  # Plot the pairwise comparison of AIC values
  plot <- ggplot(AIC_diff_df, aes(x = Model1, y = Model2, fill = Difference)) +
    geom_tile() +
    scale_fill_gradientn(colors = c("red", "white", "blue"),
                         values = scales::rescale(c(-25, -10, 0, 10, 25)),
                         breaks = c(-25, -10, 10, 25),
                         limits = c(-25, 25),
                         guide = "colorbar") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11)) +
    theme(axis.text.y = element_text(angle = 45, hjust = 1, size = 11)) +
    theme(plot.title = element_text(hjust = 0.5),
          plot.title.position = "panel",
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA)) +
    labs(title = input_title,
         x = "Model 1", y = "Model 2",
         fill = "AIC Difference")
  
  return(plot)
}

# Same as generate_AIC_comparison_plot, but labels each cell with the raw Delta AIC.
generate_AIC_comparison_plot_val <- function(aic_values, model_names, input_title, maxInt = Inf, output_file = NULL) {
  comparisons <- data.frame(
    model_name = model_names,
    AIC_value = aic_values
  )

  AIC_diff <- outer(comparisons$AIC_value, comparisons$AIC_value, "-")

  rownames(AIC_diff) <- comparisons$model_name
  colnames(AIC_diff) <- comparisons$model_name

  AIC_diff_df <- as.data.frame(as.table(AIC_diff))
  names(AIC_diff_df) <- c("Model1", "Model2", "Difference")

  print(AIC_diff_df)

  AIC_diff_df <- AIC_diff_df %>%
    mutate(
      Difference_raw = Difference,
      Difference_fill = ifelse(
        Difference > -10 & Difference < 10, 0,
        ifelse(Difference > 25, 25,
               ifelse(Difference < -25, -25, Difference))
      ),
      label = ifelse(
        abs(Difference_raw) < 0.05,
        "0",
        sprintf("%.1f", Difference_raw)
      ),
      label_color = ifelse(Difference_raw > -10 & Difference_raw < 10, "black", "white")
    )

  AIC_diff_df$Model1 <- factor(AIC_diff_df$Model1, levels = model_names)
  AIC_diff_df$Model2 <- factor(AIC_diff_df$Model2, levels = model_names)

  input_title = paste(input_title, "")

  plot <- ggplot(AIC_diff_df, aes(x = Model1, y = Model2, fill = Difference_fill)) +
    geom_tile() +
    geom_text(aes(label = label, color = label_color), size = 3.5, show.legend = FALSE) +
    scale_fill_gradientn(colors = c("red", "white", "blue"),
                         values = scales::rescale(c(-25, -10, 0, 10, 25)),
                         breaks = c(-25, -10, 10, 25),
                         limits = c(-25, 25),
                         oob = scales::squish,
                         guide = "colorbar") +
    scale_color_identity() +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11)) +
    theme(axis.text.y = element_text(angle = 45, hjust = 1, size = 11)) +
    theme(plot.title = element_text(hjust = 0.5),
          plot.title.position = "panel",
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA)) +
    labs(title = input_title,
         x = "Model 1", y = "Model 2",
         fill = "AIC Difference")

  return(plot)
}

# Input:  pca_results: prcomp object (e.g. ThreeSuff_TPFull_TPPCA_results) or list with
#                   $rotation / $pca_object from run_pca()
#         variable_colors: c() of hex color strings, one per variable (same order as rotation rows)
#         variable_names: c() of display labels for variables (same order as rotation rows)
#         input_title: plot title string
#
# Output: ggplot faceted by component (PC1, PC2, ...), variables on x-axis, y-axis -1 to 1
generate_PCA_loadings_plot <- function(pca_results, variable_colors, variable_names, input_title) {
  if (inherits(pca_results, "prcomp")) {
    loadings <- pca_results$rotation
  } else if (!is.null(pca_results$rotation)) {
    loadings <- pca_results$rotation
  } else if (!is.null(pca_results$pca_object)) {
    loadings <- pca_results$pca_object$rotation
  } else {
    stop("pca_results must be a prcomp object or contain a rotation matrix.")
  }

  component_keys <- colnames(loadings)
  variable_keys <- rownames(loadings)
  n_variables <- length(variable_keys)
  component_labels <- paste0("PC", seq_along(component_keys))

  if (length(variable_colors) != n_variables) {
    stop(
      "variable_colors must have one hex value per variable (expected ",
      n_variables, ", got ", length(variable_colors), ")."
    )
  }

  if (length(variable_names) != n_variables) {
    stop(
      "variable_names must have one label per variable (expected ",
      n_variables, ", got ", length(variable_names), ")."
    )
  }

  names(variable_colors) <- variable_names

  loadings_df <- loadings %>%
    as.data.frame() %>%
    tibble::rownames_to_column("variable_key") %>%
    tidyr::pivot_longer(
      cols = -variable_key,
      names_to = "component_key",
      values_to = "loading"
    ) %>%
    mutate(
      variable = factor(
        variable_names[match(variable_key, variable_keys)],
        levels = variable_names
      ),
      component = factor(
        component_labels[match(component_key, component_keys)],
        levels = component_labels
      ),
      loading_label = sprintf("%.2f", loading)
    )

  plot <- ggplot(loadings_df, aes(x = variable, y = loading, fill = variable)) +
    geom_col(width = 0.7) +
    geom_hline(yintercept = 0, size = 0.3, color = "gray40") +
    geom_text(
      aes(
        label = loading_label,
        y = loading + ifelse(loading >= 0, 0.03, -0.03),
        vjust = ifelse(loading >= 0, 0, 1)
      ),
      size = 3.5,
      show.legend = FALSE
    ) +
    facet_wrap(~ component) +
    scale_fill_manual(values = variable_colors, name = "Variable") +
    scale_y_continuous(
      limits = c(-1.2, 1.2),
      breaks = seq(-1, 1, by = 0.5),
      expand = c(0, 0)
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16),
      axis.title = element_text(size = 15),
      axis.text = element_text(size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text = element_text(size = 14),
      panel.background = element_rect(fill = "transparent"),
      plot.background = element_rect(fill = "transparent", color = NA),
      legend.position = "none"
    ) +
    labs(
      title = input_title,
      x = "Variable",
      y = "Relative Loading"
    )

  return(plot)
}