# Helper function for producing the BIPMA plot
# Revised January 18, 2026

get_bipma_plot <- function(bipma_df, x_range = NULL, y_range = NULL, size_limits, size_range) {
  
  library(ggplot2)
  library(ggrepel)
  
  # For visibility of overlapping filled points
  bipma_df <- bipma_df[order(bipma_df$Single_bottleneck_cases_num, decreasing = TRUE), ]
  
  # Dynamically determine x_range if not provided
  if (is.null(x_range)) {
    x_range <- range(bipma_df$Importance, na.rm = TRUE)
  }
  
  # Dynamically determine y_range if not provided
  if (is.null(y_range)) {
    y_range <- range(bipma_df$Performance, na.rm = TRUE)
  }
  
  # Extract the bottleneck column name
  colname <- colnames(bipma_df)[4]
  target.Y <- target_outcome
  
  bipma_df$fill_color <- ifelse(
    grepl("\\(NN\\)", bipma_df$Predictor_with_single_cases),
    "black",
    NA_character_
  )
  bipma_df$fill_color[is.na(bipma_df$fill_color) & bipma_df$Sufficiency == "no"] <- "grey90"
  bipma_df$fill_color[is.na(bipma_df$fill_color)] <- "white"
  
  # Define dot size: fixed small size for NN, scaled otherwise
  bipma_df$dot_size <- ifelse(
    grepl("\\(NN\\)", bipma_df$Predictor_with_single_cases),
    1,
    bipma_df[[colname]]
  )
  
  # Adjust importance for plotting
  x_no <- -0.1  # x-position for "no sufficiency" predictors
  bipma_df$Importance_adj <- ifelse(
    bipma_df$Sufficiency == "no",
    x_no,
    bipma_df$Importance
  )
  
  # Define x-range (include the no sufficiency zone to give space for points)
  x_range_adj <- c(x_no - 0.02 , max(bipma_df$Importance, na.rm = TRUE) + 0.1)
  x_breaks <- pretty(bipma_df$Importance)
  
  ## 1) keep the NN & NS names BEFORE removing them
  dropped_idx <- (bipma_df$Necessity == "no" & bipma_df$Sufficiency == "no")
  dropped_names <- bipma_df$predictor[dropped_idx]
  
  # Remove predictors that are not necessary and not sufficient
  bipma_df <- bipma_df[!dropped_idx, ]
  
  
  # Mean Importance for the vertical line:
  # exclude the "no sufficiency" predictors from the mean calculation
  mean_importance <- mean(bipma_df$Importance[bipma_df$Sufficiency != "no"], na.rm = TRUE)
  
  # Mean Performance for the horizontal line (usually across all shown points)
  mean_performance <- mean(bipma_df$Performance, na.rm = TRUE)

  
  # Plot
  pc1 <- ggplot(bipma_df, aes(x = Importance_adj, y = Performance)) +
    # Mean reference lines
    geom_vline(xintercept = mean_importance, linetype = "dashed", linewidth = 0.4) +
    geom_hline(yintercept = mean_performance, linetype = "dashed", linewidth = 0.4) +
    
    #add separation line
    geom_vline(xintercept = -0.02, linetype = "dashed", linewidth = 0.7) +
    
    geom_point(
      aes(size = dot_size, fill = fill_color),
      shape = 21,
      color = "black",
      stroke = 0.7
    ) +
    scale_fill_identity() +
    
    geom_text_repel(
      aes(label = Predictor_with_single_cases),
      size = 3.5,
      box.padding = 1.2, #spacing
      #point.padding = 2.2, 
      force = 5, # allow labels to move further away
      force_pull = 3, # allow labels to move further away
      max.iter = 50000, # more work to find a non-overlapping layout
      #max.time = 2,  # allow more candidate positions
      seed = 123,
      min.segment.length = 0,  # keep labels from overlapping each other completely
    ) +
    
    coord_cartesian(xlim = x_range_adj, ylim = y_range) +
    scale_x_continuous(
      breaks = x_breaks,
      limits = x_range_adj
    ) +
    scale_y_continuous(limits = y_range) +
    scale_size_continuous(limits = size_limits, range = size_range) +
    
    labs(
      title = paste("BIPMA for target outcome Y =", target.Y, name_plot),
      x = "Importance",
      y = "Performance"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(),
      axis.ticks.x = element_line(),
      panel.grid.minor = element_blank()
    ) +
    guides(size = "none")
  
  ## 2) add the textbox in the lower-left corner (only if there were dropped ones)
  if (length(dropped_names) > 0) {
    pc1 <- pc1 +
      annotate(
        "label",
        x = x_range_adj[1] + 0.01,     # a little inside the panel
        y = y_range[1] + 0.01,
        hjust = 0,
        vjust = 0,
        label = paste(
          c("Non-significant necessity and average effects:", dropped_names),
          collapse = "\n"
        ),
        size = 3
      )
  }
  
  # Display the plot
  print(pc1)
  
  # Save the plot
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  filename <- paste0("bipma_", timestamp, ".pdf")
  ggsave(filename, pc1, width = 5, height = 5, dpi = 600, device = cairo_pdf)
  
}
