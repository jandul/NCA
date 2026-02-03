# Helper function for producing the cIPMA plot
# Revised January 18, 2026

get_cipma_plot <- function(cipma_df, x_range = NULL, y_range = NULL, size_limits, size_range) {
  
  # Load required libraries
  library(ggplot2)
  library(ggrepel)
  
  # Dynamically determine x_range if not provided
  if (is.null(x_range)) {
    x_range <- range(cipma_df$Importance, na.rm = TRUE)
  }
  
  # Dynamically determine y_range if not provided
  if (is.null(y_range)) {
    y_range <- range(cipma_df$Performance, na.rm = TRUE)
  }
  
  # Extract target Y value from column name
  colname <- colnames(cipma_df)[4]
  target.Y <- target_outcome

  # Set fill color: black only for NN cases
  cipma_df$fill_color <- ifelse(
    grepl("\\(NN\\)", cipma_df$Predictor_with_cases),
    "black",
    NA
  )
  
  # Set dot size: fixed small size for NN, scaled otherwise
  cipma_df$dot_size <- ifelse(
    grepl("\\(NN\\)", cipma_df$Predictor_with_cases),
    1,
    cipma_df[[colname]]
  )
  
  mean_importance <- mean(cipma_df$Importance, na.rm = TRUE)
  mean_performance <- mean(cipma_df$Performance, na.rm = TRUE)
  
  # Generate the plot
  pc <- ggplot(cipma_df, aes(x = Importance, y = Performance)) +
    # Mean reference lines
    geom_vline(xintercept = mean_importance, linetype = "dashed", linewidth = 0.4) +
    geom_hline(yintercept = mean_performance, linetype = "dashed", linewidth = 0.4) +
    
    geom_point(
      aes(size = dot_size),
      shape = 21,
      fill = cipma_df$fill_color,
      color = "black",
      stroke = 0.7
    ) +
    
    
    
    geom_text_repel(
      aes(label = Predictor_with_cases),
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
    
    
    # geom_text_repel(
    #   aes(label = Predictor_with_cases),
    #   size = 3.5,
    #   box.padding = 2
    # ) +
    # 
    
    coord_cartesian(xlim = x_range, ylim = y_range) +
    scale_x_continuous(limits = x_range) +
    scale_y_continuous(limits = y_range) +
    scale_size_continuous(limits = size_limits, range = size_range) +
    labs(
      title = paste("cIPMA for target outcome Y =", target.Y, " ", name_plot),
      x = "Importance",
      y = "Performance"
    ) +
    theme_minimal() +
    guides(size = "none", fill = "none")  # REMOVE LEGEND
  
  # Display the plot
  print(pc)
  
  # Save the plot
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  filename <- paste0("cipma_", timestamp, ".pdf")
  ggsave(filename, pc, width = 5, height = 5, dpi = 600, device = cairo_pdf)
  
}

