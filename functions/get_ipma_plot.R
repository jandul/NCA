# Helper function to create a IPMA plot
# Revised January 18, 2026

# Load required libraries
library(ggplot2)
library(ggrepel)

get_ipma_plot <- function(ipma_df, 
                          x_range = NULL,
                          y_range = NULL) {
  
  # Dynamically set x_range if not provided
  if (is.null(x_range)) {
    x_range <- range(ipma_df$Importance, na.rm = TRUE)
  }  
  
  # Dynamically set y_range if not provided
  if (is.null(y_range)) {
    y_range <- range(ipma_df$Performance, na.rm = TRUE)
  }  
  
  mean_importance <- mean(ipma_df$Importance, na.rm = TRUE)
  mean_performance <- mean(ipma_df$Performance, na.rm = TRUE)
  
  # Generate the IPMA plot
  p <- ggplot(ipma_df, aes(x = Importance, y = Performance)) +
    # Mean reference lines
    geom_vline(xintercept = mean_importance, linetype = "dashed", linewidth = 0.4) +
    geom_hline(yintercept = mean_performance, linetype = "dashed", linewidth = 0.4) +
    
    geom_point(color = "black", size = 5) +  # Single point layer
    geom_text_repel(
      aes(label = predictor),
      size = 3.5,
      box.padding = 2 
    ) +
    coord_cartesian(xlim = x_range, ylim = y_range) +  
    scale_x_continuous(limits = x_range) +  # Adapt x-axis limits dynamically
    scale_y_continuous(limits = y_range) +  # Adapt y-axis limits dynamically
    labs(
      title = "classic IPMA",
      x = "Importance",
      y = "Performance"
    ) +
    theme_minimal()  # Use a clean theme
  
  # Display the plot
  print(p)
  
  # Save the plot
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  filename <- paste0("ipma_", timestamp, ".pdf")
  ggsave(filename, p, width = 5, height = 5, dpi = 600, device = cairo_pdf)
}
