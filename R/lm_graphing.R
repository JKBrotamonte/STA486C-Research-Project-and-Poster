library(ggplot2)
library(gridExtra)
library(scales)
library(grid)
library(flextable)
library(webshot2)

load("cleaned_data.rdata")

# Color palette
colors <- c("#375A80", "#CC7524", "#B14044", "#5C9792", "#488C3F", "#C6A63A", "#8E617F")

# Function to create scatter plots with regression line
create_plot <- function(data, x_var, y_var, x_label, y_label, color, use_log_scale = FALSE) {
  plot <- ggplot(data, aes_string(x = x_var, y = y_var)) +
    geom_point(alpha = 0.6, color = color) +
    geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed", size = 0.5) +
    geom_hline(yintercept = 1000000, color = "#E15759", size = 1) +
    labs(title = x_label, x = NULL, y = y_label) +
    theme_bw() +
    theme(
      panel.grid.major = element_line(color = "gray90", size = 0.3),
      panel.grid.minor = element_line(color = "gray95", size = 0.2),
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.position = "none"
    )

  if (use_log_scale) {
    plot <- plot +
      scale_x_log10(labels = comma, breaks = trans_breaks("log10", function(x) 10^x)) +
      scale_y_log10(labels = comma, breaks = trans_breaks("log10", function(x) 10^x))
  } else {
    plot <- plot +
      scale_x_continuous(labels = comma) +
      scale_y_log10(labels = comma, breaks = trans_breaks("log10", function(x) 10^x))
  }

  return(plot)
}

# Create individual plots
plot1 <- create_plot(merged_data, "Total_Likes", "Likes", "Total Likes", "Likes", colors[1], use_log_scale = TRUE)
plot2 <- create_plot(merged_data, "Comments", "Likes", "Comments", "Likes", colors[2], use_log_scale = TRUE)
plot3 <- create_plot(merged_data, "Shares", "Likes", "Shares", "Likes", colors[3], use_log_scale = TRUE)
plot4 <- create_plot(merged_data, "Saves", "Likes", "Saves", "Likes", colors[4], use_log_scale = TRUE)
plot5 <- create_plot(merged_data, "Following", "Likes", "Following", "Likes", colors[5])
plot6 <- create_plot(merged_data, "Followers", "Likes", "Followers", "Likes", colors[6], use_log_scale = TRUE)
plot7 <- create_plot(merged_data, "Length", "Likes", "Length (seconds)", "Likes", colors[7])

# Arrange plots
arranged_plots <- arrangeGrob(plot1, plot2, plot3, plot4, plot5, plot6, plot7, ncol = 3, widths = c(1, 1, 1), heights = c(1, 1, 1))

# Add title and caption
title <- textGrob("Factors Influencing TikTok Video Likes", gp = gpar(fontsize = 16, fontface = "bold"))
caption <- textGrob("Figure 1. Scatter plots illustrating the relationship between various factors and TikTok video likes, with regression lines.\nHere the dashed line represents the fitted regresssion line. While the red horizontal line marks the 1 Million likes point as a marker for viral videos.",
                    gp = gpar(fontsize = 10, fontface = "italic"),
                    hjust = 0.5, x = 0.5, y = .8)

# Create the final plot layout
final_plot <- arrangeGrob(title, arranged_plots, caption, heights = c(0.1, 0.8, 0.1))

# Display the final plot
grid.draw(final_plot)

# Save the final plot (lm graphing) as a high-resolution TIFF image
ggsave("lm_graphing_plot.tiff", plot = final_plot, device = "tiff", width = 18, height = 7, units = "in", dpi = 300)

