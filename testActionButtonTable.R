library(ggplot2)
library(dplyr)

# Sample Data (Replace this with actual dataset)
data <- data.frame(
  Month = rep(c("Jan-25", "Dec-24", "Nov-24"), each = 5),  
  Location = rep(c("Afgoye", "Afmadow", "Cadado", "Baidoa Greco", "Baidoa NRC"), times = 3),
  Status = c("Alert", "Alert", "Alarm", "Alert", "Alert",
             "Alert", "Alert", "Alert", "Alarm", "Alert",
             "Normal", "Alert", "Alert", "Alert", "Alert")
)

# Define color mapping
status_colors <- c("Normal" = "green", "Alert" = "yellow", "Alarm" = "red")

# Plot
ggplot(data, aes(x = Location, y = Month, fill = Status)) +
  geom_tile(color = "black") +  # Creates the table grid
  geom_text(aes(label = Status), size = 5) +  # Display text inside the cells
  scale_fill_manual(values = status_colors) +  # Apply custom colors
  scale_x_discrete(position = "top") +  # Move x-axis labels to the top
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12, face = "bold", angle = 90, hjust = 0),  # Rotate labels for readability
    axis.text.y = element_text(size = 12, face = "bold"),  # Rotate labels for readability
    axis.title = element_blank(),  # Remove axis titles
    panel.grid = element_blank(),  # Remove background grid
    legend.position = "none"  # Hide legend (optional)
  )
